/*
 * Immersive Audio Model and Formats demuxer
 * Copyright (c) 2023 James Almer <jamrial@gmail.com>
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "libavutil/avassert.h"
#include "libavutil/intreadwrite.h"
#include "libavutil/mem.h"
#include "avformat.h"
#include "demux.h"
#include "iamf.h"
#include "iamf_reader.h"
#include "iamf_parse.h"
#include "internal.h"
#include "avio_internal.h"

//return < 0 if we need more data
static int get_score(const uint8_t *buf, int buf_size, enum IAMF_OBU_Type type, int *seq)
{
    if (type == IAMF_OBU_IA_SEQUENCE_HEADER) {
        if (buf_size < 4 || AV_RB32(buf) != MKBETAG('i','a','m','f'))
            return 0;
        *seq = 1;
        return -1;
    }
    if (type >= IAMF_OBU_IA_CODEC_CONFIG && type <= IAMF_OBU_IA_TEMPORAL_DELIMITER)
        return *seq ? -1 : 0;
    if (type >= IAMF_OBU_IA_AUDIO_FRAME && type <= IAMF_OBU_IA_AUDIO_FRAME_ID17)
        return *seq ? AVPROBE_SCORE_EXTENSION + 1 : 0;
    return 0;
}

static int iamf_probe(const AVProbeData *p)
{
    unsigned obu_size;
    enum IAMF_OBU_Type type;
    int seq = 0, cnt = 0, start_pos;
    int ret;

    while (1) {
        int size = ff_iamf_parse_obu_header(p->buf + cnt, p->buf_size - cnt,
                                            &obu_size, &start_pos, &type,
                                            NULL, NULL);
        if (size < 0)
            return 0;

        ret = get_score(p->buf + cnt + start_pos,
                        p->buf_size - cnt - start_pos,
                        type, &seq);
        if (ret >= 0)
            return ret;

        cnt += FFMIN(size, p->buf_size - cnt);
    }
    return 0;
}

static int iamf_read_header(AVFormatContext *s)
{
    IAMFDemuxContext *const c = s->priv_data;
    IAMFContext *const iamf = &c->iamf;
    int ret;

    int64_t old_offset = avio_tell(s->pb);

    ret = ff_iamfdec_read_descriptors(iamf, s->pb, INT_MAX, s);
    if (ret < 0)
        return ret;

    int64_t cur_offset = avio_tell(s->pb);
    size_t descriptors_size = cur_offset - old_offset;

    for (int i = 0; i < iamf->nb_audio_elements; i++) {
        IAMFAudioElement *audio_element = iamf->audio_elements[i];
        AVStreamGroup *stg = avformat_stream_group_create(s, AV_STREAM_GROUP_PARAMS_IAMF_AUDIO_ELEMENT, NULL);

        if (!stg)
            return AVERROR(ENOMEM);

        av_iamf_audio_element_free(&stg->params.iamf_audio_element);
        stg->id = audio_element->audio_element_id;
        /* Transfer ownership */
        stg->params.iamf_audio_element = audio_element->element;
        audio_element->element = NULL;

        for (int j = 0; j < audio_element->nb_substreams; j++) {
            IAMFSubStream *substream = &audio_element->substreams[j];
            AVStream *st = avformat_new_stream(s, NULL);

            if (!st)
                return AVERROR(ENOMEM);

            ret = avformat_stream_group_add_stream(stg, st);
            if (ret < 0)
                return ret;

            ret = avcodec_parameters_copy(st->codecpar, substream->codecpar);
            if (ret < 0)
                return ret;

            if (!i && !j && audio_element->layers[0].substream_count == 1)
                st->disposition |= AV_DISPOSITION_DEFAULT;
            else if (audio_element->nb_layers > 1 || audio_element->layers[0].substream_count > 1)
                st->disposition |= AV_DISPOSITION_DEPENDENT;
            st->id = substream->audio_substream_id;
            avpriv_set_pts_info(st, 64, 1, st->codecpar->sample_rate);
       
            // save a copy of the descriptors on the side data for later use when encoding raw IAMF
            if (i == 0 && j == 0) { 
                //we only look at the first stream later
                if (!av_packet_side_data_new(&st->codecpar->coded_side_data,
                    &st->codecpar->nb_coded_side_data,
                    AV_PKT_DATA_IAMF_DESCRIPTORS,
                    descriptors_size, 0)) {
                    return AVERROR(ENOMEM);
                }
                avio_seek(s->pb, old_offset, SEEK_SET);
                ret = avio_read(s->pb, st->codecpar->coded_side_data->data, descriptors_size);
                if (ret != descriptors_size)
                    return ret < 0 ? ret : AVERROR_INVALIDDATA;
                avio_seek(s->pb, cur_offset, SEEK_SET);
            }
        }
    }

    for (int i = 0; i < iamf->nb_mix_presentations; i++) {
        IAMFMixPresentation *mix_presentation = iamf->mix_presentations[i];
        AVStreamGroup *stg = avformat_stream_group_create(s, AV_STREAM_GROUP_PARAMS_IAMF_MIX_PRESENTATION, NULL);
        const AVIAMFMixPresentation *mix = mix_presentation->cmix;

        if (!stg)
            return AVERROR(ENOMEM);

        av_iamf_mix_presentation_free(&stg->params.iamf_mix_presentation);
        stg->id = mix_presentation->mix_presentation_id;
        /* Transfer ownership */
        stg->params.iamf_mix_presentation = mix_presentation->mix;
        mix_presentation->mix = NULL;

        for (int j = 0; j < mix->nb_submixes; j++) {
            const AVIAMFSubmix *sub_mix = mix->submixes[j];

            for (int k = 0; k < sub_mix->nb_elements; k++) {
                const AVIAMFSubmixElement *submix_element = sub_mix->elements[k];
                AVStreamGroup *audio_element = NULL;

                for (int l = 0; l < s->nb_stream_groups; l++)
                    if (s->stream_groups[l]->type == AV_STREAM_GROUP_PARAMS_IAMF_AUDIO_ELEMENT &&
                        s->stream_groups[l]->id == submix_element->audio_element_id) {
                        audio_element = s->stream_groups[l];
                        break;
                    }
                av_assert0(audio_element);

                for (int l = 0; l < audio_element->nb_streams; l++) {
                    ret = avformat_stream_group_add_stream(stg, audio_element->streams[l]);
                    if (ret < 0 && ret != AVERROR(EEXIST))
                        return ret;
                }
            }
        }
    }

    if (!s->nb_streams)
        return AVERROR_INVALIDDATA;

    return 0;
}

static int iamf_read_packet(AVFormatContext *s, AVPacket *pkt)
{
    IAMFDemuxContext *const c = s->priv_data;
    int ret;

    ret = ff_iamf_read_packet(s, c, s->pb, INT_MAX, 0, pkt);
    if (ret < 0)
        return ret;

    return 0;
}

static int iamf_read_close(AVFormatContext *s)
{
    IAMFDemuxContext *const c = s->priv_data;

    ff_iamf_read_deinit(c);

    return 0;
}

int ff_iamf_decode_side_data(AVFormatContext *target, const AVPacketSideData *sd)
{
    IAMFDemuxContext dmux = { 0 };
    FFIOContext b = { 0 };
    AVIOContext *pbc;
    int ret;
    int has_stream_ids = 0;

    if (!sd || sd->type != AV_PKT_DATA_IAMF_DESCRIPTORS)
        return AVERROR(EINVAL);

    ffio_init_read_context(&b, sd->data, sd->size);
    pbc = &b.pub;

    /* Parse descriptors */
    ret = ff_iamfdec_read_descriptors(&dmux.iamf, &b.pub, sd->size, target);
    if (ret < 0) {
        goto fail;
    }
    
    for (int k = 0; k < target->nb_streams; k++) {
        if (target->streams[k]->id != 0) has_stream_ids = 1;
    }

    for (int i = 0; i < dmux.iamf.nb_audio_elements; i++) {
        IAMFAudioElement *audio_element = dmux.iamf.audio_elements[i];
        const AVIAMFAudioElement *element;

        AVStreamGroup *stg =
            avformat_stream_group_create(target, AV_STREAM_GROUP_PARAMS_IAMF_AUDIO_ELEMENT, NULL);
        if (!stg) {
            ret = AVERROR(ENOMEM);
            goto fail;
        }

        av_iamf_audio_element_free(&stg->params.iamf_audio_element);
        stg->id = audio_element->audio_element_id;
        /* Transfer ownership */
        element = stg->params.iamf_audio_element = audio_element->element;
        audio_element->element = NULL;

        for (int j = 0; j < audio_element->nb_substreams; j++) {
            IAMFSubStream *substream = &audio_element->substreams[j];
            AVStream *stream = NULL;
            int found = 0;
            int sid;

            /* Find the output stream by matching the substream id */
            for (int k = 0; k < target->nb_streams; k++) {
                AVStream *ost = target->streams[k];
                int id = has_stream_ids ? ost->id : ost->index;
                if (id == substream->audio_substream_id) {
                    stream = ost;
                    found = 1;
                    break;
                }
            }
            if (!found) {
                continue;
            }

            ret = avcodec_parameters_copy(stream->codecpar, substream->codecpar);
            if (ret < 0)
                goto fail;

            if (!i && !j && audio_element->layers[0].substream_count == 1)
                stream->disposition |= AV_DISPOSITION_DEFAULT;
            else if (audio_element->nb_layers > 1 || audio_element->layers[0].substream_count > 1)
                stream->disposition |= AV_DISPOSITION_DEPENDENT;
            stream->id = substream->audio_substream_id;
            avpriv_set_pts_info(stream, 64, 1, stream->codecpar->sample_rate);

            ret = avformat_stream_group_add_stream(stg, stream);
            if (ret < 0)
                goto fail;
        }
    }

    for (int i = 0; i < dmux.iamf.nb_mix_presentations; i++) {
        IAMFMixPresentation *mix_presentation = dmux.iamf.mix_presentations[i];
        const AVIAMFMixPresentation *mix = mix_presentation->cmix;

        AVStreamGroup *stg =
            avformat_stream_group_create(target, AV_STREAM_GROUP_PARAMS_IAMF_MIX_PRESENTATION, NULL);
        if (!stg) {
            ret = AVERROR(ENOMEM);
            goto fail;
        }

        av_iamf_mix_presentation_free(&stg->params.iamf_mix_presentation);
        stg->id = mix_presentation->mix_presentation_id;
        /* Transfer ownership */
        stg->params.iamf_mix_presentation = mix_presentation->mix;
        mix_presentation->mix = NULL;

        for (int j = 0; j < mix->nb_submixes; j++) {
            const AVIAMFSubmix *submix = mix->submixes[j];

            for (int k = 0; k < submix->nb_elements; k++) {
                const AVIAMFSubmixElement *submix_element = submix->elements[k];
                const AVStreamGroup *audio_element = NULL;

                for (int l = 0; l < target->nb_stream_groups; l++)
                    if (target->stream_groups[l]->type == AV_STREAM_GROUP_PARAMS_IAMF_AUDIO_ELEMENT &&
                        target->stream_groups[l]->id   == submix_element->audio_element_id) {
                        audio_element = target->stream_groups[l];
                        break;
                    }
                av_assert0(audio_element);

                for (int l = 0; l < audio_element->nb_streams; l++) {
                    ret = avformat_stream_group_add_stream(stg, audio_element->streams[l]);
                    if (ret < 0 && ret != AVERROR(EEXIST))
                        goto fail;
                }
            }
        }
    }


fail:
    ff_iamf_uninit_context(&dmux.iamf);
    return ret;
}

const FFInputFormat ff_iamf_demuxer = {
    .p.name         = "iamf",
    .p.long_name    = NULL_IF_CONFIG_SMALL("Raw Immersive Audio Model and Formats"),
    .p.extensions   = "iamf",
    .p.flags        = AVFMT_GENERIC_INDEX | AVFMT_NO_BYTE_SEEK | AVFMT_NOTIMESTAMPS | AVFMT_SHOW_IDS,
    .priv_data_size = sizeof(IAMFDemuxContext),
    .flags_internal = FF_INFMT_FLAG_INIT_CLEANUP,
    .read_probe     = iamf_probe,
    .read_header    = iamf_read_header,
    .read_packet    = iamf_read_packet,
    .read_close     = iamf_read_close,
};
