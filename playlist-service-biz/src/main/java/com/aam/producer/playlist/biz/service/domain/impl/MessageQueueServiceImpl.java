package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.lib.protocol.TaskDTO;
import com.aam.producer.playlist.biz.service.domain.IMessageQueueService;
import com.aam.producer.playlist.protocol.message.PlaylistDataDTO;
import com.aam.producer.playlist.protocol.message.PosMappingBatchRequestDTO;
import com.aam.producer.playlist.protocol.message.PplContentsDataDTO;
import com.aam.producer.playlist.protocol.message.ProducerPlaylistChangedDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistDeleteRequestDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistDeletionDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistHashSyncRequestDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistSendRequestDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistSyncRequestDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistWarningDTO;
import com.aam.producer.playlist.protocol.request.PosDataDTO;
import com.aam.producer.playlist.protocol.request.PosFetchDTO;
import com.aam.producer.task.protocol.request.SyncReportModel;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.serializer.SerializerFeature;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Service;

/**
 * schema service impl
 *
 * @author oliver.lo
 * @since 2019/4/11 5:04 PM
 */
@Service
public class MessageQueueServiceImpl implements IMessageQueueService {

    private static final Logger logger = LoggerFactory.getLogger(MessageQueueServiceImpl.class);
    private final KafkaTemplate<String, String> kafkaTemplate;
    @Value("${kafka.topics.playlistHashSyncRequest}")
    private String playlistHashSyncRequest;
    @Value("${kafka.topics.playlistSyncRequest}")
    private String playlistSyncRequest;
    @Value("${kafka.topics.playlistSendRequest}")
    private String playlistSendRequest;
    @Value("${kafka.topics.playlistDeletionData}")
    private String playlistDeletionData;
    @Value("${kafka.topics.playlistWarningData}")
    private String playlistWarningData;
    @Value("${kafka.topics.posMappingRequest}")
    private String posMappingRequest;
    @Value("${kafka.topics.posMatchData}")
    private String posMatchData;
    @Value("${kafka.topics.pplContentsData}")
    private String pplContentsData;
    @Value("${kafka.topics.pplChangedData}")
    private String pplChangedData;
    @Value("${kafka.topics.posFetch}")
    private String posFetch;
    @Value("${kafka.topics.playlistDeleteRequest}")
    private String playlistDeleteRequest;
    @Value("${kafka.topics.tplPosMatchedData}")
    private String tplPosMatchedData;
    @Value("${kafka.topics.playlistData}")
    private String playlistData;

    @Autowired
    public MessageQueueServiceImpl(KafkaTemplate<String, String> kafkaTemplate) {
        this.kafkaTemplate = kafkaTemplate;
    }

    @Override
    public void playlistHashSyncRequest(TPlaylistHashSyncRequestDTO dto) {
        TaskDTO<TPlaylistHashSyncRequestDTO> data = new TaskDTO<>(playlistHashSyncRequest, dto);
        kafkaTemplate.send(playlistHashSyncRequest, toJsonString(data));
        logger.info("<{}> have sent,playlist_uuid:<{}>,complex_uuid:<{}>", playlistHashSyncRequest,
                JSON.toJSONString(dto.getPlaylistIds()), dto.getComplexId());
    }

    @Override
    public void playlistSyncRequest(TPlaylistSyncRequestDTO dto) {
        TaskDTO<TPlaylistSyncRequestDTO> data = new TaskDTO<>(playlistSyncRequest, dto);
        kafkaTemplate.send(playlistSyncRequest, toJsonString(data));
        logger.info("<{}> have sent,playlist_uuid:<{}>,complex_uuid:<{}>", playlistSyncRequest,
                dto.getPlaylistUuid(), dto.getComplexId());
    }

    @Override
    public void playlistSendRequest(TPlaylistSendRequestDTO dto) {
        TaskDTO<TPlaylistSendRequestDTO> data = new TaskDTO<>(playlistSendRequest, dto);
        kafkaTemplate.send(playlistSendRequest, toJsonString(data));
        logger.info("<{}> have sent,playlist_uuid:<{}>,complex_uuid:<{}>,receipt_uuid:<{}>",
                playlistSendRequest, dto.getPlaylist().getUuid(), dto.getComplexUuid(),
                dto.getReceiptUuid());
    }

    @Override
    public void playlistDeletionData(TPlaylistDeletionDTO dto) {
        TaskDTO<TPlaylistDeletionDTO> data = new TaskDTO<>(playlistDeletionData, dto);
        kafkaTemplate.send(playlistDeletionData, toJsonString(data));
        logger.info("<{}> have sent,deletion:<{}>", playlistDeletionData, dto);
    }

    @Override
    public void playlistWarningData(TPlaylistWarningDTO dto) {
        TaskDTO<TPlaylistWarningDTO> data = new TaskDTO<>(playlistWarningData, dto);
        kafkaTemplate.send(playlistWarningData, toJsonString(data));
        logger.info("<{}> have sent,detail:<{}>", playlistWarningData, dto);
    }

    @Override
    public void segmentWarningData(SyncReportModel dto) {
        TaskDTO<SyncReportModel> data = new TaskDTO<>(playlistWarningData, dto);
        kafkaTemplate.send(playlistWarningData, toJsonString(data));
        logger.info("<{}> have sent,detail:<{}>", playlistWarningData, JSON.toJSONString(dto));
    }

    @Override
    public void posMappingRequest(PosMappingBatchRequestDTO dto) {
        TaskDTO<PosMappingBatchRequestDTO> data = new TaskDTO<>(posMappingRequest, dto);
        kafkaTemplate.send(posMappingRequest, toJsonString(data));
        logger.info("<{}> have sent,receipt_uuid:<{}>,complex_uuid:<{}>,mappings:<{}>",
                posMappingRequest, dto.getReceiptUuid(), dto.getComplexUuid(), dto.getMappings());
    }

    @Override
    public void posMatchData(PosDataDTO dto) {
        TaskDTO<PosDataDTO> data = new TaskDTO<>(posMatchData, dto);
        kafkaTemplate.send(posMatchData, toJsonString(data));
        logger.info(
                "<{}> have sent,pos_uuid:<{}>,ppl_uuid:<{}>,tpl_uuid:<{}>,state:<{}>,mapping_in_sys:<{}>,automatic:<{}>",
                posMatchData, dto.getPosUuid(), dto.getPplUuid(), dto.getTplUuid(), dto.getState(),
                dto.getMappingInSystem(), dto.getPplAutomatic());
    }

    @Override
    public void pplContentsData(PplContentsDataDTO dto) {
        TaskDTO<PplContentsDataDTO> data = new TaskDTO<>(pplContentsData, dto);
        kafkaTemplate.send(pplContentsData, toJsonString(data));
        logger.info("<{}> have sent,contents:<{}>", pplContentsData, dto);
    }

    @Override
    public void producerPlaylistChangedData(ProducerPlaylistChangedDTO dto) {
        TaskDTO<ProducerPlaylistChangedDTO> data = new TaskDTO<>(pplChangedData, dto);
        kafkaTemplate.send(pplChangedData, toJsonString(data));
        logger.info("<{}> have sent,ppl changed:<{}>", pplChangedData, dto);
    }

    @Override
    public void posFetch(PosFetchDTO dto) {
        TaskDTO<PosFetchDTO> data = new TaskDTO<>(posFetch, dto);
        kafkaTemplate.send(posFetch, toJsonString(data));
        logger.info("<{}> have sent,fetch pos:<{}>", posFetch, dto);
    }

    @Override
    public void playlistDeleteRequest(TPlaylistDeleteRequestDTO dto) {
        TaskDTO<TPlaylistDeleteRequestDTO> data = new TaskDTO<>(playlistDeleteRequest, dto);
        kafkaTemplate.send(playlistDeleteRequest, toJsonString(data));
        logger.info("<{}> have sent,delete request:<{}>", playlistDeleteRequest, dto);
    }

    @Override
    public void tplPosMatchedData(Map<String, List<String>> map) {
        TaskDTO<Map<String, List<String>>> data = new TaskDTO<>(tplPosMatchedData, map);
        kafkaTemplate.send(tplPosMatchedData, toJsonString(data));
        logger.info("<{}> have sent,matched:<{}>", tplPosMatchedData, map);
    }

    @Override
    public void playlistData(PlaylistDataDTO dto) {
        TaskDTO<PlaylistDataDTO> data = new TaskDTO<>(playlistData, dto);
        kafkaTemplate.send(playlistData, toJsonString(data));
        logger.info("<{}> have sent,data request:<{}>", playlistData, dto);
    }

    private String toJsonString(TaskDTO data) {
        return JSON.toJSONString(data, SerializerFeature.WriteMapNullValue);
    }
}
