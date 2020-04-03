package com.aam.producer.playlist.biz.convert;

import com.aam.producer.playlist.biz.enums.TPlaylistActionStatusEnum;
import com.aam.producer.playlist.protocol.request.TPlaylistActionDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistDeletedDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistDeliveryDTO;
import com.aam.producer.playlist.repository.entity.PlaylistDeleteLogDO;
import com.aam.producer.playlist.repository.entity.PlaylistSendLogDO;
import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import com.alibaba.fastjson.JSON;
import java.util.List;
import java.util.UUID;
import org.apache.commons.lang3.StringUtils;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

/**
 * TPlaylistSendConvert
 *
 * @author oliver.lo
 * @since 2019/5/15 3:36 PM
 */
@Mapper
public interface TPlaylistSendConvert {

    TPlaylistSendConvert MAPPER = Mappers.getMapper(TPlaylistSendConvert.class);

    default PlaylistSendLogDO toPlaylistSendLogDO(String posId, String complexId, String deviceId,
            Integer method, TmsPlaylistDO playlistToBeSent) {
        PlaylistSendLogDO sendLogDO = new PlaylistSendLogDO();
        sendLogDO.setReceiptUuid(UUID.randomUUID().toString());
        sendLogDO.setPosId(posId);
        sendLogDO.setComplexId(complexId);
        sendLogDO.setDeviceUuid(deviceId);
        sendLogDO.setPplUuid(playlistToBeSent.getSourcePplId());
        sendLogDO.setPlaylistUuid(playlistToBeSent.getPlaylistUuid());
        sendLogDO.setMethod(method);
        sendLogDO.setStatus(TPlaylistActionStatusEnum.MARKED.getCode());
        sendLogDO.setMessage(TPlaylistActionStatusEnum.MARKED.name());
        sendLogDO.setTitle(playlistToBeSent.getTitle());
        sendLogDO.setContentIds(playlistToBeSent.getContentIds());
        sendLogDO.setJson(playlistToBeSent.getJson());
        return sendLogDO;
    }

    default void playlistSendLogDOFilling(PlaylistSendLogDO sendLogDO, Integer status,
            String message, TmsPlaylistDO playlistToBeSent) {
        if (status != null) {
            sendLogDO.setStatus(status);
        }
        if (message != null) {
            sendLogDO.setMessage(message);
        }
        if (playlistToBeSent != null) {
            sendLogDO.setTitle(playlistToBeSent.getTitle());
            sendLogDO.setContentIds(playlistToBeSent.getContentIds());
            sendLogDO.setJson(playlistToBeSent.getJson());
            // json for site
            MessageQueueConvert.MAPPER.toPlaylistDtoForSite(sendLogDO);
        }
    }

    default void playlistSendLogDODelivered(PlaylistSendLogDO sendLogDO, TPlaylistDeliveryDTO dto) {
        if (Boolean.TRUE.equals(dto.getDeliveryStatus())) {
            sendLogDO.setStatus(TPlaylistActionStatusEnum.DELIVERED.getCode());
            sendLogDO.setMessage(TPlaylistActionStatusEnum.DELIVERED.name());
            sendLogDO.setActionId(dto.getActionId());
        } else {
            sendLogDO.setStatus(TPlaylistActionStatusEnum.FAILED.getCode());
            sendLogDO.setMessage(dto.getMessage());
        }
    }

    default void playlistSendLogDOActioned(PlaylistSendLogDO sendLogDO, TPlaylistActionDTO dto) {
        if (Boolean.TRUE.equals(dto.getSuccess())) {
            sendLogDO.setStatus(TPlaylistActionStatusEnum.DONE.getCode());
            sendLogDO.setMessage(TPlaylistActionStatusEnum.DONE.name());
        } else {
            sendLogDO.setStatus(TPlaylistActionStatusEnum.FAILED.getCode());
            sendLogDO.setMessage(dto.getMessage());
        }
    }

    default PlaylistSendLogDO toUniqueLog(String playlistUuid, String complexId) {
        if (StringUtils.isEmpty(playlistUuid) || StringUtils.isEmpty(complexId)) {
            return null;
        }
        PlaylistSendLogDO logDO = new PlaylistSendLogDO();
        logDO.setPlaylistUuid(playlistUuid);
        logDO.setComplexId(complexId);
        return logDO;
    }

    default PlaylistDeleteLogDO toPlaylistDeleteLogDO(String complexId, String deviceUuid,
            List<String> tplIds, Integer deleteSys) {
        PlaylistDeleteLogDO aDo = new PlaylistDeleteLogDO();
        aDo.setReceiptUuid(UUID.randomUUID().toString());
        aDo.setComplexId(complexId);
        aDo.setDeviceUuid(deviceUuid);
        aDo.setTplUuids(JSON.toJSONString(tplIds));
        aDo.setDeleteSystem(deleteSys);
        aDo.setStatus(TPlaylistActionStatusEnum.REQUESTED.name());
        aDo.setMessage(TPlaylistActionStatusEnum.REQUESTED.name());
        return aDo;
    }

    default void playlistDeleteLogDOFilling(PlaylistDeleteLogDO logDO, TPlaylistDeletedDTO dto,
            Long attempted) {
        logDO.setStatus(
                Boolean.TRUE.equals(dto.getSuccess()) ? TPlaylistActionStatusEnum.DELIVERED.name()
                        : TPlaylistActionStatusEnum.FAILED.name());
        logDO.setMessage(Boolean.TRUE.equals(dto.getSuccess()) ? dto.getRespData()
                : dto.getMessage());
        logDO.setAttempted(attempted);
    }
}
