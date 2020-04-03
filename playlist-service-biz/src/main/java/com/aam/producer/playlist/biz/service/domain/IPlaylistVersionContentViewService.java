package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.playlist.protocol.response.ContentInfo;
import com.aam.producer.playlist.repository.entity.PlaylistVersionContentAssociationDO;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import java.util.List;
import org.apache.commons.lang3.StringUtils;

public interface IPlaylistVersionContentViewService {

    List<ContentInfo> getByPplVersionUuid(String pplVersionUuid);

    default ContentInfo toContentInfo(PlaylistVersionContentAssociationDO contentAssociationDO) {
        ContentInfo contentInfo = new ContentInfo();
        contentInfo.setContentAssociationUuid(contentAssociationDO.getUuid());
        contentInfo.setContentId(contentAssociationDO.getContentId());
        contentInfo.setPplVersionId(contentAssociationDO.getPplVersionId());
        contentInfo.setContentType(
                ContentTypeEnum.getByCode(contentAssociationDO.getContentType()).getName());
        contentInfo.setExtension(contentAssociationDO.getExtension());

        if (StringUtils.isNotEmpty(contentAssociationDO.getExtension())) {
            JSONObject jsonObject = JSON.parseObject(contentAssociationDO.getExtension());
            contentInfo.setContentKind(jsonObject.getString("content_kind"));
        }

        contentInfo.setTitle(contentAssociationDO.getTitle());
        contentInfo.setVersion(contentAssociationDO.getPplVersionId());
        return contentInfo;
    }

    List<PlaylistVersionContentAssociationDO> getByContentId(String contentId);

    PlaylistVersionContentAssociationDO getByContentAssociationUuid(String contentAssociationUuid);

    List<String> getPlaylistUuidsByContentUuid(String contentUuid);

    List<ContentInfo> getByPplVersionUuids(List<String> pplVersionUuids);
}
