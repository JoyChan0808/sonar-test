package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.protocol.request.ContentDTO;
import com.aam.producer.playlist.protocol.request.PublishDTO;
import com.aam.producer.playlist.protocol.request.SegmentSplitDTO;
import com.aam.producer.playlist.protocol.request.SegmentSplitOptionDTO;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import java.util.Set;

public interface ISegmentSplitActionService {

    /**
     * 添加草稿版本,从已发布版本里复制一份草稿
     *
     * @param associationUuid 播放列表占位符uuid
     * @param titleUuid       title uuid
     * @param copy            是否拷贝分组内容
     */
    String createSegmentSplitDraft(String associationUuid, String titleUuid, Boolean copy);

    /**
     * 添加子分组规则
     *
     * @param segmentSplitOptionDTO 分租规则
     */
    void createSegmentSplit(SegmentSplitOptionDTO segmentSplitOptionDTO);

    /**
     * 修改分租规则
     *
     * @param segmentSplitOptionDTO 分租规则
     */
    void updateSegmentSplit(SegmentSplitOptionDTO segmentSplitOptionDTO);

    /**
     * 根据分组uuid删除分组,同时会删除该分组下的子分组.如果同一层级删剩default(default组不能由参数指定删除)组,则会删除该default组
     *
     * @param uuid 分组uuid
     */
    void deleteSegmentSplit(String uuid);

    /**
     * 修改分组内容,同时指定多个分租与其对应的内容修改.
     *
     * @param contentMap map
     */
    void updateSegmentSplitContent(Map<String, List<ContentDTO>> contentMap);

    /**
     * 根据播放列表占位符uuid和标题uuid删除草稿版本分租规则
     *
     * @param contentAssociationUuid 播放列表占位符uuid
     * @param titleUuid              标题uuid
     */
    void deleteDraftByAssociationUuid(String contentAssociationUuid, String titleUuid);

    /**
     * 发布占位符,如果存在ppl使用了该占位符,则ppl也会重新发布
     *
     * @param contentAssociationUuid 播放列表占位符uuid
     * @param titleUuid              标题uuid
     * @param publishDTO             发布模式信息
     */
    void publishSegmentSplitByAssociation(String contentAssociationUuid, String titleUuid,
            PublishDTO publishDTO);

    /**
     * 取消发布占位符,,如果存在ppl使用了该占位符,则ppl也会重新发布
     *
     * @param contentAssociationUuid 播放列表占位符uuid
     * @param titleUuid              标题uuid
     * @param keepRelease            是否保留发布版本
     */
    void unPublishSegmentSplitByAssociation(String contentAssociationUuid, String titleUuid,
            Boolean keepRelease);

    /**
     * 新增或修改分组
     *
     * @param titleTypeSegmentSplitDTO 分组规则
     * @return 分租uuid
     */
    String createOrUpdateSegmentSplit(SegmentSplitDTO titleTypeSegmentSplitDTO);

    /**
     * 创建根分组规则
     *
     * @param segmentSplitDTO 分租信息
     * @return 分组uuid
     */
    String createRootSegmentSplit(SegmentSplitDTO segmentSplitDTO);

    /**
     * 根据播放列表版本uuid,删除该版本下的占位符下的所有分组及分组内容
     *
     * @param versionUuid 播放列表版本uuid(ppl version uuid)
     */
    void deleteByVersionUuid(String versionUuid);

    /**
     * 为feature segment自动生成分组规则,如果segment已经存在分组规则,则不会再生成
     *
     * @param associationUuid 播放列表占位符uuid
     * @param titleUuid       title uuid
     */
    void createDefaultSegmentSplit(String associationUuid, String titleUuid);


    void resetSegmentSplit(String contentAssociationUuid, String titleUuid);

    /**
     * 复制分租规则
     *
     * @param oldContentAssociationUuid 旧播放列表segment uuid
     * @param contentAssociationUuid    新播放列表segment uuid
     * @param pplVersion                ppl version uuid
     */
    void copy(String oldContentAssociationUuid, String contentAssociationUuid, String pplVersion);

    /**
     * 按周创建分组规则
     */
    void createSplitByWeek(String rootUuid, String playlistUuid, String pplVersionId,
            String titleUuid, String segmentAssociationUuid);

    /**
     * 扫描发布
     */
    void scanAndPublish(Calendar cal);

    /**
     *  自动创建周分组
     * @param associationUuid 播放列表segment uuid
     * @param titleUuid title uuid
     */
    void autoCreateDraftForWeekSplitSegment(String associationUuid, String titleUuid, Calendar cal);

    /**
     *  校验分组草稿是否填充完整
     * @param contentAssociationUuid contentAssociationUuid
     * @param titleUuid titleUuid
     * @return true/false
     */
    boolean checkSegmentSplitDraft(String contentAssociationUuid, String titleUuid);

    /**
     * 更新发布ppl
     *
     * @param pplUuids pplUuids
     */
    void publish(Set<String> pplUuids);
}
