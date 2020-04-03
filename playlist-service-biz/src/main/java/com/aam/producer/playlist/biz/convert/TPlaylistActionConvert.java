package com.aam.producer.playlist.biz.convert;

import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.lib.enums.PlaylistSourceEnum;
import com.aam.producer.lib.enums.SegmentTypeEnum;
import com.aam.producer.playlist.biz.enums.AutomationEnum;
import com.aam.producer.playlist.biz.model.AutomationModel;
import com.aam.producer.playlist.biz.model.AutomationModel.Specific;
import com.aam.producer.playlist.biz.model.CompositionModel;
import com.aam.producer.playlist.biz.model.CompositionModel.Rating;
import com.aam.producer.playlist.biz.model.PlaylistFlatModel;
import com.aam.producer.playlist.biz.model.PlaylistPublishModel;
import com.aam.producer.playlist.biz.model.PlaylistPublishModel.PlaylistContent;
import com.aam.producer.playlist.biz.model.PlaylistPublishModel.SegmentSplit;
import com.aam.producer.playlist.biz.model.PlaylistPublishModel.Shows;
import com.aam.producer.playlist.biz.model.SegmentModel;
import com.aam.producer.playlist.biz.model.TPlaylistEventModel;
import com.aam.producer.playlist.biz.model.ValidationModel;
import com.aam.producer.playlist.common.PlaylistConst;
import com.aam.producer.playlist.common.utils.InitUtils;
import com.aam.producer.playlist.protocol.message.TPlaylistDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistWarningDTO.IssueDetail;
import com.aam.producer.playlist.protocol.response.TmsPlaylistInfo;
import com.aam.producer.playlist.repository.entity.PlaylistSyncLogDO;
import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;

/**
 * TPlaylistActionConvert
 *
 * @author oliver.lo
 * @since 2019/5/15 3:37 PM
 */
@Mapper
public interface TPlaylistActionConvert {

    TPlaylistActionConvert MAPPER = Mappers.getMapper(TPlaylistActionConvert.class);

    default TmsPlaylistInfo toPlaylistInfo(TmsPlaylistDO tmsPlaylistDO, String title) {
        TmsPlaylistInfo info;
        String json = tmsPlaylistDO.getJson();
        if (StringUtils.isNotEmpty(json)) {
            info = JSON.parseObject(json, TmsPlaylistInfo.class);
        } else {
            info = new TmsPlaylistInfo();
        }
        info.setUuid(tmsPlaylistDO.getPlaylistUuid());
        info.setPplUuid(tmsPlaylistDO.getSourcePplId());
        info.setComplexUuid(tmsPlaylistDO.getSourceComplexId());
        info.setTitle(StringUtils.isEmpty(title) ? tmsPlaylistDO.getTitle() : title);
        info.setContentIds(JSON.parseArray(tmsPlaylistDO.getContentIds(), String.class));
        // set issue
        String validation = tmsPlaylistDO.getValidation();
        if (StringUtils.isNotEmpty(validation)) {
            ValidationModel validationModel = JSON.parseObject(validation, ValidationModel.class);
            Map<String, IssueDetail> issueDetails = validationModel.getIssueDetails();
            if (MapUtils.isNotEmpty(issueDetails)) {
                info.setIssueDetails(new ArrayList<>(issueDetails.values()));
            }
        }
        return info;
    }

    default void setPro2CplKey(TmsPlaylistInfo info) {
        String eventsStr = JSON.toJSONString(info.getEvents());
        JSONArray events = JSON.parseArray(eventsStr);
        for (int i = 0, len = events.size(); i < len; i++) {
            JSONObject event = events.getJSONObject(i);
            String type = event.getString(PlaylistConst.PLAYLIST_EVENT_TYPE_KEY);
            if ("composition".equals(type)) {
                List<String> format = new ArrayList<>();
                // audio_type
                String audioTypeStr = event.getString("audio_type");
                List<String> audioTypes = JSON.parseArray(audioTypeStr, String.class);
                if (CollectionUtils.isNotEmpty(audioTypes)) {
                    format.addAll(audioTypes);
                }
                // aspect_ratio
                String aspectRatio = event.getString("aspect_ratio");
                if (StringUtils.isNotEmpty(aspectRatio)) {
                    format.add(aspectRatio);
                }
                // resolution
                String resolution = event.getString("resolution");
                if (StringUtils.isNotEmpty(resolution)) {
                    format.add(resolution);
                }
                // playback_mode
                String playbackMode = event.getString("playback_mode");
                if (StringUtils.isNotEmpty(playbackMode)) {
                    format.add(playbackMode);
                }

                event.putIfAbsent("format", format);
            }
        }
        info.setEvents(events);
    }

    @Mappings({
            @Mapping(target = "id", ignore = true),
            @Mapping(source = "syncLogDO.playlistUuid", target = "playlistUuid"),
            @Mapping(source = "syncLogDO.title", target = "title"),
            @Mapping(source = "syncLogDO.json", target = "json"),
            @Mapping(source = "syncLogDO.contentIds", target = "contentIds"),
            @Mapping(source = "syncLogDO.templated", target = "templated"),
            @Mapping(source = "source", target = "source"),
            @Mapping(source = "changed", target = "changed"),
            @Mapping(source = "syncLogDO.complexId", target = "sourceComplexId"),
            @Mapping(source = "syncLogDO.deviceUuid", target = "sourceDeviceId"),
            @Mapping(source = "tplUuid", target = "sourcePplId"),
            @Mapping(source = "tplUuid", target = "sourcePplVersionId"),
            @Mapping(target = "sourceSegmentSplitUuid", ignore = true),
            @Mapping(target = "automaticallyApply", ignore = true),
            @Mapping(target = "created", ignore = true),
            @Mapping(target = "lastModified", ignore = true),
            @Mapping(source = "organizationUuid", target = "organizationId"),
    })
    TmsPlaylistDO toTmsPlaylistDOFromSynced(Integer source, Boolean changed,
            PlaylistSyncLogDO syncLogDO, String organizationUuid, String tplUuid);

    default void tmsPlaylistDOFillingFromSynced(TmsPlaylistDO tmsPlaylistDO,
            PlaylistSyncLogDO syncLogDO) {
        tmsPlaylistDO.setTitle(syncLogDO.getTitle());
        tmsPlaylistDO.setJson(syncLogDO.getJson());
        tmsPlaylistDO.setContentIds(syncLogDO.getContentIds());
        tmsPlaylistDO.setTemplated(syncLogDO.getTemplated());
        tmsPlaylistDO.setChanged(true);
    }

    default List<TmsPlaylistDO> toTmsPlaylistDOFromPublish(PlaylistPublishModel dto,
            Map<String, String> posTplMap, Map<String, TmsPlaylistDO> uniqueOldMap) {
        List<TmsPlaylistDO> result = new ArrayList<>();
        List<PlaylistFlatModel> playlistFlatModels = toPlaylistFlatModelFromPublish(dto);

        // tpl title list
        List<String> tplTitles = new ArrayList<>();
        if (MapUtils.isNotEmpty(uniqueOldMap)) {
            uniqueOldMap.values().forEach(x -> tplTitles.add(x.getTitle()));
        }
        boolean multiVersion = playlistFlatModels.size() > 1 || dto.getAutomatic();

        for (PlaylistFlatModel flatModel : playlistFlatModels) {

            TmsPlaylistDO tmsPlaylistDO = buildTmsPlaylistDOFromFlatten(flatModel, uniqueOldMap,
                    tplTitles, multiVersion);

            // automatic ppl do not generate new tpl if this flat no match pos
            if (tmsPlaylistDO == null) {
                continue;
            }

            // auto ppl match flat pos,static ppl match all pos
            String tplUuid = tmsPlaylistDO.getPlaylistUuid();
            if (multiVersion) {
                flatModel.getPosIds().forEach(p -> posTplMap.put(p, tplUuid));
            } else {
                if (CollectionUtils.isNotEmpty(dto.getPosUuidList())) {
                    dto.getPosUuidList().forEach(p -> posTplMap.put(p, tplUuid));
                }
            }

            result.add(tmsPlaylistDO);
        }
        return result;
    }

    default List<PlaylistFlatModel> toPlaylistFlatModelFromPublish(PlaylistPublishModel dto) {
        PlaylistFlatModel first = toPlaylistFlatModel(dto, null, null, null, false);
        List<PlaylistFlatModel> result = new ArrayList<>();
        List<PlaylistContent> contentList = dto.getContentList();
        if (CollectionUtils.isNotEmpty(contentList)) {
            for (PlaylistContent content : contentList) {
                List<SegmentSplit> segmentSplits = content.getSegmentSplits();
                if (CollectionUtils.isEmpty(segmentSplits)) {
                    first.getContents().add(content);
                    result.forEach(x -> x.getContents().add(content));
                } else {
                    List<PlaylistFlatModel> splitPlaylist = new ArrayList<>();
                    for (SegmentSplit split : segmentSplits) {
                        if (CollectionUtils.isEmpty(result)) {
                            PlaylistFlatModel model = toPlaylistFlatModel(dto, first,
                                    content, split, true);
                            splitPlaylist.add(model);
                        } else {
                            for (PlaylistFlatModel parent : result) {
                                PlaylistFlatModel model = toPlaylistFlatModel(dto, parent,
                                        content, split, false);
                                splitPlaylist.add(model);
                            }
                        }
                    }
                    if (CollectionUtils.isNotEmpty(splitPlaylist)) {
                        result.clear();
                        result.addAll(splitPlaylist);
                    }
                }
            }
        }
        if (CollectionUtils.isEmpty(result)) {
            result.add(first);
        }
        return result;
    }

    default TmsPlaylistDO buildTmsPlaylistDOFromFlatten(PlaylistFlatModel flat,
            Map<String, TmsPlaylistDO> uniqueOldMap, List<String> tplTitles, boolean multiVersion) {
        // default true,until some segment is empty
        boolean filled = true;
        // playlist uuid building
        List<String> uuidList = new ArrayList<>();
        uuidList.add(flat.getPplUuid());
        uuidList.add(flat.getPplVersionUuid());
        List<SegmentSplit> splits = flat.getSplits();
        for (SegmentSplit split : splits) {
            uuidList.add(split.getSegmentSplitUuid());
            if (CollectionUtils.isEmpty(split.getContentList())) {
                split.setFilled(false);
                filled = false;
            } else {
                split.setFilled(true);
            }
        }

        String tplTitle =
                StringUtils.isEmpty(flat.getTplTitle()) ? flat.getPplTitle() : flat.getTplTitle();
        Long version = null;
        if (multiVersion) {
            String finalTplTitle = tplTitle;
            version = tplTitles.stream().filter(x -> x.startsWith(finalTplTitle)).count() + 1;
        }
        TmsPlaylistDO tmsPlaylistDO;
        String uniqueId = UUID
                .nameUUIDFromBytes(JSON.toJSONString(uuidList).getBytes(StandardCharsets.UTF_8))
                .toString();
        if (uniqueOldMap != null && uniqueOldMap.containsKey(uniqueId)) {
            TmsPlaylistDO old = uniqueOldMap.get(uniqueId);
            tmsPlaylistDO = new TmsPlaylistDO();
            tmsPlaylistDO.setId(old.getId());
            tmsPlaylistDO.setPlaylistUuid(old.getPlaylistUuid());
            tmsPlaylistDO.setCreated(old.getCreated());
            tmsPlaylistDO.setLastModified(old.getLastModified());
            if (old.getTitle().startsWith(tplTitle)) {
                tplTitle = old.getTitle();
            } else {
                // ppl title changed
                if (version != null) {
                    tplTitle += " (v" + String.format("%03d", version) + ")";
                }
                tplTitles.add(tplTitle);
            }
        } else if (Boolean.TRUE.equals(flat.getAutomatic()) && CollectionUtils
                .isEmpty(flat.getPosIds())) {
            // if ppl is automatic and this flat no match pos,then do not generate tpl
            return null;
        } else {
            tmsPlaylistDO = new TmsPlaylistDO();
            tmsPlaylistDO.setPlaylistUuid(UUID.randomUUID().toString());
            if (version != null) {
                tplTitle += " (v" + String.format("%03d", version) + ")";
            }
            tplTitles.add(tplTitle);
        }

        tmsPlaylistDO.setOrganizationId(flat.getOrganizationId());
        tmsPlaylistDO.setFilled(filled);
        tmsPlaylistDO.setTitle(tplTitle);
        tmsPlaylistDO.setSource(PlaylistSourceEnum.PRODUCER.getCode());
        tmsPlaylistDO.setSourcePplId(flat.getPplUuid());
        tmsPlaylistDO.setSourcePplVersionId(flat.getPplVersionUuid());
        tmsPlaylistDO.setSourceSegmentSplitUuid(JSON.toJSONString(splits));
        tmsPlaylistDO.setAutomaticallyApply(flat.getAutomatic());
        tmsPlaylistJsonFilling(tmsPlaylistDO, flat.getContents(), tplTitle);
        return tmsPlaylistDO;
    }

    default PlaylistFlatModel toPlaylistFlatModel(PlaylistPublishModel dto,
            PlaylistFlatModel parent, PlaylistContent content, SegmentSplit split,
            boolean firstSegment) {
        PlaylistFlatModel model = new PlaylistFlatModel();
        model.setOrganizationId(dto.getOrganizationId());
        model.setPplUuid(dto.getPplUuid());
        model.setPplVersionUuid(dto.getPplVersionUuid());
        model.setAutomatic(dto.getAutomatic());
        model.setPplTitle(dto.getTitle());
        if (parent != null) {
            // delivery parent tpl title
            model.setTplTitle(parent.getTplTitle());
            model.getContents().addAll(parent.getContents());
            model.getSplits().addAll(parent.getSplits());
            model.getPosIds().addAll(parent.getPosIds());
        } else {
            model.setTplTitle(model.getPplTitle());
        }
        if (content != null && split != null) {
            PlaylistContent segment = new PlaylistContent();
            segment.setContentType(content.getContentType());
            segment.setContentUuid(content.getContentUuid());
            segment.setContentAssociationUuid(content.getContentAssociationUuid());
            segment.setSortNumber(content.getSortNumber());
            JSONObject jsonObject = JSON.parseObject(content.getExtension());
            List<JSONObject> segmentEvents = new ArrayList<>();
            Optional.ofNullable(split.getContentList()).ifPresent(x -> {
                for (PlaylistContent playlistContent : x) {
                    segmentEvents.add(JSON.parseObject(playlistContent.getExtension()));
                }
            });
            jsonObject.put(PlaylistConst.PLAYLIST_EVENTS_KEY, segmentEvents);
            segment.setExtension(JSON.toJSONString(jsonObject));
            model.getContents().add(segment);
            model.getSplits().add(split);
            List<String> splitPosIds = split.getShows().stream().map(Shows::getPosUuid)
                    .collect(Collectors.toList());
            if (firstSegment) {
                model.setPosIds(splitPosIds);
            }
            if (CollectionUtils.isNotEmpty(model.getPosIds())) {
                model.getPosIds().retainAll(splitPosIds);
            }

            // set automatic tpl title
            String contentKind = jsonObject
                    .getString(PlaylistConst.PLAYLIST_EVENT_CONTENT_KIND_KEY);
            if (SegmentTypeEnum.AUTOMATIC_SEGMENT.getName().equals(contentKind)) {
                String tplTitle = model.getPplTitle();
                if (StringUtils.isNotEmpty(split.getTitle())) {
                    tplTitle = split.getTitle() + " " + tplTitle;
                }
                if (StringUtils.isNotEmpty(split.getShowType())) {
                    tplTitle += " " + split.getShowType();
                }
                model.setTplTitle(tplTitle);
            }
        }
        return model;
    }

    default void tmsPlaylistJsonFilling(TmsPlaylistDO tmsPlaylistDO,
            List<PlaylistContent> contentList, String tplTitle) {
        if (CollectionUtils.isNotEmpty(contentList)) {
            TPlaylistDTO tPlaylistDTO = new TPlaylistDTO();
            tPlaylistDTO.setUuid(tmsPlaylistDO.getPlaylistUuid());
            tPlaylistDTO.setTitle(tplTitle);
            // default value
            tmsPlaylistDO.setTemplated(false);
            tPlaylistDTO.setAs3d(false);
            tPlaylistDTO.setAs4k(false);
            tPlaylistDTO.setAsHfr(false);

            List<Object> events = new ArrayList<>();
            Set<String> contentIds = new HashSet<>();
            List<String> preCplAttributes = null;
            boolean changeVisual = false;
            boolean changeAudio = false;
            for (PlaylistContent content : contentList) {
                int contentType = content.getContentType();
                JSONObject event = JSON.parseObject(content.getExtension());
                List<AutomationModel> automationModels = JSON
                        .parseArray(event.getString(PlaylistConst.PLAYLIST_EVENT_AUTOMATION_KEY),
                                AutomationModel.class);
                ContentTypeEnum contentTypeEnum = ContentTypeEnum.getByCode(contentType);
                // segment
                if (ContentTypeEnum.SEGMENT.equals(contentTypeEnum)) {
                    // set content_association_uuid to segment
                    event.putIfAbsent("content_association_uuid",
                            content.getContentAssociationUuid());

                    SegmentModel segment = JSON
                            .parseObject(event.toJSONString(), SegmentModel.class);
                    if (Boolean.TRUE.equals(segment.getVisualAutomation())) {
                        changeVisual = true;
                    }
                    if (Boolean.TRUE.equals(segment.getAudioAutomation())) {
                        changeAudio = true;
                    }
                    tmsPlaylistDO.setTemplated(true);
                    JSONArray segmentEvents = event.getJSONArray(PlaylistConst.PLAYLIST_EVENTS_KEY);
                    if (CollectionUtils.isNotEmpty(segmentEvents)) {
                        boolean changeVisualInSegment = Boolean.TRUE
                                .equals(segment.getVisualAutomation2());
                        boolean changeAudioInSegment = Boolean.TRUE
                                .equals(segment.getAudioAutomation2());
                        List<String> preCplAttributesInSegment = null;
                        Map<Integer, TPlaylistEventModel> addEventInSegmentMap = new HashMap<>();
                        for (int i = 0, len = segmentEvents.size(); i < len; i++) {
                            JSONObject segmentEvent = segmentEvents.getJSONObject(i);
                            eventFilling(tPlaylistDTO, contentIds, segmentEvent);
                            setAutomation(automationModels, segmentEvent);

                            List<String> currentCplAttributes = JSON
                                    .parseArray(segmentEvent.getString("format"), String.class);

                            if (changeVisualInSegment) {
                                TPlaylistEventModel visualEventInSegment = autoChangeVisual(
                                        preCplAttributesInSegment, currentCplAttributes, false);
                                if (visualEventInSegment != null) {
                                    visualEventInSegment.setPart(segmentEvent.getString("part"));
                                    addEventInSegmentMap.put(i, visualEventInSegment);
                                }
                            }

                            if (changeAudioInSegment) {
                                TPlaylistEventModel audioEventInSegment = autoChangeAudio(
                                        preCplAttributesInSegment, currentCplAttributes, false);
                                if (audioEventInSegment != null) {
                                    audioEventInSegment.setPart(segmentEvent.getString("part"));
                                    addEventInSegmentMap.put(i, audioEventInSegment);
                                }
                            }

                            preCplAttributesInSegment = JSON
                                    .parseArray(segmentEvent.getString("format"), String.class);

                            if (i == 0) {
                                // visual or audio automation setting
                                if (changeVisual) {
                                    TPlaylistEventModel visualEvent = autoChangeVisual(
                                            preCplAttributes, currentCplAttributes, true);
                                    if (visualEvent != null) {
                                        events.add(visualEvent);
                                    }
                                }
                                if (changeAudio) {
                                    TPlaylistEventModel audioEvent = autoChangeAudio(
                                            preCplAttributes, currentCplAttributes, true);
                                    if (audioEvent != null) {
                                        events.add(audioEvent);
                                    }
                                }
                            }

                            if (i == len - 1) {
                                // get the last segment event format
                                preCplAttributes = currentCplAttributes;
                            }
                        }
                        // add event to segment events
                        if (addEventInSegmentMap.size() != 0) {
                            int addEventInSegmentOffset = 0;
                            for (Entry<Integer, TPlaylistEventModel> entry : addEventInSegmentMap
                                    .entrySet()) {
                                segmentEvents.add(entry.getKey() + addEventInSegmentOffset,
                                        entry.getValue());
                                addEventInSegmentOffset++;
                            }
                        }
                    }
                } else {
                    eventFilling(tPlaylistDTO, contentIds, event);
                    setAutomation(automationModels, event);

                    // get format
                    preCplAttributes = JSON.parseArray(event.getString("format"), String.class);
                }
                // for ui
                event.putIfAbsent("title", event.getString("text"));
                events.add(event);
            }
            tPlaylistDTO.setEvents(events);

            tmsPlaylistDO.setContentIds(JSON.toJSONString(contentIds));
            tmsPlaylistDO.setJson(JSON.toJSONString(tPlaylistDTO));
        }
    }

    default void eventFilling(TPlaylistDTO tPlaylistDTO, Set<String> contentIds, JSONObject event) {
        if (event != null) {
            // sw identify
            if (event.containsKey(PlaylistConst.PLAYLIST_CPL_PLAYBACK_MODE_KEY)
                    && PlaylistConst.PLAYLIST_ATTRIBUTE_3D
                    .equals(event.getString(PlaylistConst.PLAYLIST_CPL_PLAYBACK_MODE_KEY))) {
                tPlaylistDTO.setAs3d(true);
            }
            // cpl_service identify
            if (event.containsKey(PlaylistConst.PLAYLIST_CPL_STEREOSCOPIC_KEY) && event
                    .getBooleanValue(PlaylistConst.PLAYLIST_CPL_STEREOSCOPIC_KEY)) {
                tPlaylistDTO.setAs3d(true);
            }
            if (event.containsKey(PlaylistConst.PLAYLIST_CPL_RESOLUTION_KEY)
                    && PlaylistConst.PLAYLIST_ATTRIBUTE_4K
                    .equals(event.getString(PlaylistConst.PLAYLIST_CPL_RESOLUTION_KEY))) {
                tPlaylistDTO.setAs4k(true);
            }
            if (event.containsKey(PlaylistConst.PLAYLIST_CPL_EDIT_RATE_KEY)) {
                // FPS above which we consider content to be HFR
                // var HFR_FPS = 30;
                Object editRate = event.get(PlaylistConst.PLAYLIST_CPL_EDIT_RATE_KEY);
                if (editRate != null) {
                    if (editRate instanceof Integer) {
                        tPlaylistDTO.setAsHfr(((Integer) editRate) > 30);
                    } else {
                        List<Integer> tmsEditRate = JSON.parseArray(
                                event.getString(PlaylistConst.PLAYLIST_CPL_EDIT_RATE_KEY),
                                Integer.class);
                        if (tmsEditRate != null && tmsEditRate.size() > 1 && tmsEditRate.get(1) != 0
                                && (tmsEditRate.get(0) / tmsEditRate.get(1)) > 30) {
                            tPlaylistDTO.setAsHfr(true);
                        }
                    }
                }
            }
            // remove this log,jira 557
            /*if (event.containsKey(PlaylistConst.PLAYLIST_CPL_FRAME_RATE_KEY)) {
                Integer frameRate = event.getInteger(PlaylistConst.PLAYLIST_CPL_FRAME_RATE_KEY);
                if (frameRate != null && frameRate > 30) {
                    tPlaylistDTO.setAsHfr(true);
                }
            }*/
            if (event.containsKey(PlaylistConst.PLAYLIST_CPL_ID_KEY) && StringUtils
                    .isNotEmpty(event.getString(PlaylistConst.PLAYLIST_CPL_ID_KEY))) {
                contentIds.add(event.getString(PlaylistConst.PLAYLIST_CPL_ID_KEY));
            }

            // remove not in list rating
            String proRatingStr = event.getString(PlaylistConst.CPL_PRODUCER_RATINGS_KEY);
            if (StringUtils.isNotEmpty(proRatingStr)) {
                List<Rating> proRatings = JSON.parseArray(proRatingStr, Rating.class);
                List<Rating> invalidProRatings = proRatings.stream()
                        .filter(x -> Boolean.FALSE.equals(x.getRegistered()))
                        .collect(Collectors.toList());
                if (proRatings.removeAll(invalidProRatings)) {
                    event.put(PlaylistConst.CPL_PRODUCER_RATINGS_KEY, proRatings);
                }
            }
        }
    }

    default void setAutomation(List<AutomationModel> automationModels, JSONObject cplEvent) {
        if (CollectionUtils.isNotEmpty(automationModels)) {
            CompositionModel compositionModel = JSON
                    .parseObject(cplEvent.toJSONString(), CompositionModel.class);
            List<AutomationModel> cplEventAutos = compositionModel.getAutomation();
            if (CollectionUtils.isNotEmpty(cplEventAutos)) {
                automationModels = cplEventAutos;
            }
            List<AutomationModel> autos = new ArrayList<>(automationModels.size());
            automationModels.forEach(x -> Optional.ofNullable(x.getSpecific()).ifPresent(u -> {
                Integer seconds = compositionModel
                        .getSecondsByAutomation(AutomationEnum.getByText(x.getName())) / 1000;
                AutomationModel model = new AutomationModel();
                model.setName(x.getName());
                model.setType(x.getType());
                Specific specific = new Specific();
                specific.setSeconds(seconds);
                specific.setFrames((long) (seconds * compositionModel.getFrameRate()));
                model.setSpecific(specific);
                autos.add(model);
            }));
            cplEvent.put(PlaylistConst.PLAYLIST_EVENT_AUTOMATION_KEY, autos);
        }
    }

    default TPlaylistEventModel autoChangeVisual(List<String> preCplAttributes,
            List<String> currentCplAttributes, boolean addDefault) {
        if (CollectionUtils.isEmpty(currentCplAttributes)) {
            return null;
        }

        Map<String, AutomationEnum> map = new HashMap<>();
        map.put("_2D_Flat", AutomationEnum.TO_2D_FLAT);
        map.put("_2D_Scope", AutomationEnum.TO_2D_SCOPE);
        map.put("_3D_Flat", AutomationEnum.TO_3D_FLAT);
        map.put("_3D_Scope", AutomationEnum.TO_3D_SCOPE);
        String[] attributes = {"2D", "3D", "Flat", "Scope"};

        if (CollectionUtils.isEmpty(preCplAttributes)) {
            if (addDefault) {
                return addDefaultChangeAutomation(attributes, map, currentCplAttributes);
            }
            return null;
        } else {
            return addChangeAutomation(attributes, map, preCplAttributes, currentCplAttributes);
        }
    }

    default TPlaylistEventModel autoChangeAudio(List<String> preCplAttributes,
            List<String> currentCplAttributes, boolean addDefault) {
        if (CollectionUtils.isEmpty(currentCplAttributes)) {
            return null;
        }

        Map<String, AutomationEnum> map = new HashMap<>();
        map.put("_5.1", AutomationEnum.TO_51);
        map.put("_7.1", AutomationEnum.TO_71);
        String[] attributes = {"5.1", "7.1"};

        if (CollectionUtils.isEmpty(preCplAttributes)) {
            if (addDefault) {
                return addDefaultChangeAutomation(attributes, map, currentCplAttributes);
            }
            return null;
        } else {
            return addChangeAutomation(attributes, map, preCplAttributes, currentCplAttributes);
        }
    }

    default TPlaylistEventModel addChangeAutomation(String[] attributes,
            Map<String, AutomationEnum> map, List<String> preCplAttributes,
            List<String> currentCplAttributes) {
        StringBuilder pre = new StringBuilder();
        StringBuilder current = new StringBuilder();
        for (String attribute : attributes) {
            if (preCplAttributes.contains(attribute)) {
                pre.append("_").append(attribute);
            }
            if (currentCplAttributes.contains(attribute)) {
                current.append("_").append(attribute);
            }
        }

        AutomationEnum preEnum = map.get(pre.toString());
        AutomationEnum currentEnum = map.get(current.toString());
        if (preEnum != null && currentEnum != null && !preEnum.equals(currentEnum)) {
            return genAutomationEvent(currentEnum);
        } else {
            return null;
        }
    }

    default TPlaylistEventModel addDefaultChangeAutomation(String[] attributes,
            Map<String, AutomationEnum> map, List<String> currentCplAttributes) {
        StringBuilder current = new StringBuilder();
        for (String attribute : attributes) {
            if (currentCplAttributes.contains(attribute)) {
                current.append("_").append(attribute);
            }
        }
        AutomationEnum currentEnum = map.get(current.toString());
        if (currentEnum != null) {
            return genAutomationEvent(currentEnum);
        } else {
            return null;
        }
    }

    default TPlaylistEventModel genAutomationEvent(AutomationEnum currentEnum) {
        // add change automation
        TPlaylistEventModel eventModel = new TPlaylistEventModel();
        eventModel.setType(currentEnum.getType());
        eventModel.setText(currentEnum.getText());
        eventModel.setTitle(currentEnum.getText());
        eventModel.setUuid(InitUtils
                .genUuid(PlaylistConst.PLAYLIST_EVENT_AUTOMATION_KEY, currentEnum.getText(),
                        false));
        return eventModel;
    }
}
