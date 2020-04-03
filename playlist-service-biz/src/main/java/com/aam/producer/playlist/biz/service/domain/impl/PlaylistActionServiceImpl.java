package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.playlist.biz.enums.PlaylistStatusEnum;
import com.aam.producer.playlist.biz.enums.ResultCodeEnum;
import com.aam.producer.playlist.biz.event.ChangePplStatusEvent;
import com.aam.producer.playlist.biz.service.IPlaylistService;
import com.aam.producer.playlist.biz.service.IPlaylistVersionService;
import com.aam.producer.playlist.biz.service.domain.IMessageQueueService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistActionService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionActionService;
import com.aam.producer.playlist.biz.service.impl.PlaylistServiceImpl;
import com.aam.producer.playlist.protocol.message.ProducerPlaylistChangedDTO;
import com.aam.producer.playlist.protocol.request.PlaylistDTO;
import com.aam.producer.playlist.protocol.request.PlaylistVersionDTO;
import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.aam.producer.playlist.repository.entity.PlaylistVersionDO;
import com.aam.utils.enums.BaseResultCode;
import com.aam.utils.exception.BizException;
import com.alibaba.fastjson.JSON;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class PlaylistActionServiceImpl implements IPlaylistActionService, ApplicationListener {

    private static final Logger logger = LoggerFactory.getLogger(PlaylistServiceImpl.class);

    private final IPlaylistService iPlaylistService;

    private final IPlaylistVersionService iPlaylistVersionService;

    private final IPlaylistVersionActionService iPlaylistVersionActionService;

    private final IMessageQueueService iMessageQueueService;

    @Autowired
    public PlaylistActionServiceImpl(IPlaylistService iPlaylistService,
            IPlaylistVersionActionService iPlaylistVersionActionService,
            IPlaylistVersionService iPlaylistVersionService,
            IMessageQueueService iMessageQueueService
    ) {
        this.iPlaylistService = iPlaylistService;
        this.iPlaylistVersionService = iPlaylistVersionService;
        this.iPlaylistVersionActionService = iPlaylistVersionActionService;
        this.iMessageQueueService = iMessageQueueService;
    }

    @Override
    public void onApplicationEvent(@NotNull ApplicationEvent applicationEvent) {
        if (applicationEvent instanceof ChangePplStatusEvent) {
            ChangePplStatusEvent pplStatusEvent = (ChangePplStatusEvent) applicationEvent;
            String pplUuid = (String) pplStatusEvent.getSource();
            PlaylistDO playlistDO = iPlaylistService.getPlaylist(pplUuid);
            if (playlistDO == null) {
                return;
            }
            List<PlaylistVersionDO> playlistVersionDOS = iPlaylistVersionService
                    .getPlaylistVersionByPplUuid(pplUuid);
            if (playlistVersionDOS.isEmpty()) {
                return;
            }
            Integer status = playlistVersionDOS.stream().mapToInt(PlaylistVersionDO::getStatus)
                    .sum();
            if (!PlaylistStatusEnum.contains(status)) {
                throw new BizException(BaseResultCode.SYSTEM_ERROR, "ppl status exception");
            }
            playlistDO.setStatus(status);
            iPlaylistService.updateById(playlistDO);
        }
    }

    @Transactional
    @Override
    public String createPlaylist(PlaylistDTO playlistDTO) {
        logger.info("create a playlist. detail:<{}>", JSON.toJSONString(playlistDTO));
        //title不能重复
        if (iPlaylistService.getByTitle(playlistDTO.getTitle()) != null) {
            throw new BizException(ResultCodeEnum.TITLE_ALREADY_EXISTS);
        }

        //创建ppl
        String pplUuid = iPlaylistService
                .createPlaylist(playlistDTO.getTitle(), playlistDTO.getAutomaticallyApply());

        //创建ppl version
        PlaylistVersionDTO playlistVersionDTO = new PlaylistVersionDTO();
        playlistVersionDTO.setPlaylistUuid(pplUuid);
        playlistVersionDTO.setShowAttributeGroups(playlistDTO.getShowAttributeGroups());
        iPlaylistVersionActionService.createPlaylistVersion(playlistVersionDTO);
        return pplUuid;
    }

    @Transactional
    @Override
    public void updatePlaylist(String playUuid, String title) {
        logger.info("update playlist<{}> title. title:<{}>", playUuid, title);
        PlaylistDO playlistDO = iPlaylistService.getPlaylist(playUuid);
        if (playlistDO == null || Objects.equals(playlistDO.getTitle(), title)) {
            return;
        }
        if (iPlaylistService.getByTitle(title) == null) {
            iPlaylistService.updatePlaylist(playUuid, title);
            iPlaylistVersionActionService.createTpl(playlistDO, new ArrayList<>(), 6);
            ProducerPlaylistChangedDTO producerPlaylistChangedDTO = new ProducerPlaylistChangedDTO();
            producerPlaylistChangedDTO.setTitle(title);
            producerPlaylistChangedDTO.setUuid(playUuid);
            iMessageQueueService.producerPlaylistChangedData(producerPlaylistChangedDTO);
        } else {
            throw new BizException(ResultCodeEnum.TITLE_ALREADY_EXISTS);
        }
    }

    @Transactional
    @Override
    public void deletePlaylist(String playlistUuid) {
        logger.info("delete playlist<{}>.", playlistUuid);
        List<PlaylistVersionDO> versionDOList = iPlaylistVersionService
                .getPlaylistVersionByPplUuid(playlistUuid);
        Set<PlaylistVersionDO> versionDOSet = versionDOList.stream().filter(playlistVersionDO ->
                PlaylistStatusEnum.RELEASE.getStatus().equals(playlistVersionDO.getStatus()))
                .collect(
                        Collectors.toSet());
        versionDOList.forEach(playlistVersionDO -> iPlaylistVersionActionService
                .deletePlaylistVersion(playlistVersionDO.getUuid()));
        versionDOSet.forEach(playlistVersionDO -> iPlaylistVersionActionService
                .unPublish(playlistUuid, playlistVersionDO.getUuid()));
        iPlaylistService.deletePlaylist(playlistUuid);
    }

    @Transactional
    @Override
    public String copyPlaylist(String playlistUuid, PlaylistDTO playlistDTO) {
        logger.info("copy playlist<{}>.options:<{}>", playlistUuid, JSON.toJSONString(playlistDTO));
        String newPlaylistUuid = createPlaylist(playlistDTO);
        iPlaylistVersionActionService
                .copyPlaylistVersion(playlistUuid, newPlaylistUuid, playlistDTO.getDraft(),
                        playlistDTO.getShowAttributeGroups());
        return newPlaylistUuid;
    }

    @Transactional
    @Override
    public void allSegmentPublish(String playlistUuid) {
        PlaylistVersionDO playlistVersionDO = iPlaylistVersionService.getReleasePlaylistVersionByPplUuid(playlistUuid);
        if(playlistVersionDO!=null){
            iPlaylistVersionActionService.allSegmentPublish(playlistUuid,playlistVersionDO.getUuid());
        }
    }

}
