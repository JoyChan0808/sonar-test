package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.playlist.biz.convert.MessageQueueConvert;
import com.aam.producer.playlist.biz.convert.TPlaylistWarningConvert;
import com.aam.producer.playlist.biz.model.ValidationModel;
import com.aam.producer.playlist.biz.service.ITmsPlaylistService;
import com.aam.producer.playlist.biz.service.domain.IMessageQueueService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistWarningService;
import com.aam.producer.playlist.protocol.message.TPlaylistWarningDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistWarningDTO.IssueDetail;
import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import com.alibaba.fastjson.JSON;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * playlist warning service impl
 *
 * @author oliver.lo
 * @since 2019-08-05 11:38
 */
@Service
public class TPlaylistWarningServiceImpl implements ITPlaylistWarningService {

    private static final Logger logger = LoggerFactory.getLogger(TPlaylistWarningServiceImpl.class);

    private final IMessageQueueService mqService;
    private final ITmsPlaylistService tmsPlaylistService;

    @Autowired
    public TPlaylistWarningServiceImpl(
            IMessageQueueService mqService,
            ITmsPlaylistService tmsPlaylistService) {
        this.mqService = mqService;
        this.tmsPlaylistService = tmsPlaylistService;
    }

    @Override
    @Transactional
    public void verifyTpl(String pplUuid, String pplTitle, Set<String> tplUuidList) {
        List<TmsPlaylistDO> tplList = tmsPlaylistService
                .getTplListForUpdate(pplUuid, tplUuidList);
        verify(pplUuid, pplTitle, tplList);
    }

    @Override
    @Transactional
    public void verifyTyl(String pplUuid, String pplTitle, List<TmsPlaylistDO> tplList) {
        verify(pplUuid, pplTitle, tplList);
    }

    private void verify(String pplUuid, String pplTitle, List<TmsPlaylistDO> tplList) {
        if (CollectionUtils.isNotEmpty(tplList)) {
            List<TPlaylistWarningDTO> warnings = new ArrayList<>(tplList.size());

            List<TmsPlaylistDO> changedTplList = createValidationAndSorted(pplUuid, pplTitle,
                    tplList, warnings);

            // save validation
            tmsPlaylistService.updateBatchById(changedTplList);

            // send warnings
            warnings.forEach(mqService::playlistWarningData);
        }
    }

    private List<TmsPlaylistDO> createValidationAndSorted(String pplUuid, String pplTitle,
            List<TmsPlaylistDO> tplList, List<TPlaylistWarningDTO> warnings) {
        tplList.forEach(x -> {
            ValidationModel validation = TPlaylistWarningConvert.mapper.verifyTpl(pplUuid, x);
            x.setValidation(JSON.toJSONString(validation));

            Map<String, IssueDetail> issueDetails = validation.getIssueDetails();
            List<IssueDetail> issues = MapUtils.isNotEmpty(issueDetails) ? new ArrayList<>(
                    issueDetails.values()) : new ArrayList<>();

            TPlaylistWarningDTO dto = MessageQueueConvert.MAPPER
                    .toPlaylistWarningDTO(x, issues, pplTitle);
            warnings.add(dto);
        });
        return tplList.stream().sorted(Comparator.comparingLong(TmsPlaylistDO::getId))
                .collect(Collectors.toList());
    }
}
