package com.aam.producer.playlist.sal.client.impl;

import com.aam.producer.playlist.sal.client.ITaskFacadeClient;
import com.aam.producer.playlist.sal.client.handler.TaskHandler;
import com.aam.producer.task.protocol.response.PPLIssueModel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.util.Strings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
public class TaskFacadeClientImpl implements ITaskFacadeClient {

    private final static Logger logger = LoggerFactory.getLogger(TaskFacadeClientImpl.class);
    private final RestTemplate restTemplate;
    @Value("${thunderstorm.taskService.queryPplIssue}")
    private String queryPplIssue;
    @Value("${thunderstorm.taskService.queryOnePplIssue}")
    private String queryOnePplIssue;

    @Autowired
    public TaskFacadeClientImpl(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }


    @Override
    public Map<String, PPLIssueModel> queryPplIssue(String userId, String title,
                                                    List<String> pplUuids, String orgUuid) {
        Map<String, Object> params = new HashMap<>();
        params.put("user_id", userId);
        params.put("title_uuid", title);
        params.put("organization_uuid", orgUuid);
        params.put("ppl_ids", Strings.join(pplUuids, ','));

        try {
            String forObject = restTemplate.getForObject(queryPplIssue, String.class, params);
            return TaskHandler.queryPplIssue(forObject);
        } catch (Exception e) {
            logger.error(
                    "query ppl issue exception.user_id:<{}> title_uuid:<{}> organization_uuid:<{}> ppl_ids:<{}>",
                    userId, title, orgUuid, Strings.join(pplUuids, ','));
            logger.error("query ppl issue exception.", e);
            return new HashMap<>();
        }

    }

    @Override
    public List<PPLIssueModel> queryOnePplIssue(String userId, String title,
                                                String pplUuid, String orgUuid, String contentAssociationUuid) {
        Map<String, Object> params = new HashMap<>();
        params.put("user_id", userId);
        params.put("title_uuid", title);
        params.put("organization_uuid", orgUuid);
        params.put("ppl_uuid", pplUuid);
        params.put("content_association_uuid", contentAssociationUuid);

        try {
            String forObject = restTemplate.getForObject(queryOnePplIssue, String.class, params);
            return TaskHandler.queryOnePplIssue(forObject);
        } catch (Exception e) {
            //降级处理
            logger.error(
                    "query a ppl issue exception.user_id:<{}> title_uuid:<{}> organization_uuid:<{}> ppl_ids:<{}> content_association_uuid:<{}>",
                    userId, title, orgUuid, pplUuid, contentAssociationUuid);
            logger.error("query a ppl issue exception.", e);
            return new ArrayList<>();
        }

    }

}
