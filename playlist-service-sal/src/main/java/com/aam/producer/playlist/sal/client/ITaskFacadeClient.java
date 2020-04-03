package com.aam.producer.playlist.sal.client;


import com.aam.producer.task.protocol.response.PPLIssueModel;
import java.util.List;
import java.util.Map;

public interface ITaskFacadeClient {

    Map<String, PPLIssueModel> queryPplIssue(String userId, String title, List<String> pplUuids,
            String orgUuid);

    List<PPLIssueModel> queryOnePplIssue(String userId, String title,
            String pplUuid, String orgUuid,String contentAssociationUuid);
}
