package com.aam.producer.playlist.sal.client;

import com.aam.producer.playlist.sal.response.CplInfo;
import java.util.List;
import java.util.Map;
import java.util.Set;

public interface ICplFacadeClient {

    List<CplInfo> getCplInfoByUuids(Set<String> cplUuids, String orgId);

    @Deprecated
    Map<String, Boolean> checkCplRatings(Map<String, Map<String, String>> cplRatings);
}
