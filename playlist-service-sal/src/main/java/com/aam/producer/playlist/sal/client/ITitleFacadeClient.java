package com.aam.producer.playlist.sal.client;

import com.aam.producer.playlist.sal.response.TitleInfo;
import java.util.List;
import java.util.Set;

public interface ITitleFacadeClient {

    List<TitleInfo> geTitlesByUuids(Set<String> titleUuids, List<String> groupUuid);

    TitleInfo geTitlesByUuid(String titleUuid);

}
