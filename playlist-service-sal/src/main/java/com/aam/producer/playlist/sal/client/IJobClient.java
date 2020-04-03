package com.aam.producer.playlist.sal.client;

import java.util.Map;

public interface IJobClient {

    String add(Map<String,String> paramMap);

    void remove(String id);

}
