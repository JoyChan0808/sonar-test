package com.aam.producer.playlist.common.utils;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.apache.commons.io.IOUtils;
import org.springframework.core.io.ClassPathResource;

/**
 * init utils
 *
 * @author oliver.lo
 * @since 2019-10-09 10:27
 */
public class InitUtils {

    private static final String initUuidJson = "config/initialize_uuids.json";

    private static final String ratingsClipJson = "config/ratings_clip.json";

    public static String genUuid(String main, String sub, boolean random) {

        ClassPathResource resource = new ClassPathResource(initUuidJson);

        try {
            String jsonStr = IOUtils.toString(resource.getInputStream(), StandardCharsets.UTF_8);
            Map<String, Map<String, String>> map = JSON
                    .parseObject(jsonStr, new TypeReference<Map<String, Map<String, String>>>() {
                    });
            return map.getOrDefault(main, new HashMap<>())
                    .getOrDefault(sub, random ? UUID.randomUUID().toString() : null);
        } catch (Exception e) {
            return random ? UUID.randomUUID().toString() : null;
        }
    }

    public static Map<String, List<String>> getRatingClip() {
        ClassPathResource resource = new ClassPathResource(ratingsClipJson);

        try {
            String jsonStr = IOUtils.toString(resource.getInputStream(), StandardCharsets.UTF_8);
            return JSON.parseObject(jsonStr, new TypeReference<Map<String, List<String>>>() {
            });
        } catch (Exception e) {
            return new HashMap<>();
        }
    }
}
