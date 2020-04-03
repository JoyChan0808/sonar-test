package com.aam.producer.playlist.sal.client.handler;

import com.aam.producer.playlist.sal.response.FilmHallInfo;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import com.alibaba.fastjson.annotation.JSONField;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class FilmHallHandler extends BaseHandler {

    public static List<FilmHallInfo> getFilmHallInfosByScreenUuids(String resultInfo) {
        String objString = getResult(resultInfo);
        List<FilmHallInfoTemplate> filmHallInfoTemplates = JSON
                .parseObject(objString, new TypeReference<List<FilmHallInfoTemplate>>() {
                });
        return filmHallInfoTemplates.stream().map(filmHallInfoTemplate -> {
            FilmHallInfo filmHallInfo = new FilmHallInfo();
            filmHallInfo.setScreenUuid(filmHallInfoTemplate.getUuid());

            if (filmHallInfoTemplate.getCapabilities().stream()
                    .anyMatch(capability -> "5.1".equalsIgnoreCase(capability.getValue()))) {
                filmHallInfo.setCapability("5.1");
            }

            if (filmHallInfoTemplate.getCapabilities().stream()
                    .anyMatch(capability -> "7.1".equalsIgnoreCase(capability.getValue()))) {
                filmHallInfo.setCapability("7.1");
            }

            if (filmHallInfoTemplate.getCapabilities().stream()
                    .anyMatch(capability -> "ATMOS".equalsIgnoreCase(capability.getValue()))) {
                filmHallInfo.setCapability("ATMOS");
            }

            if (filmHallInfoTemplate.getShowAttrs().stream()
                    .anyMatch(showAttr -> "IMAX".equalsIgnoreCase(showAttr.getName()))) {
                filmHallInfo.setImax(true);
            }

            return filmHallInfo;
        }).collect(Collectors.toList());
    }

    static class FilmHallInfoTemplate {

        private String uuid;
        private List<Capability> capabilities = new ArrayList<>();
        @JSONField(name = "show_attributes")
        private List<ShowAttr> showAttrs = new ArrayList<>();

        public String getUuid() {
            return uuid;
        }

        public void setUuid(String uuid) {
            this.uuid = uuid;
        }

        public List<Capability> getCapabilities() {
            return capabilities;
        }

        public void setCapabilities(List<Capability> capabilities) {
            this.capabilities = capabilities;
        }

        public List<ShowAttr> getShowAttrs() {
            return showAttrs;
        }

        public void setShowAttrs(List<ShowAttr> showAttrs) {
            this.showAttrs = showAttrs;
        }

        static class ShowAttr {

            private String name;
            private String uuid;

            public String getName() {
                return name;
            }

            public void setName(String name) {
                this.name = name;
            }

            public String getUuid() {
                return uuid;
            }

            public void setUuid(String uuid) {
                this.uuid = uuid;
            }
        }

        static class Capability {

            private String name;
            private String value;

            public String getName() {
                return name;
            }

            public void setName(String name) {
                this.name = name;
            }

            public String getValue() {
                return value;
            }

            public void setValue(String value) {
                this.value = value;
            }
        }
    }

}
