package com.aam.producer.playlist.biz.enums;

/**
 * content kind
 *
 * @author oliver.lo
 * @since 2019-08-29 16:43
 */
public enum ContentKindEnum {

    FEATURE("feature"),
    SHORT("short"),
    RATING("rating"),
    TRANSITIONAL("transitional"),
    ADVERTISEMENT("advertisement"),
    TRAILER("trailer"),
    TEASER("teaser"),
    TEST("test"),
    POLICY("policy"),
    PSA("psa");

    private String name;

    ContentKindEnum(String name) {
        this.name = name;
    }

    public static ContentKindEnum getByName(String name) {
        switch (name) {
            case "feature":
                return FEATURE;
            case "short":
                return SHORT;
            case "rating":
                return RATING;
            case "transitional":
                return TRANSITIONAL;
            case "advertisement":
                return ADVERTISEMENT;
            case "trailer":
                return TRAILER;
            case "teaser":
                return TEASER;
            case "test":
                return TEST;
            case "policy":
                return POLICY;
            case "psa":
                return PSA;
            default:
                return null;
        }
    }

    public String getName() {
        return name;
    }
}
