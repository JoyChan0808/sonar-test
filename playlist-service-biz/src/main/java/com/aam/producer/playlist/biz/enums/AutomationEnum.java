package com.aam.producer.playlist.biz.enums;

/**
 * automation enum
 *
 * @author oliver.lo
 * @since 2019-07-24 18:12
 */
public enum AutomationEnum {

    CREDIT_OFFSET("Credit Offset", "credit_offset"),
    INTERMISSION("Intermission", "intermission"),
    TO_2D_SCOPE("To 2D Scope", "macro_pack"),
    TO_3D_SCOPE("To 3D Scope", "macro_pack"),
    TO_2D_FLAT("To 2D Flat", "macro_pack"),
    TO_3D_FLAT("To 3D Flat", "macro_pack"),
    TO_51("To 5.1", "macro_pack"),
    TO_71("To 7.1", "macro_pack"),
    ;

    private String text;

    private String type;

    AutomationEnum(String text, String type) {
        this.text = text;
        this.type = type;
    }

    public static AutomationEnum getByText(String text) {
        switch (text) {
            case "Credit Offset":
                return CREDIT_OFFSET;
            case "Intermission":
                return INTERMISSION;
            case "To 2D Scope":
                return TO_2D_SCOPE;
            case "To 3D Scope":
                return TO_3D_SCOPE;
            case "To 2D Flat":
                return TO_2D_FLAT;
            case "To 3D Flat":
                return TO_3D_FLAT;
            case "To 5.1":
                return TO_51;
            case "To 7.1":
                return TO_71;
            default:
                return null;
        }
    }

    public String getText() {
        return text;
    }

    public String getType() {
        return type;
    }
}
