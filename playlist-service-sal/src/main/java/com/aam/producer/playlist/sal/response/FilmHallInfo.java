package com.aam.producer.playlist.sal.response;

import java.io.Serializable;

public class FilmHallInfo implements Serializable {

    private static final long serialVersionUID = -1L;

    private String screenUuid;

    private String capability;

    private boolean imax;

    public String getCapability() {
        return capability;
    }

    public void setCapability(String capability) {
        this.capability = capability;
    }

    public String getScreenUuid() {
        return screenUuid;
    }

    public void setScreenUuid(String screenUuid) {
        this.screenUuid = screenUuid;
    }

    public boolean isImax() {
        return imax;
    }

    public void setImax(boolean imax) {
        this.imax = imax;
    }
}
