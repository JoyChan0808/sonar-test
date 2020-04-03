package com.aam.producer.playlist.sal;

import java.io.Serializable;

/**
 * api common result
 *
 * @author oliver.lo
 * @since 2019/6/10 3:01 PM
 */
public class CommonResult<T> implements Serializable {

    private static final long serialVersionUID = 2687197033243891394L;

    private int code;

    private T data;

    private String message;

    public int getCode() {
        return code;
    }

    public void setCode(int code) {
        this.code = code;
    }

    public T getData() {
        return data;
    }

    public void setData(T data) {
        this.data = data;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}
