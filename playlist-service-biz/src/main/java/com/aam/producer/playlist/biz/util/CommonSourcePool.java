package com.aam.producer.playlist.biz.util;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;


public class CommonSourcePool {

    private CommonSourcePool() {
    }

    private static final int THREAD_POOL_SIZE = 4;

    private volatile static ExecutorService executorService;

    public static ExecutorService getExecutorService() {
        if (executorService == null) {
            synchronized (CommonSourcePool.class) {
                if (executorService == null) {
                    executorService = Executors.newFixedThreadPool(THREAD_POOL_SIZE);
                }
            }
        }
        return executorService;
    }

}
