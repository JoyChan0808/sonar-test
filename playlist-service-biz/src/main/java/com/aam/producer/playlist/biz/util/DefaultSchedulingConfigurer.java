package com.aam.producer.playlist.biz.util;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.SchedulingException;
import org.springframework.stereotype.Component;

@Component
public class DefaultSchedulingConfigurer {

    private static final Logger logger = LoggerFactory.getLogger(DefaultSchedulingConfigurer.class);

    private Map<String, ScheduledFuture<?>> taskFutures = new ConcurrentHashMap<>();

    private final ScheduledExecutorService executorService = Executors.newScheduledThreadPool(2);

    public void addTriggerTask(String taskId, long time, Runnable runnable) {
        if (taskFutures.containsKey(taskId)) {
            throw new SchedulingException("the taskId[" + taskId + "] was added.");
        }
        long delay = time - System.currentTimeMillis();
        logger.info("the taskId[{}] will be running after {} milliseconds", taskId, delay);
        ScheduledFuture<?> scheduledFuture =  executorService.schedule(runnable, delay, TimeUnit.MILLISECONDS);
        taskFutures.put(taskId, scheduledFuture);
    }


    public void cancelTriggerTask(String taskId) {
        ScheduledFuture<?> future = taskFutures.get(taskId);
        if (future != null) {
            future.cancel(true);
        }
        logger.info("Successful cancellation of taskId[{}]", taskId);
        taskFutures.remove(taskId);
    }

}
