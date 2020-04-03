package com.aam.producer.playlist.app;

import com.aam.authentication.acl.EnableAuthAcl;
import com.aam.producer.lib.ImportTsLib;
import com.aam.utils.ImportCommonUtils;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.retry.annotation.EnableRetry;

/**
 * spring-boot application
 *
 * @author oliver.lo
 * @since 2019/3/22 3:16 PM
 */
@SpringBootApplication
@ComponentScan(value = {"com.aam.producer.playlist"})
@EnableAuthAcl
@ImportCommonUtils
@ImportTsLib
@EnableCaching
@EnableRetry
public class PlaylistApplication {

    public static void main(String[] args) {
        SpringApplication.run(PlaylistApplication.class, args);
    }

}
