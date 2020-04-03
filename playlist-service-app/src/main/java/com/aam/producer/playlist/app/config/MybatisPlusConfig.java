package com.aam.producer.playlist.app.config;

import com.aam.authentication.acl.utils.UserInfoUtil;
import com.aam.producer.playlist.biz.enums.ResultCodeEnum;
import com.aam.producer.playlist.biz.util.OrgUtil;
import com.aam.utils.exception.BizException;
import com.baomidou.mybatisplus.core.handlers.MetaObjectHandler;
import com.baomidou.mybatisplus.extension.plugins.PaginationInterceptor;
import com.baomidou.mybatisplus.extension.plugins.PerformanceInterceptor;
import com.baomidou.mybatisplus.extension.plugins.tenant.TenantHandler;
import com.baomidou.mybatisplus.extension.plugins.tenant.TenantSqlParser;
import com.google.common.collect.Lists;
import java.util.List;
import net.sf.jsqlparser.expression.Expression;
import net.sf.jsqlparser.expression.StringValue;
import org.apache.ibatis.mapping.MappedStatement;
import org.apache.ibatis.reflection.MetaObject;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;

/**
 * mybatis plus config
 *
 * @author oliver.lo
 * @since 2019/3/25 9:50 AM
 */
@Configuration
@MapperScan("com.aam.producer.playlist.repository.dao")
public class MybatisPlusConfig {

    private static final String SYSTEM_TENANT_ID = "organization_id";

    private static final List<String> IGNORE_TENANT_TABLES = Lists
            .newArrayList("playlist_sync_log", "playlist_send_log", "playlist_publish_log",
                    "tms_playlist", "pos_playlist_mapping", "pos_mapping_send_log",
                    "playlist_delete_log");

    private static final List<String> IGNORE_TENANT_SQL_IDS = Lists.newArrayList(
            "com.aam.producer.playlist.repository.dao.SegmentMapper.getAutomaticSegment",
            "com.aam.producer.playlist.repository.dao.SegmentMapper.getApiSegmentNames",
            "com.aam.producer.playlist.repository.dao.SegmentMapper.getAllSplitWeekSegment",
            "com.aam.producer.playlist.repository.dao.PlaylistSegmentSplitAssociationMapper.getAllSegmentSplitByTitle",
            "com.aam.producer.playlist.repository.dao.PlaylistMapper.getByIdIgnoreOrgId",
            "com.aam.producer.playlist.repository.dao.PlaylistMapper.getAllSegment"
    );

    @Bean
    public PaginationInterceptor paginationInterceptor() {
        PaginationInterceptor paginationInterceptor = new PaginationInterceptor();

        TenantSqlParser tenantSqlParser = new TenantSqlParser()
                .setTenantHandler(new TenantHandler() {

                    @Override
                    public Expression getTenantId() {
                        String organization;
                        if (UserInfoUtil.get() == null) {
                            organization = OrgUtil.orgContenter.get();
                        } else {
                            organization = UserInfoUtil.getOrganizationId();
                        }
                        if (organization == null) {
                            throw new BizException(ResultCodeEnum.ORGANIZATION_NOT_FIND);
                        }
                        return new StringValue(organization);
                    }

                    @Override
                    public String getTenantIdColumn() {
                        return SYSTEM_TENANT_ID;
                    }

                    @Override
                    public boolean doTableFilter(String tableName) {
                        return IGNORE_TENANT_TABLES.stream()
                                .anyMatch((e) -> e.equalsIgnoreCase(tableName));
                    }
                });
        paginationInterceptor.setSqlParserList(Lists.newArrayList(tenantSqlParser));
        paginationInterceptor.setSqlParserFilter(metaObject -> {
            final MappedStatement mappedStatement = (MappedStatement) metaObject
                    .getValue("delegate.mappedStatement");
            if (IGNORE_TENANT_SQL_IDS.stream()
                    .anyMatch((e) -> e.equalsIgnoreCase(mappedStatement.getId()))) {
                return true;
            }
            return UserInfoUtil.isAdmin();
        });
        return paginationInterceptor;
    }


    @Bean(name = "performanceInterceptor")
    public PerformanceInterceptor performanceInterceptor() {
        PerformanceInterceptor performanceInterceptor = new PerformanceInterceptor();
        performanceInterceptor.setMaxTime(3000L);
        performanceInterceptor.setWriteInLog(true);
        return performanceInterceptor;
    }

    @Component
    public static class MybatisObjectHandler implements MetaObjectHandler {

        @Override
        public void insertFill(MetaObject metaObject) {
            Long time = System.currentTimeMillis();
            this.setFieldValByName("created", time, metaObject);
            this.setFieldValByName("lastModified", time, metaObject);
        }

        @Override
        public void updateFill(MetaObject metaObject) {
            Long time = System.currentTimeMillis();
            this.setFieldValByName("lastModified", time, metaObject);
        }
    }

}
