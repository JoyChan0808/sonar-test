package com.aam.producer.playlist.generator;

//import com.baomidou.mybatisplus.generator.AutoGenerator;
//import com.baomidou.mybatisplus.generator.config.DataSourceConfig;
//import com.baomidou.mybatisplus.generator.config.GlobalConfig;
//import com.baomidou.mybatisplus.generator.config.PackageConfig;
//import com.baomidou.mybatisplus.generator.config.StrategyConfig;
//import com.baomidou.mybatisplus.generator.config.rules.NamingStrategy;

/**
 * mybatis plus generator
 *
 * @author oliver.lo
 * @since 2019/3/22 2:26 PM
 */
public class MybatisPlusGenerator {

    private final static String outputDir = "/Users/luoshaolong/code/test/mybatis-plus";
    private final static String entityName = "%sDO";

    private final static String dbDriverName = "com.mysql.jdbc.Driver";
    private final static String dbUserName = "root";
    private final static String dbPassword = "root";
    private final static String dbUrl = "jdbc:mysql://localhost:3306/playlist";

    private final static String parentPackage = "com.aam.producer.playlist";
    private final static String entityPackage = "repository.entity";
    private final static String mapperPackage = "repository.dao";
    private final static String xmlPackage = "repository.mappers";
    private final static String servicePackage = "biz.service";
    private final static String serviceImplPackage = "biz.service.impl";
    private final static String controllerPackage = "app.rest";

    public static void main(final String[] args) {

//        // generator global config
//        final GlobalConfig config = new GlobalConfig();
//        config.setOutputDir(outputDir);
//        config.setFileOverride(true);
//        config.setBaseResultMap(true); // XML resultMap
//        config.setBaseColumnList(false); // XML
//        config.setEntityName(entityName);
//
//        // database config
//        final DataSourceConfig dataConfig = new DataSourceConfig();
//        dataConfig.setDriverName(dbDriverName);
//        dataConfig.setUsername(dbUserName);
//        dataConfig.setPassword(dbPassword);
//        dataConfig.setUrl(dbUrl);
//
//        // strategy config
//        final StrategyConfig strategy = new StrategyConfig();
//        strategy.setNaming(NamingStrategy.underline_to_camel);
//        strategy.setColumnNaming(NamingStrategy.underline_to_camel);
//        strategy.setInclude(
//                "tms_playlist",
//                "playlist_sync_log",
//                "playlist_send_log");
//        //strategy.setExclude("alembic_version");
//        strategy.setRestControllerStyle(true);
//        strategy.entityTableFieldAnnotationEnable(true);
//        strategy.setEntityBooleanColumnRemoveIsPrefix(true);
//        // auto fill
//        //final List<TableFill> tableFillList = new ArrayList<>();
//        //tableFillList.add(new TableFill("created", FieldFill.INSERT));
//        //tableFillList.add(new TableFill("last_modified", FieldFill.INSERT_UPDATE));
//        //strategy.setTableFillList(tableFillList);
//
//        // package config
//        final PackageConfig packageConfig = new PackageConfig();
//        packageConfig.setParent(parentPackage);
//        packageConfig.setEntity(entityPackage);
//        packageConfig.setMapper(mapperPackage);
//        packageConfig.setXml(xmlPackage);
//        packageConfig.setService(servicePackage);
//        packageConfig.setServiceImpl(serviceImplPackage);
//        packageConfig.setController(controllerPackage);
//
//        final AutoGenerator autoGenerator = new AutoGenerator();
//        autoGenerator.setGlobalConfig(config);
//        autoGenerator.setDataSource(dataConfig);
//        autoGenerator.setStrategy(strategy);
//        autoGenerator.setPackageInfo(packageConfig);
//
//        autoGenerator.execute();
    }
}
