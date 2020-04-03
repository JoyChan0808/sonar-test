ARG REGISTRY=886366864302.dkr.ecr.eu-west-1.amazonaws.com
FROM ${REGISTRY}/artsalliancemedia/jdk:8-v0.0.1

MAINTAINER aam

ARG JAR_FILE
ENV JVM_OPTS ""

COPY ${JAR_FILE} /usr/share/playlist/service.jar

ENTRYPOINT java ${JVM_OPTS} -jar /usr/share/playlist/service.jar
