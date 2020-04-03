# Playlist Service

Playlist service is a micro service that provide management interfaces and responsible for sync/send playlist.

## publish playlist
```text
Publish playlist will send playlist.data.
```

## transfer playlist
```text
Received pos.data,check assigned playlist,send playlist to site or sync playlist from site.
```
### send playlist to site
```text
playlist.send.request -> playlist.send.response -> playlist.action.response
```

### sync playlist from site
```text
playlist.hash-sync.request -> playlist.hash-sync.response -> playlist.sync.request -> playlist.sync.response -> playlist.data
```

## fast track api

Fast track to send message manually and mandatory.

1.sync playlist from site
```text
/api/core/playlist/mq/request_playlist_hash
```

2.send playlist to site
```text
/api/core/playlist/mq/request_playlist_send
```

3.send playlist.data
```text
/api/core/playlist/mq/send_playlist_data
```

## authentication config

1.if authentication use redis config like this
```yaml
  redis:
    host: k8s-dev-1.aamcn.com.cn
    port: 32204
    password:
```
2.config authentication center endpoint like this
```yaml
thunderstorm:
  auth:
    pathPatterns:
      - /**
    excludePathPatterns:
      - /api/core/**
      - /swagger-ui.html/**
      - /webjars/**
      - /v2/**
      - /swagger-resources/**
      - /api-docs/**
      - /actuator/**
    centerUrl: http://k8s-dev-1.aamcn.com.cn:32115
    securityUri: ${thunderstorm.auth.centerUrl}/api/core/auth/security
```

## other micro service config

For example:
```yaml
  complexService:
    endpoint: http://k8s-dev-1.aamcn.com.cn:32102
    getLmsUri: ${thunderstorm.complexService.endpoint}/api/core/complex/{complex_uuid}/lms
  userService:
    endpoint: http://k8s-dev-1.aamcn.com.cn
    getOrgUri: ${thunderstorm.userService.endpoint}/api/core/user/complex/{complex_uuid}/organization
```
