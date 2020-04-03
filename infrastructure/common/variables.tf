variable "region" {
  default = "eu-west-1"
}

variable "container_port" {
  default = 8888
}

variable "service_name" {
  default = "ts-playlist-svc"
}

variable "db_name" {
  default = "playlist"
}

variable "db_user" {
  default = "root"
}

variable "db_port" {
  default = 3306
}

variable "docker_registry" {
  default = "886366864302.dkr.ecr.eu-west-1.amazonaws.com"
}

variable "dns_name" {
  default = "playlist-svc"
}

variable "jvm_opt_min_memory" {
  default = 256
}

variable "jvm_opt_max_memory" {
  default = 512
}

variable "sentry_dsn" {
  default = "https://c5e4c86982284924beb48ec54bd285fe:8bae4e0852c041a2866188cb7e2a7b1d@sentry.aamts.io/30"
}

variable "api_count" {
  default = 1
}

variable "max_capacity" {}

# From remote state
variable "broker" {}

variable "broker_user" {}

variable "broker_vhost" {}

variable "cluster_name" {}

variable "ecs_iam_role_arn" {}

variable "ecs_iam_role_id" {}

variable "db_host_1" {}

variable "ecs_instances_sg_id" {}

variable "vpc_id" {}

variable "alb_private_https_listener_arn" {}

variable "alb_private_http_listener_arn" {}

variable "alb_private_dns_name" {}

# From command line
variable "service_image" {}

variable "release" {}

# Infra
variable "db_password" {}

variable "environment" {}

variable "broker_passwd" {}

variable "redis_host" {}

variable "redis_port" {}

variable "thunderstorm_kafka_bootstrapaddress" {}

# service endpoint
variable "thunderstorm_complexservice_endpoint" {}

variable "thunderstorm_userservice_endpoint" {}

variable "thunderstorm_posservice_endpoint" {}

variable "thunderstorm_titleservice_endpoint" {}

variable "thunderstorm_cplservice_endpoint" {}

variable "thunderstorm_producerviewservice_endpoint" {}

variable "thunderstorm_taskservice_endpoint" {}

variable "thunderstorm_playlistservice_endpoint" {}

variable "thunderstorm_jobservice_endpoint" {}


# profile
variable "spring_profiles_active" {}
