variable "region" {
  default = "eu-west-1"
}

variable "environment" {
  default = "production"
}

variable "remote_state_bucket" {
  default = "aam-tf-prodcd-eu-west-1-app"
}

variable "remote_rabbit_state_key" {
  default = "aam-production-prodcd-ts-rabbitmq"
}

variable "remote_cluster_state_key" {
  default = "aam-production-prodcd-china-cluster"
}

variable "db_password" {
  default = "DH2RCmH5n2CPmTPp"
}

variable "rabbitmq_core_password" {
  default = "ZVtcrRfHGPamD9Md"
}

# From command line
variable "service_image" {}

variable "release" {}

variable "api_count" {
  default = 2
}

variable "max_capacity" {
  default = 10
}

# Service Endpoints
variable "thunderstorm_complexservice_endpoint" {
  default = "https://complex-service.production.aamts.io"
}

variable "thunderstorm_userservice_endpoint" {
  default = "https://user.production.aamts.io"
}

variable "thunderstorm_posservice_endpoint" {
  default = "https://pos.production.aamts.io"
}

variable "thunderstorm_titleservice_endpoint" {
  default = "https://title-service.aamthunderstorm.com"
}

variable "thunderstorm_cplservice_endpoint" {
  default = "https://cpl-service.aamthunderstorm.com"
}

variable "thunderstorm_producerviewservice_endpoint" {
  default = "https://producer-view.production.aamts.io"
}

variable "thunderstorm_kafka_bootstrapaddress" {
  default = "kafka-1.production.aamts.io:9092,kafka-2.production.aamts.io:9092,kafka-3.production.aamts.io:9092"
}

variable "thunderstorm_taskservice_endpoint" {
  default = "https://task-service.production.aamts.io"
}

variable "thunderstorm_playlistservice_endpoint" {
  default = "https://playlist-svc.production.aamts.io"
}

# profile
variable "spring_profiles_active" {
  default = "prod"
}

variable "thunderstorm_jobservice_endpoint" {
  default = "https://job-service.staging.aamts.io"
}