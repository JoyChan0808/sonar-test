variable "region" {
  default = "eu-west-1"
}

variable "environment" {
  default = "staging"
}

variable "remote_state_bucket" {
  default = "aam-tf-prodcd-eu-west-1-app"
}

variable "remote_rabbit_state_key" {
  default = "aam-staging-prodcd-ts-rabbitmq"
}

variable "remote_cluster_state_key" {
  default = "aam-staging-prodcd-china-cluster"
}

variable "db_password" {
  default = "3C8CPmTPpDH2RCmH"
}

variable "rabbitmq_core_password" {
  default = "s7XfaEmNtcPsHT7f"
}

# From command line
variable "service_image" {}

variable "release" {}

variable "api_count" {
  default = 1
}

variable "max_capacity" {
  default = 2
}


# Service Endpoints
variable "thunderstorm_complexservice_endpoint" {
  default = "https://complex-service.staging.aamts.io"
}

variable "thunderstorm_userservice_endpoint" {
  default = "https://user.staging.aamts.io"
}

variable "thunderstorm_posservice_endpoint" {
  default = "https://pos.staging.aamts.io"
}

variable "thunderstorm_titleservice_endpoint" {
  default = "https://title-service-staging.aamthunderstorm.com"
}

variable "thunderstorm_cplservice_endpoint" {
  default = "https://cpl-service-staging.aamthunderstorm.com"
}

variable "thunderstorm_producerviewservice_endpoint" {
  default = "https://producer-view.staging.aamts.io"
}

variable "thunderstorm_kafka_bootstrapaddress" {
  default = "kafka-1.staging.aamts.io:9092,kafka-2.staging.aamts.io:9092,kafka-3.staging.aamts.io:9092"
}

variable "spring_profiles_active" {
  default = "staging"
}

variable "thunderstorm_taskservice_endpoint" {
  default = "https://task-service.staging.aamts.io"
}

variable "thunderstorm_playlistservice_endpoint" {
  default = "https://playlist-svc.staging.aamts.io"
}

variable "thunderstorm_jobservice_endpoint" {
  default = "https://job-service.staging.aamts.io"
}
