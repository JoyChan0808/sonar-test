module "production_playlist_service" {
  source = "../common/"

  # From environment
  environment   = "${var.environment}"
  db_password   = "${var.db_password}"
  broker_passwd = "${var.rabbitmq_core_password}"

  # From remote state
  broker                         = "${data.terraform_remote_state.rabbit_state.rabbitmq_internal_address}"
  broker_user                    = "${data.terraform_remote_state.rabbit_state.rabbitmq_core_user}"
  broker_vhost                   = "${data.terraform_remote_state.rabbit_state.rabbitmq_core_vhost}"
  redis_host                     = "${data.terraform_remote_state.cluster_state.redis_host}"
  redis_port                     = "${data.terraform_remote_state.cluster_state.redis_port}"
  cluster_name                   = "${data.terraform_remote_state.cluster_state.cluster_name}"
  ecs_iam_role_arn               = "${data.terraform_remote_state.cluster_state.ecs_iam_role_arn}"
  ecs_iam_role_id                = "${data.terraform_remote_state.cluster_state.ecs_iam_role_id}"
  db_host_1                      = "${data.terraform_remote_state.cluster_state.db_host_1}"
  ecs_instances_sg_id            = "${data.terraform_remote_state.cluster_state.ecs_instances_sg_id}"
  vpc_id                         = "${data.terraform_remote_state.cluster_state.vpc_id}"
  alb_private_https_listener_arn = "${data.terraform_remote_state.cluster_state.alb_private_https_listener_arn}"
  alb_private_http_listener_arn  = "${data.terraform_remote_state.cluster_state.alb_private_http_listener_arn}"
  alb_private_dns_name           = "${data.terraform_remote_state.cluster_state.alb_private_dns_name}"

  # From command line
  service_image = "${var.service_image}"
  release = "${var.release}"
  api_count     = "${var.api_count}"
  max_capacity  = "${var.max_capacity}"

  # Kafka
  thunderstorm_kafka_bootstrapaddress = "${var.thunderstorm_kafka_bootstrapaddress}"

  # Service endpoint
  thunderstorm_complexservice_endpoint      = "${var.thunderstorm_complexservice_endpoint}"
  thunderstorm_userservice_endpoint         = "${var.thunderstorm_userservice_endpoint}"
  thunderstorm_posservice_endpoint          = "${var.thunderstorm_posservice_endpoint}"
  thunderstorm_titleservice_endpoint        = "${var.thunderstorm_titleservice_endpoint}"
  thunderstorm_cplservice_endpoint          = "${var.thunderstorm_cplservice_endpoint}"
  thunderstorm_producerviewservice_endpoint = "${var.thunderstorm_producerviewservice_endpoint}"
  thunderstorm_taskservice_endpoint         = "${var.thunderstorm_taskservice_endpoint}"
  thunderstorm_playlistservice_endpoint     = "${var.thunderstorm_playlistservice_endpoint}"
  thunderstorm_jobservice_endpoint          = "${var.thunderstorm_jobservice_endpoint}"

  # profile
  spring_profiles_active = "${var.spring_profiles_active}"
}
