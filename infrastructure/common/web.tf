resource "aws_ecs_service" "ecs_web_service" {
  name            = "${var.service_name}-${var.environment}-tf"
  task_definition = "${aws_ecs_task_definition.web_td.arn}"
  cluster         = "${var.cluster_name}"
  iam_role        = "${var.ecs_iam_role_id}"
  desired_count   = "${var.api_count}"

  load_balancer {
    target_group_arn = "${aws_alb_target_group.web.arn}"

    // needs to have the same name defined in the web_td definition
    container_name = "${var.service_name}-${var.environment}"
    container_port = "${var.container_port}"
  }

  // at least 1 task in 5 needs to stay up
  // deployment_minimum_healthy_percent = 20
}

resource "aws_alb_target_group" "web" {
  name                 = "${var.service_name}-${var.environment}-tf"
  port                 = 80
  protocol             = "HTTP"
  vpc_id               = "${var.vpc_id}"
  deregistration_delay = 30
  slow_start           = 600

  health_check {
    healthy_threshold   = 2
    unhealthy_threshold = 10
    timeout             = 5
    protocol            = "HTTP"
    path                = "/actuator/health"
    interval            = 30
    matcher             = "200,301,302"
  }
}

resource "aws_alb_listener_rule" "web" {
  listener_arn = "${var.alb_private_https_listener_arn}"
  priority     = 84

  action {
    type             = "forward"
    target_group_arn = "${aws_alb_target_group.web.arn}"
  }

  condition {
    field  = "host-header"
    values = ["${aws_route53_record.web_private_r53.fqdn}"]
  }

  depends_on = ["aws_route53_record.web_private_r53"]
}

data "aws_route53_zone" "private" {
  name         = "${var.environment}.aamts.io."
  private_zone = true
}

resource "aws_route53_record" "web_private_r53" {
  zone_id = "${data.aws_route53_zone.private.zone_id}"
  name    = "${var.dns_name != "" ? "${var.dns_name}" : "${var.service_name}"}"
  type    = "CNAME"
  ttl     = "60"
  records = ["${var.alb_private_dns_name}"]
}

resource "aws_alb_listener_rule" "web_unsafe" {
  listener_arn = "${var.alb_private_http_listener_arn}"
  priority     = 84

  action {
    type             = "forward"
    target_group_arn = "${aws_alb_target_group.web.arn}"
  }

  condition {
    field  = "host-header"
    values = ["${aws_route53_record.web_private_r53.fqdn}"]
  }

  depends_on = ["aws_route53_record.web_private_r53"]
}

resource "aws_ecs_task_definition" "web_td" {
  family                = "${var.service_name}-${var.environment}-tf"
  container_definitions = "${data.template_file.web_td_template.rendered}"
}

data "template_file" "web_td_template" {
  template = "${file("${path.module}/task_definitions/web_td_template.json")}"

  vars {
    service_name   = "${var.service_name}-${var.environment}"
    service_image  = "${var.docker_registry}/${var.service_image}"
    release  = "${var.release}"
    container_port = "${var.container_port}"
    cluster_name   = "${var.cluster_name}"

    # env variables in the container
    db_host     = "${var.db_host_1}"
    db_name     = "${var.db_name}"
    db_pass     = "${var.db_password}"
    db_port     = "${var.db_port}"
    db_user     = "${var.db_user}"
    sentry_dsn  = "${var.sentry_dsn}"
    environment = "${var.environment}"

    # RabbitMQ
    broker        = "${var.broker}"
    broker_user   = "${var.broker_user}"
    broker_passwd = "${var.broker_passwd}"
    broker_vhost  = "${var.broker_vhost}"

    # Java
    jvm_opt_min_memory = "${var.jvm_opt_min_memory}"
    jvm_opt_max_memory = "${var.jvm_opt_max_memory}"

    # Redis
    redis_host = "${var.redis_host}"
    redis_port = "${var.redis_port}"

    # Kafka
    thunderstorm_kafka_bootstrapaddress = "${var.thunderstorm_kafka_bootstrapaddress}"

    # Service Endpoint
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
}
