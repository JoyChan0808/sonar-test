output "task_definition_arns" {
  value = ["${aws_ecs_task_definition.web_td.arn}"]
}

output "cluster" {
  value = "${var.cluster_name}"
}

output "playlist_service_url" {
  value = "${aws_route53_record.web_private_r53.fqdn}"
}

output "ecs_role" {
  value = "${var.ecs_iam_role_id}"
}
