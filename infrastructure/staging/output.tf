output "task_definition_arns" {
  value = "${module.staging_playlist_service.task_definition_arns}"
}

output "cluster" {
  value = "${module.staging_playlist_service.cluster}"
}

output "playlist_service_url" {
  value = "${module.staging_playlist_service.playlist_service_url}"
}

