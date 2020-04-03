data "terraform_remote_state" "cluster_state" {
  backend = "s3"

  config {
    region = "${var.region}"
    bucket = "${var.remote_state_bucket}"
    key    = "${var.remote_cluster_state_key}"
  }
}

data "terraform_remote_state" "rabbit_state" {
  backend = "s3"

  config {
    region = "${var.region}"
    bucket = "${var.remote_state_bucket}"
    key    = "${var.remote_rabbit_state_key}"
  }
}
