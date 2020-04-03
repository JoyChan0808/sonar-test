provider "mysql" {
  alias    = "content_management_service_mysql"
  endpoint = "${var.db_host_1}:3306"
  username = "${var.db_user}"
  password = "${var.db_password}"
}

// resource "mysql_database" "content_management_service_db" {
//   name     = "${var.db_name}"
//   provider = "mysql.content_management_service_mysql"
// }

