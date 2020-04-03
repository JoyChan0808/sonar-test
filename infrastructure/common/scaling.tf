resource "aws_cloudwatch_metric_alarm" "memory_usage_gte" {
  alarm_name                = "${var.service_name}-memory-usage-gte-${var.environment}-tf"
  comparison_operator       = "GreaterThanOrEqualToThreshold"
  evaluation_periods        = "1"
  metric_name               = "MemoryUtilization"
  namespace                 = "AWS/ECS"
  period                    = "60"
  statistic                 = "Average"
  threshold                 = "95"
  alarm_description         = "This metric monitors ecs service memory usage"
  insufficient_data_actions = []

  dimensions {
    ClusterName = "${var.cluster_name}"
    ServiceName = "${var.service_name}-${var.environment}-tf"
  }

  // trigger the policy memory_scale_up the increments the count of desired instances
  alarm_actions = ["${aws_appautoscaling_policy.memory_scale_up.arn}"]
}

resource "aws_cloudwatch_metric_alarm" "memory_usage_lte" {
  alarm_name                = "${var.service_name}-memory-usage-lte-${var.environment}-tf"
  comparison_operator       = "LessThanOrEqualToThreshold"
  evaluation_periods        = "1"
  metric_name               = "MemoryUtilization"
  namespace                 = "AWS/ECS"
  period                    = "120"
  statistic                 = "Average"
  threshold                 = "75"
  alarm_description         = "This metric monitors ecs service memory usage"
  insufficient_data_actions = []

  dimensions {
    ClusterName = "${var.cluster_name}"
    ServiceName = "${var.service_name}-${var.environment}-tf"
  }

  // trigger the policy memory_scale_down the decrements the count of desired instances
  alarm_actions = ["${aws_appautoscaling_policy.memory_scale_down.arn}"]
}

resource "aws_cloudwatch_metric_alarm" "cpu_usage_gte" {
  alarm_name                = "${var.service_name}-cpu-usage-gte-${var.environment}-tf"
  comparison_operator       = "GreaterThanOrEqualToThreshold"
  evaluation_periods        = "3"
  metric_name               = "CPUUtilization"
  namespace                 = "AWS/ECS"
  period                    = "60"
  statistic                 = "Average"
  threshold                 = "90"
  alarm_description         = "This metric monitors ecs service cpu usage"
  insufficient_data_actions = []

  dimensions {
    ClusterName = "${var.cluster_name}"
    ServiceName = "${var.service_name}-${var.environment}-tf"
  }

  // trigger the policy cpu_scale_up the increments the count of desired instances
  alarm_actions = ["${aws_appautoscaling_policy.cpu_scale_up.arn}"]
}

resource "aws_cloudwatch_metric_alarm" "cpu_usage_lte" {
  alarm_name                = "${var.service_name}-cpu-usage-lte-${var.environment}-tf"
  comparison_operator       = "LessThanOrEqualToThreshold"
  evaluation_periods        = "1"
  metric_name               = "CPUUtilization"
  namespace                 = "AWS/ECS"
  period                    = "180"
  statistic                 = "Average"
  threshold                 = "30"
  alarm_description         = "This metric monitors ecs service cpu usage"
  insufficient_data_actions = []

  dimensions {
    ClusterName = "${var.cluster_name}"
    ServiceName = "${var.service_name}-${var.environment}-tf"
  }

  // trigger the policy cpu_scale_down the decrements the count of desired instances
  alarm_actions = ["${aws_appautoscaling_policy.cpu_scale_down.arn}"]
}

resource "aws_appautoscaling_policy" "memory_scale_up" {
  name               = "${var.service_name}-memory-scale-up-${var.environment}-tf"
  resource_id        = "service/${var.cluster_name}/${var.service_name}-${var.environment}-tf"
  scalable_dimension = "ecs:service:DesiredCount"
  service_namespace  = "ecs"

  step_scaling_policy_configuration {
    adjustment_type         = "ChangeInCapacity"
    cooldown                = 60
    metric_aggregation_type = "Average"

    step_adjustment {
      scaling_adjustment          = 1
      metric_interval_lower_bound = 5.0
    }
  }

  depends_on = ["aws_appautoscaling_target.ecs_web_target"]
}

resource "aws_appautoscaling_policy" "memory_scale_down" {
  name               = "${var.service_name}-memory-scale-down-${var.environment}-tf"
  resource_id        = "service/${var.cluster_name}/${var.service_name}-${var.environment}-tf"
  scalable_dimension = "ecs:service:DesiredCount"
  service_namespace  = "ecs"

  step_scaling_policy_configuration {
    adjustment_type         = "ChangeInCapacity"
    cooldown                = 60
    metric_aggregation_type = "Average"

    step_adjustment {
      scaling_adjustment          = -1
      metric_interval_upper_bound = 0  # If Memory falls below 30% (from alarm) scale down
    }
  }

  depends_on = ["aws_appautoscaling_target.ecs_web_target"]
}

resource "aws_appautoscaling_policy" "cpu_scale_up" {
  name               = "${var.service_name}-cpu-scale-up-${var.environment}-tf"
  resource_id        = "service/${var.cluster_name}/${var.service_name}-${var.environment}-tf"
  scalable_dimension = "ecs:service:DesiredCount"
  service_namespace  = "ecs"

  step_scaling_policy_configuration {
    adjustment_type         = "ChangeInCapacity"
    cooldown                = 60
    metric_aggregation_type = "Average"

    step_adjustment {
      scaling_adjustment          = 1
      metric_interval_lower_bound = 5.0
    }
  }

  depends_on = ["aws_appautoscaling_target.ecs_web_target"]
}

resource "aws_appautoscaling_policy" "cpu_scale_down" {
  name               = "${var.service_name}-cpu-scale-down-${var.environment}-tf"
  resource_id        = "service/${var.cluster_name}/${var.service_name}-${var.environment}-tf"
  scalable_dimension = "ecs:service:DesiredCount"
  service_namespace  = "ecs"

  step_scaling_policy_configuration {
    adjustment_type         = "ChangeInCapacity"
    cooldown                = 60
    metric_aggregation_type = "Average"

    step_adjustment {
      scaling_adjustment          = -1
      metric_interval_upper_bound = 0  # If CPU falls below 5% (from alarm) scale down
    }
  }

  depends_on = ["aws_appautoscaling_target.ecs_web_target"]
}

resource "aws_appautoscaling_target" "ecs_web_target" {
  max_capacity       = "${var.max_capacity}"
  min_capacity       = "${var.api_count}"
  resource_id        = "service/${var.cluster_name}/${var.service_name}-${var.environment}-tf"
  role_arn           = "${var.ecs_iam_role_arn}"
  scalable_dimension = "ecs:service:DesiredCount"
  service_namespace  = "ecs"
}
