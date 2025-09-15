#' Resolve Bedrock Agent Runtime host + region
#' @keywords internal
resolve_bedrock_endpoint <- function(region = NULL) {
  region <- region %||%
    Sys.getenv("AWS_REGION", unset = "") %||%
    Sys.getenv("AWS_DEFAULT_REGION", unset = "")
  if (!nzchar(region)) region <- "us-west-2"
  
  endpoint_override <- Sys.getenv("AWS_BEDROCK_ENDPOINT", unset = "")
  host <- if (nzchar(endpoint_override)) endpoint_override else sprintf("bedrock-agent-runtime.%s.amazonaws.com", region)
  list(region = region, host = host)
}
