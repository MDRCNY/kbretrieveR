#' Resolve AWS credentials
#'
#' Wrapper around aws.signature::locate_credentials() with simple error messaging.
#' @keywords internal
get_aws_creds <- function() {
  creds <- tryCatch(aws.signature::locate_credentials(), error = function(e) NULL)
  if (is.null(creds) || is.null(creds$key) || is.null(creds$secret)) {
    stop("Could not locate AWS credentials. Set AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY or rely on an IAM role.", call. = FALSE)
  }
  creds
}