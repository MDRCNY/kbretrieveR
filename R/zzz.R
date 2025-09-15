.onLoad <- function(libname, pkgname) {
  op <- options()
  op.mykb <- list(
    mykb.default_region = Sys.getenv("AWS_REGION", Sys.getenv("AWS_DEFAULT_REGION", "us-west-2"))
  )
  toset <- !(names(op.mykb) %in% names(op))
  if (any(toset)) options(op.mykb[toset])
  invisible()
}
