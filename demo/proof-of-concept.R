# --- Bedrock KB retrieve (GovCloud) via httr + aws.signature ------------------
# Copy–paste this whole script and run.

library(httr)
library(jsonlite)
library(aws.signature)
library(digest)
library(tibble)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

kb_retrieve_httr <- function(kb_id,
                             question,
                             region = "us-gov-west-1",
                             number_of_results = 5,
                             verbose = TRUE) {
  stopifnot(is.character(kb_id), nchar(kb_id) > 0)
  stopifnot(is.character(question), nchar(question) > 0)
  
  # Use whatever creds are present (env vars, profile, etc.)
  creds <- aws.signature::locate_credentials()
  if (isTRUE(verbose)) {
    msg <- sprintf("Signing in region=%s with key=%s…", region, substr(creds$key %||% "", 1, 4))
    message(msg)
  }
  
  # Build payload for Agent Runtime /retrieve
  body <- list(
    retrievalQuery = list(text = question),
    retrievalConfiguration = list(
      vectorSearchConfiguration = list(numberOfResults = number_of_results)
    )
  )
  json_str <- jsonlite::toJSON(body, auto_unbox = TRUE)
  json_raw <- charToRaw(json_str)
  
  host     <- sprintf("bedrock-agent-runtime.%s.amazonaws.com", region)
  uri      <- sprintf("/knowledgebases/%s/retrieve", kb_id)
  endpoint <- paste0("https://", host, uri)
  
  amz_date <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  token    <- creds$session_token %||% Sys.getenv("AWS_SESSION_TOKEN")
  body_sha <- digest::digest(json_raw, algo = "sha256")
  
  # Canonical headers to sign (do NOT include the security token here)
  canon <- list(
    host                   = host,
    `x-amz-date`           = amz_date,
    `x-amz-content-sha256` = body_sha
  )
  
  sig <- aws.signature::signature_v4_auth(
    datetime          = amz_date,
    region            = region,
    service           = "bedrock",
    verb              = "POST",
    action            = uri,
    query_args        = list(),
    canonical_headers = canon,
    request_body      = json_raw,          # raw bytes
    key               = creds$key,
    secret            = creds$secret,
    session_token     = token
  )
  
  # Build headers exactly as signed (+ security token header)
  hdrs <- c(
    "Host"                 = host,
    "Content-Type"         = "application/json",
    "X-Amz-Date"           = amz_date,
    "X-Amz-Content-Sha256" = body_sha,
    "Authorization"        = sig$SignatureHeader
  )
  if (nzchar(token)) {
    hdrs["X-Amz-Security-Token"] <- token
  }
  
  resp <- httr::POST(
    endpoint,
    body   = json_str,
    encode = "raw",
    httr::add_headers(.headers = hdrs)
    # add httr::verbose() if you want wire logs
  )
  
  if (resp$status_code != 200) {
    stop(sprintf("KB retrieve failed: HTTP %s\n%s",
                 resp$status_code, httr::content(resp, "text")))
  }
  
  out <- httr::content(resp, as = "parsed")
  
  if (!length(out$retrievalResults)) {
    return(tibble(score = double(), uri = character(), text = character()))
  }
  
  # NOTE: /retrieve returns results directly (no retrievedReferences)
  tbl <- tibble::as_tibble(do.call(rbind, lapply(out$retrievalResults, function(r) {
    data.frame(
      score = r$score %||% NA_real_,
      uri   = r$location$s3Location$uri %||% (r$location$webLocation$url %||% NA_character_),
      text  = r$content$text %||% "",
      stringsAsFactors = FALSE
    )
  })))
  
  # Order by score (desc) if present
  if ("score" %in% names(tbl)) {
    tbl <- tbl[order(tbl$score, decreasing = TRUE), , drop = FALSE]
    rownames(tbl) <- NULL
  }
  tbl
}

# ------------------------------ EXAMPLES --------------------------------------

kb_id  <- Sys.getenv("AWS_KB_ID")     # e.g. "YAYHHVA7HP"
region <- "us-gov-west-1"

# 1) ellmer question
res1 <- kb_retrieve_httr(
  kb_id    = kb_id,
  question = "Tell me about ellmer?",
  region   = region,
  number_of_results = 5
)
print(res1[, c("score", "uri")])
cat("\nFirst snippet:\n", substr(res1$text[1] %||% "", 1, 400), "\n")

# 2) RStudio Workbench question
res2 <- kb_retrieve_httr(
  kb_id    = kb_id,
  question = "What is RStudio Workbench?",
  region   = region,
  number_of_results = 5
)
print(res2[, c("score", "uri")])
cat("\nFirst snippet:\n", substr(res2$text[1] %||% "", 1, 400), "\n")
