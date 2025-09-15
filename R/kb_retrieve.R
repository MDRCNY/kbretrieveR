#' Retrieve context from Bedrock Knowledge Base (robust)
#'
#' @param kb_id Knowledge Base ID (character).
#' @param question Query string.
#' @param region AWS region (optional). If `NULL` it will use `AWS_REGION` / `AWS_DEFAULT_REGION` or fallback to "us-west-2".
#' @param number_of_results Number of vector search results to return.
#' @param verbose Logical; print signing info.
#' @param return_raw Logical; if TRUE return list(parsed = tibble, raw_text = <json>, raw_parsed = <list>).
#' @param verbose_http Logical; if TRUE include httr::verbose() for wire logs.
#' @return By default a tibble with columns `score`, `uri`, `text`, and metadata. If `return_raw=TRUE` a list with elements `parsed`, `raw_text`, `raw_parsed`.
#' @export
kb_retrieve_httr <- function(kb_id,
                             question,
                             region = NULL,
                             number_of_results = 5,
                             verbose = TRUE,
                             return_raw = FALSE,
                             verbose_http = FALSE) {
  stopifnot(is.character(kb_id), nchar(kb_id) > 0)
  stopifnot(is.character(question), nchar(question) > 0)
  
  creds <- get_aws_creds()
  ep <- resolve_bedrock_endpoint(region)
  region <- ep$region
  host <- ep$host
  
  if (isTRUE(verbose)) {
    message(sprintf("Using region=%s (host=%s). Signing with key=%s…",
                    region, host, substr(creds$key %||% "", 1, 4)))
  }
  
  body <- list(
    retrievalQuery = list(text = question),
    retrievalConfiguration = list(
      vectorSearchConfiguration = list(numberOfResults = number_of_results)
    )
  )
  json_str <- jsonlite::toJSON(body, auto_unbox = TRUE)
  json_raw <- charToRaw(json_str)    # bytes used for signing and sending
  
  uri <- sprintf("/knowledgebases/%s/retrieve", kb_id)
  endpoint <- paste0("https://", host, uri)
  
  amz_date <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  token <- creds$session_token %||% Sys.getenv("AWS_SESSION_TOKEN", unset = "")
  body_sha <- digest::digest(json_raw, algo = "sha256")
  
  canon <- list(
    host = host,
    `x-amz-date` = amz_date,
    `x-amz-content-sha256` = body_sha
  )
  service_name <- Sys.getenv("AWS_BEDROCK_SERVICE", unset = "bedrock")
  
  sig <- aws.signature::signature_v4_auth(
    datetime = amz_date,
    region = region,
    service = service_name,
    verb = "POST",
    action = uri,
    query_args = list(),
    canonical_headers = canon,
    request_body = json_raw,
    key = creds$key,
    secret = creds$secret,
    session_token = token
  )
  
  hdrs <- c(
    "Host" = host,
    "Content-Type" = "application/json",
    "X-Amz-Date" = amz_date,
    "X-Amz-Content-Sha256" = body_sha,
    "Authorization" = sig$SignatureHeader
  )
  if (nzchar(token)) hdrs["X-Amz-Security-Token"] <- token
  
  # Use internal wrapper so tests can mock. Pass raw bytes (json_raw).
  if (isTRUE(verbose_http)) {
    resp <- .kbretrieve_post(endpoint, json_raw, hdrs, verbose = TRUE)
  } else {
    resp <- .kbretrieve_post(endpoint, json_raw, hdrs, verbose = FALSE)
  }
  
  raw_text <- httr::content(resp, as = "text", encoding = "UTF-8")
  
  if (resp$status_code != 200) {
    stop(sprintf("KB retrieve failed: HTTP %s\n%s", resp$status_code, raw_text), call. = FALSE)
  }
  
  out_parsed <- tryCatch(jsonlite::fromJSON(raw_text, simplifyVector = FALSE),
                         error = function(e) stop("Failed to parse JSON response: ", e$message, call. = FALSE))
  
  tbl <- parse_kb_retrieve(out_parsed)
  
  if (return_raw) {
    return(list(parsed = tbl, raw_text = raw_text, raw_parsed = out_parsed))
  }
  
  tbl
}

# Internal POST wrapper to make the network call testable/mocked
.kbretrieve_post <- function(endpoint, raw_body, headers, verbose = FALSE) {
  if (isTRUE(verbose)) {
    httr::POST(endpoint,
               body = raw_body,
               encode = "raw",
               httr::add_headers(.headers = headers),
               httr::verbose())
  } else {
    httr::POST(endpoint,
               body = raw_body,
               encode = "raw",
               httr::add_headers(.headers = headers))
  }
}
