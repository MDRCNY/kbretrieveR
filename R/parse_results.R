#' Parse /retrieve JSON into tibble (robust, with metadata)
#'
#' Robust parser for the Bedrock Agent Runtime `/retrieve` response.
#' It accepts multiple shapes for `retrievalResults`:
#' - list of lists (expected)
#' - list of JSON strings (each element is JSON text)
#' - atomic scalars (e.g. numeric or string) — stored in `text` or moved to `score` if numeric-like
#'
#' The returned tibble includes provenance columns:
#' `source_uri`, `chunk_id`, `data_source_id`, and `type` (when available).
#'
#' @param out Parsed JSON (list) returned by `jsonlite::fromJSON(..., simplifyVector = FALSE)` or `httr::content(resp, as = "parsed")`.
#' @return A tibble with columns: `score` (numeric, may be NA), `uri` (character, may be NA),
#'   `text` (character), `source_uri`, `chunk_id`, `data_source_id`, `type`.
#' @name parse_kb_retrieve
#' @keywords internal
NULL

parse_kb_retrieve <- function(out) {
  # empty tidy result with metadata columns
  empty_tbl <- tibble::tibble(
    score = double(),
    uri = character(),
    text = character(),
    source_uri = character(),
    chunk_id = character(),
    data_source_id = character(),
    type = character()
  )
  
  if (is.null(out)) return(empty_tbl)
  
  rr <- out$retrievalResults %||% out[["retrievalResults"]]
  if (is.null(rr) || length(rr) == 0) return(empty_tbl)
  
  # Safely pluck nested keys from a (possibly nested) list.
  pluck_safe <- function(x, ...) {
    keys <- list(...)
    cur <- x
    for (k in keys) {
      if (is.null(cur)) return(NULL)
      # If cur is an atomic JSON string, attempt to parse it to a list.
      if (is.atomic(cur) && is.character(cur) && length(cur) == 1 && grepl("^\\s*\\{", cur)) {
        parsed <- tryCatch(jsonlite::fromJSON(cur, simplifyVector = FALSE), error = function(e) NULL)
        if (!is.null(parsed)) cur <- parsed
      }
      if (is.list(cur) && !is.null(cur[[k]])) {
        cur <- cur[[k]]
      } else {
        return(NULL)
      }
    }
    cur
  }
  
  # Helper to detect numeric-like strings
  is_numeric_like <- function(x) {
    if (is.null(x)) return(FALSE)
    if (!is.character(x)) x <- as.character(x)
    grepl("^\\s*[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?\\s*$", x)
  }
  
  rows <- lapply(seq_along(rr), function(i) {
    r <- rr[[i]]
    
    # If element is a JSON string, parse it
    if (is.atomic(r) && is.character(r) && length(r) == 1 && grepl("^\\s*\\{", r)) {
      parsed <- tryCatch(jsonlite::fromJSON(r, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(parsed)) r <- parsed
    }
    
    # If element is an atomic scalar (number/string), wrap into content$text
    if (is.atomic(r) && !is.list(r)) {
      r <- list(content = list(text = as.character(r)))
    }
    
    # Safely pluck fields; may return NULL
    score_val <- pluck_safe(r, "score")
    uri_val   <- pluck_safe(r, "location", "s3Location", "uri")
    if (is.null(uri_val)) uri_val <- pluck_safe(r, "location", "webLocation", "url")
    text_val  <- pluck_safe(r, "content", "text")
    
    # metadata fields (if present)
    meta_source <- pluck_safe(r, "metadata", "x-amz-bedrock-kb-source-uri")
    meta_chunk  <- pluck_safe(r, "metadata", "x-amz-bedrock-kb-chunk-id")
    meta_ds     <- pluck_safe(r, "metadata", "x-amz-bedrock-kb-data-source-id")
    type_val    <- pluck_safe(r, "content", "type") %||% pluck_safe(r, "location", "type")
    
    # Final type-safety / defaults
    if (is.null(score_val) || length(score_val) == 0) score_val <- NA_real_
    if (is.null(uri_val)   || length(uri_val)   == 0) uri_val   <- NA_character_
    if (is.null(text_val)  || length(text_val)  == 0) text_val  <- ""
    if (is.null(meta_source) || length(meta_source) == 0) meta_source <- NA_character_
    if (is.null(meta_chunk)  || length(meta_chunk)  == 0) meta_chunk  <- NA_character_
    if (is.null(meta_ds)     || length(meta_ds)     == 0) meta_ds     <- NA_character_
    if (is.null(type_val)    || length(type_val)    == 0) type_val    <- NA_character_
    
    data.frame(
      score = as.numeric(score_val),
      uri = as.character(uri_val),
      text = as.character(text_val),
      source_uri = as.character(meta_source),
      chunk_id = as.character(meta_chunk),
      data_source_id = as.character(meta_ds),
      type = as.character(type_val),
      stringsAsFactors = FALSE
    )
  })
  
  tbl <- tryCatch(tibble::as_tibble(do.call(rbind, rows)), error = function(e) empty_tbl)
  
  # Convert numeric-like text entries into score if score is NA
  idx <- is.na(tbl$score) & vapply(tbl$text, is_numeric_like, logical(1))
  if (any(idx)) {
    tbl$score[idx] <- as.numeric(tbl$text[idx])
    tbl$text[idx]  <- ""
  }
  
  # Sort by score (desc) if any non-NA scores exist
  if (any(!is.na(tbl$score))) {
    ord <- order(tbl$score, decreasing = TRUE, na.last = TRUE)
    tbl  <- tbl[ord, , drop = FALSE]
    rownames(tbl) <- NULL
  }
  
  tbl
}
