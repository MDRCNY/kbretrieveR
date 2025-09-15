#' KBClient: R6 client for Bedrock KB + chat integration
#'
#' Lightweight R6 client that wraps `kb_retrieve_httr()` to fetch context from an
#' AWS Bedrock Knowledge Base and then calls a configurable chat client (e.g.,
#' an `ellmer::chat_aws_bedrock()` object or any `function(prompt) -> character`).
#' It stores convenient defaults (kb_id, region, chat client, and prompt limits),
#' so you can call `$retrieve()` and `$chat()` without repeating parameters.
#'
#' @name KBClient
#' @docType class
#' @format An \code{R6Class} generator object.
#' @export
#'
#' @field kb_id Knowledge Base ID (character). May be `NULL`; falls back to `Sys.getenv("AWS_KB_ID")`.
#' @field region AWS region (character or `NULL`). If `NULL`, retrieval resolves region internally.
#' @field chat_client A chat client (e.g., from `ellmer`) or a function `function(prompt) -> character`.
#' @field default_number_of_results Default number of KB results to retrieve (integer, default `5`).
#' @field default_max_snippets Default number of snippets injected into the LLM prompt (integer, default `5`).
#' @field default_snippet_chars Maximum characters per snippet (integer, default `1500`).
#' @field include_metadata Logical; include basic metadata when building prompts (default `TRUE`).
#' @field verbose Logical; print progress messages (default `TRUE`).
#' @export
#' @exportClass KBClient 
KBClient <- R6::R6Class(
  "KBClient",
  public = list(
    
    #-----------------------------#
    # Fields (R6 public members)  #
    #-----------------------------#
    kb_id = NULL,
    region = NULL,
    chat_client = NULL,
    default_number_of_results = 5,
    default_max_snippets = 5,
    default_snippet_chars = 1500,
    include_metadata = TRUE,
    verbose = TRUE,
    
    #' Initialize a KBClient
    #'
    #' @description
    #' Constructor for the KBClient. Stores defaults for future calls to
    #' `$retrieve()` and `$chat()`.
    #'
    #' @param kb_id KB ID (character) or `NULL` to use `Sys.getenv("AWS_KB_ID")`.
    #' @param region AWS region (character) or `NULL` to let retrieval resolve it.
    #' @param chat_client Chat client (e.g. `ellmer::chat_aws_bedrock(...)`), or a function `function(prompt) -> character`.
    #' @param default_number_of_results Default KB retrieve size (integer, default `5`).
    #' @param default_max_snippets Default number of snippets to inject into prompt (integer, default `5`).
    #' @param default_snippet_chars Default maximum characters per snippet (integer, default `1500`).
    #' @param include_metadata Logical; include metadata when building prompts (default `TRUE`).
    #' @param verbose Logical; print progress messages (default `TRUE`).
    #' @return A new `KBClient` object.
    initialize = function(kb_id = NULL,
                          region = NULL,
                          chat_client = NULL,
                          default_number_of_results = 5,
                          default_max_snippets = 5,
                          default_snippet_chars = 1500,
                          include_metadata = TRUE,
                          verbose = TRUE) {
      self$kb_id <- if (!is.null(kb_id)) kb_id else Sys.getenv("AWS_KB_ID", unset = NA_character_)
      self$region <- region
      self$chat_client <- chat_client %||% getOption("kbretrieveR.chat_client", NULL)
      self$default_number_of_results <- default_number_of_results %||% 5
      self$default_max_snippets <- default_max_snippets %||% 5
      self$default_snippet_chars <- default_snippet_chars %||% 1500
      self$include_metadata <- include_metadata
      self$verbose <- verbose
    },
    
    #' Retrieve KB context (snippets) for a question
    #'
    #' @description
    #' Calls Bedrock KB `/retrieve` and returns a tibble of snippets (or a raw list
    #' when `return_raw = TRUE`).
    #'
    #' @param question User question (character, non-empty).
    #' @param number_of_results Integer override for KB retrieval size. If `NULL`,
    #'   uses `self$default_number_of_results`.
    #' @param return_raw Logical; if `TRUE`, returns a list with `parsed`, `raw_text`,
    #'   and `raw_parsed` from the API response.
    #' @param verbose Logical override; if `NULL`, uses `self$verbose`.
    #' @return A tibble with columns like `score`, `uri`, `text`, and metadata;
    #'   or a list when `return_raw = TRUE`.
    retrieve = function(question,
                        number_of_results = NULL,
                        return_raw = FALSE,
                        verbose = NULL) {
      stopifnot(is.character(question), nchar(question) > 0)
      
      number_of_results <- number_of_results %||% self$default_number_of_results
      verbose <- verbose %||% self$verbose
      kb_id <- if (!is.na(self$kb_id) && nzchar(self$kb_id)) self$kb_id else Sys.getenv("AWS_KB_ID", unset = "")
      if (!nzchar(kb_id)) stop("kb_id not set in client and AWS_KB_ID env var is empty.", call. = FALSE)
      
      kb_retrieve_httr(
        kb_id = kb_id,
        question = question,
        region = self$region,
        number_of_results = number_of_results,
        verbose = verbose,
        return_raw = return_raw
      )
    },
    
    #' Chat using KB context
    #'
    #' @description
    #' Retrieves KB context for the question, builds a prompt (via `kb_chat_with_ellmer()`),
    #' and returns the assistant text (or a debug list). When `append_sources = TRUE`,
    #' a **Sources** section is appended to the reply with the URIs (and chunk ids)
    #' of the snippets included.
    #'
    #' @param question User question (character, non-empty).
    #' @param kb_id Optional KB ID override (character). If `NULL`, uses client field or env var.
    #' @param chat_client Optional one-off chat client; if `NULL`, uses `self$chat_client`.
    #' @param number_of_results Optional integer override for KB retrieval size. If `NULL`, uses client default.
    #' @param max_snippets Integer; number of top snippets to inject into the LLM prompt. Defaults to client setting.
    #' @param snippet_chars Integer; maximum characters per snippet. Defaults to client setting.
    #' @param append_sources Logical; when `TRUE`, append a **Sources** section to the reply (default `FALSE`).
    #' @param return_raw Logical; if `TRUE`, returns a list with `response`, `parsed_kb`,
    #'   `raw_parsed`, and `prompt`.
    #' @param verbose Logical override; if `NULL`, uses `self$verbose`.
    #' @return Character assistant reply, or a list when `return_raw = TRUE`.
    chat = function(question,
                    kb_id = NULL,
                    chat_client = NULL,
                    number_of_results = NULL,
                    max_snippets = NULL,
                    snippet_chars = NULL,
                    append_sources = FALSE,
                    return_raw = FALSE,
                    verbose = NULL) {
      stopifnot(is.character(question), nchar(question) > 0)
      
      # resolve client to use
      client_to_use <- chat_client %||% self$chat_client
      if (is.null(client_to_use)) stop("No chat_client available. Use $set_chat_client() or supply chat_client.", call. = FALSE)
      
      # resolve verbosity and snippet limits
      verbose <- verbose %||% self$verbose
      max_snippets  <- max_snippets %||% self$default_max_snippets
      snippet_chars <- snippet_chars %||% self$default_snippet_chars
      
      # resolve kb_id (argument -> client field -> env var)
      kb_id_resolved <- kb_id %||% (if (!is.null(self$kb_id) && nzchar(self$kb_id)) self$kb_id else Sys.getenv("AWS_KB_ID", unset = ""))
      if (!nzchar(kb_id_resolved)) stop("kb_id not set (argument, client, or AWS_KB_ID env var).", call. = FALSE)
      
      # resolve number_of_results (argument -> client default -> fallback literal 5)
      number_of_results_resolved <- as.integer(number_of_results %||% self$default_number_of_results %||% 5)
      
      # call the helper that builds the prompt and calls the client
      kb_chat_with_ellmer(
        kb_id = kb_id_resolved,
        question = question,
        chat_client = client_to_use,
        region = self$region,
        number_of_results = number_of_results_resolved,
        max_snippets = max_snippets,
        snippet_chars = snippet_chars,
        include_metadata = self$include_metadata,
        append_sources = append_sources,
        return_raw = return_raw,
        verbose = verbose
      )
    },
    
    #' Inspect raw parsed retrieval results
    #'
    #' @description
    #' Convenience method that pretty-prints a compact summary of the KB retrieval
    #' (using `inspect_kb()`) and invisibly returns the raw retrieval list.
    #'
    #' @param question User question (character).
    #' @param number_of_results Optional integer override for KB retrieval size.
    #' @param max_chars Integer; maximum characters to show per snippet preview (default `160`).
    #' @param verbose Logical override; if `NULL`, uses `self$verbose`.
    #' @return Invisibly returns the raw retrieval list (when available).
    inspect = function(question,
                       number_of_results = NULL,
                       max_chars = 160,
                       verbose = NULL) {
      verbose <- verbose %||% self$verbose
      res <- self$retrieve(question = question, number_of_results = number_of_results, return_raw = TRUE, verbose = verbose)
      if (is.list(res) && !is.null(res$raw_parsed)) {
        inspect_kb(res$raw_parsed, max_chars = max_chars)
        invisible(res)
      } else {
        message("No raw_parsed available to inspect.")
        invisible(res)
      }
    },
    
    #' Set or replace the chat client
    #'
    #' @description
    #' Replaces `self$chat_client` with `client`.
    #'
    #' @param client A chat client (e.g. from `ellmer`) or a function `function(prompt) -> character`.
    #' @return Invisibly returns `self`.
    set_chat_client = function(client) {
      self$chat_client <- client
      invisible(self)
    },
    
    #' Format a single parsed row into a citation string
    #'
    #' @description
    #' Creates a short citation string for a single parsed row (1-row tibble/named list),
    #' optionally as a markdown link.
    #'
    #' @param row A 1-row tibble or a named list with fields like `source_uri`, `uri`, and `chunk_id`.
    #' @param markdown Logical; when `TRUE` (default) return a markdown link; otherwise plain text.
    #' @return Character string (citation) or `NA_character_` if not enough info.
    format_citation = function(row, markdown = TRUE) {
      if (is.null(row)) return(NA_character_)
      if (is.data.frame(row)) {
        if (nrow(row) < 1) return(NA_character_)
        row <- as.list(row[1, , drop = TRUE])
      }
      source_uri <- row$source_uri %||% row$uri %||% NA_character_
      chunk_id <- row$chunk_id %||% NA_character_
      if (is.na(source_uri) || !nzchar(source_uri)) return(NA_character_)
      if (markdown) {
        if (!is.na(chunk_id) && nzchar(chunk_id)) {
          sprintf("[%s (chunk %s)](%s)", basename(source_uri), chunk_id, source_uri)
        } else {
          sprintf("[%s](%s)", basename(source_uri), source_uri)
        }
      } else {
        if (!is.na(chunk_id) && nzchar(chunk_id)) {
          paste0(source_uri, " (chunk: ", chunk_id, ")")
        } else {
          source_uri
        }
      }
    },
    
    #' Return internals as a list (for debugging)
    #'
    #' @description
    #' Returns a list snapshot of key client internals and defaults.
    #'
    #' @return A named list with `kb_id`, `region`, `chat_client`, and a `defaults` sub-list.
    as_list = function() {
      list(
        kb_id = self$kb_id,
        region = self$region,
        chat_client = self$chat_client,
        defaults = list(
          number_of_results = self$default_number_of_results,
          max_snippets = self$default_max_snippets,
          snippet_chars = self$default_snippet_chars,
          include_metadata = self$include_metadata
        )
      )
    }
  )
)
