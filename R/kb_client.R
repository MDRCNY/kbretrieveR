# R/kb_client.R

#' KBClient: R6 client for Bedrock KB + chat integration (with optional streaming)
#'
#' A lightweight R6 client that wraps `kb_retrieve_httr()` to fetch context from an
#' AWS Bedrock Knowledge Base and then calls a configurable chat client (e.g.,
#' an `ellmer::chat_aws_bedrock()` object or any `function(prompt) -> character`).
#'
#' This version adds optional **streaming** support:
#' `KBClient$chat(..., stream = TRUE)` attempts to invoke a streaming entrypoint on
#' the chat client (if available) and returns the underlying stream/promise object
#' so Shiny UIs (e.g., **shinychat**) can render incremental tokens.
#'
#' @section Usage:
#' \preformatted{
#' kb <- KBClient$new(kb_id = "KB123", region = "us-east-1",
#'                    chat_client = ellmer::chat_aws_bedrock("anthropic..."))
#' kb$retrieve("What is X?", number_of_results = 5)
#' kb$chat("Tell me about X", number_of_results = 5)
#' kb$chat("Stream this", stream = TRUE)  # returns stream/promise if client supports it
#' }
#'
#' @section Fields:
#' See individual `@field` entries below.
#'
#' @section Methods:
#' - `$initialize(kb_id, region, chat_client, default_number_of_results, default_max_snippets, default_snippet_chars, include_metadata, verbose)`
#' - `$retrieve(question, number_of_results, return_raw, verbose)`
#' - `$chat(question, kb_id, chat_client, number_of_results, max_snippets, snippet_chars, append_sources, return_raw, verbose, stream, stream_fn)`
#' - `$inspect(question, number_of_results, max_chars, verbose)`
#' - `$set_chat_client(client)`
#' - `$format_citation(row, markdown)`
#' - `$as_list()`
#'
#' @seealso [kb_chat_with_ellmer()]
#' @export
KBClient <- R6::R6Class(
  "KBClient",
  public = list(
    
    #-----------------------------#
    # Fields (R6 public members)  #
    #-----------------------------#
    
    #' @field kb_id Knowledge Base ID (character). If `NULL`, falls back to `Sys.getenv("AWS_KB_ID")`.
    kb_id = NULL,
    
    #' @field region AWS region (character or `NULL`). If `NULL`, retrieval resolves region internally.
    region = NULL,
    
    #' @field chat_client A chat client (e.g., from `ellmer`) **or** a function `function(prompt) -> character`.
    chat_client = NULL,
    
    #' @field default_number_of_results Default number of KB results to retrieve (integer, default `5`).
    default_number_of_results = 5L,
    
    #' @field default_max_snippets Default number of snippets injected into the LLM prompt (integer, default `5`).
    default_max_snippets = 5L,
    
    #' @field default_snippet_chars Maximum characters per snippet (integer, default `1500`).
    default_snippet_chars = 1500L,
    
    #' @field include_metadata Logical; include basic metadata when building prompts (default `TRUE`).
    include_metadata = TRUE,
    
    #' @field verbose Logical; print progress messages (default `TRUE`).
    verbose = TRUE,
    
    #-----------------------------#
    # Methods                     #
    #-----------------------------#
    
    #' @description
    #' Create a new `KBClient`. Stores defaults for later `$retrieve()` and `$chat()` calls.
    #'
    #' @param kb_id KB ID (character) or `NULL` to use `Sys.getenv("AWS_KB_ID")`.
    #' @param region AWS region (character) or `NULL` to let retrieval resolve it.
    #' @param chat_client Chat client (e.g. `ellmer::chat_aws_bedrock(...)`), or a function `function(prompt) -> character`.
    #' @param default_number_of_results Default KB retrieve size (integer, default `5`).
    #' @param default_max_snippets Default number of snippets to inject into prompt (integer, default `5`).
    #' @param default_snippet_chars Default maximum characters per snippet (integer, default `1500`).
    #' @param include_metadata Logical; include metadata when building prompts (default `TRUE`).
    #' @param verbose Logical; print progress messages (default `TRUE`).
    initialize = function(kb_id = NULL,
                          region = NULL,
                          chat_client = NULL,
                          default_number_of_results = 5L,
                          default_max_snippets = 5L,
                          default_snippet_chars = 1500L,
                          include_metadata = TRUE,
                          verbose = TRUE) {
      self$kb_id <- if (!is.null(kb_id)) kb_id else Sys.getenv("AWS_KB_ID", unset = NA_character_)
      self$region <- region
      self$chat_client <- chat_client %||% getOption("kbretrieveR.chat_client", NULL)
      self$default_number_of_results <- as.integer(default_number_of_results %||% 5L)
      self$default_max_snippets <- as.integer(default_max_snippets %||% 5L)
      self$default_snippet_chars <- as.integer(default_snippet_chars %||% 1500L)
      self$include_metadata <- include_metadata
      self$verbose <- verbose
    },
    
    #' @description
    #' Retrieve KB context (snippets) for a question via Bedrock KB `/retrieve`.
    #'
    #' @param question User question (character, non-empty).
    #' @param number_of_results Integer override for KB retrieval size. If `NULL`, uses client default.
    #' @param return_raw Logical; if `TRUE`, returns a list with `parsed`, `raw_text`, and `raw_parsed`.
    #' @param verbose Logical override; if `NULL`, uses client default.
    #' @return A tibble with columns like `score`, `uri`, `text`, and metadata; or a list when `return_raw = TRUE`.
    retrieve = function(question,
                        number_of_results = NULL,
                        return_raw = FALSE,
                        verbose = NULL) {
      stopifnot(is.character(question), nchar(question) > 0)
      number_of_results <- as.integer(number_of_results %||% self$default_number_of_results)
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
    
    #' @description
    #' Chat using KB context. Retrieves KB context, builds a KB-aware prompt,
    #' and calls the configured `chat_client`.
    #'
    #' When `stream = TRUE`, returns the underlying stream/promise object if the
    #' `chat_client` supports streaming (so UIs like **shinychat** can render
    #' incremental tokens). Otherwise returns a character response.
    #'
    #' @param question User question (character, non-empty).
    #' @param kb_id Optional KB ID override (character). If `NULL`, uses client field or env var.
    #' @param chat_client Optional one-off chat client; if `NULL`, uses `self$chat_client`.
    #' @param number_of_results Optional integer override for KB retrieval size. If `NULL`, uses client default.
    #' @param max_snippets Integer; number of top snippets to inject into the LLM prompt. Defaults to client setting.
    #' @param snippet_chars Integer; maximum characters per snippet. Defaults to client setting.
    #' @param append_sources Logical; when `TRUE`, append a **Sources** section (non-streaming path).
    #' @param return_raw Logical; if `TRUE`, returns a list with `response`, `parsed_kb`, `raw_parsed`, and `prompt`.
    #' @param verbose Logical override; if `NULL`, uses client default.
    #' @param stream Logical; attempt to stream tokens (return a stream/promise) if the chat client supports it.
    #' @param stream_fn Optional function `(prompt, chat_client) -> stream/promise` to override streaming mechanics.
    #' @return Character assistant reply (sync) or a stream/promise object (when `stream = TRUE`).
    chat = function(question,
                    kb_id = NULL,
                    chat_client = NULL,
                    number_of_results = NULL,
                    max_snippets = NULL,
                    snippet_chars = NULL,
                    append_sources = FALSE,
                    return_raw = FALSE,
                    verbose = NULL,
                    stream = FALSE,
                    stream_fn = NULL) {
      stopifnot(is.character(question), nchar(question) > 0)
      
      client_to_use <- chat_client %||% self$chat_client
      if (is.null(client_to_use)) stop("No chat_client available. Use $set_chat_client() or supply chat_client.", call. = FALSE)
      
      verbose <- verbose %||% self$verbose
      max_snippets  <- as.integer(max_snippets %||% self$default_max_snippets)
      snippet_chars <- as.integer(snippet_chars %||% self$default_snippet_chars)
      
      kb_id_resolved <- kb_id %||% (if (!is.null(self$kb_id) && nzchar(self$kb_id)) self$kb_id else Sys.getenv("AWS_KB_ID", unset = ""))
      if (!nzchar(kb_id_resolved)) stop("kb_id not set (argument, client, or AWS_KB_ID env var).", call. = FALSE)
      
      number_of_results_resolved <- as.integer(number_of_results %||% self$default_number_of_results %||% 5L)
      
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
        verbose = verbose,
        stream = stream,
        stream_fn = stream_fn
      )
    },
    
    #' @description
    #' Pretty-print a compact summary of the KB retrieval via `inspect_kb()` and
    #' invisibly return the raw retrieval list (when available).
    #'
    #' @param question User question (character).
    #' @param number_of_results Optional integer override for KB retrieval size.
    #' @param max_chars Integer; maximum characters to show per snippet preview (default `160`).
    #' @param verbose Logical override; if `NULL`, uses client default.
    #' @return Invisibly returns the raw retrieval list (when available).
    inspect = function(question,
                       number_of_results = NULL,
                       max_chars = 160,
                       verbose = NULL) {
      verbose <- verbose %||% self$verbose
      res <- self$retrieve(question = question, number_of_results = number_of_results, return_raw = TRUE, verbose = verbose)
      if (is.list(res) && !is.null(res$raw_parsed)) {
        if (exists("inspect_kb")) {
          inspect_kb(res$raw_parsed, max_chars = max_chars)
        } else {
          message("inspect_kb() not found; returning raw parsed.")
        }
        invisible(res)
      } else {
        message("No raw_parsed available to inspect.")
        invisible(res)
      }
    },
    
    #' @description
    #' Replace the current `chat_client` with `client`.
    #'
    #' @param client A chat client (e.g. from `ellmer`) or a function `function(prompt) -> character`.
    #' @return Invisibly returns `self`.
    set_chat_client = function(client) {
      self$chat_client <- client
      invisible(self)
    },
    
    #' @description
    #' Format a single parsed row into a short citation string, optionally as markdown.
    #'
    #' @param row A 1-row tibble or a named list with fields like `source_uri`, `uri`, and `chunk_id`.
    #' @param markdown Logical; when `TRUE` (default) return a markdown link; otherwise plain text.
    #' @return Character string (citation) or `NA_character_` if insufficient info.
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
    
    #' @description
    #' Return a snapshot of key client internals and defaults (for debugging).
    #'
    #' @return Named list with `kb_id`, `region`, `chat_client`, and a `defaults` sub-list.
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

# ---- helpers (internal) ----------------------------------------------------

#' Null-coalescing helper
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

#' Build a KB-aware prompt from retrieved snippets (internal)
#' @keywords internal
build_kb_prompt <- function(parsed_kb,
                            max_snippets = 5L,
                            snippet_chars = 1500L,
                            include_metadata = TRUE) {
  if (is.null(parsed_kb)) return("")
  if (is.list(parsed_kb) && !is.data.frame(parsed_kb) && !is.null(parsed_kb$parsed)) {
    parsed_kb <- parsed_kb$parsed
  }
  mk <- function(txt) {
    if (nchar(txt) > snippet_chars) paste0(substr(txt, 1, snippet_chars - 3), "...") else txt
  }
  if (is.data.frame(parsed_kb)) {
    rows <- utils::head(parsed_kb, max_snippets)
    snippets <- vapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, , drop = TRUE]
      txt <- as.character(row$text %||% row$snippet %||% "")
      txt <- mk(txt)
      if (isTRUE(include_metadata)) {
        src <- as.character(row$source_uri %||% row$uri %||% row$source %||% "")
        cid <- as.character(row$chunk_id %||% "")
        meta <- paste0(if (nzchar(src)) paste0("Source: ", src) else "", if (nzchar(cid)) paste0(" (chunk: ", cid, ")") else "")
        paste0("----\n", meta, "\n", txt)
      } else paste0("----\n", txt)
    }, character(1))
    paste(snippets, collapse = "\n\n")
  } else if (is.list(parsed_kb)) {
    chunks <- parsed_kb[seq_len(min(length(parsed_kb), max_snippets))]
    snippets <- vapply(chunks, function(ch) {
      txt <- as.character(ch$text %||% ch$snippet %||% "")
      txt <- mk(txt)
      if (isTRUE(include_metadata)) {
        src <- as.character(ch$source_uri %||% ch$uri %||% ch$source %||% "")
        cid <- as.character(ch$chunk_id %||% "")
        meta <- paste0(if (nzchar(src)) paste0("Source: ", src) else "", if (nzchar(cid)) paste0(" (chunk: ", cid, ")") else "")
        paste0("----\n", meta, "\n", txt)
      } else paste0("----\n", txt)
    }, character(1))
    paste(snippets, collapse = "\n\n")
  } else ""
}

#' Chat helper for KB + ellmer client (internal)
#'
#' Orchestrates: retrieve -> build prompt -> call chat client (sync or streaming).
#'
#' @keywords internal
kb_chat_with_ellmer <- function(kb_id,
                                question,
                                chat_client,
                                region = NULL,
                                number_of_results = 5L,
                                max_snippets = 5L,
                                snippet_chars = 1500L,
                                include_metadata = TRUE,
                                append_sources = FALSE,
                                return_raw = FALSE,
                                verbose = TRUE,
                                stream = FALSE,
                                stream_fn = NULL) {
  stopifnot(is.character(question), nchar(question) > 0)
  if (is.null(chat_client)) stop("chat_client is required", call. = FALSE)
  
  retrieved <- kb_retrieve_httr(
    kb_id = kb_id,
    question = question,
    region = region,
    number_of_results = number_of_results,
    verbose = verbose,
    return_raw = TRUE
  )
  
  parsed <- NULL
  raw_parsed <- NULL
  if (is.list(retrieved) && !is.null(retrieved$raw_parsed)) {
    raw_parsed <- retrieved$raw_parsed
    parsed <- retrieved$parsed %||% raw_parsed
  } else if (is.list(retrieved) && !is.null(retrieved$parsed)) {
    parsed <- retrieved$parsed
    raw_parsed <- retrieved$raw_parsed
  } else {
    parsed <- retrieved
  }
  
  kb_block <- build_kb_prompt(parsed, max_snippets = max_snippets, snippet_chars = snippet_chars, include_metadata = include_metadata)
  system_instruction <- "You are an assistant that answers concisely using the provided knowledge base snippets when relevant. If the KB does not contain the answer, say you don't know."
  prompt <- paste(
    system_instruction,
    "\n\n### KB CONTEXT ###\n",
    if (nzchar(kb_block)) kb_block else "(no KB snippets found)",
    "\n\n### USER QUESTION ###\n",
    question,
    sep = "\n"
  )
  
  if (isTRUE(stream)) {
    if (!is.null(stream_fn) && is.function(stream_fn)) {
      return(stream_fn(prompt, chat_client))
    }
    
    # Try common streaming entrypoints
    try_call <- function(fun) {
      if (is.null(fun) || !is.function(fun)) return(NULL)
      tryCatch(fun(prompt), error = function(e) NULL)
    }
    
    # $stream
    s <- tryCatch({ if (is.function(chat_client$stream)) chat_client$stream else NULL }, error = function(e) NULL)
    if (!is.null(s)) {
      res <- try_call(s); if (!is.null(res)) return(res)
    }
    
    # $stream_async
    sa <- tryCatch({ if (is.function(chat_client$stream_async)) chat_client$stream_async else NULL }, error = function(e) NULL)
    if (!is.null(sa)) {
      res <- try_call(sa); if (!is.null(res)) return(res)
    }
    
    # $call(prompt, stream = TRUE) or $call(prompt)
    callf <- tryCatch({ if (is.function(chat_client$call)) chat_client$call else NULL }, error = function(e) NULL)
    if (!is.null(callf)) {
      res <- tryCatch(callf(prompt = prompt, stream = TRUE), error = function(e) {
        tryCatch(callf(prompt), error = function(e2) NULL)
      })
      if (!is.null(res)) return(res)
    }
    
    # function client
    if (is.function(chat_client)) {
      res <- tryCatch(chat_client(prompt, stream = TRUE), error = function(e) NULL)
      if (!is.null(res)) return(res)
      res <- tryCatch(chat_client(prompt), error = function(e) NULL)
      if (!is.null(res)) return(res)
    }
    
    stop("Streaming requested but chat_client has no recognized streaming entrypoint. Supply `stream_fn` or update the client.", call. = FALSE)
  }
  
  # ---- synchronous path ----
  response <- NULL
  if (is.function(chat_client$call)) {
    response <- tryCatch(chat_client$call(prompt = prompt), error = function(e) {
      stop("Error calling chat_client$call(): ", e$message, call. = FALSE)
    })
  } else if (is.function(chat_client)) {
    response <- tryCatch(chat_client(prompt), error = function(e) {
      stop("Error calling chat_client function: ", e$message, call. = FALSE)
    })
  } else if (is.function(chat_client$chat)) {
    response <- tryCatch(chat_client$chat(prompt), error = function(e) {
      stop("Error calling chat_client$chat(): ", e$message, call. = FALSE)
    })
  } else {
    stop("Unable to call chat_client synchronously: no 'call' or function interface detected.", call. = FALSE)
  }
  
  if (isTRUE(append_sources)) {
    sources <- character(0)
    if (!is.null(parsed)) {
      if (is.data.frame(parsed)) {
        # Prefer uri/source_uri columns if present
        col_candidates <- intersect(c("source_uri", "uri", "source"), names(parsed))
        if (length(col_candidates)) {
          sources <- unique(as.character(parsed[[col_candidates[1]]]))
        }
      } else if (is.list(parsed) && length(parsed)) {
        sources <- unique(vapply(parsed, function(x) as.character(x$source_uri %||% x$uri %||% x$source %||% NA_character_), character(1)))
      }
      sources <- sources[!is.na(sources) & nzchar(sources)]
    }
    if (length(sources)) {
      sources_block <- paste0("\n\n---\nSources included from KB:\n", paste0("* ", sources, collapse = "\n"))
      if (is.list(response) && !is.null(response$text)) {
        response$text <- paste0(response$text, sources_block)
      } else if (is.character(response)) {
        response <- paste0(response, sources_block)
      }
    }
  }
  
  if (isTRUE(return_raw)) {
    return(list(response = response, parsed_kb = parsed, raw_parsed = raw_parsed, prompt = prompt))
  }
  response
}
