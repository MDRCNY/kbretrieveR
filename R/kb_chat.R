#' Ask ellmer with KB context injected
#'
#' Retrieve context from a Bedrock KB and inject it into a prompt for an ellmer chat client.
#'
#' The function will:
#' 1) call `kb_retrieve_httr(..., return_raw = TRUE)` to get snippets,
#' 2) build a prompt of the form:
#'    [User Provided This Prompt]
#'
#'    <user question>
#'
#'    [KB retrieval - use the following context to answer the question if available]
#'    SOURCE: <source_uri> (chunk: <chunk_id>)
#'    <snippet>
#'    ---
#' 3) send that prompt to the provided `chat_client`.
#'
#' Optionally, if `append_sources = TRUE`, append a "Sources" section listing the
#' URIs (and chunk ids) of the snippets included in the prompt.
#'
#' @param kb_id Knowledge Base ID (character).
#' @param question Character: user prompt / question.
#' @param chat_client Either:
#'   - an ellmer client object with `$chat()` method, or
#'   - a function that takes a single string prompt and returns a response.
#'   If `NULL`, the function will try `getOption("kbretrieveR.chat_client")`.
#' @param region AWS region for KB retrieval (optional).
#' @param number_of_results Number of KB results to request (default 5).
#' @param max_snippets How many top KB snippets to include (default 5).
#' @param snippet_chars Truncate each snippet to this many characters (default 1500).
#' @param include_metadata Logical: include `source_uri` + `chunk_id` headings above each snippet.
#' @param append_sources Logical: when TRUE, append a "Sources" section to the reply (default FALSE).
#' @param return_raw Logical: if TRUE return a list with response, parsed_kb, raw_parsed, prompt, sources.
#' @param verbose Logical; print some info.
#' @return By default the chat response (character). If `return_raw=TRUE` returns a list with
#'   `response`, `parsed_kb`, `raw_parsed`, `prompt`, and `sources` (or NULL when not appended).
#' @export
kb_chat_with_ellmer <- function(kb_id,
                                question,
                                chat_client = NULL,
                                region = NULL,
                                number_of_results = 5,
                                max_snippets = 5,
                                snippet_chars = 1500,
                                include_metadata = TRUE,
                                append_sources = FALSE,
                                return_raw = FALSE,
                                verbose = TRUE) {
  stopifnot(is.character(kb_id), nchar(kb_id) > 0)
  stopifnot(is.character(question), nchar(question) > 0)
  
  # find chat client
  if (is.null(chat_client)) {
    chat_client <- getOption("kbretrieveR.chat_client", NULL)
    if (is.null(chat_client)) {
      stop("No chat_client supplied. Set options(kbretrieveR.chat_client = <ellmer client>) or pass chat_client argument.", call. = FALSE)
    }
  }
  
  # 1) Retrieve KB context (raw + parsed)
  kb_out <- kb_retrieve_httr(
    kb_id = kb_id,
    question = question,
    region = region,
    number_of_results = number_of_results,
    verbose = verbose,
    return_raw = TRUE
  )
  
  parsed_kb  <- kb_out$parsed
  raw_parsed <- kb_out$raw_parsed
  
  # Build prompt
  if (nrow(parsed_kb) == 0) {
    context_block <- "[KB retrieval - no context available]"
  } else {
    use_rows <- seq_len(min(nrow(parsed_kb), max_snippets))
    block_parts <- vapply(use_rows, FUN.VALUE = character(1), function(i) {
      row <- parsed_kb[i, , drop = TRUE]
      snippet_text <- substr(as.character(row$text %||% ""), 1, snippet_chars)
      if (isTRUE(include_metadata)) {
        source_line <- paste0(
          "SOURCE: ",
          row$source_uri %||% row$uri %||% "<unknown>",
          if (!is.na(row$chunk_id) && nzchar(row$chunk_id)) paste0(" (chunk: ", row$chunk_id, ")") else ""
        )
        paste0(source_line, "\n", snippet_text, "\n---")
      } else {
        paste0(snippet_text, "\n---")
      }
    }, USE.NAMES = FALSE)
    context_block <- paste(block_parts, collapse = "\n")
  }
  
  prompt <- paste0(
    "[User Provided This Prompt]\n\n",
    question,
    "\n\n[KB retrieval - use the following context to answer the question if available]\n\n",
    context_block
  )
  
  if (isTRUE(verbose)) {
    message("Sending prompt to chat client (prompt length: ", nchar(prompt), " chars).")
  }
  
  # 2) Call the chat client
  resp_text <- NULL
  if (is.function(chat_client)) {
    resp_text <- chat_client(prompt)
  } else if (is.environment(chat_client) || (is.list(chat_client) && !is.null(chat_client$chat))) {
    if (!is.null(chat_client$chat) && is.function(chat_client$chat)) {
      resp_text <- chat_client$chat(prompt)
    } else if (!is.null(chat_client$chat) && is.character(chat_client$chat)) {
      fn <- get(chat_client$chat)
      resp_text <- fn(prompt)
    } else {
      stop("chat_client provided but has no callable $chat method.", call. = FALSE)
    }
  } else {
    res_try <- tryCatch(chat_client$chat(prompt), error = function(e) NULL)
    if (!is.null(res_try)) resp_text <- res_try
  }
  
  # normalize output occasionally returned as list
  if (is.list(resp_text) && !is.null(resp_text$text)) {
    resp_text <- resp_text$text
  } else if (is.list(resp_text) && length(resp_text) == 1 && is.character(resp_text[[1]])) {
    resp_text <- resp_text[[1]]
  }
  
  if (is.null(resp_text)) {
    stop("Chat client returned NULL or an unexpected object. Inspect chat_client behavior.", call. = FALSE)
  }
  
  # Optional "Sources" block (URIs for snippets we injected)
  sources_block <- NULL
  if (isTRUE(append_sources)) {
    if (nrow(parsed_kb) > 0) {
      use_rows <- seq_len(min(nrow(parsed_kb), max_snippets))
      src_lines <- vapply(use_rows, FUN.VALUE = character(1), function(i) {
        row <- parsed_kb[i, , drop = TRUE]
        uri <- row$source_uri %||% row$uri %||% NA_character_
        if (is.na(uri) || !nzchar(uri)) return("")
        line <- paste0("- [", basename(uri), "](", uri, ")")
        if (!is.na(row$chunk_id) && nzchar(row$chunk_id)) {
          line <- paste0(line, " (chunk: ", row$chunk_id, ")")
        }
        line
      }, USE.NAMES = FALSE)
      src_lines <- src_lines[nzchar(src_lines)]
      sources_block <- if (length(src_lines)) paste(c("### Sources", src_lines), collapse = "\n") else "### Sources\nNo KB sources returned."
    } else {
      sources_block <- "### Sources\nNo KB sources returned."
    }
  }
  
  final <- if (isTRUE(append_sources) && !is.null(sources_block)) {
    paste0(resp_text, "\n\n", sources_block)
  } else {
    resp_text
  }
  
  if (isTRUE(return_raw)) {
    return(list(
      response   = final,
      parsed_kb  = parsed_kb,
      raw_parsed = raw_parsed,
      prompt     = prompt,
      sources    = sources_block
    ))
  }
  
  final
}
