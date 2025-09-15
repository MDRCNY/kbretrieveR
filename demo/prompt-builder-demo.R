# Load your package code
devtools::load_all()

# A dummy chat client that ignores the prompt (we'll read the prompt from return_raw)
echo_client <- function(prompt) "[dummy LLM response]"

res <- kb_chat_with_ellmer(
  kb_id              = Sys.getenv("AWS_KB_ID"),
  question           = "Tell me about ellmer",
  chat_client        = echo_client,
  region             = Sys.getenv("AWS_REGION"),
  number_of_results  = 2,
  max_snippets       = 2,
  snippet_chars      = 300,
  include_metadata   = TRUE,
  append_sources     = TRUE,   # toggle to FALSE if you don't want sources appended
  return_raw         = TRUE,   # <-- this is the key to get the prompt back
  verbose            = TRUE
)

cat("---- PROMPT ----\n")
cat(res$prompt)
cat("\n----------------\n")

# If you prefer going through the R6 client:
client <- KBClient$new(
  kb_id       = Sys.getenv("AWS_KB_ID"),
  region      = Sys.getenv("AWS_REGION"),
  chat_client = echo_client
)

out <- client$chat(
  "Tell me about ellmer",
  number_of_results = 2,
  max_snippets      = 2,
  snippet_chars     = 300,
  append_sources    = TRUE,
  return_raw        = TRUE     # <-- again, get the prompt back
)

cat("---- PROMPT (via KBClient) ----\n")
cat(out$prompt)
cat("\n--------------------------------\n")
