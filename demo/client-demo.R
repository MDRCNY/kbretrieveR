# restart R (to clear stale definitions)
devtools::document()
devtools::load_all()

chat_client = getOption("kbretrieveR.chat_client")


client <- KBClient$new(
  kb_id = Sys.getenv("AWS_KB_ID"),
  region = Sys.getenv("AWS_REGION"),
  chat_client = ellmer::chat_aws_bedrock("anthropic.claude-3-5-sonnet-20240620-v1:0")
)
# Defualt number of results = 5
client$chat("Tell me about ellmer")

# Limit to 1 Results
client$retrieve("Tell me about ellmer in 1 sentence", number_of_results = 1)
client$chat("Tell me about ellmer in 1 sentence", number_of_results=1)

# Limit to 3 Results 
client$retrieve("Tell me about ellmer in 1 sentence", number_of_results = 3)
client$chat("Tell me about ellmer in 1 sentence", number_of_results=3)
client$chat("Tell me about ellmer in 1 sentence", number_of_results=3, append_sources = TRUE)
# default: no sources appended
cat(client$chat("Tell me about ellmer in 1 sentence"))

# opt-in: append sources at the end
cat(client$chat("Tell me about ellmer in 1 sentence", number_of_results=1, append_sources = TRUE))
