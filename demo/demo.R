devtools::load_all()    # load the updated package into your session
res <- kb_retrieve_httr(kb_id = Sys.getenv("AWS_KB_ID"),
                        question = "Tell me about mdrcDataViz?",
                        region = Sys.getenv("AWS_REGION"),
                        number_of_results = 5,
                        verbose = TRUE)
print(res)
print(res$chunk_id)


#  Using an ellmer client object with $chat():


# assume you set this somewhere globally or created earlier:
options(kbretrieveR.chat_client = ellmer::chat_aws_bedrock("anthropic.claude-3-5-sonnet-20240620-v1:0"))

# call with the option-based client
ans <- kb_chat_with_ellmer(kb_id = Sys.getenv("AWS_KB_ID"),
                           question = "Tell me about mdrcDataViz",
                           number_of_results = 2)
cat(ans)
