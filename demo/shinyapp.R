# app.R
# Run: shiny::runApp("path/to/app.R")
library(shiny)
library(shinychat)   # CRAN package
#library(kbretrieveR) # your KB client package
library(glue)

#devtools::document()
#devtools::load_all()

# ---------------------------
# Build KB client once (global)
# ---------------------------
chat_client_opt <- getOption("kbretrieveR.chat_client", NULL)
if (is.null(chat_client_opt)) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Please install 'ellmer' or set options(kbretrieveR.chat_client = <client>) before launching.")
  }
  chat_client_opt <- ellmer::chat_aws_bedrock(
    model = Sys.getenv("KB_BEDROCK_MODEL", "anthropic.claude-3-5-sonnet-20240620-v1:0")
  )
  options(kbretrieveR.chat_client = chat_client_opt)
}

kb_id <- Sys.getenv("AWS_KB_ID", "")
region <- Sys.getenv("AWS_REGION", "us-east-1")
if (!nzchar(kb_id)) stop("Set AWS_KB_ID in your environment before running the app.")

KB <- tryCatch(
  KBClient$new(kb_id = kb_id, region = region, chat_client = chat_client_opt),
  error = function(e) stop("Failed to construct KBClient: ", e$message)
)

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  titlePanel("kbretrieveR + shinychat — KB Chat"),
  fluidRow(
    column(3,
           wellPanel(
             h4("KB / Model Info"),
             p(strong("Model:")),
             verbatimTextOutput("model_info"),
             tags$hr(),
             p(strong("KB ID:")), verbatimTextOutput("kb_id"),
             p(strong("Region:")), verbatimTextOutput("region"),
             tags$hr(),
             p(em("Tip: type in the chat box and press Enter"))
           )
    ),
    column(9,
           chat_ui("kbchat", placeholder = "Ask the knowledge base... (press Enter)")
    )
  )
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  output$model_info <- renderText({
    tryCatch({
      paste(KB$chat_client$get_model(), collapse = ", ")
    }, error = function(e) paste0("Error: ", e$message))
  })
  
  output$kb_id <- renderText({ kb_id })
  output$region <- renderText({ region })
  
  observeEvent(input$kbchat_user_input, {
    query <- input$kbchat_user_input
    if (!nzchar(trimws(query))) return()
    
    withProgress(message = "Querying KB & model...", value = 0, {
      incProgress(0.3)
      
      # Try streaming; if the client supports it, KB$chat returns a stream/promise.
      resp <- tryCatch({
        KB$chat(
          question = query,
          number_of_results = 5,
          append_sources = TRUE,
          stream = TRUE
        )
      }, error = function(e) {
        # Fallback: non-streaming single string
        paste0("Assistant error: ", e$message)
      })
      
      incProgress(0.8)
      
      # chat_append accepts plain strings OR a streaming/promise object
      shinychat::chat_append("kbchat", resp, role = "assistant", session = session)
      
      incProgress(1)
    })
  })
}

shinyApp(ui, server)
