
# load in packages
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(shiny)
library(rollama)
library(stringr)
library(RSQLite)
library(DBI)
library(shinyjs)
model_params <- list()

# connect to a sql database to store our chat history. If it doesn't exist, it will be created
con <- dbConnect(RSQLite::SQLite(), "YOUR/FILE/PATH/chatbot.db")

# we call in new_chat() to erase the existing history (if we already were working on it)
new_chat()

# An error catching function to create a new table for our chat history if it does not yet exist
tryCatch({
  dbExecute(con, "
  CREATE TABLE chat_history (
    role TEXT,
    content TEXT,
    time TEXT,
    id INTEGER
  )
")
}, error = function(e) {
  return(NULL)
})

# We query our database to get the chat history we have stored. This can be updated later in the server
initial_history_db <- dbGetQuery(con, "SELECT * FROM chat_history")
row.names(initial_history_db) <- paste0(initial_history_db$time,initial_history_db$id)

# The questions and answers are stored line by line, we group them and paste them with some html tags to give them proper visual display in our shiny app 
# We also want the first message to show as an identifier for your chat
initial_history_db_grouped <- initial_history_db %>% 
  group_by(id) %>% 
  reframe(text = paste0("<b>", role, ":</b>", "<br> <pre>", content, " </pre> <br>"),first_message = first(substr(content,1,20))) %>% 
  group_by(id) %>% 
  reframe(text = paste(text, collapse = ""),first_message = first(first_message))

# vectors for our selectizers

# vector to filter our chats by their ID
id_vect <- c(unique(initial_history_db_grouped$id))

# a vector to list the current models downloaded in ollama
suppressWarnings({
  if (length(list_models()) > 0) {
    models <- list_models() %>% select(1) %>% distinct_all()
    models_vector <- models$name
  } else {
    models_vector <- "NO MODELS FOUND"
  }
})

# functions including the stylesheets for codeblocks. Shoutout to stack overflow
prismCodeBlock <- function(code) {
  tagList(
    HTML(code),
    tags$script("Prism.highlightAll()")
  )
}

prismDependencies <- tags$head(
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js"),
  tags$link(rel = "stylesheet", type = "text/css",
            href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism.min.css")
)

# We use only R as a general color scheme at the moment
prismRDependency <- tags$head(
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-r.min.js")
)


ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "RShiny LLM App"),

  controlbar = dashboardControlbar(
    id = "controlbar",
    width = 300,
    overlay = FALSE,
      controlbarItem(
        "Model Config",
        tags$style(HTML("#controlbar input[type='number'] { height: 15px;font-size:10px;}")),
        tags$style(HTML(".control-sidebar-dark{padding-left: 5px;padding-right: 5px; }")),
        tags$style(HTML(".form-group.shiny-input-container{margin-bottom: 0px;}")),
        tags$style(HTML("#controlbar .control-label{margin-bottom: 0px; font-size:11px; color:black}")),
        tags$style(HTML(".control-sidebar-dark, .control-sidebar-dark+.control-sidebar-bg {background:#ccc;}")),
        tags$style(HTML(".checkbox label, .radio label {font-weight: 700;cursor: pointer; color: black; font-size:13px}")),
        tags$style(HTML(".checkbox, .radio {margin-top: 5px; margin-bottom: 5px;}")),
        tags$style(HTML("#controlbar .row{padding-left: 5px; padding-right: 5px; margin-left: -5px;}")),
        fluidRow(
          actionButton("useParams", "Use Parameters", style = "margin-top: 10px; margin-bottom:10px;margin-left: 0px;"),
          actionButton("resetParams", "Reset to model base", style = "margin-top: 10px;margin-bottom:10px;margin-left: 0px;")
        ),
        numericInput("num_keep", "Number to Keep", value = 0, min = 0),
        numericInput("seed", "Seed", value = 42),
        numericInput("num_predict", "Number to Predict", value = 100, min = 1),
        numericInput("top_k", "Top K", value = 20, min = 1),
        numericInput("top_p", "Top P", value = 0.9, min = 0, max = 1, step = 0.01),
        numericInput("tfs_z", "TFS Z", value = 0.5, min = 0, max = 10, step = 0.01),
        numericInput("typical_p", "Typical P", value = 0.7, min = 0, max = 1, step = 0.01),
        numericInput("repeat_last_n", "Repeat Last N", value = 64, min = -1),
        numericInput("temperature", "Temperature", value = 0.8, min = 0, max = 1, step = 0.01),
        numericInput("repeat_penalty", "Repeat Penalty", value = 1.2, min = 0, step = 0.1),
        numericInput("presence_penalty", "Presence Penalty", value = 1.5, min = 0, step = 0.1),
        numericInput("frequency_penalty", "Frequency Penalty", value = 1.0, min = 0, step = 0.1),
        numericInput("mirostat", "Mirostat", value = 0, min = 0, max = 2, step = 1),
        numericInput("mirostat_tau", "Mirostat Tau", value = 0.8, min = 0, max = 10, step = 0.01),
        numericInput("mirostat_eta", "Mirostat Eta", value = 0.6, min = 0, max = 1, step = 0.01),
        numericInput("num_ctx", "Num Context", value = 4096, min = 1024),
        numericInput("num_batch", "Num Batch", value = 2, min = 1),
        numericInput("num_gqa", "Num GQA", value = 1, min = 0),
        numericInput("num_gpu", "Num GPU", value = 1, min = 0),
        numericInput("main_gpu", "Main GPU", value = 0, min = 0),
        numericInput("rope_frequency_base", "ROPE Frequency Base", value = 1.1, min = 0, step = 0.1),
        numericInput("rope_frequency_scale", "ROPE Frequency Scale", value = 0.8, min = 0, max = 1, step = 0.01),
        numericInput("num_thread", "Number of Threads", value = 8, min = 1),
        checkboxInput("penalize_newline", "Penalize Newline", value = TRUE),
        checkboxInput("numa", "NUMA", value = FALSE),
        checkboxInput("low_vram", "Low VRAM", value = FALSE),
        checkboxInput("f16_kv", "F16 KV", value = TRUE),
        checkboxInput("vocab_only", "Vocab Only", value = FALSE),
        checkboxInput("use_mmap", "Use MMAP", value = TRUE),
        checkboxInput("use_mlock", "Use MLOCK", value = FALSE),
        checkboxInput("embedding_only", "Embedding Only", value = FALSE)
        
        
       
    )
  ),
  
  # side bar takes in general inputs, choosing a model, downloading new models and setting config message. config message not working a.t.m.
  dashboardSidebar(minified=FALSE,
    tags$head(
      tags$style(HTML(" .box_config { width: 300px; margin: 10px auto; margin-left: 15px;  text-align: left; }")),
      tags$style(HTML(" label { color:black;}")),
      tags$style(HTML(" strong { color:black;}")),
      tags$style(HTML(" #config_message { color:black;}")),
      tags$style(HTML(" p { color:black; font-weight:400}")),
      tags$style(  HTML(".shiny-notification {  position:fixed; top: calc(45%);    left: calc(30%);  ;width:400px; height:200px;font-size:20px;font-weight:700} "  )    ),
      tags$style(HTML(" .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {  background-color: #ccc;  } }"))),
    tags$span(class="control-label",
              tags$style(HTML(".row.sidebar{margin-right:0px;padding-right:0px}")),
              tags$style(HTML(".col-sm-9.sidebar{margin-right:0px;padding-right:0px}")),
              tags$style(HTML(".col-sm-3.sidebar{margin-right:0px;padding-right:0px;padding-left:10px}"))),
    width = "350px",
    column(12,
      fluidRow(selectizeInput( "modelSelect",  "Select a Model:",  choices = models_vector,  multiple = FALSE,width ="100%")) # we use our models vector as an input
    ),
    fluidRow(class="sidebar",
      column(width=9,textInput("configInput", "Set System message:"),class= "sidebar"),
      column(width=3,actionButton("configSet", "Set", style = "margin-top: 36px;", width="50px"),class= "sidebar")
      ),
      fluidRow(column(12,tags$div(class = "box_config",
                                  h5(strong("Current System Message")),
                                  textOutput("config_message")
      ))
      ),
    column(12, h4(strong("How to find new models?")),       
               p("Visit ollama.com/library to see which models you can pull. Fill them in below and press Pull to download them.")
      ),
    fluidRow(class="sidebar",
      column(9, textInput("pullInput", "Download a new Model:"),class= "sidebar"), 
      column(3,actionButton("pullButton", "Pull", style = "margin-top: 36px;", width="50px"),class= "sidebar")
      ),
    fluidRow(
    column(12,fileInput("imageInput", "Choose an Image File",
                        accept = c(
                          "image/png",
                          "image/jpeg",
                          "image/jpg"
                        ),width="100%")))
  ),
  
  dashboardBody(
    # load in the needed dependencies in the dashboard body, as the code blocks will be here
    useShinyjs(),  
    prismDependencies,
    prismRDependency,
    
    # generic styling options
    tags$head(
      tags$style(HTML("
        pre { 
        white-space: pre-wrap;
        }
      .collapsible {
        cursor: pointer;
        padding: 10px;
        width: 100%;
        border: 1px solid ;
        text-align: left;
        outline: none;
        font-size: 16px;
        margin-top:5px;
        font-weight: bold;
      }
      .active, .collapsible:hover {
        background-color: #ccc;
      }
      .my-content {
        padding: 0 18px;
        display: none;
        overflow: hidden;
        border: 1px solid ;
      }
    "
    ))),
    
    # Now that the generic styling is done we will build our output boxes.
    fluidRow( # within this row  there will be 2 boxes, 1 for the output and 1 for the chat history
     
       box( # this box is your chat window
        status = "primary",
        solidHeader = TRUE,
        title = "Your Chat",
        width = 7,
        height = "calc(100vh - 150px)",
        
        column(width = 12, 
               # We want our text output to match the width of the box, so we set it to 100% and it will thus scale with the width scaling of the box. 
               # The height doesn't scale automatically, so we set this by taking the view height (vw) and subtracting some pixels for headers and buttons. So it will scale with any screen height
               fluidRow(tags$div(id = "text_output", style = "width: 100%; height: calc(100vh - 350px); overflow: auto; border: 1px solid #ccc;margin-top: 20px; padding: 10px;", uiOutput("formatted_text"))
               ),
               # the input field below will take in your questions to the ai model
               fluidRow( textInput("questionInput", "Enter your question:")
               ),
               # we have 3 buttons below, one to send your message, one to reset the history and one to save your chat
               fluidRow(actionButton("sendButton", "Chat", style = "margin-top: 5px;margin-left: 10px;"),
                        actionButton("newButton", "Reset History", style = "margin-top: 5px;margin-left: 10px;"),
                        actionButton("saveButton", "Save Chat", style = "margin-top: 5px;margin-left: 10px;")
               )
        )
      ),
      
      box( # this box is your chat history
          status = "primary",
          solidHeader = TRUE,
          title = "Chat History",
          width = 5,
          height = "calc(100vh - 150px)",
        
          column(width = 12,
                 fluidRow( tags$div(id = "collapsible", style = "width: 100%; height: calc(100vh - 350px); overflow: auto; border: 1px solid #ccc;margin-top: 20px; padding: 10px;", uiOutput("collapsibleUI")))
          ),
          column(width =12,
          fluidRow(selectizeInput("select_chat", "Select a chat from history:", choices = id_vect, multiple = FALSE, width= "200px"),
                   actionButton("Remove", "Remove", style = "margin-top: 5px;margin-left: 10px;"),
                   actionButton("Use", "use", style = "margin-top: 5px;margin-left: 10px;")
          )
        ))
    ) 
  )
)




server <- function(input, output, session) {
  
  
  
  
# reactive chat history data
  
  # we transform the chat history dataframes we pulled when starting the app into reactive format. So we can update them and the ui easily when adding or removing chats during our session
  history_db <- reactiveVal(initial_history_db)
  history_db_grouped <- reactiveVal(initial_history_db_grouped %>% arrange(desc(id)))
  
  
# Sidebar server components
  
  observeEvent(input$pullButton, {
    # the model will be pulled and the vector for current models will be updated along with the selectizeinput
    pull_model(model = input$pullInput)
    models <- list_models() %>% select(1)
    models_vector <- models$name
    
    updateSelectizeInput(session, "modelSelect", choices = models_vector)
  })
  
  observeEvent(input$configSet, {
    options(rollama_config=input$configInput)
    output$config_message <- renderText({if (is.null(getOption("rollama_config"))){"no message set"} else{getOption("rollama_config")}})
  })
  
  output$config_message <- renderText({if (is.null(getOption("rollama_config"))){"no message set"} else{getOption("rollama_config")}})
  

  
  observeEvent(input$useParams, {
    model_params <<- list(
      num_keep = input$num_keep,
      seed = input$seed,
      num_predict = input$num_predict,
      top_k = input$top_k,
      top_p = input$top_p,
      tfs_z = input$tfs_z,
      typical_p = input$typical_p,
      repeat_last_n = input$repeat_last_n,
      temperature = input$temperature,
      repeat_penalty = input$repeat_penalty,
      presence_penalty = input$presence_penalty,
      frequency_penalty = input$frequency_penalty,
      mirostat = input$mirostat,
      mirostat_tau = input$mirostat_tau,
      mirostat_eta = input$mirostat_eta,
      penalize_newline = input$penalize_newline,
      numa = input$numa,
      num_ctx = input$num_ctx,
      num_batch = input$num_batch,
      num_gqa = input$num_gqa,
      num_gpu = input$num_gpu,
      main_gpu = input$main_gpu,
      low_vram = input$low_vram,
      f16_kv = input$f16_kv,
      vocab_only = input$vocab_only,
      use_mmap = input$use_mmap,
      use_mlock = input$use_mlock,
      embedding_only = input$embedding_only,
      rope_frequency_base = input$rope_frequency_base,
      rope_frequency_scale = input$rope_frequency_scale,
      num_thread = input$num_thread
    )
    
  })
  
  observeEvent(input$resetParams,{
    model_params <- list()
  })
  
  
# chat window server components

  observeEvent(input$sendButton, {
    print(model_params)
    # To start a conversation we call the chat() function. The function checks if history is available and uses it.
    data <- chat(model = input$modelSelect, q = input$questionInput, images =   input$imageInput$datapath,screen = FALSE, model_params=model_params)
    # we then call history as we want to display the full message on screen
    history <- chat_history()
    # we check for the number of codeblocks displayed
    count_codeblocks <- max(str_count(history$content, "```"))
    
    # we replace codeblocks with <pre> and <code> tags to have a nicer display output than just plain text
    for (i in 1:(count_codeblocks/2)){  
      history$content <- str_replace(history$content, "```", "<pre> <code class='language-r'>")
      history$content <- str_replace(history$content, "```", "</code> </pre>")}
    
    # we empty the text input to make it faster to type a new question
    updateTextInput(session, "questionInput", value = "")
    
    # we render the html output to be displayed, it takes the role, the content and then encodes the html.
    output$formatted_text <- renderUI({
      formatted_text <- lapply(1:nrow(history), function(i) {
        role <- history$role[i]
        content <- history$content[i]
        paste0("<b>", role, ":</b>", "<br> <pre>", content, " </pre> <br>")
      })
      prismCodeBlock(HTML(paste(formatted_text, collapse = "")))
    })
  })
  
  
  observeEvent(input$newButton, {
    # we remove the history
    new_chat()
    # we render the text output to blank
    output$formatted_text <- renderUI({""})
  })
  
  
  observeEvent(input$saveButton, {
    # first we check whether there is any history and return error if not
    if  (length(chat_history()$role)==0){
      output$formatted_text <- renderUI({ "No history available, start chatting first!" })
    } else{
    # we call in history and give it an ID that is one bigger than the max currently in our database
    history <- chat_history()
    history$id <- ifelse((nrow(history_db()) == 0), 0, max(history_db()$id) + 1)
    history$time <- as.character(history$time)
    
    # we write it to our database and update our UI
    dbWriteTable(con, name = 'chat_history', value = history, append = TRUE, row.names = FALSE)
    output$formatted_text <- renderUI({"Conversation Saved"})
    
    # than update our html output by sending a query to our db and adding tags for html
    updated_history_db <- dbGetQuery(con, "SELECT * FROM chat_history")
    updated_history_db_grouped <- updated_history_db %>% 
      group_by(id) %>% 
      reframe(text = paste0("<b>", role, ":</b>", "<br> <pre>", content, " </pre> <br>"),first_message = first(substr(content,1,20))) %>% 
      group_by(id) %>% 
      reframe(text = paste(text, collapse = ""),first_message = first(first_message))%>%
      arrange(desc(id))
    
    # we then update our reactive values so that our chat history can be updated in the later parts of the server
    history_db(updated_history_db)
    history_db_grouped(updated_history_db_grouped)
    
    # we also update our vectors for choosing a chat in the later parts of our server
    id_vect <- c(unique(updated_history_db_grouped$id))
    updateSelectizeInput(session, "select_chat", choices = id_vect)
    }
  })
  
  
# chat history server components
  
observeEvent(input$Remove, {
    
    # we delete the chat we selected from our reactive values
    history_db_grouped(
      history_db_grouped() %>% filter(id != input$select_chat)
     )
    
    # we delete the chat from our database
    dbExecute(con, sprintf("DELETE FROM chat_history WHERE id = %d", input$Remove))
    
    # we update the vector with IDs
    id_vect <- c(unique(history_db_grouped()$id))
    updateSelectizeInput(session, "select_chat", choices = id_vect)
    
  })
  
  
observeEvent(input$Use, {
    
    # as there is no options within the package to use other text as a history within chat(), we will add our old chats to the package environment so that they will be seen as current history
  
    #we call a new chat to clear the history
    new_chat()
  
    # we call the package environment where prompts and responses are stored 
    the_env <- rollama:::the
    
    # we call our reactie values of the current chats, set the rownames like in the package and select the selected chat
    use <- history_db()
    row.names(use) <- use$time
    use <- use %>% filter(id == input$select_chat)
    
    # we split the df to get the prompts and responses separately and push them to the environment
    prompts_df <- use %>% filter(role=="user") %>% select(content)
    prompts <- as.vector(prompts_df$content)
    names(prompts) <- row.names(prompts_df)
    the_env$prompts <- prompts
    
    responses_df <- use %>% filter(role=="assistant") %>% select(content)
    responses <- as.vector(responses_df$content)
    names(responses) <- row.names(responses_df)
    the_env$responses <- responses
    
    # now that the prompts and responses of the selected chat are pushed to the environment a simply chat_history() call will show them as history and make it able to push them to the current chat output
    history <- chat_history()
    history$content <- str_replace(history$content, "```", "<pre> <code class='language-r'>")
    history$content <- str_replace(history$content, "```", "</code> </pre>")
    updateTextInput(session, "textInputModel", value = "")
    output$formatted_text <- renderUI({
      formatted_text <- lapply(1:nrow(history), function(i) {
        role <- history$role[i]
        content <- history$content[i]
        paste0("<b>", role, ":</b>", "<br> <pre>", content, " </pre> <br>")
      })
      prismCodeBlock(HTML(paste(formatted_text, collapse = "")))
    })
    
    # the chat from the history will now display in the chat window and when chatting will take this chat into consideration until it is saved or new_chat() is called
    
  })
  
  
  
  
# Here we render the output for the chat history, we render buttons to expand and use the text behind it.

  output$collapsibleUI <- renderUI({
    lapply(1:nrow(history_db_grouped()), function(i) {
      id <- history_db_grouped()$id[i]
      text <- history_db_grouped()$text[i]
      first_message <- history_db_grouped()$first_message[i]
      div(
        actionButton(paste0("btn_", id), paste0("Chat ", id,": ", first_message), class = "collapsible"),
        div(id = paste0("content_", id), class = "my-content", prismCodeBlock(HTML(text)))
      )
    })
  })
  
  observe({
    observeEvent(history_db_grouped(), {
      lapply(1:nrow(history_db_grouped()), function(i) {
        id <- history_db_grouped()$id[i]
        shinyjs::onclick(paste0("btn_", id), {
          shinyjs::toggle(id = paste0("content_", id))
        })
      })
    }, ignoreNULL = FALSE)
  })
}

shinyApp(ui = ui, server = server)