library(shiny)
library(tidyverse)
library(shinyjs)
library(bslib)
library(lubridate)

# rsconnect::deployApp(appDir="dadbot3000", appName="dadBot3000")
# to deploy to https://emmacooperpeterson.shinyapps.io/dadBot3000


# ui ----------------------------------------------------------------------
ui <- fluidPage(

  theme = bslib::bs_theme(bootswatch = "lux"),

  useShinyjs(),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "dadbot.css")
  ),

  verticalLayout(

    titlePanel(
      title="Welcome to dadBot3000™",
      windowTitle="dadBot3000™"
    ),

    wellPanel(

      radioButtons(
        "markov_state_size",
        label="Markov State Size (probabilities can be based on the previous
               one or two words)",
        choices=c("1", "2"),
        selected="2",
        inline=TRUE
      ),

      radioButtons(
        "max_overlap_ratio",
        label="Maximum Overlap Ratio (exclude generated texts that overlap an
               original text by more than this percentage)",
        choices=c("20%" = 0.2, "40%" = 0.4, "60%" = 0.6, "80%" = 0.8),
        selected=0.6,
        inline=TRUE
      ),

      radioButtons(
        "maximum_sentence_length",
        label="Maximum Generrated Text Length",
        choices=c(20, 40, 60, 80, 100),
        selected=40,
        inline=TRUE
      ),

      actionButton("build_model", "Build DadBot3000™", class="button"),
      hidden(actionButton("generate", "Generate DadText™", class="button"))
    ),

    br(),

    fluidRow(
      column(3, imageOutput("image")),
      column(9, align="left", textOutput("dad_text"))
    )
  )
)


# server ------------------------------------------------------------------
server <- function(input, output, session) {

  is_dads_birthday <- FALSE
  now <- today(tzone="EST")
  if (month(now) == 9 & day(now) == 3) is_dads_birthday <- TRUE

  welcome_message <- if_else(
    is_dads_birthday,
    "HAPPY BIRTHDAY!!!",
    "Welcome to dadBot3000™"
  )

  description <- "dadBot3000™ is a Markov chain text generator. He was trained
  on a year of texts sent to Emma by her dad. Once you've built the bot to your
  specifications, he will deliver newly generated texts where each word is based
  on the probability of that word appearing after the previous word(s) in the
  training texts."

  showModal(modalDialog(
    title = welcome_message,
    description,
    footer = modalButton("cool beans !", icon=icon("check"))
  ))

  inputs <- readRDS("data/inputs.RDS")
  generated_texts <- readRDS("data/generated_texts.RDS")

  # object to store reactive values <-  referenced later via `values$...`
  values <- reactiveValues()

  # when any of the inputs are adjusted, hide the current display
  observeEvent(as.integer(input$markov_state_size)|as.numeric(input$max_overlap_ratio)|as.numeric(input$maximum_sentence_length), {
    shinyjs::hide("image")
    shinyjs::hide("generate")
    shinyjs::hide("dad_text")
  })


  dadBot <- reactive({

    index <-
      inputs %>%
      filter(markov_state_size == as.integer(input$markov_state_size),
             max_overlap_ratio == input$max_overlap_ratio,
             maximum_sentence_length == input$maximum_sentence_length) %>%
      pull(index)

    dad_bot <- generated_texts[[index]]
  })



  observeEvent(input$build_model, {

    # if anything has already been rendered, hide it while we re-build the model
    shinyjs::hide("dad_text")
    shinyjs::hide("image")
    shinyjs::hide("generate")

    # simulate a progress bar
    # the model actually builds very quickly
    # which can make it look like the app isn't doing anything
    progress <- Progress$new()
    on.exit(progress$close()) # make sure it closes when we exit this reactive
    progress$set(message="building dadBot3000™", value=0)
    progress$inc(1/3) # begin the progress bar before building model

    values$generated_texts <- sample(dadBot())  # sample randomizes the order
    values$counter <- 1

    # simulate the completion of the progress bar once the model is built
    Sys.sleep(0.5)
    progress$inc(2/3)
    Sys.sleep(1)
    progress$inc(3/3)

    # render the image
    output$image <- renderImage({
      filename <- normalizePath(file.path('./images',"dadbot.png"))

      # Return a list containing the filename
      list(src = filename, width = "300", height = "300")
    }, deleteFile = FALSE)

    shinyjs::show("image")
    shinyjs::show("generate")
  })



  observeEvent(input$generate, {

    # case where model produced nothing
    if (all(is.na(values$generated_texts))) {
      msg <- "dadBot3000™ does not like this combination of inputs :( pls rebuild"
      output$dad_text <- renderText(msg)

    } else {
      # if we have any generated texts left, print the next one
      if (values$counter < length(values$generated_texts)) {
        output$dad_text <- renderText({values$generated_texts[values$counter]})
        values$counter = values$counter + 1

     # otherwise, print that we're out of texts
      } else {
        output$dad_text <- renderText("dadBot3000™ has nothing more to say :(")
      }
    }
    shinyjs::show("dad_text")
  })

}



# app ---------------------------------------------------------------------
shinyApp(ui, server)
