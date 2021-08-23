library(shiny)
source("dad_bot.R")
library(shinyjs)
library(bslib)


# ui ----------------------------------------------------------------------
ui <- fluidPage(

  theme = bslib::bs_theme(bootswatch = "lux"),

  useShinyjs(),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "dadbot.css")
  ),

  verticalLayout(

    titlePanel(
      title="Welcome to DadBot3000™",
      windowTitle="DadBot3000™"
    ),

    wellPanel(

      radioButtons(
        "markov_state_size",
        label="Markov State Size",
        choices=c("1", "2"),
        selected="2",
        inline=TRUE
      ),

      sliderInput(
        "max_overlap_ratio",
        label="Maximum Overlap Ratio",
        min=0.25,
        max=1,
        step=0.05,
        value=0.75
      ),

      sliderInput(
        "maximum_sentence_length",
        label="Maximum Sentence Length",
        min=10,
        max=100,
        step=5,
        value=50
      ),

      actionButton("build_model", "Build DadBot3000™", class="button")
    ),

    br(),

    fluidRow(
      column(
        12,
        align="center",
        hidden(actionButton("generate", "Generate DadText™", class="button"))
      )
    ),

    fluidRow(
      column(6, imageOutput("image")),
      column(6, textOutput("dad_text"))
    )
  )
)


# server ------------------------------------------------------------------
server <- function(input, output, session) {

  # object to store reactive values <-  referenced later via `values$...`
  values <- reactiveValues()

  # when any of the sliders are adjusted, hide the current display
  observeEvent(as.integer(input$markov_state_size)|input$max_overlap_ratio|input$maximum_sentence_length, {
    hide("image")
    hide("generate")
    hide("dad_text")
  })


  dadBotModel <- reactive({
    model <- create_model(text_data, input$markov_state_size, input$max_overlap_ratio)
  })

  dadBot <- reactive({
    dad_bot <- generate_texts(dadBotModel(), input$maximum_sentence_length, 100)
  })



  observeEvent(input$build_model, {

    # if anything has already been rendered, hide it while we re-build the model
    hide("dad_text")
    hide("image")
    hide("generate")

    # simulate a progress bar
    # the model actually builds very quickly
    # which can make it look like the app isn't doing anything
    progress <- Progress$new()
    on.exit(progress$close()) # make sure it closes when we exit this reactive
    progress$set(message="building dadBot3000™", value=0)
    progress$inc(1/3) # begin the progress bar before building model

    values$generated_texts <- dadBot()
    values$counter <- 1

    # simulate the completion of the progress bar once the model is built
    Sys.sleep(0.5)
    progress$inc(2/3)
    Sys.sleep(1)
    progress$inc(3/3)

    # render the image
    output$image <- renderImage({
      filename <- normalizePath(file.path('./images',"img.png"))

      # Return a list containing the filename
      list(src = filename, width = "200", height = "200")
    }, deleteFile = FALSE)

    show("image")
    show("generate")
  })



  observeEvent(input$generate, {

    # case where model produced nothing
    if (all(is.na(values$generated_texts))) {
      output$dad_text <- renderText("dadBot3000™ is broken :( pls rebuild")

    } else {
      # if we have any generated texts left, print the next one
      if (values$counter < length(values$generated_texts)) {
        output$dad_text <- renderText({values$generated_texts[values$counter]})
        values$counter = values$counter + 1

     # otherwise, print that we're out of texts
      } else {
        output$dad_text <- renderText("all out :(")
      }
    }
    show("dad_text")
  })

}



# app ---------------------------------------------------------------------
shinyApp(ui, server)
