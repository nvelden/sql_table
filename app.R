library(shiny)
library(DT)
library(RSQLite)
library(pool)
library(shinyjs)
library(uuid)

#Create sql lite database
pool <- dbPool(RSQLite::SQLite(), dbname = "db.sqlite")

#Create sql lite df
responses_df <- data.frame(row_id = character(),
                           name = character(),
                           sex = character(),
                           age = character(), 
                           comment = character(),
                           date = as.Date(character()),
                           stringsAsFactors = FALSE)

#Create responses table in sql database
dbWriteTable(pool, "responses_df", responses_df, overwrite = FALSE, append = TRUE)

#Label mandatory fields
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"

# ui
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  fluidRow(
    actionButton("add_button", "Add", icon("plus")),
    actionButton("edit_button", "Edit", icon("edit")),
    actionButton("copy_button", "Copy", icon("copy")),
    actionButton("delete_button", "Delete", icon("trash-alt"))
  ),
  br(),
  fluidRow(
    dataTableOutput("responses_table")
  )
)

# Server
server <- function(input, output, session) {

#load responses_df and make reactive to inputs  
responses_df <- reactive({
  
  dbReadTable(pool, "responses_df")

})  

#List of mandatory fields for submission
fieldsMandatory <- c("name", "sex")

#define which input fields are mandatory 
observe({
  
  mandatoryFilled <-
    vapply(fieldsMandatory,
           function(x) {
             !is.null(input[[x]]) && input[[x]] != ""
           },
           logical(1))
  mandatoryFilled <- all(mandatoryFilled)
  
  shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
})

#Form for data entry
entry_form <- function(){
  
  showModal(
    modalDialog(
      div(id=("entry_form"),
          tags$head(tags$style(".modal-dialog{ width:400px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            fluidRow(
              splitLayout(
                cellWidths = c("250px", "100px"),
                cellArgs = list(style = "vertical-align: top"),
                textInput("name", labelMandatory("Name"), placeholder = ""),
                selectInput("sex", labelMandatory("Sex"), multiple = FALSE, choices = c("", "M", "F"))
              ),
              sliderInput("age", "Age", 0, 100, 1, ticks = TRUE, width = "354px"),
              textAreaInput("comment", "Comment", placeholder = "", height = 100, width = "354px"),
              helpText(labelMandatory(""), paste("Mandatory field.")),
              actionButton("submit", "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

#
fieldsAll <- c("name", "sex", "age", "comment")

epochTime <- function() {
  Sys.time()
}
#save form data into data_frame format
formData <- reactive({
  
  formData <- data.frame(row_id = UUIDgenerate(),
                         name = input$name,
                         sex = input$sex,
                         age = input$age, 
                         comment = input$comment,
                         date = as.Date(epochTime()),
                         stringsAsFactors = FALSE)
  print(formData)
  return(formData)
  
})

observeEvent(input$add_button, {
    
    entry_form()
  
})

observeEvent(input$submit, {
  
  formData()
})




output$responses_table <- DT::renderDataTable({
  
  table <- datatable(responses_df(), 
                     rownames = FALSE,
                     options = list(searching = FALSE, lengthChange = FALSE)
  )

})

}

# Run the application 
shinyApp(ui = ui, server = server)

