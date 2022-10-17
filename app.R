library(shiny)
library(DT)
library(RSQLite)
library(pool)
library(shinyjs)
library(uuid)
library(dplyr)

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
  fluidRow(width="100%",
    dataTableOutput("responses_table", width = "100%")
  )
)

# Server
server <- function(input, output, session) {

#load responses_df and make reactive to inputs  
responses_df <- reactive({
  
  #make reactive to
  input$submit
  input$submit_edit
  input$copy_button
  input$delete_button
  
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
entry_form <- function(button_id){
  
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
              actionButton(button_id, "Submit")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}

#
fieldsAll <- c("name", "sex", "age", "comment")

#save form data into data_frame format
formData <- reactive({
  
  formData <- data.frame(row_id = UUIDgenerate(),
                         name = input$name,
                         sex = input$sex,
                         age = input$age, 
                         comment = input$comment,
                         date = as.character(format(Sys.Date(), format="%d-%m-%Y")),
                         stringsAsFactors = FALSE)
  return(formData)
  
})

#Add data
appendData <- function(data){
  quary <- sqlAppendTable(pool, "responses_df", data, row.names = FALSE)
  dbExecute(pool, quary)
}

observeEvent(input$add_button, priority = 20,{
    
    entry_form("submit")
  
})

observeEvent(input$submit, priority = 20,{
  
  appendData(formData())
  shinyjs::reset("entry_form")
  removeModal()
  
})

#delete data
deleteData <- reactive({
  
  SQL_df <- dbReadTable(pool, "responses_df")
  row_selection <- SQL_df[input$responses_table_rows_selected, "row_id"]
  
  quary <- lapply(row_selection, function(nr){
    
    dbExecute(pool, sprintf('DELETE FROM "responses_df" WHERE "row_id" == (\'%s\')', nr))
  })
})

observeEvent(input$delete_button, priority = 20,{
  
  if(length(input$responses_table_rows_selected)>=1 ){
    deleteData()
  }
  
  showModal(
    
    if(length(input$responses_table_rows_selected) < 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select row(s)." ),easyClose = TRUE
      )
    })
})

#copy data
unique_id <- function(data){
  replicate(nrow(data), UUIDgenerate())
}

copyData <- reactive({
  
  SQL_df <- dbReadTable(pool, "responses_df")
  row_selection <- SQL_df[input$responses_table_rows_selected, "row_id"] 
  SQL_df <- SQL_df %>% filter(row_id %in% row_selection)
  SQL_df$row_id <- unique_id(SQL_df)
  
  quary <- sqlAppendTable(pool, "responses_df", SQL_df, row.names = FALSE)
  dbExecute(pool, quary)

})

observeEvent(input$copy_button, priority = 20,{
  
  if(length(input$responses_table_rows_selected)>=1 ){
    copyData()
  }
  
  showModal(
    
    if(length(input$responses_table_rows_selected) < 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select row(s)." ),easyClose = TRUE
      )
    })
  
})

#edit data
observeEvent(input$edit_button, priority = 20,{
  
  SQL_df <- dbReadTable(pool, "responses_df")
  
  showModal(
    if(length(input$responses_table_rows_selected) > 1 ){
      modalDialog(
        title = "Warning",
        paste("Please select only one row." ),easyClose = TRUE)
    } else if(length(input$responses_table_rows_selected) < 1){
      modalDialog(
        title = "Warning",
        paste("Please select a row." ),easyClose = TRUE)
    })  
  
  if(length(input$responses_table_rows_selected) == 1 ){
    
    entry_form("submit_edit")
    
    updateTextInput(session, "name", value = SQL_df[input$responses_table_rows_selected, "name"])
    updateSelectInput(session, "sex", selected = SQL_df[input$responses_table_rows_selected, "sex"])
    updateSliderInput(session, "age", value = SQL_df[input$responses_table_rows_selected, "age"])
    updateTextAreaInput(session, "comment", value = SQL_df[input$responses_table_rows_selected, "comment"])

  }
  
})

observeEvent(input$submit_edit, priority = 20, {
  
  SQL_df <- dbReadTable(pool, "responses_df")
  row_selection <- SQL_df[input$responses_table_row_last_clicked, "row_id"] 
  dbExecute(pool, sprintf('UPDATE "responses_df" SET "name" = ?, "sex" = ?, "age" = ?,
                          "comment" = ? WHERE "row_id" = ("%s")', row_selection), 
            param = list(input$name,
                         input$sex,
                         input$age,
                         input$comment))
  removeModal()

})


output$responses_table <- DT::renderDataTable({
  
  table <- responses_df() %>% select(-row_id) 
  names(table) <- c("Date", "Name", "Sex", "Age", "Comment")
  table <- datatable(table, 
                     rownames = FALSE,
                     options = list(searching = FALSE, lengthChange = FALSE)
  )

})

}

# Run the application 
shinyApp(ui = ui, server = server)

