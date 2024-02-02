#Selah Dean 
#Opposing Pitching Chart

library(shiny)
library(tidyverse)
library(DT)
library(RColorBrewer)

ui <- fluidPage(
  
  titlePanel(
    fluidRow(
      column(1, downloadButton("download", "Save Chart")),
      column(1, offset = 1, actionButton("newgame", "New Game"))
      )
  ),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(2, actionButton("newpitcher", "New Pitcher")), 
        column(4, offset=2, textOutput("currentpitcher"))
      ),
      fluidRow(
        column(2,actionButton("newinning", "New Inning"), style = "padding-top:10px"), 
        column(4, offset=2, textOutput("currentinning"), style = "padding-top:10px")
      ),
      fluidRow(
        column(2,actionButton("newbatter", "New Batter"),style = "padding-top:10px"), 
        column(4, offset=2, textOutput("currentbatter"), style = "padding-top:10px")
      ),
      fluidRow(
        column(2,actionButton("newpitch", "New Pitch"), style = "padding-top:10px"), 
        column(4, offset=2, textOutput("pitchnum"), style = "padding-top:10px"), 
      ),
      fluidRow(
        column(2,actionButton("editdata", "Edit"), style = "padding-top:10px"), 
      ),
      fluidRow(
        column(8, textOutput("pitcherlisttitle"), style = "padding-top:100px"),
      ),
      fluidRow(
        column(8, textOutput("pitcherlist"), style = "padding-top-10px"),
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Full", plotOutput("plot")), 
        tabPanel("Right vs Left", plotOutput("rightleftplot"))
      )
    )
  )
)

server <- function(input, output) {
  
  values <- reactiveValues(df = data.frame(`S/B` = as.character(), `Type` = as.character(),
                                           `Batter` = as.numeric(), `Batter Hand` = as.character(),
                                           `Pitcher` = as.numeric(), `Pitcher Hand` = as.character(), 
                                           `Count(B)` = as.numeric(), `Count(S)` = as.numeric(), 
                                           `Inning` = as.numeric(), check.names = FALSE))
  
  observeEvent(input$newgame, {
    if(!is.null(values$m)){
      showModal(modalDialog(
        tags$h2('Do you want to clear current data?'),
        footer=tagList(
          actionButton("yes", "Yes"),
          actionButton("no", "No"),
        )
      )
      )}
    else{
      showModal(modalDialog(
        textInput("opponent", "Opponent"), 
        dateInput("date", "Date:", format="mm/dd/yyyy"),
        radioButtons("gamenum", "Game Number", c(1,2)),
        fileInput('uploaded_data',"Upload Existing Game",
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    '.csv'
                  )),
        footer=tagList(
          actionButton("submitgame", "Save"), 
          modalButton("Cancel")
        )
      ))
    }
  })
  
  observeEvent(input$newpitcher, {
    showModal(modalDialog(
      numericInput("pitchernum", "Pitcher Number", value = NULL, min=0, max = 100),
      radioButtons("pitcherhand", "Pitcher Hand", c("Right", "Left")),
      footer=tagList(
        actionButton("submitpitcher", "Save Pitcher"), 
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$newpitch, {
    if(is.null(input$opponent)){
      showNotification("Please enter game info", duration = NULL, closeButton = TRUE)
      Sys.sleep(1)
    }
    if(is.null(input$pitchernum)){
      showNotification("Please enter pitcher info", duration = NULL, closeButton = TRUE)
      Sys.sleep(1)
    }
    if(is.null(input$inning)){
      showNotification("Please enter inning", duration = NULL, closeButton = TRUE)
      Sys.sleep(1)
    }
    if(is.null(input$batternum)){
      showNotification("Please enter batter info", duration = NULL, closeButton = TRUE)
      Sys.sleep(1)
    }
    req(input$opponent, input$pitchernum, input$inning, input$batternum)
    showModal(modalDialog(
      tags$h2('Enter Pitch Info'),
      radioButtons("strike", "S/B", c("Ball", "Called Strike", "Swing and Miss", "Foul", "In Play")),
      radioButtons("type", "Type", c("Fastball", "Curveball", "Slider", "Changeup", "Other")),
      footer=tagList(
        actionButton("submitpitch", "Save Pitch"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$newbatter, {
    showModal(modalDialog(
      numericInput("batternum", "Batter Number", value = NULL, min=0, max = 100),
      radioButtons("batterhand", "Batter Hand", c("Right", "Left")),
      footer=tagList(
        actionButton("submitbatter", "Save Batter"), 
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$newinning, {
    showModal(modalDialog(
      numericInput("inning", "Inning", value = NULL, min = 1, max = 30), 
      footer=tagList(
        actionButton("saveinning", "Save Inning"), 
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$editdata, {
    showModal(modalDialog(
      DTOutput("table"),
      footer=tagList(
        actionButton("ok", "Ok")
      )
    ))
  })
  
  observeEvent(input$submitpitch, {
    removeModal()
    temp <- values$m
    
    last_row <- tail(temp, n=1)
    
    if(is.null(last_row)){
      balls <- 0
      strikes <- 0
    } else if ((last_row$`Batter` != input$`batternum`) | (last_row$`S/B` == "In Play")){
      balls <- 0 
      strikes <- 0 
    } else {
      if (last_row$`S/B` == "Called Strike" | last_row$`S/B`== "Swing and Miss"){
        strikes <- last_row$`Count(S)` + 1
        balls <- last_row$`Count(B)`
      } else if (last_row$`S/B` == "Ball"){
        strikes <- last_row$`Count(S)`
        balls <-last_row$`Count(B)` + 1
      } else if (last_row$`S/B` == "Foul"){
        balls <- last_row$`Count(B)`
        if (last_row$`Count(S)` < 2){
          strikes <- last_row$`Count(S)` + 1
        } else {
          strikes <- last_row$`Count(S)`
        }
      }
    }
    
    new_row <- data.frame(`S/B` = input$strike, `Type` = input$type,
                          `Batter` = input$batternum, `Batter Hand` = input$batterhand,
                          `Pitcher` = input$pitchernum, `Pitcher Hand` = input$pitcherhand, 
                          `Count(B)` = balls, `Count(S)` = strikes, 
                          `Inning` = input$inning, check.names = FALSE)
    
    temp <- rbind(temp, new_row)
    
    values$m <- temp
  })
  
  rv <- reactiveValues(
    upload_state = NULL
  )
  
  observeEvent(input$uploaded_data, {
    rv$upload_state <- "uploaded"
  })
  
  observeEvent(input$submitpitcher, {
    removeModal()
  })
  
  observeEvent(input$submitbatter, {
    removeModal()
  })
  
  observeEvent(input$saveinning, {
    removeModal()
  })
  
  observeEvent(input$submitgame, {
    removeModal()
  })
  
  observeEvent(input$ok, {
    removeModal()
  })
  
  observeEvent(input$yes,{
    values$m <- NULL
    rv$upload_state <- NULL
    removeModal()
    showModal(modalDialog(
      textInput("opponent", "Opponent"), 
      dateInput("date", "Date:", format="mm/dd/yyyy"),
      radioButtons("gamenum", "Game Number", c(1,2)),
      fileInput('uploaded_data',"Upload Existing Game",
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )),
      footer=tagList(
        actionButton("submitgame", "Save"), 
        modalButton("Cancel")
      )
    ))
  })
  
  
  
  uploaded_data <- reactive({
    inFile <- input$uploaded_data
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = ",")
    df <- select(df, -X)
    colnames(df) <- c("S/B", "Type", "Batter", "Batter Hand","Pitcher", "Pitcher Hand", "Count(B)", "Count(S)", "Inning")
    return(df)
  })
  
  output$plot <- renderPlot({
    if(is.null(values$m) & !is.null(rv$upload_state)){
      values$m <- uploaded_data() 
    }
    if(!is.null(values$m)){
      data <- values$m %>% filter(Pitcher == input$pitchernum) %>% mutate(Count = paste0(`Count(B)`, "-", `Count(S)`))
      
      if(nrow(data) > 0){
        ggplot(data, aes(x = Type, fill=`S/B`)) + geom_bar(stat="count") + theme_bw() +
          facet_wrap(~Count) + xlab("Pitch Type") + ylab("Frequency") +
          labs(fill="Pitch Result") +
          theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
          scale_fill_brewer(palette = "Blues")
      }
    }
  })
  
  output$rightleftplot <- renderPlot({
    if(!is.null(values$m)){
      data <- values$m %>% filter(Pitcher == input$pitchernum) %>% mutate(Count = paste0(`Count(B)`, "-", `Count(S)`))

      if(nrow(data)>0){
        ggplot(data, aes(x = Type, fill=`S/B`)) + geom_bar(stat="count") + theme_bw() +
          facet_wrap(~Count + `Batter Hand`) + xlab("Pitch Type") + ylab("Frequency") +
          labs(fill="Pitch Result") +
          theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
          scale_fill_brewer(palette = "Blues")
      }
    }
  })
  
  output$table <- renderDataTable({
    if (is.null(values$m) & !is.null(rv$upload_state)){
      values$m <- uploaded_data()
    }
    datatable(values$m, editable = TRUE, 
              options = list(searching=FALSE, pageLength = 150, dom = "t", 
                             columnDefs = list(list(visible=FALSE, targets=c(7,8)))))
  })
  
  observeEvent(input$table_cell_edit, {
    row  <- input$table_cell_edit$row
    clmn <- input$table_cell_edit$col
    if (clmn == 3){
      if(values$m[row-1, clmn] != input$table_cell_edit$value){
        values$m[row, 7] <- 0
        values$m[row, 8] <- 0
      } else {
        previous_pitch <- values$m[row-1, 1]
        previous_strike <- values$m[row-1, 8]
        previous_ball <- values$m[row-1, 7]
        if (previous_pitch == "Called Strike" | previous_pitch == "Swing and Miss"){
          strikes <- previous_strike + 1
          balls <- previous_ball
        } else if (previous_pitch == "Ball"){
          strikes <- previous_strike
          balls <- previous_ball + 1
        } else if (previous_pitch == "Foul"){
          balls <- previous_ball
          if (previous_strike < 2){
            strikes <- previous_strike + 1
          } else {
            strikes <- previous_strike
          }
        }
        values$m[row, 8] <- strikes
        values$m[row, 7] <- balls
      }
    }
    values$m[row, clmn] <- input$table_cell_edit$value
  })
  
  output$download <- downloadHandler(
    filename = function(){
      paste0(input$date, input$opponent, input$gamenum, ".csv", sep="")
    },
    content = function(file){
      write.csv(values$m, file)
    }
  )
  
  output$currentpitcher <- renderText({paste0("Current Pitcher: ", input$`pitchernum`)})
  output$currentbatter <- renderText({paste0("Current Batter: ", input$`batternum`)})
  output$currentinning <- renderText({paste0("Current Inning: ", input$`inning`)})
  output$pitchnum <- renderText({
    paste0("Pitches Thrown: ", 
           ifelse(!is.null(values$m) ,nrow(filter(values$m, Pitcher == input$pitchernum)), ""))})
  output$pitcherlisttitle <- renderText({"Previous Pitchers:"})
  output$pitcherlist <- renderText({paste(unique(values$m$Pitcher), sep="\n")})
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)
