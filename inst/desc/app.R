library(shiny)
library(shinycssloaders)
library(shinymanager)
library(shinyWidgets)
library(rmarkdown)
library(kableExtra)
library(knitr)
library(dplyr)
library(pastecs)
library(ggpubr)
library(magrittr)
library(summarytools)

ui <- fluidPage(
  setBackgroundColor(
    color = c("#faf1d2", "#ffffff"),
    gradient = "radial",
    direction = c("bottom", "right")
  ),
  titlePanel(tags$b('Descriptive Statistics & Visualization')),
  sidebarPanel(
    fileInput("file1", "CSV File (upload in csv format)",
              accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    checkboxInput("header", "Header", TRUE),
    selectInput('req', 'Please select the analysis type',
                c(SUMMARY= 'summary',
                  SUMMARY_by_Group = 'sumbygrp',
                  BOX_PLOT= 'boxplot',
                  Histogram='histogram',
                  QQ_Plot='qqplot',
                  Normality_Test ='nt'
                )
                ,'summary')
    ,uiOutput('var'),
    tags$br(),
    conditionalPanel("$('#summaryout').hasClass('recalculating')",
                     tags$div(tags$b('Loading ...please wait  '), style="color:#da42f5"))
    ,h5(tags$div(
      "Created by:",
      tags$br(),
      tags$b("Dr.Pratheesh P. Gopinath"),
      tags$br(),
      tags$b("Assistant Professor,"),
      tags$br(),
      tags$b("Agricultural Statistics,"),
      tags$br(),
      tags$b("Kerala Agricultural University"),
      tags$br(),
      "post your queries at: pratheesh.pg@kau.in"))

  )
  , mainPanel(
    conditionalPanel("$('#summaryout').hasClass('recalculating')",
                     tags$div(tags$b('Loading ...please wait'), style="color:#da42f5")),
    htmlOutput('note'),
    uiOutput('data_set'),
    tableOutput('summaryout'),
   verbatimTextOutput('nort'),
   htmlOutput('text3'),
    tableOutput('bygroup'),
    uiOutput('var1'),
   uiOutput('var2'),
   uiOutput('plot'),
    htmlOutput('text')
)
)
server = function(input, output, session) {

  csvfile <- reactive({
    csvfile <- input$file1
    if (is.null(csvfile)){return(NULL)}
    dt <- read.csv(csvfile$datapath, header=input$header, sep=",")
    dt
  })

  output$var <- renderUI({
    if(is.null(input$file1$datapath)){return()}
    if(is.null(input$req)){return()}
    if(input$req == 'boxplot'){
      list (checkboxGroupInput("variable", HTML("Please select the variable for box plot"), choices = names(csvfile())),
            textInput("xlab", "Enter required x-axis label", "X-axis"),
            selectInput('color', 'Please select the border colour',
                        c(Black= '#141413',
                          Red = '#ff0d1d',
                          Blue= '#0d45ff',
                          Green='#0dff0d',
                          Yellow='#ffdf0d',
                          Orange='#ff8a0d'
                        )
                        ,'#141413'),
            selectInput('colorbox', 'Please select the box colour',
                        c(white= '#faf9f7',
                          Red = '#ffa299',
                          Blue= '#c1cff7',
                          Green='#d0f7b2',
                          Yellow='#f7f7ad',
                          Orange='#f5c084'
                        )
                        ,'#faf9f7'),
            actionBttn(
              inputId = "submit1",
              label = "SUBMIT!",
              color = "danger",
              style = "jelly"
            )
            )
    }
    else if(input$req=='histogram'){
      list (selectInput("variable", HTML("Please select the variable for histogram"), choices = names(csvfile())),
            textInput("xlab", "Enter required x-axis label", "X-axis"),
            selectInput('color', 'Please select the border colour',
                        c(Black= '#141413',
                          Red = '#ff0d1d',
                          Blue= '#0d45ff',
                          Green='#0dff0d',
                          Yellow='#ffdf0d',
                          Orange='#ff8a0d'
                        )
                        ,'#141413'),
            selectInput('colorbox', 'Please select the box colour',
                        c(white= '#faf9f7',
                          Red = '#ffa299',
                          Blue= '#c1cff7',
                          Green='#d0f7b2',
                          Yellow='#f7f7ad',
                          Orange='#f5c084'
                        )
                        ,'#faf9f7'),
            actionBttn(
              inputId = "submit2",
              label = "SUBMIT!",
              color = "danger",
              style = "jelly"
            )
            )
    }
    else if(input$req=='qqplot'){
      list (selectInput("variable", HTML("Please select the variable for Q-Q plot"), choices = names(csvfile())),
            radioButtons("style", HTML("Please select the required style"), choices = c("Style 1", "Style 2")),
            selectInput('color', 'Select the line colour',
                        c(Black= '#141413',
                          Red = '#ff0d1d',
                          Blue= '#0d45ff',
                          Green='#0dff0d',
                          Yellow='#ffdf0d',
                          Orange='#ff8a0d'
                        )
                        ,'#141413'),
            actionBttn(
              inputId = "submit3",
              label = "SUBMIT!",
              color = "danger",
              style = "jelly"
            )
            )
    }
    else if(input$req=='summary'){
      list (checkboxGroupInput("var", HTML("Please select the variables <br/> (Quantitative variables only)"), choices =    names(csvfile())),
            actionBttn(
              inputId = "submit4",
              label = "SUBMIT!",
              color = "danger",
              style = "jelly"
            )
      )
    }
    else if(input$req=='sumbygrp'){
      list (checkboxGroupInput("var", HTML("Please select the variables <br/> (Quantitative variables only)"), choices =    names(csvfile())),
            radioButtons("group", HTML("Please select the group if any <br/> (Qualitative variables if any)"), choices = names(csvfile())),
            actionBttn(
              inputId = "submit5",
              label = "SUBMIT!",
              color = "danger",
              style = "jelly"
            )
      )
    }
    else if(input$req=='nt'){
      list (radioButtons("var", HTML("Please select the variables <br/> (Quantitative variables only)"), choices =    names(csvfile())),
            actionBttn(
              inputId = "submit",
              label = "SUBMIT!",
              color = "danger",
              style = "jelly"
            )
      )
    }
  })


  output$var1 <- renderUI({
    if(is.null(input$file1$datapath)){return()}
    if(is.null(input$req)){return()}
    if(is.null(input$submit4)){return()}
      if(input$req == 'summary'){
        if(input$submit4 > 0){
        list( radioButtons("format", "Download report:", c("HTML"),
                           inline = TRUE
        ),
        downloadButton("downloadReport")
        )
      }
    }
  })

  output$var2 <- renderUI({
    if(is.null(input$file1$datapath)){return()}
    if(is.null(input$req)){return()}
    if(is.null(input$submit5)){return()}
      if(input$req == 'sumbygrp'){
        if(input$submit5 > 0){
        list( radioButtons("format", "Download report:", c("HTML"),
                           inline = TRUE
        ),
        downloadButton("downloadReport")
        )
        }
      }
  })


output$summaryout = function(){
    if(is.null(input$file1$datapath)){return()}
    if(is.null(input$req)){return()}
    if(is.null(input$submit4)){return()}
    if(input$req == 'summary'){
      if(input$submit4 > 0){
        input$reload
        Sys.sleep(2)
        y<-subset(csvfile(),select=input$var)
        final<-
          descr(y) %>%
          tb(order = 3) %>%
          kable( digits = 2,caption = "Summary Statistics") %>%
          kable_styling("bordered", full_width = F) %>%
          collapse_rows(columns = 1, valign = "top")
        final

      }
    }
  }

  output$nort<- renderPrint({
    if(is.null(input$file1$datapath)){return(invisible())}
    if(is.null(input$req)){return(invisible())}
    if(is.null(input$submit)){return(invisible())}
    if(input$req == 'nt'){
      if(input$submit > 0){
        y<-subset(csvfile(),select=input$var)
        data<-as.data.frame(y)
        colnames(data)<-"variable_under_study"
    test<-shapiro.test(data$variable_under_study)
    test
      }
      }
})

  output$bygroup= function(){
    if(is.null(input$file1$datapath)){return()}
    if(is.null(input$req)){return()}
    if(is.null(input$submit5)){return()}
    if(input$req == 'sumbygrp'){
      if(input$submit5 > 0){
        y1<-subset(csvfile(),select=input$var)
        y2<-subset(csvfile(),select=input$group)
        final<-
          stby(y1, y2, descr) %>%
          tb(order = 3) %>%
          kable(digits = 2,caption = "Summary Statistics by Group") %>%
          kable_styling("bordered", full_width = F) %>%
          collapse_rows(columns = 1, valign = "top")
        final
      }
    }
  }

  output$text<- renderUI({
    if(is.null(input$file1$datapath)){return()}
    if(is.null(input$req)){return()}
   if(is.null(input$submit)){return()}
    if(input$submit > 0){
       if(input$req == 'nt'){
        HTML(paste0(" Shapiro-Wilk normality test: If the<b>p-value > 0.05</b> implying that the distribution of the data are <b>not significantly different from normal distribution</b>. In other words, we can assume the normality."))
      }
    }
  })

  output$text3<- renderUI({
    if(is.null(input$file1$datapath)){return()}
    if(is.null(input$req)){return()}
    if(is.null(input$submit)){return()}
    if(input$submit > 0){
      if(input$req == 'nt'){
        y<-subset(csvfile(),select=input$var)
        data<-as.data.frame(y)
        colnames(data)<-"variable_under_study"
        test<-shapiro.test(data$variable_under_study)
        if(test$p.value <=0.05){
        HTML(paste0(" Here <b>p-value is < 0.05</b>, so we can say that <b>data does not follows a normal distribution</b>at 5% level of significance</b>."))
        }
        else{
          HTML(paste0(" Here <b>p-value is > 0.05</b>, so we can say that <b>data follows a normal distribution</b>at 5% level of significance</b>."))

        }
          }
    }
  })

  output$plot <- renderUI({
    if(is.null(input$file1$datapath)){return()}
    if(is.null(input$req)){return()}
    if(input$req == 'boxplot'){
      if(is.null(input$submit1)){return()}
      output$boxplot = renderPlot({
        if(input$submit1 > 0){
          boxplot(csvfile()[,input$variable],
                  xlab=input$xlab,
                  col=input$colorbox,
                  border = input$color)
        }
      },bg="transparent")
      plotOutput("boxplot")
    }
    else if(input$req == 'histogram'){
      if(is.null(input$submit2)){return()}
      output$histogram = renderPlot({
        if(input$submit2 > 0){
          hist(csvfile()[,input$variable],
               main="Histogram",
               xlab=input$xlab,
               col=input$colorbox,
               border = input$color,
               freq=TRUE)
        }
      },bg="transparent")
      plotOutput("histogram")
    }

    else if(input$req == 'qqplot'){
      if(is.null(input$submit3)){return()}
      output$qqplot = renderPlot({
        if(input$style == 'Style 1'&& input$submit3>0){
          qqnorm(csvfile()[,input$variable],
                 pch = 3,
                 frame = FALSE)
          qqline(csvfile()[,input$variable],
                 col = input$color,
                 lwd = 2)
        }
        else if(input$style == 'Style 2'&& input$submit3>0){

          ggqqplot(csvfile()[,input$variable], color = input$color)
        }

      },bg="transparent")
      plotOutput("qqplot")
    }

  })

  output$note<- renderUI({
    if(is.null(input$file1$datapath)){return(
      HTML(paste0(" <h4> To perform analysis using your own dataset prepare excel file in csv format by reading instruction below  </h4>
<p>
<ui>
<li>Open a new blank excel file</li>
<li>Copy and paste observations into a new sheet (use only one sheet) of a new excel file</li>
<li>Observations should be pasted as columns </li>
<li>Don't type or delete anything on other cells without data</li>
<li>You can use any names for your columns. No space is allowed in the Column name. If space is required use underscore ‘_’ or ‘.’ full stop; for example ‘Variable name’ should be written as Variable_name or Variable.name</li>
<li>Data should be arranged towards upper left corner and row above the data should not be left blank </li>
<li>Don't type and delete anything on other cells without data. If so select those cells, right click and click clear contents </li>
<li>Give names to all column, Don't add any unnecessary columns that is not required for analysis</li>
<li>Once all these are done, your file is ready. Now save it as CSV file. </li>
<li><b>Upload file by clicking browse in the app </b></li>
</ui>
</p>
<h5> You can download a model data set from below and test the App  </h5>
"))
   )}

   else{
      return()
    }
  })

  output$data_set<- renderUI({
    if(is.null(input$file1$datapath)){
      list(
        selectInput("dataset", "Choose a dataset:",
                    choices = c("iris", "pressure", "cars")),

        downloadButton("downloadData", label="Download csv file to test", class = "butt1")

      )
    }

    else{
      return()
    }
  })

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("my-report", sep = ".", switch(
        input$format, HTML = "html"
      ))
    },

    content = function(file) {
      src <- normalizePath("report.Rmd")
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.Rmd", overwrite = TRUE)

      out <- render("report.Rmd", switch(
        input$format,
        HTML = html_document()
      ))
      file.rename(out, file)
    }
  )

  datasetInput <- reactive({
    switch(input$dataset,
           "iris" = iris,
           "pressure" = pressure,
           "cars" = cars)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )


}
shinyApp(ui=ui,server=server)
