library(shiny)
library(ggplot2)
library(Hmisc)
library(agricolae)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(rmarkdown)
library(RColorBrewer)
library(ggpubr)

ui <- fluidPage(
  setBackgroundColor(
    color = c("#ffb9b3", "#ffffff"),
    gradient = "radial",
    direction = c("bottom", "right")
  ),
  titlePanel(tags$div(tags$b('Two-way Analysis of Variance',style="color:#000000"))),
  sidebarPanel(
    fileInput("file1", "CSV File (upload in csv format)", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    checkboxInput("header", "Header", TRUE),
    uiOutput('var'),
    tags$br(),
    conditionalPanel("$('#trtmeans').hasClass('recalculating')",
                     tags$div(tags$b('Loading ...please wait while we are calculating in the background.....please dont press submit button again '), style="color:green")),
    tags$br(),
    h5(
      tags$div(
        tags$br(),
        "Developed by:",
        tags$br(),
        tags$b("Dr. Pratheesh P. Gopinath"),
        tags$br(),
        tags$b("Assistant Professor,"),
        tags$br(),
        tags$b("Agricultural Statistics,"),
        tags$br(),
        tags$b("Kerala Agricultural University"),
        tags$br(),
        tags$br(),
        "Contribution:",
        tags$br(),
        tags$b("Adarsh V.S."),
        tags$br(),
        tags$b("PhD, Agricultural Statistics"),
        tags$br(),
        tags$br(),
        "post your queries at: pratheesh.pg@kau.in"
        ,style="color:#343aeb")
    )
  )
  , mainPanel(
    tabsetPanel(type = "tab",
                tabPanel("Analysis.Results",
                         tableOutput('trtmeans'),
                         conditionalPanel("$('#trtmeans').hasClass('recalculating')",
                                          tags$div(tags$b('Loading ...please wait while we are calculating in the background.....please dont press submit button again '), style="color:green")),
                         tags$style(  type="text/css", "#trtmeans th,td {border: medium solid #000000;text-align:center}"),
                         tags$style(  type="text/css", "#trtmeans td {border: medium solid #000000;text-align:center}"),

                         tableOutput('aovSummary'),
                         tags$style(  type="text/css", "#aovSummary th,td {border: medium solid #000000;text-align:center}"),
                         tags$style(  type="text/css", "#aovSummary td {border: medium solid #000000;text-align:center}"),

                         tableOutput('SEM'),
                         tags$style(  type="text/css", "#SEM th,td {border: medium solid #000000;text-align:center}"),
                         tags$style(  type="text/css", "#SEM td {border: medium solid #000000;text-align:center}"),

                         htmlOutput('text'),
                         h3(""),
                         uiOutput('inference'),

                         tableOutput('multi'),
                         tags$style(  type="text/css", "#multi th,td {border: medium solid #000000;text-align:center}"),
                         tags$style(  type="text/css", "#multi td {border: medium solid #000000;text-align:center}"),

                         tableOutput('group'),
                         tags$style(  type="text/css", "#group th,td {border: medium solid #000000;text-align:center}"),
                         tags$style(  type="text/css", "#group td {border: medium solid #000000;text-align:center}"),
                         uiOutput('var1'),
                         uiOutput('start_note'),
                         tags$br(),
                         uiOutput('data_set'),# data set to test,
                         tags$br(),
                         uiOutput('start_note2'),
                         tags$br(),
                         tags$br()
                ),
                tabPanel("Plots & Graphs",
                         uiOutput('varplot'),
                         uiOutput('varboxplot'),
                         tags$br(),
                         uiOutput('start_note3'),
                         plotOutput('boxplot')%>% withSpinner(color="#0dc5c1"),
                         tags$br(),
                         tags$br(),
                         uiOutput('image_down'),#image to download
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br()

                )
    )
  )
)
  ####################
  server = function(input, output, session) {
    csvfile <- reactive({
      csvfile <- input$file1
      if (is.null(csvfile)){return(NULL)}
      dt <- read.csv(csvfile$datapath, header=input$header, sep=",")
      dt
    })

    output$var <- renderUI({
      if(is.null(input$file1$datapath)){return()}
      else{
        list (numericInput("trt", "Number of Treatments:", 0, min = 2, max = 100),
              numericInput("rep", "Number of Replications:", 0, min = 2, max = 100),
              radioButtons("treatment", "Please pick the name given for Treatment column in your file", choices =    names(csvfile())),
              radioButtons("Replication", "Please pick the name given for Replication column in your file", choices = names(csvfile())),
              radioButtons("yield", "Please pick the name given for variable of interest in your file", choices = names(csvfile())),
              tags$br(),
              actionBttn(
                inputId = "submit",
                label = "Run Analysis!",
                color = "danger",
                style = "jelly"
              )
        )
      }
    })

    output$var1 <- renderUI({
      if(is.null(input$file1$datapath)){return()}
      if(is.null(input$submit)){return()}
      if(input$submit > 0){
        list( radioButtons("format", "Download report:", c("HTML", "PDF", "Word"),
                           inline = TRUE
        ),
        downloadButton("downloadReport")
        )

      }
    })

   #####################################TRT means
     output$trtmeans<- renderTable({
      if(is.null(input$file1$datapath)){return()}
      if(is.null(input$submit)){return()}
      if(input$submit > 0){
        input$reload
        Sys.sleep(2)
        d=as.data.frame(csvfile())
        r=as.numeric(input$rep)
        t=as.numeric(input$trt)
        response=d[,input$yield]
        treatment=d[,input$treatment]
        replication=d[,input$Replication]
        treatment=factor(treatment)
        replication=factor(replication)
        anvaTable=lm(response~treatment+replication)
        result=as.data.frame( anova(anvaTable) )
        out<-agricolae::LSD.test(csvfile()[,input$yield], csvfile()[,input$treatment], result[3,1], result[3,3])
        trtmeans<-out$means
        colnames(trtmeans)[1] <- "Treatment_means"
        drops <- c("r","Q25", "Q50", "Q75")
        trtmeans[ , !(names(trtmeans) %in% drops)]
      }
    },digits= 3,caption=('<b> Treatment means & other statistics </b>'),bordered = TRUE,align='c',caption.placement = getOption("xtable.caption.placement", "top"),rownames = TRUE)

    ################################## ANOVA
     output$aovSummary<- renderTable({
      if(is.null(input$file1$datapath)){return()}
      if(is.null(input$submit)){return()}
      if(input$submit > 0){
        d=as.data.frame(csvfile())
        r=as.numeric(input$rep)
        t=as.numeric(input$trt)
        response=d[,input$yield]
        treatment=d[,input$treatment]
        replication=d[,input$Replication]
        treatment=factor(treatment)
        replication=factor(replication)
        anvaTable=lm(response~treatment+replication)
        result=as.data.frame( anova(anvaTable) )
        SoV <- c("Treatment", "Replication","Error")
        final<-cbind(SoV,result)
        final
      }
    },digits=3,caption=('<b> ANOVA TABLE </b>'),bordered = TRUE,align='c',caption.placement = getOption("xtable.caption.placement", "top"))


    ######################## SEM & other values
    output$SEM <- renderTable({
      if(is.null(input$file1$datapath)){return()}
      if(is.null(input$submit)){return()}
      if(input$submit > 0){
        d=as.data.frame(csvfile())
        r=as.numeric(input$rep)
        t=as.numeric(input$trt)
        response=d[,input$yield]
        treatment=d[,input$treatment]
        replication=d[,input$Replication]
        treatment=factor(treatment)
        replication=factor(replication)
        anvaTable=lm(response~treatment+replication)
        result=as.data.frame( anova(anvaTable) )
        out<-agricolae::LSD.test(d[,input$yield], d[,input$treatment], result[3,1], result[3,3])
        colnam<-c("MSE","SE(d)","SE(m)","CV(%)")
        stat<-out$statistics
        MSE<-stat[1,1]
        SED<-sqrt((2*MSE)/r)
        SEM<-sqrt(MSE/r)
        CV<-stat[1,4]
        Result<-cbind(MSE,SED,SEM,CV)
        colnames(Result)<-colnam
        Result
      }

    },digits=3,caption=('<b> SEM & Other statistics </b>'),bordered = TRUE,align='c',caption.placement = getOption("xtable.caption.placement", "top"))

  ################################## Inference on p value text
    output$text<- renderUI({
      if(is.null(input$file1$datapath)){return()}
      if(is.null(input$submit)){return()}
      if(input$submit > 0){
        d=as.data.frame(csvfile())
        r=as.numeric(input$rep)
        t=as.numeric(input$trt)
        response=d[,input$yield]
        treatment=d[,input$treatment]
        replication=d[,input$Replication]
        treatment=factor(treatment)
        replication=factor(replication)
        anvaTable=lm(response~treatment+replication)
        result=as.data.frame( anova(anvaTable) )
        if(result[1,5]<= 0.05){
          HTML(paste0("<b>","Since the P-value in ANOVA table is < 0.05, there is a significant difference between atleast
                    a pair of treatments, so multiple comparison is required to identify best treatment(s) ","</b>"))
        }
        else {HTML(paste0("<b>","Treatment means are not significantly different"))
        }
      }
    })

   ################# Multiple comparison options
    output$inference <- renderUI({
      if(is.null(input$file1$datapath)){return()}
      if(is.null(input$submit)){return()}
      if(input$submit > 0){
        d=as.data.frame(csvfile())
        r=as.numeric(input$rep)
        t=as.numeric(input$trt)
        response=d[,input$yield]
        treatment=d[,input$treatment]
        replication=d[,input$Replication]
        treatment=factor(treatment)
        replication=factor(replication)
        anvaTable=lm(response~treatment+replication)
        result=as.data.frame( anova(anvaTable) )
        if(result[1,5]<= 0.05){
          list (selectInput('req', 'Please select multiple comparison method (alpha =0.05)',
                            c(LSD= 'lsd',
                              DMRT= 'dmrt',
                              TUKEY='tukey'
                            )
                            ,'lsd'))
        }
        else {return()}
      }
    })

   ################## SEM, CD, CV etc
     output$multi <- renderTable({
      if(is.null(input$file1$datapath)){return()}
      if(is.null(input$submit)){return()}
      if(is.null(input$req)){return()}
      if(input$submit > 0){
        if(input$req=='lsd'){
          d=as.data.frame(csvfile())
          r=as.numeric(input$rep)
          t=as.numeric(input$trt)
          response=d[,input$yield]
          treatment=d[,input$treatment]
          replication=d[,input$Replication]
          treatment=factor(treatment)
          replication=factor(replication)
          anvaTable=lm(response~treatment+replication)
          result=as.data.frame( anova(anvaTable) )
          if(result[1,5]<= 0.05){
          out<-agricolae::LSD.test(d[,input$yield], d[,input$treatment], result[3,1], result[3,3])
          colnam<-c("MSE","SE(d)","SE(m)","CD","t value","CV(%)")
          stat<-out$statistics
          MSE<-stat[1,1]
          SED<-sqrt((2*MSE)/r)
          SEM<-sqrt(MSE/r)
          CD<-stat[1,6]
          t<-stat[1,5]
          CV<-stat[1,4]
          Result<-cbind(MSE,SED,SEM,CD,t,CV)
          colnames(Result)<-colnam
          Result
          }
        }
        else if(input$req=='dmrt'){
          d=as.data.frame(csvfile())
          r=as.numeric(input$rep)
          t=as.numeric(input$trt)
          response=d[,input$yield]
          treatment=d[,input$treatment]
          replication=d[,input$Replication]
          treatment=factor(treatment)
          replication=factor(replication)
          anvaTable=lm(response~treatment+replication)
          result=as.data.frame( anova(anvaTable) )
          if(result[1,5]<= 0.05){
          out<-agricolae::duncan.test(d[,input$yield], d[,input$treatment], result[3,1], result[3,3], alpha = 0.05, group=TRUE, main = NULL,console=FALSE)
          out$duncan
          }
        }
        else if(input$req=='tukey'){
          d=as.data.frame(csvfile())
          r=as.numeric(input$rep)
          t=as.numeric(input$trt)
          response=d[,input$yield]
          treatment=d[,input$treatment]
          replication=d[,input$Replication]
          treatment=factor(treatment)
          replication=factor(replication)
          anvaTable=lm(response~treatment+replication)
          result=as.data.frame( anova(anvaTable) )
          if(result[1,5]<= 0.05){
          out<-agricolae::HSD.test(d[,input$yield], d[,input$treatment], result[3,1], result[3,3], alpha = 0.05, group=TRUE, main = NULL,unbalanced=FALSE,console=FALSE)
          out$statistics
          }
        }
      }
    },digits=3,caption = "alpha =0.05", bordered = TRUE,align='c',caption.placement = getOption("xtable.caption.placement", "bottom"))

   ############################# grouping
     output$group <- renderTable({
      if(is.null(input$file1$datapath)){return()}
      if(is.null(input$submit)){return()}
      if(is.null(input$req)){return()}
      if(input$submit > 0){
        if(input$req=='lsd'){
          d=as.data.frame(csvfile())
          r=as.numeric(input$rep)
          t=as.numeric(input$trt)
          response=d[,input$yield]
          treatment=d[,input$treatment]
          replication=d[,input$Replication]
          treatment=factor(treatment)
          replication=factor(replication)
          anvaTable=lm(response~treatment+replication)
          result=as.data.frame( anova(anvaTable) )
          if(result[1,5]<= 0.05){
          out<-agricolae::LSD.test(d[,input$yield], d[,input$treatment], result[3,1], result[3,3])
          outgroup<-out$groups
          colnames(outgroup) <- c("trt_mean","grouping")
          outgroup
          }
        }
        else if(input$req=='dmrt'){
          d=as.data.frame(csvfile())
          r=as.numeric(input$rep)
          t=as.numeric(input$trt)
          response=d[,input$yield]
          treatment=d[,input$treatment]
          replication=d[,input$Replication]
          treatment=factor(treatment)
          replication=factor(replication)
          anvaTable=lm(response~treatment+replication)
          result=as.data.frame( anova(anvaTable) )
          if(result[1,5]<= 0.05){
          out<-agricolae::duncan.test(d[,input$yield], d[,input$treatment], result[3,1], result[3,3], alpha = 0.05, group=TRUE, main = NULL,console=FALSE)
          outgroup<-out$groups
          colnames(outgroup) <- c("trt_mean","grouping")
          outgroup
          }
        }
        else if(input$req=='tukey'){
          d=as.data.frame(csvfile())
          r=as.numeric(input$rep)
          t=as.numeric(input$trt)
          response=d[,input$yield]
          treatment=d[,input$treatment]
          replication=d[,input$Replication]
          treatment=factor(treatment)
          replication=factor(replication)
          anvaTable=lm(response~treatment+replication)
          result=as.data.frame( anova(anvaTable) )
          if(result[1,5]<= 0.05){
          out<-agricolae::HSD.test(d[,input$yield], d[,input$treatment], result[3,1], result[3,3], alpha = 0.05, group=TRUE, main = NULL,unbalanced=FALSE,console=FALSE)
          outgroup<-out$groups
          colnames(outgroup) <- c("trt_mean","grouping")
          outgroup
          }
        }
      }
    },digits=3,caption = "Treatments with same letters are not significantly different",bordered = TRUE,align='c', caption.placement = getOption("xtable.caption.placement", "bottom"),rownames = TRUE)


#########################Interactive for plots
     output$varplot <- renderUI({
       if(is.null(input$file1$datapath)){return()}
       if(is.null(input$submit)){return()}
       if(input$submit>0){
         list (selectInput('plotreq', 'Please select the required plot',
                           c(
                             Boxplot= 'boxplot',
                             Barchart='barchart'
                           )
                           ,'barchart')
         )
       }
     })

     output$varboxplot<- renderUI({
       if(is.null(input$file1$datapath)){return()}
       if(is.null(input$submit)){return()}
       if(is.null(input$plotreq)){return()}
       if(input$plotreq=='boxplot'){
         if(input$submit > 0){
           list (textInput("xlab", "Enter required x-axis label", "X-axis"),
                 textInput("ylab", "Enter required y-axis label", "Y-axis"),
                 selectInput('col1', 'Line colour pattern',
                             c("Pattern 1" = "Dark2",
                               "Pattern 2"= "Set2",
                               "Pattern 3"= "Set3",
                               "Pattern 4" = "Accent",
                               "Pattern 5" = "Set1"
                             )
                             ,"Dark2"),
                 sliderInput("size", "Required width of the line:",
                             min = 0.5, max = 2, value = 0.5
                 ),
                 sliderInput("size1", "X-axis label size:",
                             min = 10, max = 20, value = 10
                 ),
                 sliderInput("size2", "Y-axis label size:",
                             min = 10, max = 20, value = 10
                 ),
                 actionBttn(
                   inputId = "submit1",
                   label = "Click here to Draw",
                   color = "success",
                   style = "stretch"
                 )


           )
         }
       }
       else if(input$plotreq=='barchart'){
         if(input$submit > 0){
           list (textInput("xlab", "Enter required x-axis label", "X-axis"),
                 textInput("ylab", "Enter required y-axis label", "Y-axis"),
                 textInput("title", "Enter required Title", "title"),
                 selectInput('col1', 'Select colour pattern',
                             c("Pattern 1" = "Dark2",
                               "Pattern 2"= "Set2",
                               "Pattern 3"= "Set3",
                               "Pattern 4" = "Accent",
                               "Pattern 5" = "Set1"

                             )
                             ,"Dark2"),
                 sliderInput("width1", "Required width of the bar:",
                             min = 0.1, max = 1, value = 0.5
                 ),
                 sliderInput("width2", "Required width of error bar:",
                             min = 0.1, max = 1, value = 0.2
                 ),
                 sliderInput("width3", "X-axis label size:",
                             min = 10, max = 20, value = 10
                 ),
                 sliderInput("width4", "Y-axis label size:",
                             min = 10, max = 20, value = 10
                 ),
                 actionBttn(
                   inputId = "submit1",
                   label = "Click here to draw",
                   color = "success",
                   style = "stretch"
                 )


           )
         }
       }
     })

#######################################PLots and Graphs
      output$boxplot<- renderPlot({

        plotInput()

    }, bg="transparent")


############################# Download Report


    output$downloadReport <- downloadHandler(
      filename = function() {
        paste("my-report", sep = ".", switch(
          input$format, PDF = "pdf", HTML = "html", Word = "docx"
        ))
      },
      content = function(file) {
        src <- normalizePath("report.Rmd")
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, "report.Rmd", overwrite = TRUE)
        out <- render("report.Rmd", switch(
          input$format,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
      }
    )


  ######################## Download Image
    output$image_down <- renderUI({
      if(is.null(input$file1$datapath)){return()}
      if(is.null(input$plotreq)){return()}
      if(is.null(input$submit1)){return()}
      if(input$plotreq == 'boxplot'){
        if(input$submit1 > 0){
          list(downloadButton("downloadImage1",
                              label="Download BoxPlot", class = "butt1"))
        }
      }

      else if(input$plotreq == 'barchart'){
        if(input$submit1 > 0){
          list(downloadButton("downloadImage2",
                              label="Download Barchart", class = "butt1"))
        }
      }

    })


     ### plotting
    plotInput <- reactive({
      if(is.null(input$file1$datapath)){return()}
      if (is.null(csvfile)){return()}
      if(is.null(input$plotreq)){return()}
      if(is.null(input$submit1)){return()}

      if(input$plotreq=='boxplot'){
        if(input$submit1 > 0){

          nb.cols <- as.numeric(input$trt)
          mycolors <- colorRampPalette(brewer.pal(8, input$col1))(nb.cols)

          x<-as.matrix(csvfile()[,input$treatment])
          y<-as.matrix(csvfile()[,input$yield])
          my_data <- data.frame(
            group=x,
            obs = y)

          colnames(my_data)<-c("Treatments","obs")
          ggpubr::ggboxplot(my_data, x = "Treatments", y = "obs",
                            color = "Treatments", palette = mycolors,
                            ylab = input$ylab, xlab = input$xlab, size = input$size)+
            rotate_x_text()+
            font("x.text", size = input$size1)+
            font("y.text", size = input$size2)
        }
      }

      else if(input$plotreq=='barchart'){
        if(input$submit1 > 0){

          d=as.data.frame(csvfile())
          treatment<-as.factor(d[,input$treatment])

          nb.cols <- as.numeric(input$trt)
          mycolors <- colorRampPalette(brewer.pal(8, input$col1))(nb.cols)
          p<-ggplot2::ggplot(data=d, aes(x=d[,input$treatment],
                                         y=d[,input$yield],fill=treatment))+
            stat_summary(fun = mean, geom = "bar",width=input$width1) +
            stat_summary(fun.data = mean_cl_normal,
                         geom = "errorbar", width = input$width2) +
            labs(x = input$xlab, y = input$ylab)+
            ggtitle(input$title)+
            scale_fill_manual(values = mycolors)+theme_bw()+
            theme(plot.title = element_text(size=22,hjust = 0.5),axis.text.x=element_text(angle = 90, size = input$width3,hjust = 1),
                  axis.text.y=element_text(size = input$width4))
          p
        }
      }

    })

    ###
    output$downloadImage1 = downloadHandler(
      filename = 'boxplot.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 500, units = "in")
        }
        ggsave(file, plot = plotInput(), device = device)
      }
    )

    output$downloadImage2 = downloadHandler(
      filename = 'barchart.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 500, units = "in")
        }
        ggsave(file, plot = plotInput(), device = device)
      }
    )

    ########################### end image download

    ############################## download data set
    output$data_set<- renderUI({
      if(is.null(input$file1$datapath)){
        list(
          selectInput("dataset", "Choose a dataset:",
                      choices = c("mango data", "feed data", "sample data")),

          downloadButton("downloadData", label="Download csv file to test", class = "butt1")

        )
      }

      else{
        return()
      }
    })
    datasetInput <- reactive({
      mango<- read.csv(system.file("extdata/rbd_data",
                                   "mango_rbd.csv",
                                   package = "grapes") )
      feed<- read.csv(system.file("extdata/rbd_data",
                                     "feed_rbd.csv",
                                     package = "grapes") )
      sample1<- read.csv(system.file("extdata/rbd_data",
                                   "sample_rbd.csv",
                                   package = "grapes") )
      switch(input$dataset,
             "mango data" = mango,
             "feed data" = feed,
             "sample data" = sample1)
    })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )
    ######################### end data set download
    ############################### this note appear on opening
    output$start_note<- renderUI({
      if(is.null(input$file1$datapath)){return(
        HTML(paste0(
          " <h4> To perform analysis using your own dataset, prepare excel file in csv format by reading instruction below </h4>
<p>
<ui>
<li>Open a new blank excel file</li>
<li>Copy and paste observations into a new sheet (use only one sheet) of a new excel file</li>
<li>Three columns are required Treatment, Replication and Response. </li>
<li>You can use any names for your columns</li>
<li>You can also add more than one response as columns</li>
<li>No space is allowed in the treatment name. </li>
<li>If space is required use underscore ‘_’ or ‘.’ full stop; for example ‘Treatment name’ should be written as Treatment_name or Treatment.name</li>
<li>Don't type and delete anything on other cells without data. If so select those cells, right click and click clear contents. </li>
<li>Give names to all column, Don't add any unnecessary columns that is not required for analysis</li>
<li>Data should be arranged towards upper left corner and row above the data should not be left blank. </li>
<li>Once all these are done, your file is ready. Now save it as CSV file. </li>
<li><b>Upload file by clicking browse in the app </b></li>
</ui>
</p>
<b> You can download a model data set from below and test the App  </b>

"))
      )}

      else{
        return()
      }
    })

    output$start_note2<- renderUI({
      if(is.null(input$file1$datapath)){return(
        HTML(paste0(
          "<li> <b>Note</b>: Don't forget to enter
        Treatment number and Replication number
          while doing analysis</li>"))
      )}

      else{
        return()
      }
    })

    output$start_note3<- renderUI({
      if(is.null(input$file1$datapath)){return(
        HTML(paste0(
          "<b>Plots will appear here once you upload the csv file by
        clicking 'Browse' and click submit</b>
        "))
      )}

      else{
        return()
      }
    })

     }

  shinyApp(ui=ui,server=server)

