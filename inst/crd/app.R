library(shiny)
library(shinyWidgets)
library(rmarkdown)
library(dplyr)
library(ggplot2)
###############
library(Hmisc)
library(agricolae)
library(RColorBrewer)
library(ggpubr)
library(gtools)


ui <- fluidPage(

  ######## BACKGROUND
  setBackgroundColor(
    color = c("#fffacc", "#ffffff"),
    gradient = "radial",
    direction = c("bottom", "right")
  ),

  ########
  titlePanel(
    title = tags$div(tags$b("Completely Randomized Design (One-way ANOVA)", style = "color:#000000")),
    windowTitle = "Completely Randomized Design"
  ),
  sidebarPanel(
    fileInput("file1", "CSV File (upload in csv format)", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    checkboxInput("header", "Header", TRUE),
    uiOutput("var"),
    tags$br(),
    conditionalPanel(
      "$('#trtmeans').hasClass('recalculating')",
      tags$div(tags$b("Loading ...please wait while we are calculating in the background.....please dont press submit button again "), style = "color:green")
    ),
    tags$br(),
    h5(
      tags$div(
        tags$br(),
        "Developed by:",
        tags$br(),
        tags$b("Dr.Pratheesh P. Gopinath"),
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
        tags$b("MSc, Agricultural Statistics"),
        tags$br(),
        tags$br(),
        "post your queries at: pratheesh.pg@kau.in",
        style = "color:#343aeb"
      )
    )
  ),
  mainPanel(
    tabsetPanel(
      type = "tab",
      tabPanel(
        "Analysis.Results",
        conditionalPanel(
          "$('#trtmeans').hasClass('recalculating')",
          tags$div(tags$b("Loading ...please wait while we are calculating in the background.....please dont press submit button again "), style = "color:green")
        ),
        tableOutput("trtmeans"),
        tags$style(type = "text/css", "#trtmeans th,td {border: medium solid #000000;text-align:center}"),
        tags$style(type = "text/css", "#trtmeans td {border: medium solid #000000;text-align:center}"),
        tableOutput("aovSummary"),
        tags$style(type = "text/css", "#aovSummary th,td {border: medium solid #000000;text-align:center}"),
        tags$style(type = "text/css", "#aovSummary td {border: medium solid #000000;text-align:center}"),
        tableOutput("SEM"),
        tags$style(type = "text/css", "#SEM th,td {border: medium solid #000000;text-align:center}"),
        tags$style(type = "text/css", "#SEM td {border: medium solid #000000;text-align:center}"),
        h3(""),
        htmlOutput("text"),
        h3(""),
        uiOutput("inference"),
        tableOutput("multi"),
        tags$style(type = "text/css", "#multi th,td {border: medium solid #000000;text-align:center}"),
        tags$style(type = "text/css", "#multi td {border: medium solid #000000;text-align:center}"),
        tableOutput("unequal"),
        tags$style(type = "text/css", "#unequal th,td {border: medium solid #000000;text-align:center}"),
        tags$style(type = "text/css", "#unequal td {border: medium solid #000000;text-align:center}"),
        tableOutput("group"),
        tags$style(type = "text/css", "#group th,td {border: medium solid #000000;text-align:center}"),
        tags$style(type = "text/css", "#group td {border: medium solid #000000;text-align:center}"),
        uiOutput("var1"),
        uiOutput("start_note"),
        tags$br(),
        uiOutput("data_set"), # data set to test,
        tags$br(),
        uiOutput("start_note2"),
        tags$br(),
        tags$br()
      ),
      tabPanel(
        "Plots & Graphs",
        uiOutput("varplot"),
        uiOutput("varboxplot"),
        tags$br(),
        tags$br(),
        uiOutput("start_note3"),
        plotOutput("boxplot"),
        uiOutput("plot_note3"),
        tags$br(),
        tags$br(),
        uiOutput("image_down"), # image to download
        tags$br(),
        tags$br(),
        tags$br(),
        tags$br()
      )
    )
  )
)


################## server
server <- function(input, output, session) {
  csvfile <- reactive({
    csvfile <- input$file1
    if (is.null(csvfile)) {
      return(NULL)
    }
    dt <- read.csv(csvfile$datapath, header = input$header, sep = ",", check.names = FALSE)
    dt
  })

  ################ appear after uploading file
  output$var <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    else {
      list(
        selectInput(
          "filerepli", "Replication",
          c(
            Equal.replication = "equal",
            Unequal.replication = "unequal"
          ),
          "equal"
        ),
        numericInput("trt", "Number of Treatments:", 0, min = 2, max = 100),
        radioButtons("treatment", "Please pick the name given for Treatment column in your file", choices = names(csvfile())),
        radioButtons("yield", "Please pick the name given for variable of interest in your file", choices = names(csvfile())),
        actionBttn(
          inputId = "submit",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
  })
  ################################### Download Button

  output$var1 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit)) {
      return()
    }
    if (input$submit > 0) {
      list(
        radioButtons("format", "Download report (Note: if you are changing the file name after download give '.html' extension):", c("HTML"),
          inline = TRUE
        ),
        downloadButton("downloadReport")
      )
    }
  })

  ################### Trt means
  output$trtmeans <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
        validate(
          need(input$treatment != input$yield, "Warning 1: Both input variables selected (Treatment and response) are same. Choose Treatment and response correctly for meaningful result")
        )
        input$reload
        Sys.sleep(2)
        d <- as.data.frame(csvfile())
        t <- as.numeric(input$trt)
        response <- d[, input$yield]
        treatment <- d[, input$treatment]
        treatment <- factor(treatment)
        anvaTable <- lm(response ~ treatment)
        result <- as.data.frame(stats::anova(anvaTable))
        out <- agricolae::LSD.test(csvfile()[, input$yield], csvfile()[, input$treatment], result[2, 1], result[2, 3])
        trtmeans <- out$means
        trtmeans <- trtmeans[ gtools::mixedsort(row.names(trtmeans)), ]
        colnames(trtmeans)[1] <- "Treatment_means"
        drops <- c("r", "Q25", "Q50", "Q75")
        trtmeans[, !(names(trtmeans) %in% drops)]
      }
    },
    digits = 3,
    caption = ("<b> Treatment means & other statistics </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE
  )

  ##################################### ANOVA TABLE
  output$aovSummary <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$submit > 0) {
        validate(
          need(input$trt != 0, "Warning 2: Please enter the number of treatments correctly")
        )
        validate(
          need(input$treatment != input$yield, "")
        )
        d <- as.data.frame(csvfile())
        t <- as.numeric(input$trt)
        response <- d[, input$yield]
        treatment <- d[, input$treatment]
        treatment <- factor(treatment)
        anvaTable <- lm(response ~ treatment)
        result <- as.data.frame(stats::anova(anvaTable))
        SoV <- c("Treatment", "Error")
        final <- cbind(SoV, result)
        final
      }
    },
    digits = 3,
    caption = ("<b> ANOVA TABLE </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top")
  )

  ########################################## SEM CD CV etc
  output$SEM <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (is.null(input$filerepli)) {
        return()
      }
      if (input$submit > 0) {
        validate(
          need(input$treatment != input$yield, "")
        )
        d <- as.data.frame(csvfile())
        t <- as.numeric(input$trt)
        response <- d[, input$yield]
        treatment <- d[, input$treatment]
        treatment <- factor(treatment)
        anvaTable <- lm(response ~ treatment)
        result <- as.data.frame(stats::anova(anvaTable))
        out <- agricolae::LSD.test(csvfile()[, input$yield], csvfile()[, input$treatment], result[2, 1], result[2, 3])
        colnam <- c("MSE", "SE(d)", "SE(m)", "CV(%)")
        stat <- out$statistics
        repl <- out$means
        r <- repl[1, 3]
        MSE <- stat[1, 1]
        SED <- sqrt((2 * MSE) / r)
        SEM <- sqrt(MSE / r)
        CV <- stat[1, 4]
        Result <- cbind(MSE, SED, SEM, CV)
        colnames(Result) <- colnam
        Result
      }
      else {
        return()
      }
    },
    digits = 3,
    caption = ("<b> Other important statistics </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
  )

  ####################################
  output$text <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit)) {
      return()
    }
    if (input$submit > 0) {
      validate(
        need(input$treatment != input$yield, "")
      )
      d <- as.data.frame(csvfile())
      t <- as.numeric(input$trt)
      response <- d[, input$yield]
      treatment <- d[, input$treatment]
      treatment <- factor(treatment)
      anvaTable <- lm(response ~ treatment)
      result <- as.data.frame(stats::anova(anvaTable))
      if (result[1, 5] <= 0.05) {
        HTML(paste0("<b>", "Since the P-value in ANOVA table is < 0.05, there is a significant difference between atleast
                    a pair of treatments, so multiple comparison is required to identify best treatment(s) ", "</b>"))
      }
      else {
        HTML(paste0("<b>", "Treatment means are not significantly different"))
      }
    }
  })
  #####################################
  output$inference <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit)) {
      return()
    }
    if (input$submit > 0) {
      validate(
        need(input$treatment != input$yield, "")
      )
      d <- as.data.frame(csvfile())
      t <- as.numeric(input$trt)
      response <- d[, input$yield]
      treatment <- d[, input$treatment]
      treatment <- factor(treatment)
      anvaTable <- lm(response ~ treatment)
      result <- as.data.frame(stats::anova(anvaTable))
      if (result[1, 5] <= 0.05) {
        list(selectInput(
          "req", "Please select multiple comparison method (alpha =0.05)",
          c(
            LSD = "lsd",
            DMRT = "dmrt",
            TUKEY = "tukey"
          ),
          "lsd"
        ))
      }
      else {
        return()
      }
    }
  })
  ################################## MUlTIPLE COMPARISON
  output$multi <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (is.null(input$req)) {
        return()
      }
      if (is.null(input$filerepli)) {
        return()
      }
      if (input$submit > 0) {
        if (input$filerepli == "equal") {
          if (input$req == "lsd") {
            validate(
              need(input$treatment != input$yield, "")
            )
            d <- as.data.frame(csvfile())
            t <- as.numeric(input$trt)
            response <- d[, input$yield]
            treatment <- d[, input$treatment]
            treatment <- factor(treatment)
            anvaTable <- lm(response ~ treatment)
            result <- as.data.frame(stats::anova(anvaTable))
            if (result[1, 5] <= 0.05) {
              out <- agricolae::LSD.test(d[, input$yield], d[, input$treatment], result[2, 1], result[2, 3])
              out$statistics
            }
          }
          else if (input$req == "dmrt") {
            validate(
              need(input$treatment != input$yield, "")
            )
            d <- as.data.frame(csvfile())
            t <- as.numeric(input$trt)
            response <- d[, input$yield]
            treatment <- d[, input$treatment]
            treatment <- factor(treatment)
            anvaTable <- lm(response ~ treatment)
            result <- as.data.frame(stats::anova(anvaTable))
            if (result[1, 5] <= 0.05) {
              out <- agricolae::duncan.test(d[, input$yield], d[, input$treatment], result[2, 1], result[2, 3], alpha = 0.05, group = TRUE, main = NULL, console = FALSE)
              out$duncan
            }
          }
          else if (input$req == "tukey") {
            validate(
              need(input$treatment != input$yield, "")
            )
            d <- as.data.frame(csvfile())
            t <- as.numeric(input$trt)
            response <- d[, input$yield]
            treatment <- d[, input$treatment]
            treatment <- factor(treatment)
            anvaTable <- lm(response ~ treatment)
            result <- as.data.frame(stats::anova(anvaTable))
            if (result[1, 5] <= 0.05) {
              out <- agricolae::HSD.test(d[, input$yield], d[, input$treatment], result[2, 1], result[2, 3], alpha = 0.05, group = TRUE, main = NULL, unbalanced = FALSE, console = FALSE)
              out$statistics
            }
          }
        }
        else if (input$filerepli == "unequal") {
          if (input$req == "lsd") {
            validate(
              need(input$treatment != input$yield, "")
            )
            d <- as.data.frame(csvfile())
            t <- as.numeric(input$trt)
            response <- d[, input$yield]
            treatment <- d[, input$treatment]
            treatment <- factor(treatment)
            anvaTable <- lm(response ~ treatment)
            result <- as.data.frame(stats::anova(anvaTable))
            if (result[1, 5] <= 0.05) {
              out <- agricolae::LSD.test(d[, input$yield], d[, input$treatment], result[2, 1], result[2, 3])
              out$statistics
            }
          }
        }
      }
    },
    digits = 3,
    caption = "alpha =0.05",
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "bottom")
  )

  ############################################# Matrix of CD in case of unequal replication
  output$unequal <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (is.null(input$req)) {
        return()
      }
      if (is.null(input$filerepli)) {
        return()
      }
      if (input$submit > 0) {
        if (input$filerepli == "unequal") {
          if (input$req == "lsd") {
            validate(
              need(input$treatment != input$yield, "")
            )
            d <- as.data.frame(csvfile())
            t <- as.numeric(input$trt)
            response <- d[, input$yield]
            treatment <- d[, input$treatment]
            treatment <- factor(treatment)
            anvaTable <- lm(response ~ treatment)
            result <- as.data.frame(stats::anova(anvaTable))
            if (result[1, 5] <= 0.05) {
              count <- table(d[, input$treatment]) # count the number of replications of treatment
              t <- (result[1, 1] + 1) # no.of treatments
              repli <- as.data.frame(count)
              reciproc <- 1 / repli[, 2] # 1/ri
              npw <- choose(t, 2) # number of pairwise combinations
              sumres <- as.vector(apply(combn(reciproc, 2), 2, sum)) # all pairwise sum of reciprocals (1/ri+1/rj)
              ems <- as.vector(replicate(npw, (result[2, 3])))
              SE_D <- sqrt((ems * sumres)) # standard error of difference
              tvalue <- qt(0.975, result[2, 1]) # tvalue
              vect <- replicate(npw, tvalue) # vector of t value
              CD <- vect * SE_D # critical difference
              means <- aggregate(response, list(treatment), mean)
              std <- aggregate(response, list(treatment), sd)
              finalmean <- cbind(means, std[, 2])
              rownames(finalmean) <- NULL
              colnam <- c("Treatment", "mean", "std")
              colnames(finalmean) <- colnam
              b <- matrix(0, t, t)
              b[upper.tri(b, diag = FALSE)] <- CD
              b[lower.tri(b, diag = FALSE)] <- CD
              name <- finalmean$Treatment
              colnames(b) <- name
              row.names(b) <- name
              b
            }
          }
        }
      }
    },
    digits = 3,
    rownames = TRUE,
    caption = "Matrix of CD values",
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top")
  )


  ######################################### Treatment Grouping
  output$group <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (is.null(input$req)) {
        return()
      }
      if (input$submit > 0) {
        if (input$req == "lsd") {
          validate(
            need(input$treatment != input$yield, "")
          )
          d <- as.data.frame(csvfile())
          t <- as.numeric(input$trt)
          response <- d[, input$yield]
          treatment <- d[, input$treatment]
          treatment <- factor(treatment)
          anvaTable <- lm(response ~ treatment)
          result <- as.data.frame(stats::anova(anvaTable))
          if (result[1, 5] <= 0.05) {
            out <- agricolae::LSD.test(d[, input$yield], d[, input$treatment], result[2, 1], result[2, 3])
            outgroup <- out$groups
            colnames(outgroup) <- c("trt_mean", "grouping")
            outgroup
          }
        }
        else if (input$req == "dmrt") {
          validate(
            need(input$treatment != input$yield, "")
          )
          d <- as.data.frame(csvfile())
          t <- as.numeric(input$trt)
          response <- d[, input$yield]
          treatment <- d[, input$treatment]
          treatment <- factor(treatment)
          anvaTable <- lm(response ~ treatment)
          result <- as.data.frame(stats::anova(anvaTable))
          if (result[1, 5] <= 0.05) {
            out <- agricolae::duncan.test(d[, input$yield], d[, input$treatment], result[2, 1], result[2, 3], alpha = 0.05, group = TRUE, main = NULL, console = FALSE)
            outgroup <- out$groups
            colnames(outgroup) <- c("trt_mean", "grouping")
            outgroup
          }
        }
        else if (input$req == "tukey") {
          validate(
            need(input$treatment != input$yield, "")
          )
          d <- as.data.frame(csvfile())
          t <- as.numeric(input$trt)
          response <- d[, input$yield]
          treatment <- d[, input$treatment]
          treatment <- factor(treatment)
          anvaTable <- lm(response ~ treatment)
          result <- as.data.frame(stats::anova(anvaTable))
          if (result[1, 5] <= 0.05) {
            out <- agricolae::HSD.test(d[, input$yield], d[, input$treatment], result[2, 1], result[2, 3], alpha = 0.05, group = TRUE, main = NULL, unbalanced = FALSE, console = FALSE)
            outgroup <- out$groups
            colnames(outgroup) <- c("trt_mean", "grouping")
            outgroup
          }
        }
      }
    },
    digits = 3,
    caption = "Treatments with same letters are not significantly different",
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "bottom"),
    rownames = TRUE
  )

  ######################################### plots

  ########################## Plot UI
  output$varplot <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit)) {
      return()
    }
    if (input$submit > 0) {
      list(selectInput(
        "plotreq", "Please select the required plot",
        c(
          "Barchart" = "barchart",
          "Barchart with grouping" = "bcg",
          "Boxplot" = "boxplot"
        ),
        "barchart"
      ))
    }
  })
  ########################################### PLOTS
  output$boxplot <- renderPlot(
    {
      plotInput()
    },
    bg = "transparent"
  )

  ############## interactive for plots
  output$varboxplot <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$submit)) {
      return()
    }
    if (is.null(input$plotreq)) {
      return()
    }
    if (input$plotreq == "boxplot") {
      if (input$submit > 0) {
        list(
          textInput("xlab", "Enter required x-axis label", "X-axis"),
          textInput("ylab", "Enter required y-axis label", "Y-axis"),
          selectInput(
            "col1", "Line colour pattern",
            c(
              "Pattern 1" = "Dark2",
              "Pattern 2" = "Set2",
              "Pattern 3" = "Set3",
              "Pattern 4" = "Accent",
              "Pattern 5" = "Set1"
            ),
            "Dark2"
          ),
          sliderInput("size", "Required width of the line:",
            min = 0.5, max = 2, value = 0.5
          ),
          sliderInput("size1", "X-axis label size:",
            min = 10, max = 20, value = 10
          ),
          sliderInput("size2", "Y-axis label size:",
            min = 10, max = 20, value = 10
          ),
          checkboxInput(
            "grey_box",
            "Convert the plot to grey scale", FALSE
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
    else if (input$plotreq == "barchart" ) {
      if (input$submit > 0) {
        list(
          textInput("xlab", "Enter required x-axis label", "X-axis"),
          textInput("ylab", "Enter required y-axis label", "Y-axis"),
          textInput("title", "Enter required Title", "title"),
          selectInput(
            "col1", "Select colour pattern",
            c(
              "Pattern 1" = "Dark2",
              "Pattern 2" = "Set2",
              "Pattern 3" = "Set3",
              "Pattern 4" = "Accent",
              "Pattern 5" = "Set1"
            ),
            "Dark2"
          ),
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
          sliderInput("trans", "colour Transparency of bar:",
                      min = 0.1, max = 1, value = 1
          ),
          sliderInput("trans1", "colour Transparency of error bar:",
                      min = 0, max = 1, value = 1
          ),
          checkboxInput(
            "grey",
            "Convert the plot to grey scale", FALSE
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
    else if (input$plotreq == "bcg") {
      if (input$submit > 0) {
        list(
          textInput("xlab", "Enter required x-axis label", "X-axis"),
          textInput("ylab", "Enter required y-axis label", "Y-axis"),
          textInput("title", "Enter required Title", "title"),
          selectInput(
            "col1", "Select colour pattern",
            c(
              "Pattern 1" = "Dark2",
              "Pattern 2" = "Set2",
              "Pattern 3" = "Set3",
              "Pattern 4" = "Accent",
              "Pattern 5" = "Set1"
            ),
            "Dark2"
          ),
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
          sliderInput("trans", "colour Transparency of bar:",
                      min = 0.1, max = 1, value = 1
          ),
          sliderInput("trans1", "colour Transparency of error bar:",
                      min = 0, max = 1, value = 1
          ),
          sliderInput("width5", "Font label size of grouping letters:",
                      min = 1.5, max = 6.5, value = 3.5
          ),
          checkboxInput(
            "grey",
            "Convert the plot to grey scale", FALSE
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


  ############################################

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("my-report", sep = ".", switch(input$format,
        HTML = "html"
      ))
    },
    content = function(file) {
      src <- normalizePath("report.Rmd")
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.Rmd", overwrite = TRUE)
      out <- render("report.Rmd", switch(input$format,
        HTML = html_document()
      ))
      file.rename(out, file)
    }
  )

  #################################### Download Image
  #################################### Download Image
  output$image_down <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$plotreq)) {
      return()
    }
    if (is.null(input$submit1)) {
      return()
    }
    if (input$plotreq == "boxplot") {
      if (input$submit1 > 0) {
        list(downloadButton("downloadImage1",
                            label = "Download BoxPlot", class = "butt1"
        ))
      }
    }

    else if (input$plotreq == "barchart" || input$plotreq == "bcg") {
      if (input$submit1 > 0) {
        list(downloadButton("downloadImage2",
                            label = "Download Barchart", class = "butt1"
        ))
      }
    }
  })

  ### plotting
  plotInput <- reactive({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$plotreq)) {
      return()
    }
    if (is.null(input$submit1)) {
      return()
    }

    if (input$plotreq == "boxplot" && input$submit1 > 0) {
      nb.cols <- as.numeric(input$trt + 2)
      x <- as.matrix(csvfile()[, input$treatment])
      y <- as.matrix(csvfile()[, input$yield])
      my_data <- data.frame(
        group = x,
        obs = y
      )

      colnames(my_data) <- c("Treatments", "obs")
      #sorting x label
      my_data$Treatments<-factor(my_data$Treatments, levels = unique(my_data$Treatments))
      mycolors <- colorRampPalette(brewer.pal(8, input$col1))(nb.cols)
      if (input$grey_box > 0) {
        p <- ggplot2::ggplot(
          my_data,
          aes(
            x = Treatments, y = obs,
            fill = Treatments
          )
        ) +
          scale_x_discrete(labels=unique(my_data$Treatments))+
          geom_boxplot(lwd = input$size) +
          scale_fill_grey(start = 0.3, end = .9) +
          theme_bw() +
          labs(x = input$xlab, y = input$ylab) +
          theme(
            axis.text.x = element_text(
              angle = 90,
              size = input$size1,
              hjust = 1
            ),
            axis.text.y = element_text(size = input$size2)
          )
        p
      }
      else {
        p <- ggplot2::ggplot(
          my_data,
          aes(
            x = Treatments, y = obs,
            fill = Treatments
          )
        ) +
          scale_x_discrete(labels=unique(my_data$Treatments))+
          geom_boxplot(lwd = input$size) +
          scale_fill_manual(values = mycolors) +
          theme_bw() +
          labs(x = input$xlab, y = input$ylab) +
          theme(
            axis.text.x = element_text(
              angle = 90,
              size = input$size1,
              hjust = 1
            ),
            axis.text.y = element_text(size = input$size2)
          )
        p
      }
    }

    else if (input$plotreq == "barchart" && input$submit1 > 0) {

      d <- as.data.frame(csvfile())
      t <- as.numeric(input$trt)
      response <- d[, input$yield]
      treatment <- d[, input$treatment]
      treatment <- factor(treatment)
      anvaTable <- lm(response ~ treatment)
      result <- as.data.frame(stats::anova(anvaTable))
      out <- agricolae::LSD.test(
        d[, input$yield],
        d[, input$treatment],
        result[2, 1], result[2, 3]
      )
      d1 <- out$means
      alpha <- 0.05
      t <- qt(alpha / 2, result[2, 1], lower.tail = FALSE) # tvalue
      d1$std_er <- d1$std / (sqrt(d1$r))
      drops <- c(
        "r", "Q25", "Q50", "Q75",
        "Max", "Min", "LCL", "UCL", "r", "std"
      )
      d1 <- d1[, !(names(d1) %in% drops)]
      d2 <- out$groups
      d <- merge(d1, d2, by = 0, all = TRUE)
      d <- d[-4]
      d$ic <- t * d$std_er
      colnames(d) <- c(
        "treatment", "Response", "std.er",
        "group", "ic"
      )
      # arrange treatments inorder during plotting
      d <- d[gtools::mixedorder(d$treatment), ]
      Treatments <- factor(d$treatment, levels = d$treatment)
      nb.cols <- as.numeric(input$trt + 2)
      mycolors <- colorRampPalette(brewer.pal(8, input$col1))(nb.cols)

      if (input$grey > 0) {
        p <- ggplot(d, aes(x = as.factor(treatment), y = Response, fill = Treatments)) +
          geom_bar(
            stat = "identity",
            position = position_dodge(width = 1),
            alpha = input$trans, width = input$width1
          ) +
          geom_errorbar(aes(ymin = Response - ic, ymax = Response + ic),
                        width = input$width2, colour = "black", alpha = input$trans1,
                        size = 0.5
          ) + scale_x_discrete(labels=unique(d$treatment))+
          labs(x = input$xlab, y = input$ylab) +
          ggtitle(input$title) +
          scale_fill_grey(start = 0.3, end = .9) +
          scale_y_continuous(expand = expansion(mult = c(0, .3))) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 22, hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1, size = input$width3),
            axis.text.y = element_text(size = input$width4)
          )
        p
      }
      else {

        p <- ggplot(d, aes(x = as.factor(treatment), y = Response, fill = Treatments)) +
          geom_bar(
            stat = "identity",
            position = position_dodge(width = 1),
            alpha = input$trans, width = input$width1
          ) +
          geom_errorbar(aes(ymin = Response - ic, ymax = Response + ic),
                        width = input$width2, colour = "black", alpha = input$trans1,
                        size = 0.5
          ) +  scale_x_discrete(labels = unique(d$treatment))+
          labs(x = input$xlab, y = input$ylab) +
          ggtitle(input$title) +
          scale_fill_manual(values = mycolors) +
          scale_y_continuous(expand = expansion(mult = c(0, .3))) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 22, hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1, size = input$width3),
            axis.text.y = element_text(size = input$width4)
          )


        p
      }
    }
    else if (input$plotreq == "bcg" && input$submit1>0) {
      d <- as.data.frame(csvfile())
      t <- as.numeric(input$trt)
      response <- d[, input$yield]
      treatment <- d[, input$treatment]
      treatment <- factor(treatment)
      anvaTable <- lm(response ~ treatment)
      result <- as.data.frame(stats::anova(anvaTable))
      out <- agricolae::LSD.test(
        d[, input$yield],
        d[, input$treatment],
        result[2, 1], result[2, 3]
      )
      d1 <- out$means
      alpha <- 0.05
      t <- qt(alpha / 2, result[2, 1], lower.tail = FALSE) # tvalue
      d1$std_er <- d1$std / (sqrt(d1$r))
      drops <- c(
        "r", "Q25", "Q50", "Q75",
        "Max", "Min", "LCL", "UCL", "r", "std"
      )
      d1 <- d1[, !(names(d1) %in% drops)]
      d2 <- out$groups
      d <- merge(d1, d2, by = 0, all = TRUE)
      d <- d[-4]
      d$ic <- t * d$std_er
      colnames(d) <- c(
        "treatment", "Response", "std.er",
        "group", "ic"
      )
      d <- d[gtools::mixedorder(d$treatment), ]
      Treatments <- factor(d$treatment, levels = d$treatment)
      nb.cols <- as.numeric(input$trt + 2)
      mycolors <- colorRampPalette(brewer.pal(8, input$col1))(nb.cols)

      # plotting
      if (input$grey > 0) {
        p <- ggplot(d, aes(x = as.factor(treatment), y = Response, fill = Treatments)) +
          geom_bar(
            stat = "identity",
            position = position_dodge(width = 1),
            alpha = input$trans, width = input$width1
          ) +
          geom_errorbar(aes(ymin = Response - ic, ymax = Response + ic),
                        width = input$width2, colour = "black", alpha = input$trans1,
                        size = 0.5
          ) +
          scale_x_discrete(labels = unique(d$treatment))+
          geom_text(aes(label = group, y = Response + ic), vjust = -0.5,
                    size=input$width5) +
          labs(x = input$xlab, y = input$ylab) +
          ggtitle(input$title) +
          scale_fill_grey(start = 0.3, end = .9) +
          scale_y_continuous(expand = expansion(mult = c(0, .3))) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 22, hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1, size = input$width3),
            axis.text.y = element_text(size = input$width4)
          )


        p
      }

      else {
        p <- ggplot(d, aes(x = as.factor(treatment), y = Response, fill = Treatments)) +
          geom_bar(
            stat = "identity",
            position = position_dodge(width = 1),
            alpha = input$trans, width = input$width1
          ) +
          geom_errorbar(aes(ymin = Response - ic, ymax = Response + ic),
                        width = input$width2, colour = "black", alpha = input$trans1,
                        size = 0.5
          ) +
          scale_x_discrete(labels = unique(d$treatment))+
          geom_text(aes(label = group, y = Response + ic), vjust = -0.5,
                    size=input$width5) +
          labs(x = input$xlab, y = input$ylab) +
          ggtitle(input$title) +
          scale_fill_manual(values = mycolors) +
          scale_y_continuous(expand = expansion(mult = c(0, .3))) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 22, hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1, size = input$width3),
            axis.text.y = element_text(size = input$width4)
          )


        p
      }
    }
  })

  ###
  output$downloadImage1 <- downloadHandler(
    filename = "boxplot.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(...,
          width = width, height = height,
          res = 500, units = "in"
        )
      }
      ggsave(file, plot = plotInput(), device = device)
    }
  )

  output$downloadImage2 <- downloadHandler(
    filename = "barchart.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(...,
          width = width, height = height,
          res = 500, units = "in"
        )
      }
      ggsave(file, plot = plotInput(), device = device)
    }
  )

  ########################### end image download

  ############################## download data set
  output$data_set <- renderUI({
    if (is.null(input$file1$datapath)) {
      list(
        selectInput("dataset", "Choose a dataset:",
          choices = c("equal_replication", "unequal_replication", "mango_data")
        ),
        downloadButton("downloadData", label = "Download csv file to test", class = "butt1")
      )
    }

    else {
      return()
    }
  })
  datasetInput <- reactive({
    equal <- read.csv(system.file("extdata/crd_data",
      "equal_replication.csv",
      package = "grapesAgri1"
    ))
    unequal <- read.csv(system.file("extdata/crd_data",
      "unequal_replication.csv",
      package = "grapesAgri1"
    ))
    mango <- read.csv(system.file("extdata/crd_data",
      "mango.csv",
      package = "grapesAgri1"
    ))
    switch(input$dataset,
      "equal_replication" = equal,
      "unequal_replication" = unequal,
      "mango_data" = mango
    )
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
  output$start_note <- renderUI({
    if (is.null(input$file1$datapath)) {
      return(
        HTML(paste0(
          " <h4> To perform analysis using your own dataset, prepare excel file in csv format by reading instruction below </h4>
<p>
<ui>
<li>Open a new blank excel file</li>
<li>Copy and paste observations into a new sheet (use only one sheet) of a new excel file</li>
<li>Two columns are required Treatment and Response. </li>
<li>You can use any names for your columns</li>
<li>You can also add more than one response as columns</li>
<li>No space is allowed in the treatment name. </li>
<li>If space is required use underscore ‘_’ or ‘.’ full stop; for example ‘Treatment name’ should be written as Treatment_name or Treatment.name</li>
<li>No need to add replication column, system will automatically identifies replication. </li>
<li>Don't type and delete anything on other cells without data. If so select those cells, right click and click clear contents. </li>
<li>Give names to all column, Don't add any unnecessary columns that is not required for analysis</li>
<li>Data should be arranged towards upper left corner and row above the data should not be left blank. </li>
<li>Once all these are done, your file is ready. Now save it as CSV file. </li>
<li><b>Upload file by clicking browse in the app </b></li>
</ui>
</p>
<b> You can download a model data set from below and test the App  </b>

"
        ))
      )
    }

    else {
      return()
    }
  })

  output$start_note2 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return(
        HTML(paste0(
          "<li> <b>Note</b>: Don't forget to enter
        treatment number while doing analysis</li>"
        ))
      )
    }

    else {
      return()
    }
  })

  output$start_note3 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return(
        HTML(paste0(
          "<b>Plots will appear here once you upload the csv file by
        clicking 'Browse' and click submit</b>
        "
        ))
      )
    }

    else {
      return()
    }
  })


  output$plot_note3 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$plotreq)) {
      return()
    }
    if (is.null(input$submit1)) {
      return()
    }
    if (input$plotreq == "bcg") {
      if (input$submit1 > 0) {
        return(
          HTML(paste0(
            "<b>Note:</b> Grouping is based on LSD test.
            <p>Treatments with same letters are not significantly different at 5% level.</p>
        "
          ))
        )
      }
    }
  })


  }

shinyApp(ui = ui, server = server)
