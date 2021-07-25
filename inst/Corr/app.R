library(rmarkdown)
library(shinyWidgets)
library(dplyr)
library(gridGraphics)
library(RColorBrewer)
library(knitr)
library(corrplot)
library(Hmisc)
library(reshape2)
library(ggplot2)
library(datasets)
library(grid)



############################### ui

ui <- fluidPage(
  setBackgroundColor(
    color = c("#bbedfc", "#ffffff"),
    gradient = "radial",
    direction = c("bottom", "right")
  ),
  tags$script("$(document).on('shiny:connected', function(event) {
var myWidth = $(window).width();
Shiny.onInputChange('shiny_width',myWidth)

});"),
  tags$script("$(document).on('shiny:connected', function(event) {
var myHeight = $(window).height();
Shiny.onInputChange('shiny_height',myHeight)

});"),
  titlePanel(
    title = tags$div(tags$b("Correlation Analysis", style = "color:#fc03f4")),
    windowTitle = "Correlation Analysis"
  ),
  sidebarPanel(
    fileInput("file1", "CSV File (upload in csv format)", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    checkboxInput("header", "Header", TRUE),
    selectInput(
      "req1", "Please select the analysis type",
      c(
        Simple_correlation = "correlation",
        Correlation_Matrix = "corrmat",
        Scatter_Plot = "scatplot",
        Correlogram = "corrplot"
      ),
      "correlation"
    ),
    uiOutput("var"),
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
    htmlOutput("note1"),
    uiOutput("data_set"), # for data set download

    tableOutput("correlation"),
    tags$style(type = "text/css", "#correlation th,td {border: medium solid #000000;text-align:center}"),
    tags$style(type = "text/css", "#correlation td {border: medium solid #000000;text-align:center}"),
    tableOutput("ci"),
    tags$style(type = "text/css", "#ci th,td {border: medium solid #000000;text-align:center}"),
    tags$style(type = "text/css", "#ci td {border: medium solid #000000;text-align:center}"),
    tableOutput("corrmat"),
    tags$style(type = "text/css", "#corrmat th,td {border: medium solid #000000;text-align:center}"),
    tags$style(type = "text/css", "#corrmat td {border: medium solid #000000;text-align:center}"),
    htmlOutput("note"),
    tags$br(),
    tableOutput("sigmat"),
    tags$style(type = "text/css", "#sigmat th,td {border: medium solid #000000;text-align:center}"),
    tags$style(type = "text/css", "#sigmat td {border: medium solid #000000;text-align:center}"),
    uiOutput("plot"),
    tags$br(),
    tags$br(),
    uiOutput("image_down"), # for image download
    uiOutput("var1")
  )
)
############################ SERVER
server <- function(input, output, session) {
  csvfile <- reactive({
    csvfile <- input$file1
    if (is.null(csvfile)) {
      return(NULL)
    }
    dt <- read.csv(csvfile$datapath, header = input$header, sep = ",", check.names = FALSE)
    dt
  })

  output$var <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (input$req1 == "correlation") {
      list(
        radioButtons("dvar", "Please pick 'X' Variable", choices = names(csvfile())),
        radioButtons("ivar", "Please pick 'Y' Variable", choices = names(csvfile())),
        selectInput(
          "req", "Please select the method",
          c(
            PEARSON = "pearson",
            KENDALL = "kendall",
            SPEARMAN = "spearman"
          ),
          "pearson"
        ),
        selectInput(
          "ci", "Level of Significance",
          c(
            Five_percent = 0.95,
            One_percent = 0.99,
            Ten_percent = 0.90
          ),
          0.95
        ),
        selectInput(
          "alt", "Alternative Hypotesis",
          c(
            Two_sided = "two.sided",
            Less_than_Zero = "less",
            Greater_than_Zero = "greater"
          ),
          "two.sided"
        ),
        actionBttn(
          inputId = "submit",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
    else if (input$req1 == "scatplot") {
      list(
        radioButtons("dvar", "Please pick 'X' Variable", choices = names(csvfile())),
        radioButtons("ivar", "Please pick 'Y' Variable", choices = names(csvfile())),
        textInput("main", "Enter required title for the plot", "main title"),
        textInput("xlab", "Enter required x-axis label", "X-axis"),
        textInput("ylab", "Enter required x-axis label", "y-axis"),
        selectInput(
          "color", "Please select point colour",
          c(
            Black = "#141413",
            Red = "#ff0d1d",
            Blue = "#0d45ff",
            Green = "#0dff0d",
            Yellow = "#ffdf0d",
            Orange = "#ff8a0d"
          ), "#141413"
        ),
        actionBttn(
          inputId = "submit1",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
    else if (input$req1 == "corrmat") {
      list(
        checkboxGroupInput("selvar", "Please select more than one variables", choices = names(csvfile())),
        selectInput(
          "req", "Please select the method",
          c(
            PEARSON = "pearson",
            SPEARMAN = "spearman"
          ),
          "pearson"
        ),
        actionBttn(
          inputId = "submit2",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
    else if (input$req1 == "corrplot") {
      list(
        checkboxGroupInput("selvar", "Please select the variables", choices = names(csvfile())),
        selectInput(
          "req2", "Please select the method",
          c(
            PEARSON = "pearson",
            SPEARMAN = "spearman"
          ),
          "pearson"
        ),
        selectInput(
          "shape", "Please select the visualization method",
          c(
            circle = "circle",
            square = "square",
            ellipse = "ellipse",
            number = "number",
            shade = "shade",
            color = "color",
            pie = "pie"
          ),
          "circle"
        ),
        selectInput(
          "layout", "Please select the correlogram layout",
          c(
            full = "full",
            upper = "upper",
            lower = "lower"
          ),
          "circle"
        ),
        selectInput(
          "style", "Please select colour pattern of your choice",
          c(
            style1 = "BrBG",
            style2 = "PiYG",
            style3 = "PRGn",
            style4 = "PuOr",
            style5 = "RdBu",
            style6 = "RdGy",
            style7 = "RdYlBu",
            style8 = "RdYlGn"
          ),
          "BrBG"
        ),
        checkboxInput(
          "remove_corr",
          "Don't show correlation coefficient", FALSE
        ),
        sliderInput("cex", "Required Font size of coefficient:",
          min = 0.5, max = 3, value = 1
        ),
        sliderInput("tlcex", "Required Font size of axis label:",
                    min = 0.5, max = 3, value = 1
        ),
        selectInput(
          "txcol", "Please select correlation coefficient colour",
          c(
            Black = "#141413",
            Transparent = "#00141413",
            Red = "#ff0d1d",
            Blue = "#0d45ff",
            Green = "#0dff0d",
            Yellow = "#ffdf0d",
            Orange = "#ff8a0d"
          ), "#141413"
        ),
        checkboxInput(
          "significance",
          "Mark non-significant correlations in the plot", FALSE
        ),
        radioButtons("sig", "Please pick significance level", choices = c("0.05", "0.01")),
        actionBttn(
          inputId = "submit3",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
  })

  ###################
  # correlaton
  output$correlation <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$req1)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$req1 == "correlation") {
        if (input$submit > 0) {
          validate(
            need(input$dvar != input$ivar, "Both input variables selected (x and y) are same, correlation will be one.")
          )
          a <- as.vector(csvfile()[, input$dvar])
          y <- as.vector(csvfile()[, input$ivar])
          x <- cor.test(a, y,
            method = input$req, conf.level = as.numeric(input$ci),
            alternative = input$alt, exact = FALSE
          )
          t_value <- round(x$statistic, 3)
          correlation <- round(x$estimate, 3)
          df <- x$parameter
          pvalue <- round(x$p.value, 3)
          alt.Hypothesis <- x$alternative
          result <- cbind(correlation, t_value, df, pvalue, alt.Hypothesis)
          nam <- x$method
          rownames(result) <- nam
          result <- as.data.frame(result)
          result
        }
      }
    },
    rownames = TRUE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Simple Correlation </b>"),
    caption.placement = getOption("xtable.caption.placement", "top")
  )

  output$ci <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$req1)) {
        return()
      }
      if (is.null(input$submit)) {
        return()
      }
      if (input$req1 == "correlation") {
        if (input$submit > 0) {
          if (input$req == "pearson") {
            validate(
              need(input$dvar != input$ivar, "Are you confused?.
            Correlation measures linear relationship between two variables say x and y. To know more on correlation see Read me of package grapesAgri1. You can select different variables as x and y from the sidebar panel and see how it works!")
            )
            a <- as.vector(csvfile()[, input$dvar])
            y <- as.vector(csvfile()[, input$ivar])
            x <- cor.test(a, y,
              method = "pearson", conf.level = as.numeric(input$ci),
              alternative = input$alt, exact = FALSE
            )
            ci <- x$conf.int
            ci_nw <- reshape2::melt(ci, value.name = "Lower Limit and Upper limit")
            ci_nw
          }
        }
      }
    },
    rownames = FALSE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Confidence Interval</b>"),
    caption.placement = getOption("xtable.caption.placement", "top")
  )
  ##################################
  # Correlation Matrix
  output$corrmat <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$req1)) {
        return()
      }
      if (is.null(input$submit2)) {
        return()
      }
      if (input$req1 == "corrmat") {
        if (input$submit2 > 0) {
          validate(
            need(input$selvar, "No input selected, supply me some variables to cook a matrix!")
          )
          x <- as.data.frame(subset(csvfile(), select = input$selvar))
          cormat <- Hmisc::rcorr(as.matrix(x), type = input$req)
          R <- round(cormat$r, 3)
          p <- cormat$P
          ## Define notions for significance levels; spacing is important.
          mystars <- ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    ")))
          Rnew <- matrix(paste(R, mystars, sep = ""), ncol = ncol(x))
          diag(Rnew) <- paste(diag(R), " ", sep = "")
          row.names(Rnew) <- names(x)
          colnames(Rnew) <- names(x)
          Rnew
        }
      }
    },
    rownames = TRUE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Correlation Matrix</b>"),
    caption.placement = getOption("xtable.caption.placement", "top")
  )

  output$sigmat <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$req1)) {
        return()
      }
      if (is.null(input$submit2)) {
        return()
      }
      if (input$req1 == "corrmat") {
        if (input$submit2 > 0) {
          validate(
            need(input$selvar, "You can select input variables from sidebarpanel.")
          )
          x <- as.data.frame(subset(csvfile(), select = input$selvar))
          cormat <- Hmisc::rcorr(as.matrix(x), type = input$req)
          correlmat <- cormat$P
          row.names(correlmat) <- names(x)
          correlmat
        }
      }
    },
    digits = 3,
    rownames = TRUE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Matrix of P-values</b>"),
    caption.placement = getOption("xtable.caption.placement", "top")
  )

  output$note <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (is.null(input$submit2)) {
      return()
    }
    if (input$submit2 > 0) {
      if (input$req1 == "corrmat") {
        validate(
          need(input$selvar, "")
        )

        HTML(paste0("<p>*** Correlation is significant at 0.001 level (two tailed)</p>
                    <p>** Correlation is significant at 0.01 level (two tailed)</p>
                    <p>* Correlation is significant at 0.05 level (two tailed)</p>"))
      }
    }
  })


  ############################### PLOTS
  output$plot <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (input$req1 == "scatplot") {
      if (is.null(input$submit1)) {
        return()
      }
      output$scatplot <- renderPlot(
        {
          if (input$submit1 > 0) {
            x <- as.vector(csvfile()[, input$dvar])
            y <- as.vector(csvfile()[, input$ivar])
            plot(x, y,
              main = input$main,
              xlab = input$xlab, ylab = input$ylab,
              pch = 19, col = input$color, frame = FALSE
            )
          }
        },
        bg = "transparent"

      )
      plotOutput("scatplot")
    }
    else if (input$req1 == "corrplot") {
      if (is.null(input$submit3)) {
        return()
      }
      output$corrplot <- renderPlot(
        {
          if (input$submit3 > 0) {
            validate(
              need(input$selvar, "No input selected, supply me some variables to draw!")
            )
            x <- as.data.frame(subset(csvfile(), select = input$selvar))
            cormat1 <- cor(x, method = input$req2, use = "complete.obs")
            corrplot::corrplot(cormat1,
              method = input$shape,
              type = input$layout, tl.col = "#000000",
              col = brewer.pal(n = 8, name = input$style),
              addCoef.col = input$txcol, number.cex = input$cex,
              tl.cex = input$tlcex


            )

            if (input$significance > 0) {
              validate(
                need(input$selvar, "No input selected, supply me some variables to draw!")
              )
              x <- as.data.frame(subset(csvfile(), select = input$selvar))
              cormat1 <- cor(x, method = input$req2, use = "complete.obs")
              res1 <- corrplot::cor.mtest(x)
              corrplot::corrplot(cormat1,
                method = input$shape,
                type = input$layout, tl.col = "#000000",
                col = brewer.pal(n = 8, name = input$style),
                addCoef.col = input$txcol,tl.cex = input$tlcex,
                p.mat = res1$p, sig.level = as.numeric(input$sig)
              )
            }
            if (input$remove_corr > 0) {
              validate(
                need(input$selvar, "No input selected, supply me some variables to draw!")
              )
              x <- as.data.frame(subset(csvfile(), select = input$selvar))
              cormat1 <- cor(x, method = input$req2, use = "complete.obs")
              res1 <- corrplot::cor.mtest(x)
              corrplot::corrplot(cormat1,
                method = input$shape,
                type = input$layout, tl.col = "#000000",
                tl.cex = input$tlcex,
                col = brewer.pal(n = 8, name = input$style)
              )
            }

            if (input$remove_corr > 0 && input$significance > 0) {
              validate(
                need(input$selvar, "No input selected, supply me some variables to draw!")
              )
              x <- as.data.frame(subset(csvfile(), select = input$selvar))
              cormat1 <- cor(x, method = input$req2, use = "complete.obs")
              res1 <- corrplot::cor.mtest(x)
              corrplot::corrplot(cormat1,
                method = input$shape,
                type = input$layout, tl.col = "#000000",
                col = brewer.pal(n = 8, name = input$style),
                p.mat = res1$p, sig.level = as.numeric(input$sig),
                tl.cex = input$tlcex
              )
            }
          }
        },
        bg = "transparent"
      )
      plotOutput("corrplot")
    }
  })
  ##################



  ####################### download Report
  output$var1 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (input$req1 == "correlation") {
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
    }
    else if (input$req1 == "corrmat") {
      if (is.null(input$submit2)) {
        return()
      }
      if (input$submit2 > 0) {
        list(
          radioButtons("format", "Download report (Note: if you are changing the file name after download give '.html' extension):", c("HTML"),
            inline = TRUE
          ),
          downloadButton("downloadReport")
        )
      }
    }
  })


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

  #############################################
  ############################### this note appear on opening
  output$note1 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return(
        HTML(paste0(" <h4> To perform analysis using your own dataset, prepare excel file in csv format by reading instruction below  </h4>
<p>
<ui>
<li>Open a new blank excel file</li>
<li>Copy and paste observations into a new sheet (use only one sheet) of a new excel file</li>
<li>Observations should be pasted as columns </li>
<li>Don't type or delete anything on other cells without data</li>
<li>You can use any names for your columns. No space is allowed in the Column name. If space is required use underscore ‘_’ or ‘.’ full stop; for example ‘Variable name’ should be written as Variable_name or Variable.name</li>
<li>Data should be arranged towards upper left corner and row above the data should not be left blank </li>
<li>Type 'NA' in the cell with no observation</li>
<li>Don't type and delete anything on other cells without data. If so select those cells, right click and click clear contents </li>
<li>Give names to all column, Don't add any unnecessary columns that is not required for analysis</li>
<li>Once all these are done, your file is ready. Now save it as CSV file. </li>
<li><b>Upload file by clicking browse in the app </b></li>
</ui>
</p>
<h5> You can download a model data set from below and test the App  </h5>
"))
      )
    }

    else {
      return()
    }
  })

  ########################################## dataset download
  output$data_set <- renderUI({
    if (is.null(input$file1$datapath)) {
      list(
        selectInput("dataset", "Choose a dataset:",
          choices = c("iris", "rock", "cars")
        ),
        downloadButton("downloadData", label = "Download csv file to test", class = "butt1")
      )
    }

    else {
      return()
    }
  })
  datasetInput <- reactive({
    switch(input$dataset,
      "iris" = iris,
      "rock" = rock,
      "cars" = cars
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
  #######################################################

  #################################### Download Image
  output$image_down <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }

    if (input$req1 == "scatplot") {
      if (is.null(input$submit1)) {
        return()
      }
      if (input$submit1 > 0) {
        list(downloadButton("downloadImage1", label = "Download ScatterPlot", class = "butt1"))
      }
    }
    else if (input$req1 == "corrplot") {
      if (is.null(input$submit3)) {
        return()
      }
      if (input$submit3 > 0) {
        list(downloadButton("downloadImage2", label = "Download Correlogram", class = "butt1"))
      }
    }
  })
  ### plotting
  plotInput <- reactive({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (input$req1 == "scatplot") {
      if (is.null(input$submit1)) {
        return()
      }
      if (input$submit1 > 0) {
        x <- as.vector(csvfile()[, input$dvar])
        y <- as.vector(csvfile()[, input$ivar])
        plot(x, y,
          main = input$main,
          xlab = input$xlab, ylab = input$ylab,
          pch = 19, col = input$color, frame = FALSE
        )
      }
    }
    else if (input$req1 == "corrplot") {
      if (is.null(input$submit3)) {
        return()
      }
      if (input$submit3 > 0) {
        x <- as.data.frame(subset(csvfile(), select = input$selvar))
        cormat1 <- cor(x, method = input$req2, use = "complete.obs")
        corrplot::corrplot(cormat1,
          method = input$shape,
          type = input$layout, tl.col = "#000000",
          col = brewer.pal(n = 8, name = input$style),
          addCoef.col = input$txcol, number.cex = input$cex,
          tl.cex = input$tlcex
        )
        if (input$significance > 0) {
          x <- as.data.frame(subset(csvfile(), select = input$selvar))
          cormat1 <- cor(x, method = input$req2, use = "complete.obs")
          res1 <- corrplot::cor.mtest(x)
          corrplot::corrplot(cormat1,
            method = input$shape,
            type = input$layout, tl.col = "#000000",
            col = brewer.pal(n = 8, name = input$style), addCoef.col = input$txcol,
            p.mat = res1$p, sig.level = as.numeric(input$sig),tl.cex = input$tlcex
          )
        }
        if (input$remove_corr > 0) {
          x <- as.data.frame(subset(csvfile(), select = input$selvar))
          cormat1 <- cor(x, method = input$req2, use = "complete.obs")
          res1 <- corrplot::cor.mtest(x)
          corrplot::corrplot(cormat1,
            method = input$shape,
            type = input$layout, tl.col = "#000000",
            col = brewer.pal(n = 8, name = input$style),
            tl.cex = input$tlcex
          )
        }

        if (input$remove_corr > 0 && input$significance > 0) {
          x <- as.data.frame(subset(csvfile(), select = input$selvar))
          cormat1 <- cor(x, method = input$req2, use = "complete.obs")
          res1 <- corrplot::cor.mtest(x)
          corrplot::corrplot(cormat1,
            method = input$shape,
            type = input$layout, tl.col = "#000000",
            col = brewer.pal(n = 8, name = input$style),
            p.mat = res1$p, sig.level = as.numeric(input$sig),
            tl.cex = input$tlcex
          )
        }

        grid.echo()
        P1 <- grid.grab()

        grid.draw(P1)
      }
    }
  })



  ###
  output$downloadImage1 <- downloadHandler(
    filename = "scatter.png",
    content = function(file) {
      png(file,
        width = input$shiny_width,
        height = input$shiny_height,
        res = 150
      )
      plotInput()
      dev.off()
    }
  )

  output$downloadImage2 <- downloadHandler(
    filename = "corr.png",
    content = function(file) {
      png(file,
        width = input$shiny_width,
        height = input$shiny_height,
        res = 150
      )
      plotInput()
      dev.off()
    }
  )
  ###########################
}
shinyApp(ui = ui, server = server)
