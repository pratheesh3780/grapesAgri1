library(shiny)
library(shinyWidgets)
library(datasets)
library(rmarkdown)
library(dplyr)
library(ggplot2)
library(pastecs)
library(ggpubr)
library(PairedData)
library(reshape2)
############################### Ui
ui <- fluidPage(
  setBackgroundColor(
    color = c("#ffb9b3", "#ffffff"),
    gradient = "radial",
    direction = c("bottom", "right")
  ),
  titlePanel(title = tags$div(
    tags$b("Compare means: Small sample tests", style = "color:#000000")
  ), windowTitle = "Compare means"),
  sidebarPanel(
    fileInput(
      "file1",
      "CSV File (upload in csv format)",
      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
    ),
    checkboxInput("header", "Header", TRUE),
    selectInput(
      "req1",
      "Please select the analysis type",
      c(
        "Two sample t-test" = "ttest",
        "Paired t-test" = "pttest",
        "One sample t-test" = "ottest",
        "Welch two sample t-test" = "wttest",
        "Test Homogenity of variance" = "ftest",
        "Box Plot" = "boxplot",
        "Paired plot" = "pplot"
      ),
      "ttest"
    ),
    uiOutput("var"),
    tags$br(),
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
    uiOutput("start_note"),
    tableOutput("summary1"),
    # summary
    tags$style(
      type = "text/css",
      "#summary1 th,td {border: medium solid #000000;text-align:center}"
    ),
    tags$style(
      type = "text/css",
      "#summary1 td {border: medium solid #000000;text-align:center}"
    ),
    tableOutput("ttest"),
    # two sample ttest
    tags$style(
      type = "text/css",
      "#ttest th,td {border: medium solid #000000;text-align:center}"
    ),
    tags$style(
      type = "text/css",
      "#ttest td {border: medium solid #000000;text-align:center}"
    ),
    htmlOutput("note1"),
    tableOutput("summary2"),
    # summary
    tags$style(
      type = "text/css",
      "#summary2 th,td {border: medium solid #000000;text-align:center}"
    ),
    tags$style(
      type = "text/css",
      "#summary2 td {border: medium solid #000000;text-align:center}"
    ),
    tableOutput("wttest"),
    # welch sample ttest
    tags$style(
      type = "text/css",
      "#wttest th,td {border: medium solid #000000;text-align:center}"
    ),
    tags$style(
      type = "text/css",
      "#wttest td {border: medium solid #000000;text-align:center}"
    ),
    htmlOutput("note2"),
    tableOutput("summary3"),
    # summary
    tags$style(
      type = "text/css",
      "#summary3 th,td {border: medium solid #000000;text-align:center}"
    ),
    tags$style(
      type = "text/css",
      "#summary3 td {border: medium solid #000000;text-align:center}"
    ),
    tableOutput("ottest"),
    # one sample ttest
    tags$style(
      type = "text/css",
      "#ottest th,td {border: medium solid #000000;text-align:center}"
    ),
    tags$style(
      type = "text/css",
      "#ottest td {border: medium solid #000000;text-align:center}"
    ),
    htmlOutput("note3"),
    tableOutput("summary4"),
    # summary
    tags$style(
      type = "text/css",
      "#summary4 th,td {border: medium solid #000000;text-align:center}"
    ),
    tags$style(
      type = "text/css",
      "#summary4 td {border: medium solid #000000;text-align:center}"
    ),
    tableOutput("ftest"),
    # Homogenity of variance
    tags$style(
      type = "text/css",
      "#ftest th,td {border: medium solid #000000;text-align:center}"
    ),
    tags$style(
      type = "text/css",
      "#ftest td {border: medium solid #000000;text-align:center}"
    ),
    htmlOutput("note4"),
    tableOutput("summary5"),
    # summary
    tags$style(
      type = "text/css",
      "#summary5 th,td {border: medium solid #000000;text-align:center}"
    ),
    tags$style(
      type = "text/css",
      "#summary5 td {border: medium solid #000000;text-align:center}"
    ),
    tableOutput("pttest"),
    # Homogenity of variance
    tags$style(
      type = "text/css",
      "#pttest th,td {border: medium solid #000000;text-align:center}"
    ),
    tags$style(
      type = "text/css",
      "#pttest td {border: medium solid #000000;text-align:center}"
    ),
    htmlOutput("note5"),
    uiOutput("plot"),
    tags$br(),
    uiOutput("slider"),
    tags$br(),
    uiOutput("image_down"),
    # for image download
    uiOutput("var1"),
    uiOutput("var2"),
    uiOutput("var3"),
    uiOutput("var4"),
    uiOutput("var5"),
    uiOutput("data_set"),
    # for data set download
    tags$br(),
    tags$br(),
    tags$br()
  )
)
########################### Server
server <- function(input, output, session) {
  csvfile <- reactive({
    csvfile <- input$file1
    if (is.null(csvfile)) {
      return(NULL)
    }
    dt <-
      read.csv(csvfile$datapath,
        header = input$header,
        sep = ",", check.names = FALSE
      )
    dt
  })

  output$var <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (input$req1 == "ttest") {
      list(
        radioButtons("dvar", "Please pick 'Group A' ", choices = names(csvfile())),
        radioButtons("ivar", "Please pick 'Group B'", choices = names(csvfile())),
        selectInput(
          "alt",
          "Alternative Hypotesis",
          c(
            "Two sided" = "two.sided",
            "Less than" = "less",
            "Greater than" = "greater"
          ),
          "two.sided"
        ),
        actionBttn(
          inputId = "submit1",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
    else if (input$req1 == "ottest") {
      list(
        radioButtons("dvar", "Please pick 'Group' ",
          choices = names(csvfile())
        ),
        numericInput("mu", "Enter population mean to compare:", value = 0),
        selectInput(
          "alt",
          "Alternative Hypotesis",
          c(
            "Two sided" = "two.sided",
            "Less than" = "less",
            "Greater than" = "greater"
          ),
          "two.sided"
        ),
        actionBttn(
          inputId = "submit3",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
    else if (input$req1 == "ftest") {
      list(
        radioButtons("dvar", "Please pick 'Group A' ", choices = names(csvfile())),
        radioButtons("ivar", "Please pick 'Group B'", choices = names(csvfile())),
        selectInput(
          "alt",
          "Alternative Hypotesis",
          c(
            "Two sided" = "two.sided",
            "Less than" = "less",
            "Greater than" = "greater"
          ),
          "two.sided"
        ),
        actionBttn(
          inputId = "submit4",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
    else if (input$req1 == "pttest") {
      list(
        radioButtons("dvar", "Please pick 'Group A' ", choices = names(csvfile())),
        radioButtons("ivar", "Please pick 'Group B'", choices = names(csvfile()), selected = NULL),
        selectInput(
          "alt",
          "Alternative Hypotesis",
          c(
            "Two sided" = "two.sided",
            "Less than" = "less",
            "Greater than" = "greater"
          ),
          "two.sided"
        ),
        actionBttn(
          inputId = "submit5",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
    else if (input$req1 == "wttest") {
      list(
        radioButtons("dvar", "Please pick 'Group A' ", choices = names(csvfile())),
        radioButtons("ivar", "Please pick 'Group B'", choices = names(csvfile())),
        selectInput(
          "alt",
          "Alternative Hypotesis",
          c(
            "Two sided" = "two.sided",
            "Less than" = "less",
            "Greater than" = "greater"
          ),
          "two.sided"
        ),
        actionBttn(
          inputId = "submit2",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
    else if (input$req1 == "boxplot") {
      list(
        radioButtons("dvar", "Please pick 'Group A' ", choices = names(csvfile())),
        radioButtons("ivar", "Please pick 'Group B'", choices = names(csvfile())),
        textInput("xlab", "Enter required x-axis label", "X-axis"),
        textInput("ylab", "Enter required y-axis label", "Y-axis"),
        selectInput(
          "col1",
          "Colour of first box",
          c(
            "green" = "#00AFBB",
            "blue" = "#3bbeff",
            "red" = "#bf0003",
            "yellow" = "#E7B800"
          ),
          "#00AFBB"
        ),
        selectInput(
          "col2",
          "Colour of Second box",
          c(
            "green" = "#00AFBB",
            "blue" = "#3bbeff",
            "red" = "#bf0003",
            "yellow" = "#E7B800"
          ),
          "#E7B800"
        ),
        actionBttn(
          inputId = "submit6",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
    else if (input$req1 == "pplot") {
      list(
        radioButtons("dvar", "Please pick 'Group A' ", choices = names(csvfile())),
        radioButtons("ivar", "Please pick 'Group B'", choices = names(csvfile())),
        actionBttn(
          inputId = "submit7",
          label = "SUBMIT!",
          color = "danger",
          style = "jelly"
        )
      )
    }
  })
  # two sample ttest
  output$ttest <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$req1)) {
        return()
      }
      if (is.null(input$submit1)) {
        return()
      }
      if (input$req1 == "ttest") {
        if (input$submit1 > 0) {
          x <- as.vector(csvfile()[, input$dvar])
          y <- as.vector(csvfile()[, input$ivar])
          t <- t.test(
            x,
            y,
            alternative = input$alt,
            var.equal = TRUE,
            paired = FALSE,
            na.omit = TRUE
          )
          t_value <- round(t$statistic, 3)
          df <- t$parameter
          Pvalue <- round(t$p.value, 6)
          alt.Hypothesis <- t$alternative
          result <- cbind(t_value, df, Pvalue, alt.Hypothesis)
          result <- as.data.frame(result)
          result
        }
      }
    },
    rownames = FALSE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Two sample unpaired t-test </b>"),
    caption.placement = getOption("xtable.caption.placement", "top")
  )

  # note1
  output$note1 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (is.null(input$submit1)) {
      return()
    }
    if (input$req1 == "ttest") {
      if (input$submit1 > 0) {
        x <- as.vector(csvfile()[, input$dvar])
        y <- as.vector(csvfile()[, input$ivar])
        t <- t.test(
          x,
          y,
          alternative = input$alt,
          var.equal = TRUE,
          paired = FALSE,
          na.omit = TRUE
        )

        if (t$p.value <= 0.05) {
          HTML(
            paste0(
              "Since P-value is < 0.05 we reject the null hypothesis at 5% level of significance
                    (here null hypothesis is: Population mean of group A = Population mean of group B) "
            )
          )
        }
        else if (t$p.value > 0.05) {
          HTML(
            paste0(
              "Since P-value is > 0.05 we don't have enough evidence to
                     reject the null hypothesis at 5% level of significance
                     (here null hypothesis is: Population mean of group A = Population mean of group B)"
            )
          )
        }
      }
    }
  })
  output$summary1 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$req1)) {
        return()
      }
      if (is.null(input$submit1)) {
        return()
      }
      if (input$req1 == "ttest") {
        if (input$submit1 > 0) {
          grp1 <- subset(csvfile(), select = input$dvar)
          grp2 <- subset(csvfile(), select = input$ivar)
          final <- cbind(grp1, grp2)
          res <- pastecs::stat.desc(final)
          result <- as.data.frame(res)
          rownames(result)[rownames(result) == "nbr.val"] <-
            "Number of Obs."
          rownames(result)[rownames(result) == "nbr.null"] <-
            "null values"
          rownames(result)[rownames(result) == "nbr.na"] <- "NA"
          round(result, 3)
        }
      }
    },
    digits = 3,
    caption = ("<b> Summary Statistics </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE
  )
  # Weltch ttest
  output$wttest <- renderTable(
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
      if (input$req1 == "wttest") {
        if (input$submit2 > 0) {
          x <- as.vector(csvfile()[, input$dvar])
          y <- as.vector(csvfile()[, input$ivar])
          t <- t.test(
            x,
            y,
            alternative = input$alt,
            var.equal = FALSE,
            paired = FALSE,
            na.omit = TRUE
          )
          t_value <- round(t$statistic, 3)
          df <- round(t$parameter, 2)
          Pvalue <- round(t$p.value, 6)
          alt.Hypothesis <- t$alternative
          result <- cbind(t_value, df, Pvalue, alt.Hypothesis)
          result <- as.data.frame(result)
          result
        }
      }
    },
    rownames = FALSE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>Two sample unpaired t-test </b>"),
    caption.placement = getOption("xtable.caption.placement", "top")
  )
  # note2
  output$note2 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (is.null(input$submit2)) {
      return()
    }
    if (input$req1 == "wttest") {
      if (input$submit2 > 0) {
        x <- as.vector(csvfile()[, input$dvar])
        y <- as.vector(csvfile()[, input$ivar])
        t <- t.test(
          x,
          y,
          alternative = input$alt,
          var.equal = FALSE,
          paired = FALSE,
          na.omit = TRUE
        )

        if (t$p.value <= 0.05) {
          HTML(
            paste0(
              "Since P-value is < 0.05 we reject the null hypothesis at 5% level of significance
                    (here null hypothesis is: Population mean of group A = Population mean of group B) "
            )
          )
        }
        else if (t$p.value > 0.05) {
          HTML(
            paste0(
              "Since P-value is > 0.05 we don't have enough evidence to
                     reject the null hypothesis at 5% level of significance
                     (here null hypothesis is: Population mean of group A = Population mean of group B)"
            )
          )
        }
      }
    }
  })
  output$summary2 <- renderTable(
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
      if (input$req1 == "wttest") {
        if (input$submit2 > 0) {
          grp1 <- subset(csvfile(), select = input$dvar)
          grp2 <- subset(csvfile(), select = input$ivar)
          final <- cbind(grp1, grp2)
          res <- stat.desc(final)
          result <- as.data.frame(res)
          rownames(result)[rownames(result) == "nbr.val"] <-
            "Number of Obs."
          rownames(result)[rownames(result) == "nbr.null"] <-
            "null values"
          rownames(result)[rownames(result) == "nbr.na"] <- "NA"
          round(result, 3)
        }
      }
    },
    digits = 3,
    caption = ("<b> Summary Statistics </b>"),
    bordered = TRUE,
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE
  )
  # one sample ttest
  output$ottest <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$req1)) {
        return()
      }
      if (is.null(input$submit3)) {
        return()
      }
      if (input$req1 == "ottest") {
        if (input$submit3 > 0) {
          x <- as.vector(csvfile()[, input$dvar])
          t <- t.test(
            x,
            y = NULL,
            alternative = input$alt,
            mu = input$mu,
            na.omit = TRUE
          )
          t_value <- round(t$statistic, 3)
          df <- round(t$parameter, 2)
          Pvalue <- round(t$p.value, 6)
          alt.Hypothesis <- t$alternative
          result <- cbind(t_value, df, Pvalue, alt.Hypothesis)
          result <- as.data.frame(result)
          result
        }
      }
    },
    rownames = FALSE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>One sample t-test </b>"),
    caption.placement = getOption("xtable.caption.placement", "top")
  )

  # note3
  output$note3 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (is.null(input$submit3)) {
      return()
    }
    if (input$req1 == "ottest") {
      if (input$submit3 > 0) {
        x <- as.vector(csvfile()[, input$dvar])
        t <- t.test(
          x,
          y = NULL,
          alternative = input$alt,
          mu = input$mu,
          na.omit = TRUE
        )
        mu <- as.numeric(input$mu)

        if (t$p.value <= 0.05) {
          HTML(paste0(
            sprintf(
              "Since P-value is < 0.05 we can reject the null hypothesis at 5 percent level of significance.
                    (Here null hypothesis: Population mean= %s)",
              mu
            )
          ))
        }
        else if (t$p.value > 0.05) {
          HTML(paste0(
            sprintf(
              "Since P-value is > 0.05 we don't have enough evidence to reject null hypothesis at 5 percent level of significance.
                    (Here null hypothesis: Population mean= %s)",
              mu
            )
          ))
        }
      }
    }
  })

  output$summary3 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$req1)) {
        return()
      }
      if (is.null(input$submit3)) {
        return()
      }
      if (input$req1 == "ottest") {
        if (input$submit3 > 0) {
          grp1 <- subset(csvfile(), select = input$dvar)
          final <- grp1
          res <- stat.desc(final)
          result <- as.data.frame(res)
          rownames(result)[rownames(result) == "nbr.val"] <-
            "Number of Obs."
          rownames(result)[rownames(result) == "nbr.null"] <-
            "null values"
          rownames(result)[rownames(result) == "nbr.na"] <- "NA"
          round(result, 3)
        }
      }
    },
    digits = 3,
    caption = ("<b> Summary Statistics </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE
  )
  # Ftest homogenity
  output$ftest <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$req1)) {
        return()
      }
      if (is.null(input$submit4)) {
        return()
      }
      if (input$req1 == "ftest") {
        if (input$submit4 > 0) {
          x <- as.vector(csvfile()[, input$dvar])
          y <- as.vector(csvfile()[, input$ivar])
          f <- var.test(x, y,
            ratio = 1,
            alternative = input$alt
          )
          F_value <- round(f$statistic, 3)
          df <- as.data.frame(f$parameter)
          Pvalue <- round(f$p.value, 6)
          alt.Hypothesis <- f$alternative
          result <- cbind(F_value, t(df), Pvalue, alt.Hypothesis)
          result <- as.data.frame(result)
          result
        }
      }
    },
    rownames = FALSE,
    bordered = TRUE,
    align = "c",
    caption = ("<b>F test for Homogenity of Variance </b>"),
    caption.placement = getOption("xtable.caption.placement", "top")
  )

  # note4
  output$note4 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (is.null(input$submit4)) {
      return()
    }
    if (input$req1 == "ftest") {
      if (input$submit4 > 0) {
        x <- as.vector(csvfile()[, input$dvar])
        y <- as.vector(csvfile()[, input$ivar])
        f <- var.test(x, y,
          ratio = 1,
          alternative = input$alt
        )

        if (f$p.value <= 0.05) {
          HTML(
            paste0(
              "Since P-value is < 0.05 we can reject the null hypothesis at 5 percent level of significance.
                    (Here null hypothesis: Variances are Homogenous)"
            )
          )
        }
        else if (f$p.value > 0.05) {
          HTML(
            paste0(
              "Since P-value is > 0.05 we don't have enough evidence to reject null hypothesis at 5 percent level of significance. Variance is homogenous.
                    (Here null hypothesis: Variances are Homogenous)"
            )
          )
        }
      }
    }
  })
  output$summary4 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$req1)) {
        return()
      }
      if (is.null(input$submit4)) {
        return()
      }
      if (input$req1 == "ftest") {
        if (input$submit4 > 0) {
          grp1 <- subset(csvfile(), select = input$dvar)
          grp2 <- subset(csvfile(), select = input$ivar)
          final <- cbind(grp1, grp2)
          res <- pastecs::stat.desc(final)
          result <- as.data.frame(res)
          rownames(result)[rownames(result) == "nbr.val"] <-
            "Number of Obs."
          rownames(result)[rownames(result) == "nbr.null"] <-
            "null values"
          rownames(result)[rownames(result) == "nbr.na"] <- "NA"
          round(result, 3)
        }
      }
    },
    digits = 3,
    caption = ("<b> Summary Statistics </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE
  )
  # Paired t test
  output$pttest <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$req1)) {
        return()
      }
      if (is.null(input$submit5)) {
        return()
      }
      if (input$req1 == "pttest") {
        if (input$submit5 > 0) {
          grp1 <- subset(csvfile(), select = input$dvar)
          grp2 <- subset(csvfile(), select = input$ivar)
          final <- cbind(grp1, grp2)
          ttest <-
            t.test(final[, 1],
              final[, 2],
              paired = TRUE,
              alternative = input$alt
            )
          tvalue <- round(ttest$statistic, 3)
          df <- ttest$parameter
          pvalue <- round(ttest$p.value, 6)
          meandiff <- round(ttest$estimate, 3)
          result1 <- cbind(tvalue, df, pvalue, meandiff)
          row.names(result1) <- NULL
          result1
        }
      }
    },
    caption = ("<b> Paired ttest </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
  )

  output$note5 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (is.null(input$submit5)) {
      return()
    }
    if (input$req1 == "pttest") {
      if (input$submit5 > 0) {
        validate(
          need(input$dvar != input$ivar, "Both input variables selected (Group A and Group B) are same. Choose different input variables for meaningful result")
        )
        x <- as.vector(csvfile()[, input$dvar])
        y <- as.vector(csvfile()[, input$ivar])
        t <- t.test(
          x,
          y,
          alternative = input$alt,
          var.equal = FALSE,
          paired = TRUE,
          na.omit = TRUE
        )

        if (t$p.value <= 0.05) {
          HTML(
            paste0(
              "Since P-value is < 0.05 we reject the null hypothesis at 5% level of significance
                    (here null hypothesis is: Population mean of group A = Population mean of group B) "
            )
          )
        }
        else if (t$p.value > 0.05) {
          HTML(
            paste0(
              "Since P-value is > 0.05 we don't have enough evidence to
                     reject the null hypothesis at 5% level of significance
                     (here null hypothesis is: Population mean of group A = Population mean of group B)"
            )
          )
        }
      }
    }
  })
  output$summary5 <- renderTable(
    {
      if (is.null(input$file1$datapath)) {
        return()
      }
      if (is.null(input$req1)) {
        return()
      }
      if (is.null(input$submit5)) {
        return()
      }
      if (input$req1 == "pttest") {
        if (input$submit5 > 0) {
          grp1 <- subset(csvfile(), select = input$dvar)
          grp2 <- subset(csvfile(), select = input$ivar)
          final <- cbind(grp1, grp2)
          res <- pastecs::stat.desc(final)
          result <- as.data.frame(res)
          rownames(result)[rownames(result) == "nbr.val"] <-
            "Number of Obs."
          rownames(result)[rownames(result) == "nbr.null"] <-
            "null values"
          rownames(result)[rownames(result) == "nbr.na"] <- "NA"
          result <- round(result, 3)
          result
        }
      }
    },
    digits = 3,
    caption = ("<b> Summary Statistics </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE
  )
  # Plots
  output$plot <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }

    if (input$req1 == "boxplot") {
      if (is.null(input$submit6)) {
        return()
      }
      output$boxplot <- renderPlot(
        {
          if (input$submit6 > 0) {
            x <- as.matrix(csvfile()[, input$dvar])
            y <- as.matrix(csvfile()[, input$ivar])
            g_1 <- rep(input$dvar, each = nrow(x))
            g_2 <- rep(input$ivar, each = nrow(y))
            my_data <- data.frame(
              group = c(g_1, g_2),
              obs = c(x, y)
            )

            colnames(my_data) <- c("group", "obs")
            ggpubr::ggboxplot(
              my_data,
              x = "group",
              y = "obs",
              color = "group",
              palette = c(input$col1, input$col2),
              ylab = input$ylab,
              xlab = input$xlab,
              size = input$size
            )
          }
        },
        bg = "transparent"
      )
      plotOutput("boxplot")
    }

    else if (input$req1 == "pplot") {
      if (is.null(input$submit7)) {
        return()
      }
      output$pplot <- renderPlot(
        {
          if (input$submit7 > 0) {
            group1 <- subset(csvfile(), select = input$dvar)
            group2 <- subset(csvfile(), select = input$ivar)
            pd <- paired(group1, group2)
            plot(pd, type = "profile") + theme_bw()
          }
        },
        bg = "transparent"
      )
      plotOutput("pplot")
    }
  })

  # slider input for boxplot
  output$slider <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (is.null(input$submit6)) {
      return()
    }
    if (input$req1 == "boxplot") {
      if (input$submit6 > 0) {
        list(sliderInput(
          "size",
          "Required width of the line:",
          min = 0.5,
          max = 2,
          value = 0.5
        ))
      }
    }
  })
  # Download Image
  output$image_down <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }

    if (input$req1 == "boxplot") {
      if (is.null(input$submit6)) {
        return()
      }
      if (input$submit6 > 0) {
        list(downloadButton("downloadImage1", label = "Download Boxplot", class = "butt1"))
      }
    }
    else if (input$req1 == "pplot") {
      if (is.null(input$submit7)) {
        return()
      }
      if (input$submit7 > 0) {
        list(
          downloadButton("downloadImage2", label = "Download Paired Plot", class = "butt1")
        )
      }
    }
  })
  # plotting
  plotInput <- reactive({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }

    if (input$req1 == "boxplot") {
      if (is.null(input$submit6)) {
        return()
      }
      if (input$submit6 > 0) {
        x <- as.matrix(csvfile()[, input$dvar])
        y <- as.matrix(csvfile()[, input$ivar])
        g_1 <- rep(input$dvar, each = nrow(x))
        g_2 <- rep(input$ivar, each = nrow(y))
        my_data <- data.frame(
          group = c(g_1, g_2),
          obs = c(x, y)
        )

        colnames(my_data) <- c("group", "obs")
        ggboxplot(
          my_data,
          x = "group",
          y = "obs",
          color = "group",
          palette = c(input$col1, input$col2),
          ylab = input$ylab,
          xlab = input$xlab,
          size = input$size
        )
      }
    }

    else if (input$req1 == "pplot") {
      if (is.null(input$submit7)) {
        return()
      }
      if (input$submit7 > 0) {
        group1 <- subset(csvfile(), select = input$dvar)
        group2 <- subset(csvfile(), select = input$ivar)
        pd <- paired(group1, group2)
        plot(pd, type = "profile") + theme_bw()
      }
    }
  })



  ###
  output$downloadImage1 <- downloadHandler(
    filename = "boxplot.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(
          ...,
          width = width,
          height = height,
          res = 500,
          units = "in"
        )
      }
      ggsave(file, plot = plotInput(), device = device)
    }
  )

  output$downloadImage2 <- downloadHandler(
    filename = "paired.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(
          ...,
          width = width,
          height = height,
          res = 500,
          units = "in"
        )
      }
      ggsave(file, plot = plotInput(), device = device)
    }
  )



  # download Report
  output$var1 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (is.null(input$submit1)) {
      return()
    }
    if (input$req1 == "ttest") {
      if (input$submit1 > 0) {
        list(
          radioButtons(
            "format",
            "Download report (Note: if you are changing the file name after download give '.html' extension):",
            c("HTML"),
            inline = TRUE
          ),
          downloadButton("downloadReport")
        )
      }
    }
  })

  output$var2 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (is.null(input$submit2)) {
      return()
    }
    if (input$req1 == "wttest") {
      if (input$submit2 > 0) {
        list(
          radioButtons(
            "format",
            "Download report (Note: if you are changing the file name after download give '.html' extension):",
            c("HTML"),
            inline = TRUE
          ),
          downloadButton("downloadReport")
        )
      }
    }
  })

  output$var3 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (is.null(input$submit3)) {
      return()
    }
    if (input$req1 == "ottest") {
      if (input$submit3 > 0) {
        list(
          radioButtons(
            "format",
            "Download report (Note: if you are changing the file name after download give '.html' extension):",
            c("HTML"),
            inline = TRUE
          ),
          downloadButton("downloadReport")
        )
      }
    }
  })

  output$var4 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (is.null(input$submit4)) {
      return()
    }
    if (input$req1 == "ftest") {
      if (input$submit4 > 0) {
        list(
          radioButtons(
            "format",
            "Download report (Note: if you are changing the file name after download give '.html' extension):",
            c("HTML"),
            inline = TRUE
          ),
          downloadButton("downloadReport")
        )
      }
    }
  })

  output$var5 <- renderUI({
    if (is.null(input$file1$datapath)) {
      return()
    }
    if (is.null(input$req1)) {
      return()
    }
    if (is.null(input$submit5)) {
      return()
    }
    if (input$req1 == "pttest") {
      if (input$submit5 > 0) {
        list(
          radioButtons(
            "format",
            "Download report (Note: if you are changing the file name after download give '.html' extension):",
            c("HTML"),
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

  # this note appear on opening
  output$start_note <- renderUI({
    if (is.null(input$file1$datapath)) {
      return(HTML(
        paste0(
          " <h4> To perform analysis using your own dataset, prepare excel file in csv format by reading instruction below  </h4>
<p>
<ui>
<li>Open a new blank excel file</li>
<li>Copy and paste observations into a new sheet (use only one sheet) of a new excel file</li>
<li>Groups to be compared should be pasted as columns </li>
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
<p>
<b> Things to remember while doing t-test</b>
<li>Note that, unpaired two-samples t-test can be used only under certain conditions: </li>
<li>First condition: each of the two groups of samples being compared, should be normally distributed. This can be checked using Shapiro-Wilk test and other data visualization techniques [use grapes::descApp()]</li>
<li>Second condition: Variances of the two groups should be equal.
This can be checked using Homogenity of variance test (F test) available
with in the app. </li>
<li>If the variances of the two groups being compared are different (heteroscedasticity), it’s possible to use the Welch t test, an adaptation of Student t-test. </li>
<li>Welch t test and F-test are available within the app</li>
</p>
<b> You can download a model data set from below and test the App  </b>
"
        )
      ))
    }

    else {
      return()
    }
  })


  ########################################## dataset download
  output$data_set <- renderUI({
    if (is.null(input$file1$datapath)) {
      list(
        selectInput(
          "dataset",
          "Choose a dataset:",
          choices = c("iris", "rock", "pressure")
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
      "pressure" = pressure
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
}
shinyApp(ui = ui, server = server)
