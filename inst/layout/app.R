library(shiny)
library(shinyWidgets)
library(rmarkdown)
library(agricolae)
library(dplyr)
library(magrittr)
library(desplot)
library(knitr)

############################## Ui
ui <- fluidPage(
  setBackgroundColor(
    color = c("#fff291", "#ffffff"),
    gradient = "radial",
    direction = c("bottom", "right")
  ),
  titlePanel(
    title = tags$div(tags$b("Field Layout of experiments"), style = "color:#000000"),
    windowTitle = "Field Layout"
  ),
  sidebarPanel(
    selectInput(
      "design", tags$div(tags$b("Select the design for your experiment"),
        style = "color:#d17b02"
      ),
      c(
        "CRD" = "crd",
        "RBD" = "rbd",
        "Split Plot" = "split",
        "Augmented RCBD" = "aug",
        "Strip Plot" = "strip"
      ),
      "crd"
    ),
    h5("Note: consult a statistician before selecting proper design for your experiment"),

    ##
    uiOutput("var_crd"),
    uiOutput("var_rbd"),
    uiOutput("var_aug"),
    uiOutput("var_split"),
    uiOutput("var_strip"),
    tags$br(),
    htmlOutput("text_crd1"),
    htmlOutput("text_rbd1"),
    htmlOutput("text_aug1"),
    htmlOutput("text_split1"),
    htmlOutput("text_strip1"),
    uiOutput("var_crd1"),
    uiOutput("var_rbd1"),
    uiOutput("var_aug1"),
    uiOutput("var_split1"),
    uiOutput("var_strip1"),
    tags$br(),
    h5(" If the image is not clear you can generate a table format by clicking below"),
    uiOutput("tab_crd"),
    uiOutput("tab_rbd"),
    uiOutput("tab_aug"),
    uiOutput("tab_split"),
    uiOutput("tab_strip"),

    ##




    ##
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
    tableOutput("details_crd"),
    tags$style(type = "text/css", "#details_crd th,td {border: medium solid #000000;text-align:center}"),
    tags$style(type = "text/css", "#details_crd td {border: medium solid #000000;text-align:center}"),
    tableOutput("details_rbd"),
    tags$style(type = "text/css", "#details_rbd th,td {border: medium solid #000000;text-align:center}"),
    tags$style(type = "text/css", "#details_rbd td {border: medium solid #000000;text-align:center}"),
    tableOutput("details_aug"),
    tags$style(type = "text/css", "#details_aug th,td {border: medium solid #000000;text-align:center}"),
    tags$style(type = "text/css", "#details_aug td {border: medium solid #000000;text-align:center}"),
    tableOutput("details_split"),
    tags$style(type = "text/css", "#details_split th,td {border: medium solid #000000;text-align:center}"),
    tags$style(type = "text/css", "#details_split td {border: medium solid #000000;text-align:center}"),
    tableOutput("details_strip"),
    tags$style(type = "text/css", "#details_strip th,td {border: medium solid #000000;text-align:center}"),
    tags$style(type = "text/css", "#details_strip td {border: medium solid #000000;text-align:center}"),
    tags$br(),
    tableOutput("table_crd"),
    tags$style(type = "text/css", "#table_crd th,td {border: medium solid #000000;text-align:center}"),
    tags$style(type = "text/css", "#table_crd td {border: medium solid #000000;text-align:center}"),
    tableOutput("table_rbd"),
    tags$style(type = "text/css", "#table_rbd th,td {border: medium solid #000000;text-align:center}"),
    tags$style(type = "text/css", "#table_rbd td {border: medium solid #000000;text-align:center}"),
    tableOutput("table_aug"),
    tags$style(type = "text/css", "#table_aug th,td {border: medium solid #000000;text-align:center}"),
    tags$style(type = "text/css", "#table_aug td {border: medium solid #000000;text-align:center}"),
    tableOutput("table_split"),
    tags$style(type = "text/css", "#table_split th,td {border: medium solid #000000;text-align:center}"),
    tags$style(type = "text/css", "#table_split td {border: medium solid #000000;text-align:center}"),
    tableOutput("table_strip"),
    tags$style(type = "text/css", "#table_strip th,td {border: medium solid #000000;text-align:center}"),
    tags$style(type = "text/css", "#table_strip td {border: medium solid #000000;text-align:center}"),
    uiOutput("plot"),
    uiOutput("image_down1"),
    uiOutput("image_down2"),
    uiOutput("image_down3"),
    uiOutput("image_down4"),
    uiOutput("image_down5"),
    uiOutput("text_crd2"),
    uiOutput("text_rbd2"),
    uiOutput("text_aug2"),
    uiOutput("text_split2"),
    uiOutput("text_strip2"),
    tags$br(),
    uiOutput("text_crd3"),
    uiOutput("text_rbd3"),
    uiOutput("text_aug3"),
    uiOutput("text_split3"),
    uiOutput("text_strip3"),
    tags$br(),
    uiOutput("download_crd"),
    uiOutput("download_rbd"),
    uiOutput("download_aug"),
    uiOutput("download_split"),
    uiOutput("download_strip"),
    tags$br(),
    tags$br(),
    tags$br()
    ####
  )
)

server <- function(input, output, session) {

  ################### CRD
  output$var_crd <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (input$design == "crd") {
      list(numericInput("trt_crd", tags$div(tags$b("Please enter the number of treatments:"), style = "color:#a047ff"), 0, min = 2, max = 100))
    }
  })

  output$text_crd1 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$trt_crd)) {
      return()
    }
    if (input$design == "crd") {
      if (input$trt_crd >= 2) {
        t <- input$trt_crd
        minrep.req <- ((12 + t) / t)
        HTML(paste0(tags$b(" minimum number of replication
                            required is:", ceiling(minrep.req))))
      }
    }
    else {
      return()
    }
  })

  output$var_crd1 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$trt_crd)) {
      return()
    }
    if (input$design == "crd") {
      if (input$trt_crd >= 2) {
        list(
          tags$br(),
          numericInput("rep_crd", tags$div(tags$b("Please enter the number of replications:"), style = "color:#a047ff"), 0, min = 2, max = 100),
          tags$br(),
          actionBttn(
            inputId = "submit1",
            label = "Generate CRD Layout!",
            color = "success",
            style = "float"
          ),
          sliderInput("rand", "Generate random plots:",
            min = 1, max = 100, value = 1
          ),
          tags$br(),
          sliderInput("size", "Adjust label size of layout image here:",
            min = 0.1, max = 2, value = 1
          )
        )
      }
    }
  })


  output$details_crd <- renderTable(
    {
      if (is.null(input$rep_crd)) {
        return()
      }
      if (is.null(input$trt_crd)) {
        return()
      }
      if (is.null(input$design)) {
        return()
      }
      if (is.null(input$submit1)) {
        return()
      }
      if (input$design == "crd") {
        if (input$rep_crd > 0 && input$submit1 > 0) {
          Treatments <- input$trt_crd
          Replication <- input$rep_crd
          Experimental.units <- Treatments * Replication
          det <- cbind(Treatments, Replication, Experimental.units)
          det <- as.data.frame(det)
          det
        }
        else {
          return()
        }
      }
    },
    caption = ("<b> Details of the Experiment </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
  )

  output$text_crd2 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$rep_crd)) {
      return()
    }
    if (is.null(input$submit1)) {
      return()
    }
    if (input$design == "crd") {
      if (input$rep_crd == 0 && input$submit1 > 0) {
        HTML(paste0(tags$b("Please enter required
                            number of replications")))
      }
    }
    else {
      return()
    }
  })

  output$text_crd3 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$rep_crd)) {
      return()
    }
    if (is.null(input$trt_crd)) {
      return()
    }
    if (is.null(input$submit1)) {
      return()
    }
    if (input$design == "crd") {
      if (input$trt_crd > 0) {
        if (input$rep_crd > 0) {
          if (input$submit1 > 0) {
            HTML(paste0(tags$b("Each cell in the figure corresponds to an experimental unit ")))
          }
        }
      }
    }
  })

  ############################# END CRD

  ################### RBD
  output$var_rbd <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (input$design == "rbd") {
      list(numericInput("trt_rbd", tags$div(tags$b("Please enter the number of treatments:"), style = "color:#a047ff"), 0, min = 2, max = 100))
    }
  })

  output$text_rbd1 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$trt_rbd)) {
      return()
    }
    if (input$design == "rbd") {
      if (input$trt_rbd >= 2) {
        t <- input$trt_rbd
        minrep.req <- ((12 / (t - 1)) + 1)
        HTML(paste0(tags$b(" minimum number of blocks (replication) required is:", ceiling(minrep.req))))
      }
    }
    else {
      return()
    }
  })

  output$var_rbd1 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$trt_rbd)) {
      return()
    }
    if (input$design == "rbd") {
      if (input$trt_rbd >= 2) {
        list(
          tags$br(),
          numericInput("rep_rbd", tags$div(tags$b("Please enter the number of Blocks (replications):"), style = "color:#a047ff"), 0, min = 2, max = 100),
          tags$br(),
          actionBttn(
            inputId = "submit2",
            label = "Generate RCBD Layout!",
            color = "success",
            style = "float"
          ),
          sliderInput("rand2", "Generate random plots:",
            min = 1, max = 100, value = 1
          ),
          tags$br(),
          sliderInput("size_rbd", "Adjust label size of layout image here:",
            min = 0.1, max = 2, value = 1
          )
        )
      }
    }
  })

  output$details_rbd <- renderTable(
    {
      if (is.null(input$design)) {
        return()
      }
      if (is.null(input$rep_rbd)) {
        return()
      }
      if (is.null(input$trt_rbd)) {
        return()
      }
      if (is.null(input$submit2)) {
        return()
      }
      if (input$design == "rbd") {
        if (input$rep_rbd > 0 && input$submit2 > 0) {
          Treatments <- input$trt_rbd
          Blocks <- input$rep_rbd
          Experimental.units <- Treatments * Blocks
          det <- cbind(Treatments, Blocks, Experimental.units)
          det <- as.data.frame(det)
          det
        }
        else {
          return()
        }
      }
    },
    caption = ("<b> Details of the Experiment </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
  )

  output$text_rbd2 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$rep_rbd)) {
      return()
    }
    if (is.null(input$submit2)) {
      return()
    }
    if (input$design == "rbd") {
      if (input$rep_rbd == 0 && input$submit2 > 0) {
        HTML(paste0(tags$b("Please enter required
                            number of replications")))
      }
    }
    else {
      return()
    }
  })

  output$text_rbd3 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$rep_rbd)) {
      return()
    }
    if (is.null(input$trt_rbd)) {
      return()
    }
    if (is.null(input$submit2)) {
      return()
    }
    if (input$design == "rbd") {
      if (input$trt_rbd > 0) {
        if (input$rep_rbd > 0) {
          if (input$submit2 > 0) {
            HTML(paste0(tags$b("Each columns in the figure corresponds to a block ")))
          }
        }
      }
    }
  })

  ############################# END RBD

  ############################# AUGMENTED RCBD
  output$var_aug <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (input$design == "aug") {
      list(
        numericInput("check_aug",
          tags$div(tags$b("Please enter the number of Checks (Controls):"),
            style = "color:#a047ff"
          ), 0,
          min = 2, max = 100
        ),
        numericInput("trt_aug",
          tags$div(tags$b("Please enter the number of treatments (unreplicated):"),
            style = "color:#a047ff"
          ), 0,
          min = 2, max = 200
        )
      )
    }
  })


  output$text_aug1 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$check_aug)) {
      return()
    }
    if (input$design == "aug") {
      if (input$check_aug >= 2) {
        t <- input$check_aug
        minrep.req <- ((12 / (t - 1)) + 1)
        HTML(paste0(tags$b(" You should enter sufficiently large number of blocks below
                            . Number of blocks should be greater than:", ceiling(minrep.req))))
      }
    }
    else {
      return()
    }
  })

  output$var_aug1 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$check_aug)) {
      return()
    }
    if (input$design == "aug") {
      if (input$check_aug >= 2) {
        list(
          tags$br(),
          numericInput("rep_aug", tags$div(tags$b("Please enter the number of Blocks:"), style = "color:#a047ff"), 0, min = 2, max = 100),
          tags$br(),
          actionBttn(
            inputId = "submit3",
            label = "Generate Augmented RCBD Layout!",
            color = "success",
            style = "float"
          ),
          sliderInput("rand3", "Generate random plots:",
            min = 1, max = 100, value = 1
          ),
          tags$br(),
          sliderInput("size_aug", "Adjust label size of layout image here:",
            min = 0.1, max = 2, value = 1
          )
        )
      }
    }
  })

  output$text_aug2 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$rep_aug)) {
      return()
    }
    if (is.null(input$submit3)) {
      return()
    }
    if (input$design == "aug") {
      if (input$rep_aug == 0 && input$submit3 > 0) {
        HTML(paste0(tags$b("Please enter required
                            number of replications")))
      }
    }
    else {
      return()
    }
  })


  output$details_aug <- renderTable(
    {
      if (is.null(input$design)) {
        return()
      }
      if (is.null(input$rep_aug)) {
        return()
      }
      if (is.null(input$trt_aug)) {
        return()
      }
      if (is.null(input$check_aug)) {
        return()
      }
      if (is.null(input$submit3)) {
        return()
      }
      if (input$design == "aug") {
        if (input$check_aug > 0 && input$trt_aug && input$rep_aug > 0 && input$submit3 > 0) {
          Check <- input$check_aug
          Treatments <- input$trt_aug
          Blocks <- input$rep_aug
          Plots <- Check * Blocks + Treatments
          det <- cbind(Check, Treatments, Blocks, Plots)
          det <- as.data.frame(det)
          det
        }
        else {
          return()
        }
      }
    },
    caption = ("<b> Details of the Experiment)</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
  )

  output$text_aug3 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$rep_aug)) {
      return()
    }
    if (is.null(input$trt_aug)) {
      return()
    }
    if (is.null(input$check_aug)) {
      return()
    }
    if (is.null(input$submit3)) {
      return()
    }
    if (input$design == "aug") {
      if (input$trt_aug > 0) {
        if (input$rep_aug > 0) {
          if (input$check_aug > 0) {
            if (input$submit3 > 0) {
              HTML(paste0(tags$b("Treatment numbers are shown in cells (plots).
                                Each columns in the figure corresponds to a block ")))
            }
          }
        }
      }
    }
  })

  ############################################ SPLIT PLOT
  output$var_split <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (input$design == "split") {
      list(
        numericInput("maintrt_split",
          tags$div(tags$b("Please enter the number of Main plot treatments:"),
            style = "color:#a047ff"
          ), 0,
          min = 2, max = 100
        ),
        numericInput("subtrt_split",
          tags$div(tags$b("Please enter the number of Sub plot treatments:"),
            style = "color:#a047ff"
          ), 0,
          min = 2, max = 200
        )
      )
    }
  })


  output$text_split1 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$maintrt_split)) {
      return()
    }
    if (is.null(input$subtrt_split)) {
      return()
    }
    if (input$design == "split") {
      if (input$maintrt_split >= 2 && input$subtrt_split >= 2) {
        a <- input$maintrt_split
        b <- input$subtrt_split
        minrep.req <- ((12 / ((b - 1) * a)) + 1)
        HTML(paste0(tags$b(" minimum number of replication required is:", ceiling(minrep.req))))
      }
    }
    else {
      return()
    }
  })

  output$text_split2 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$rep_split)) {
      return()
    }
    if (is.null(input$submit4)) {
      return()
    }
    if (input$design == "split") {
      if (input$rep_split == 0 && input$submit4 > 0) {
        HTML(paste0(tags$b("Please enter required
                            number of replications")))
      }
    }
    else {
      return()
    }
  })
  output$var_split1 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$maintrt_split)) {
      return()
    }
    if (is.null(input$subtrt_split)) {
      return()
    }
    if (input$design == "split") {
      if (input$maintrt_split >= 2 && input$subtrt_split >= 2) {
        list(
          tags$br(),
          numericInput("rep_split", tags$div(tags$b("Please enter the required number of replication:"), style = "color:#a047ff"), 0, min = 2, max = 100),
          tags$br(),
          actionBttn(
            inputId = "submit4",
            label = "Generate Split Plot Layout!",
            color = "success",
            style = "float"
          ),
          sliderInput("rand4", "Generate random plots:",
            min = 1, max = 100, value = 1
          ),
          tags$br(),
          sliderInput("size_split", "Adjust label size of layout image here:",
            min = 0.1, max = 2, value = 1
          )
        )
      }
    }
  })

  output$text_split3 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$rep_split)) {
      return()
    }
    if (is.null(input$maintrt_split)) {
      return()
    }
    if (is.null(input$subtrt_split)) {
      return()
    }
    if (is.null(input$submit4)) {
      return()
    }
    if (input$design == "split") {
      if (input$maintrt_split > 0) {
        if (input$rep_split > 0) {
          if (input$subtrt_split > 0) {
            if (input$submit4 > 0) {
              HTML(paste0(tags$b("Each column corresponds to mainplot. Mainplot treatments are
                                  applied in the columns shown by corresponding colour. Subplot
                                  treatment numbers are shown in each cells shown by treatment number
                                  (sub plots).")))
            }
          }
        }
      }
    }
  })

  output$details_split <- renderTable(
    {
      if (is.null(input$design)) {
        return()
      }
      if (is.null(input$rep_split)) {
        return()
      }
      if (is.null(input$maintrt_split)) {
        return()
      }
      if (is.null(input$subtrt_split)) {
        return()
      }
      if (is.null(input$submit4)) {
        return()
      }
      if (input$design == "split") {
        if (input$maintrt_split > 0 && input$subtrt_split && input$rep_split > 0 &&
          input$submit4 > 0) {
          Main_plot_trt <- input$maintrt_split
          Sub_plot_trt <- input$subtrt_split
          Replications <- input$rep_split
          No_of_Plots <- Main_plot_trt * Sub_plot_trt * Replications
          det <- cbind(Main_plot_trt, Sub_plot_trt, Replications, No_of_Plots)
          det <- as.data.frame(det)
          det
        }
        else {
          return()
        }
      }
    },
    caption = ("<b> Details of the experiment </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
  )

  ############################################ STRIP PLOT
  output$var_strip <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (input$design == "strip") {
      list(
        numericInput("maintrt_strip",
          tags$div(tags$b("Please enter the number of Main plot A treatments:"),
            style = "color:#a047ff"
          ), 0,
          min = 2, max = 100
        ),
        numericInput("subtrt_strip",
          tags$div(tags$b("Please enter the number of Main plot B treatments:"),
            style = "color:#a047ff"
          ), 0,
          min = 2, max = 200
        )
      )
    }
  })


  output$text_strip1 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$maintrt_strip)) {
      return()
    }
    if (is.null(input$subtrt_strip)) {
      return()
    }
    if (input$design == "strip") {
      if (input$maintrt_strip >= 2 && input$subtrt_strip >= 2) {
        a <- input$maintrt_strip
        b <- input$subtrt_strip
        minrep.req <- ((12 / ((b - 1) * (a - 1))) + 1)
        HTML(paste0(tags$b(" minimum number of replication required is:", ceiling(minrep.req))))
      }
    }
    else {
      return()
    }
  })

  output$text_strip2 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$rep_strip)) {
      return()
    }
    if (is.null(input$submit5)) {
      return()
    }
    if (input$design == "strip") {
      if (input$rep_strip == 0 && input$submit5 > 0) {
        HTML(paste0(tags$b("Please enter required
                            number of replications")))
      }
    }
    else {
      return()
    }
  })
  output$var_strip1 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$maintrt_strip)) {
      return()
    }
    if (is.null(input$subtrt_strip)) {
      return()
    }
    if (input$design == "strip") {
      if (input$maintrt_strip >= 2 && input$subtrt_strip >= 2) {
        list(
          tags$br(),
          numericInput("rep_strip", tags$div(tags$b("Please enter the required number of replication:"), style = "color:#a047ff"), 0, min = 2, max = 100),
          tags$br(),
          actionBttn(
            inputId = "submit5",
            label = "Generate Strip Plot Layout!",
            color = "success",
            style = "float"
          ),
          sliderInput("rand5", "Generate random plots:",
            min = 1, max = 100, value = 1
          ),
          tags$br(),
          sliderInput("size_strip", "Adjust label size of layout image here:",
            min = 0.1, max = 2, value = 1
          )
        )
      }
    }
  })

  output$text_strip3 <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$rep_strip)) {
      return()
    }
    if (is.null(input$maintrt_strip)) {
      return()
    }
    if (is.null(input$subtrt_strip)) {
      return()
    }
    if (is.null(input$submit5)) {
      return()
    }
    if (input$design == "strip") {
      if (input$maintrt_strip > 0) {
        if (input$rep_strip > 0) {
          if (input$subtrt_strip > 0) {
            if (input$submit5 > 0) {
              rep <- input$rep_strip
              HTML(paste0(tags$b("Main Plot treatment A is applied in rows (shown by corresponding colour) and
                                  Main Plot treatment B is applied in columns (shown by letters).
                                  Replications are shown seperately (diagonally)")))
            }
          }
        }
      }
    }
  })

  output$details_strip <- renderTable(
    {
      if (is.null(input$design)) {
        return()
      }
      if (is.null(input$rep_strip)) {
        return()
      }
      if (is.null(input$maintrt_strip)) {
        return()
      }
      if (is.null(input$subtrt_strip)) {
        return()
      }
      if (is.null(input$submit5)) {
        return()
      }
      if (input$design == "strip") {
        if (input$maintrt_strip > 0 && input$subtrt_strip && input$rep_strip > 0 &&
          input$submit5 > 0) {
          Main_plot_A <- input$maintrt_strip
          Main_plot_B <- input$subtrt_strip
          Replications <- input$rep_strip
          No_of_Plots <- Main_plot_A * Main_plot_B * Replications
          det <- cbind(Main_plot_A, Main_plot_B, Replications, No_of_Plots)
          det <- as.data.frame(det)
          det
        }
        else {
          return()
        }
      }
    },
    caption = ("<b> Details of the experiment </b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
  )


  ################################## Layout plots
  output$plot <- renderUI({
    if (is.null(input$design)) {
      return()
    }

    if (input$design == "crd") {
      if (is.null(input$rep_crd)) {
        return()
      }
      if (is.null(input$trt_crd)) {
        return()
      }
      if (is.null(input$submit1)) {
        return()
      }

      if (input$trt_crd > 0) {
        if (input$rep_crd > 0) {
          output$crd_layout <- renderPlot(
            {
              if (input$submit1 > 0) {
                t <- input$trt_crd
                r <- input$rep_crd
                s <- input$size
                rand <- input$rand
                trtname <- sprintf("T%d", 1:t)
                outdesign <- agricolae::design.crd(trtname, r = r, seed = rand, serie = 0, kinds = "Super-Duper", randomization = TRUE)
                CRD <- outdesign$book
                CRD <- CRD[order(CRD$r), ]
                CRD$col <- CRD$r
                CRD$row <- rep(1:t, r)
                desplot::desplot(
                  form = trtname ~ col + row, data = CRD, text = trtname, out1 = col,
                  out2 = row, out2.gpar = list(col = "black", lwd = 3),
                  cex = s, main = "Layout of Completely Randomized Design", show.key = FALSE
                )
              }
            },
            bg = "transparent"
          )
          h <- input$trt_crd * 50
          w <- input$rep_crd * 100
          plotOutput("crd_layout", width = w, height = h)
        }
      }
    }
    else if (input$design == "rbd") {
      if (is.null(input$trt_rbd)) {
        return()
      }
      if (is.null(input$submit2)) {
        return()
      }
      if (is.null(input$rep_rbd)) {
        return()
      }

      if (input$trt_rbd > 0) {
        if (input$rep_rbd > 0) {
          output$rbd_layout <- renderPlot(
            {
              if (input$submit2 > 0) {
                t <- input$trt_rbd
                rep <- input$rep_rbd
                s <- input$size_rbd
                rand <- input$rand2
                trtname <- sprintf("T%d", 1:t)
                n <- t * rep
                outdesign <- agricolae::design.rcbd(trt = trtname, r = rep, seed = rand, serie = 0, kinds = "Super-Duper", randomization = TRUE)
                RBD <- outdesign$book
                RBD <- RBD[order(RBD$block), ]
                RBD$blocks <- as.numeric(RBD$block)
                RBD$plots <- rep(1:t, rep)
                desplot::desplot(
                  form = block ~ blocks + plots, data = RBD, text = trtname, out1 = blocks,
                  out2 = plots, out2.gpar = list(col = "green"),
                  cex = s, main = "Layout of Randomized Block Design", show.key = TRUE
                )
              }
            },
            bg = "transparent"
          )

          h <- input$trt_rbd * 80
          w <- input$rep_rbd * 100
          plotOutput("rbd_layout", width = w, height = h)
        }
      }
    }

    else if (input$design == "aug") {
      if (is.null(input$trt_aug)) {
        return()
      }
      if (is.null(input$submit3)) {
        return()
      }
      if (is.null(input$rep_aug)) {
        return()
      }
      if (is.null(input$check_aug)) {
        return()
      }

      if (input$check_aug > 0) {
        if (input$trt_aug > 0) {
          if (input$rep_aug > 0) {
            output$aug_layout <- renderPlot(
              {
                if (input$submit3 > 0) {
                  NC <- input$check_aug # no. of check
                  blk <- input$rep_aug # no: of Blocks
                  trt <- input$trt_aug # no.of trt
                  s <- input$size_aug
                  rand <- input$rand3
                  T1 <- sprintf("C%d", 1:NC) # checks
                  T2 <- sprintf("T%d", 1:trt) # treatments
                  outdesign <- agricolae::design.dau(T1, T2, r = blk, seed = rand, serie = 0, randomization = TRUE)
                  aug <- outdesign$book
                  aug <- aug[order(aug$block), ]
                  aug$blocks <- as.numeric(aug$block)
                  x <- aug %>%
                    group_by(block) %>%
                    mutate(row = row_number())
                  aug1 <- as.data.frame(cbind(aug, row = x$row))
                  desplot::desplot(
                    form = block ~ blocks + row, data = aug1, text = trt, out1 = blocks, out2 = row,
                    cex = s, main = "Layout of Augmented Randomized Block Design", show.key = FALSE
                  )
                }
              },
              bg = "transparent"
            )

            h <- ((input$trt_aug / input$rep_aug) * 80)
            w <- input$rep_aug * 70
            plotOutput("aug_layout", width = w, height = h)
          }
        }
      }
    }

    else if (input$design == "strip") {
      if (is.null(input$maintrt_strip)) {
        return()
      }
      if (is.null(input$submit5)) {
        return()
      }
      if (is.null(input$rep_strip)) {
        return()
      }
      if (is.null(input$subtrt_strip)) {
        return()
      }

      if (input$maintrt_strip > 0) {
        if (input$subtrt_strip > 0) {
          if (input$rep_strip > 0) {
            output$strip_layout <- renderPlot(
              {
                if (input$submit5 > 0) {
                  a <- input$maintrt_strip
                  b <- input$subtrt_strip
                  rep <- input$rep_strip
                  s <- input$size_strip
                  rand <- input$rand5
                  main1 <- sprintf("A%d", 1:a)
                  main2 <- sprintf("B%d", 1:b)
                  outdesign <- agricolae::design.strip(main1, main2, r = rep, serie = 0, seed = rand, kinds = "Super-Duper", randomization = TRUE)
                  strip <- outdesign$book
                  strip$block2 <- strip$block
                  v1 <- do.call(paste, as.data.frame(t(apply(strip[4:5], 1, sort))))
                  strip$row <- match(v1, unique(v1))
                  v2 <- do.call(paste, as.data.frame(t(apply(strip[2:3], 1, sort))))
                  strip$column <- match(v2, unique(v2))
                  desplot::desplot(
                    form = main1 ~ row + column, data = strip, text = main2, out1 = column,
                    out2 = row, out2.gpar = list(col = "#a83232"),
                    cex = s, main = "Layout of Strip-Plot Design", show.key = TRUE
                  )
                }
              },
              bg = "transparent"
            )

            plotOutput("strip_layout")
          }
        }
      }
    }

    else if (input$design == "split") {
      if (is.null(input$maintrt_split)) {
        return()
      }
      if (is.null(input$submit4)) {
        return()
      }
      if (is.null(input$rep_split)) {
        return()
      }
      if (is.null(input$subtrt_split)) {
        return()
      }

      if (input$maintrt_split > 0) {
        if (input$subtrt_split > 0) {
          if (input$rep_split > 0) {
            output$split_layout <- renderPlot(
              {
                if (input$submit4 > 0) {
                  a <- input$maintrt_split
                  b <- input$subtrt_split
                  rep <- input$rep_split
                  s <- input$size_split
                  rand <- input$rand4
                  main <- sprintf("A%d", 1:a)
                  sub <- sprintf("b%d", 1:b)
                  outdesign <- agricolae::design.split(main, sub, r = rep, serie = 0, seed = rand, kinds = "Super-Duper", randomization = TRUE)
                  split <- outdesign$book
                  split <- split[order(split$plots), ]
                  split$plots <- split$plots
                  split$plots <- as.numeric(split$plots)
                  split$splots <- as.numeric(split$splots)
                  desplot::desplot(
                    form = main ~ plots + splots, data = split, col = main, text = sub, out1 = plots, out2 = splots,
                    cex = s, main = "Layout of Split-Plot Design", show.key = TRUE
                  )
                }
              },
              bg = "transparent"
            )

            plotOutput("split_layout")
          }
        }
      }
    }
  })
  ########### layout end

  ############################# table output
  #### tab
  output$tab_crd <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (input$design == "crd") {
      if (is.null(input$trt_crd)) {
        return()
      }
      if (input$trt_crd >= 2) {
        list(actionBttn(
          inputId = "table_butt1",
          label = "Create layout in table format!",
          color = "success",
          style = "stretch"
        ))
      }
    }
  })

  output$tab_rbd <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (input$design == "rbd") {
      if (is.null(input$trt_rbd)) {
        return()
      }
      if (input$trt_rbd >= 2) {
        list(actionBttn(
          inputId = "table_butt2",
          label = "Create layout in table format!",
          color = "success",
          style = "stretch"
        ))
      }
    }
  })


  output$tab_aug <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (input$design == "aug") {
      if (is.null(input$check_aug)) {
        return()
      }
      if (input$check_aug >= 2) {
        list(actionBttn(
          inputId = "table_butt3",
          label = "Create layout in table format!",
          color = "success",
          style = "stretch"
        ))
      }
    }
  })

  output$tab_split <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (input$design == "split") {
      if (is.null(input$maintrt_split)) {
        return()
      }
      if (input$maintrt_split >= 2) {
        list(actionBttn(
          inputId = "table_butt4",
          label = "Create layout in table format!",
          color = "success",
          style = "stretch"
        ))
      }
    }
  })

  output$tab_strip <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (input$design == "strip") {
      if (is.null(input$maintrt_strip)) {
        return()
      }
      if (input$maintrt_strip >= 2) {
        list(actionBttn(
          inputId = "table_butt5",
          label = "Create layout in table format!",
          color = "success",
          style = "stretch"
        ))
      }
    }
  })

  ##### table layout

  output$table_crd <- renderTable(
    {
      if (is.null(input$design)) {
        return()
      }
      if (is.null(input$trt_crd)) {
        return()
      }
      if (is.null(input$table_butt1)) {
        return()
      }
      if (is.null(input$rep_crd)) {
        return()
      }
      if (input$design == "crd") {
        if (input$table_butt1 > 0) {
          t <- input$trt_crd
          r <- input$rep_crd
          n <- t * r
          trtname <- sprintf("T%d", 1:t)
          outdesign <- agricolae::design.crd(trtname, r = r, seed = input$rand, serie = 0, kinds = "Super-Duper", randomization = TRUE)
          CRD <- outdesign$book
          CRD <- CRD[order(CRD$r), ]
          CRD$exp <- rep(1:n)
          final <- as.data.frame(cbind(Experimental.Unit = CRD$exp, Treatments = as.character(CRD$trtname)))
          final
        }
      }
    },
    caption = ("<b> Treatment allocation (random)</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
  )

  output$table_rbd <- renderTable(
    {
      if (is.null(input$design)) {
        return()
      }
      if (is.null(input$trt_rbd)) {
        return()
      }
      if (is.null(input$rep_rbd)) {
        return()
      }
      if (is.null(input$table_butt2)) {
        return()
      }
      if (input$design == "rbd") {
        if (input$table_butt2 > 0) {
          t <- input$trt_rbd
          rep <- input$rep_rbd
          trtname <- sprintf("T%d", 1:t)
          n <- t * rep
          outdesign <- agricolae::design.rcbd(trt = trtname, r = rep, seed = input$rand2, serie = 0, kinds = "Super-Duper", randomization = TRUE)
          RBD <- outdesign$book
          RBD <- RBD[order(RBD$block), ]
          RBD$plot <- rep(1:t, rep)
          final <- as.data.frame(cbind(Block = RBD$block, Plot = RBD$plot, Treatment = as.character(RBD$trtname)))
          final
        }
      }
    },
    caption = ("<b> Treatment allocation (random)</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
  )

  output$table_aug <- renderTable(
    {
      if (is.null(input$trt_aug)) {
        return()
      }
      if (is.null(input$submit3)) {
        return()
      }
      if (is.null(input$rep_aug)) {
        return()
      }
      if (is.null(input$check_aug)) {
        return()
      }
      if (is.null(input$table_butt3)) {
        return()
      }
      if (input$design == "aug") {
        if (input$table_butt3 > 0) {
          NC <- input$check_aug # no. of check
          blk <- input$rep_aug # no: of Blocks
          trt <- input$trt_aug # no.of trt
          s <- input$size_aug
          T1 <- sprintf("C%d", 1:NC) # checks
          T2 <- sprintf("T%d", 1:trt) # treatments
          outdesign <- agricolae::design.dau(T1, T2, r = blk, seed = input$rand3, serie = 0, randomization = TRUE)
          aug <- outdesign$book
          aug <- aug[order(aug$block), ]
          x1 <- aug %>%
            group_by(block) %>%
            mutate(Plot = row_number())
          final <- as.data.frame(cbind(Block = x1$block, Plot = x1$Plot, Treatment = as.character(x1$trt)))
          final
        }
      }
    },
    caption = ("<b> Treatment allocation (random)</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
  )

  output$table_split <- renderTable(
    {
      if (is.null(input$design)) {
        return()
      }
      if (is.null(input$rep_split)) {
        return()
      }
      if (is.null(input$maintrt_split)) {
        return()
      }
      if (is.null(input$subtrt_split)) {
        return()
      }
      if (is.null(input$submit4)) {
        return()
      }
      if (is.null(input$table_butt4)) {
        return()
      }
      if (input$design == "split") {
        if (input$table_butt4 > 0) {
          a <- input$maintrt_split
          b <- input$subtrt_split
          rep <- input$rep_split
          s <- input$size_split
          main <- sprintf("A%d", 1:a)
          sub <- sprintf("b%d", 1:b)
          outdesign <- agricolae::design.split(main, sub, r = rep, serie = 0, seed = input$rand4, kinds = "Super-Duper", randomization = TRUE)
          split <- outdesign$book
          split <- split[order(split$plots), ]
          final <- as.data.frame(cbind(
            Main_plot = split$plots, Sub_plot = split$splots,
            Replication = split$block, Main_treatment = as.character(split$main), Sub_treatment = as.character(split$sub)
          ))
          final
        }
      }
    },
    caption = ("<b> Treatment allocation (random)</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
  )

  output$table_strip <- renderTable(
    {
      if (is.null(input$design)) {
        return()
      }
      if (is.null(input$rep_strip)) {
        return()
      }
      if (is.null(input$maintrt_strip)) {
        return()
      }
      if (is.null(input$subtrt_strip)) {
        return()
      }
      if (is.null(input$submit5)) {
        return()
      }
      if (is.null(input$table_butt5)) {
        return()
      }
      if (input$design == "strip") {
        if (input$table_butt5 > 0) {
          a <- input$maintrt_strip
          b <- input$subtrt_strip
          rep <- input$rep_strip
          s <- input$size_strip
          main1 <- sprintf("A%d", 1:a)
          main2 <- sprintf("B%d", 1:b)
          outdesign <- agricolae::design.strip(main1, main2, r = rep, serie = 0, seed = input$rand5, kinds = "Super-Duper", randomization = TRUE)
          strip <- outdesign$book
          strip$block2 <- strip$block
          v1 <- do.call(paste, as.data.frame(t(apply(strip[4:5], 1, sort))))
          strip$row <- match(v1, unique(v1))
          v2 <- do.call(paste, as.data.frame(t(apply(strip[2:3], 1, sort))))
          strip$column <- match(v2, unique(v2))
          final <- as.data.frame(cbind(
            Replication = strip$block, Horizontal_row_No. = strip$row,
            Treatment_A = as.character(strip$main1),
            Vertical_row_No. = strip$column,
            Treatment_B = as.character(strip$main2)
          ))
          final
        }
      }
    },
    caption = ("<b> Treatment allocation (random)</b>"),
    bordered = TRUE,
    align = "c",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE
  )
  ############ table end

  ####################### download Report
  output$download_crd <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$rep_crd)) {
      return()
    }
    if (is.null(input$trt_crd)) {
      return()
    }
    if (is.null(input$submit1)) {
      return()
    }
    if (input$design == "crd") {
      if (input$trt_crd > 0) {
        if (input$rep_crd > 0) {
          if (input$submit1 > 0) {
            list(
              radioButtons("format", "Download report:", c("HTML"),
                inline = TRUE
              ),
              downloadButton("downloadReport")
            )
          }
        }
      }
    }
  })

  output$download_rbd <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$rep_rbd)) {
      return()
    }
    if (is.null(input$trt_rbd)) {
      return()
    }
    if (is.null(input$submit2)) {
      return()
    }
    if (input$design == "rbd") {
      if (input$trt_rbd > 0) {
        if (input$rep_rbd > 0) {
          if (input$submit2 > 0) {
            list(
              radioButtons("format", "Download report:", c("HTML"),
                inline = TRUE
              ),
              downloadButton("downloadReport")
            )
          }
        }
      }
    }
  })

  output$download_aug <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$rep_aug)) {
      return()
    }
    if (is.null(input$trt_aug)) {
      return()
    }
    if (is.null(input$check_aug)) {
      return()
    }
    if (is.null(input$submit3)) {
      return()
    }
    if (input$design == "aug") {
      if (input$trt_aug > 0) {
        if (input$rep_aug > 0) {
          if (input$check_aug > 0) {
            if (input$submit3 > 0) {
              list(
                radioButtons("format", "Download report:", c("HTML"),
                  inline = TRUE
                ),
                downloadButton("downloadReport")
              )
            }
          }
        }
      }
    }
  })

  output$download_split <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$rep_split)) {
      return()
    }
    if (is.null(input$maintrt_split)) {
      return()
    }
    if (is.null(input$subtrt_split)) {
      return()
    }
    if (is.null(input$submit4)) {
      return()
    }
    if (input$design == "split") {
      if (input$maintrt_split > 0) {
        if (input$rep_split > 0) {
          if (input$subtrt_split > 0) {
            if (input$submit4 > 0) {
              list(
                radioButtons("format", "Download report:", c("HTML"),
                  inline = TRUE
                ),
                downloadButton("downloadReport")
              )
            }
          }
        }
      }
    }
  })

  output$download_strip <- renderUI({
    if (is.null(input$design)) {
      return()
    }
    if (is.null(input$rep_strip)) {
      return()
    }
    if (is.null(input$maintrt_strip)) {
      return()
    }
    if (is.null(input$subtrt_strip)) {
      return()
    }
    if (is.null(input$submit5)) {
      return()
    }
    if (input$design == "strip") {
      if (input$maintrt_strip > 0) {
        if (input$rep_strip > 0) {
          if (input$subtrt_strip > 0) {
            if (input$submit5 > 0) {
              list(
                radioButtons("format", "Download report:", c("HTML"),
                  inline = TRUE
                ),
                downloadButton("downloadReport")
              )
            }
          }
        }
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
}
shinyApp(ui = ui, server = server)
