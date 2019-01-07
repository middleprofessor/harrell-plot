# "Harrell" or "horizontal" dot plot - defaults to ploting treatment contrasts
# in upper panel and treatment boxplots/dotplot in lower planel
# Jeffrey A. Walker
# November 24, 2017
# note See https://github.com/thomasp85/patchwork/blob/master/README.md for alternative to Hdotplot
# working07 - added anova, 
# working08 - (1) made Hdotplot a modular function that can be used in R package without shiny. (2) changed dt from colnames=c(x, g, y) to actual variable names. (3) moved global variables tables, code to local, (4) added dashed line at y=0 to contrast part of plot
# working09 - added ability to view contrasts within treatment and within group 
# working10 - added contrasts as percent or standardized. *** known bug: percent doesn't work with group data if add_interaction==FALSE: fixed in working12b
# working11 - custom gridlines to match tick labels in upper panel. This makes grids different in lower and upper panel.
# working12 - add lmer
# working13 - Major: switched to cowplot to join subplots. Minor: (1) impoved plot download to avoid plots$p1 global variable. (2) downloads subplots. (3) raw summaries improved (really raw). (4) fixed bug where single factor could cause error if previous plot with 2 factors.
# working14 - added covariates, updated plotInput to a reactive so that model is only computed once, unless a parameter has changed
# working15 - added tools for looking at individual effects (lines in dot plot and individual effects in contrasts plot)
# working16 - re-arranged menu items. rewrote readme.
# working17 - replaced lsmeans with emmeans. Modified labels for contrasts.

library(shiny)
library(DT) # for tables
library(gridExtra) # needed for download
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(data.table)
library(MCMCpack) # bayes
library(coda) # bayes

source("harrellplot.R", local = TRUE)
source("fit_model.R", local = TRUE)
source("make_formula_str.R", local = TRUE)

# data("fly")
fish <- fread("https://www.middleprofessor.com/files/applied-biostatistics_bookdown/data/zebra_sprint.txt")
fly <- fread("https://www.middleprofessor.com/files/applied-biostatistics_bookdown/data/fly_burst.txt")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # sidebarLayout(
  # App title ----
  titlePanel("Harrell Plot"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    conditionalPanel(condition = "input.tabs=='Data'",
                     # Input file
                     fileInput("FileInput", "Choose file")
    ),
    conditionalPanel(condition = "input.tabs=='Model'",
                     # model options
                     selectInput("model", h3("Model"), 
                                 choices=c('lm','lmm', "glm"), selected = 1),
                     # reactive Input for treatment
                     uiOutput("reml"),
                     # reactive Input for grouping
                     uiOutput("response"),
                     # reactive Input for grouping
                     uiOutput("treatment"),
                     # reactive Input for response
                     uiOutput("group"),
                     # reactive interaction checkbox
                     uiOutput("interaction"),
                     # reactive Input for covariates
                     uiOutput("covariates"),
                     # reactive Input for random
                     uiOutput("random_intercept"),
                     # reactive Input for random
                     uiOutput("random_slope"),
                     # reactive Input for glm
                     uiOutput("glm_response"),
                     # reactive Input for glm
                     uiOutput("glm_distribution"),
                     
                     # contrast options
                     selectInput("contrasts.method", h3("Contrasts"), 
                                 choices = list("Coefficients" = 1,
                                                "vs. Control" = 2,
                                                "Pairwise" = 3), selected = 3),
                     uiOutput("interaction.treatment"),
                     uiOutput("interaction.group"),
                     selectInput("contrasts.scaling", h5("Contrast scaling"), 
                                 choices = list("Raw" = 1,
                                                "as percent" = 2,
                                                "Standardized" = 3),selected = 1),
                     selectInput("conf.contrast", h5("Confidence level"), 
                                 choices = list("99%" = 1, 
                                                "95%" = 2, 
                                                "90%" = 3), selected = 2),
                     checkboxInput("adjust", "Adjust for multiple tests", FALSE),
                     
                     # Treatment options
                     selectInput("display.treatment", h3("Treatments"), 
                                 choices = list("Box plot" = 1, 
                                                "CI plot" = 2), selected = 2),
                     
                     # Input: Confidence level
                     selectInput("conf.mean", h5("Confidence level"), 
                                 choices = list("99%" = 1, "95%" = 2, "90%" = 3), selected = 2),
                     
                     # Input: Confidence Interval Model
                     # selectInput("mean_intervals.method", h5("Treatment CI model"), 
                     #       choices = list("raw" = 1, "lm" = 2, "bootstrap" = 3, "Bayesian" = 4), selected = 1),
                     selectInput("mean_intervals.method", h5("Treatment CI model"), 
                                 choices = list("raw" = 1, "model" = 2, "bootstrap" = 3), selected = 2)
                     
                     
    ),
    
    conditionalPanel(condition = "input.tabs=='Plot'",
                     # Input: colors
                     selectInput("relheight", h5("Relative height of Contrast subplot"), choices=list('auto'=1, '1:2'=2, '2:3'=3, '1:1'=4, '3:2'=5, '2:1'=6), selected=1),
                     selectInput("colors", h5("Treatment Colors"), 
                                 choices = list("ggplot" = 1, "greys"= 2, "NPG" = 3,
                                                "AAAS"=4, "NEJM"=5, "Lancet"=6, "JAMA"=7,
                                                "JCO"=8), selected = 8),
                     # Input: theme
                     selectInput("theme", h5("Plot Theme"), 
                                 choices = list("gray" = 1, "bw" = 2, "classic" = 3, 
                                                "minimal" = 4, "cowplot" = 5), selected = 4),
                     checkboxInput("hide.contrasts", "Hide contrasts", FALSE),
                     checkboxInput("hide.treatments", "Hide treatments", FALSE),
                     # Input:include zero ----
                     checkboxInput("zero", "Include zero", TRUE),
                     # Input: flip axes ----
                     checkboxInput("horizontal", "Horizontal bars", TRUE),
                     
                     # Input: abbreviate factor levels
                     checkboxInput("short", "Abbreviate factor levels", FALSE),
                     
                     # Input: Checkbox for dots ----
                     checkboxInput("dots", "Show dots", TRUE),
                     # Input: Checkbox for Mean
                     checkboxInput("mean", "Show Mean", TRUE)
                     
                     
    ),
    
    conditionalPanel(condition = "input.tabs=='Table'",
                     h3("Tables"),
                     checkboxInput("notPretty", "Raw R tables", FALSE)
                     
    ),
    
    conditionalPanel(condition = "input.tabs=='Save'",
                     h3("Download"),
                     numericInput("width", "Width (inches)", 6, min = 2, max = 10, step = 0.25),
                     numericInput("height", "Height (inches)", 7, min = 2, max = 10, step = 0.25)
                     
    )
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Help",
                         includeMarkdown("README.Rmd")
                ),
                tabPanel("Data",
                         DT::dataTableOutput("data_table")
                         ),
                tabPanel("Model",
                         # Output: model
                         textOutput("model_text"),
                         # Output: CI level and adjustment for contrasts
                         textOutput("interval_text"),
                         
                         # Output: Plot
                         plotOutput("hDotPlot.model", width = "100%", height = "600px")
                ),
                tabPanel("Plot",
                         # Output: Plot
                         plotOutput("hDotPlot.plot", width = "100%", height = "600px")
                ),
                tabPanel("Table",
                         textOutput("formulaCaption"),
                         verbatimTextOutput("modelFormula"),
                         textOutput("contrastsCaption"),
                         verbatimTextOutput("modelContrasts"),
                         textOutput("sumCaption"),
                         verbatimTextOutput("modelSummary"),
                         textOutput("coefCaption"),
                         verbatimTextOutput("modelCoefficients"),
                         textOutput("meansCaption"),
                         verbatimTextOutput("modelMeans"),
                         textOutput("anovaCaption"),
                         verbatimTextOutput("modelAnova1"),
                         verbatimTextOutput("modelAnova2"),
                         verbatimTextOutput("modelAnova3")
                ),
                tabPanel("Save",
                         
                         # print the plot
                         downloadButton(outputId = "downloadPlot", label = "Download PDF"),
                         downloadButton(outputId = "download_gg_contrasts", label = "Download Contrasts Subplot (ggplot2 object)"),
                         downloadButton(outputId = "download_gg_treatments", label = "Download Treatments Subplot (ggplot2 object)")
                ),
                id = 'tabs', selected = "Data"
    )
    
  )
  #  )
)

server <- function(input, output) {
  
  # read data file
  dataInput <- reactive({
    infile <- input$FileInput # opens file browser
    if(is.null(infile)){
       df <- fly
      # return(df)
    }else{
      df <- fread(infile$datapath, stringsAsFactors = TRUE)
      return(df)
    }
  })
  
  # populate Response input
  output$response <- renderUI({
    df <-dataInput()
    if (is.null(df)) return(NULL)
    items=c(' ', names(df))
    #    names(items)=items
    selectInput("response", "Response",items)
  })
  
  # populate Treatment input
  output$treatment <- renderUI({
    df <-dataInput()
    if (is.null(df)) return(NULL)
    items=c(' ', names(df))
    #    names(items)=items
    selectInput("treatment", "Treatment 1",items)
  })
  
  # populate Group input
  output$group <- renderUI({
    df <-dataInput()
    if (is.null(df)) return(NULL)
    items=c(' ', names(df))
    #    names(items)=items
#    selectInput("group", "Treatment 2", items, selected=1)
    selectInput("group", "Treatment 2", items)
  })
  
  output$interaction <- renderUI({
    df <-dataInput()
    if (is.null(df)) return(NULL)
    if (input$group==' ') return(NULL)
    checkboxInput("interaction", "Add interaction", TRUE)
  })
  
  output$interaction.treatment <- renderUI({
    df <-dataInput()
    if (is.null(df)) return(NULL)
    if (input$group==' ') return(NULL)
    checkboxInput("interaction.treatment", "within Treatment 1", TRUE)
  })
  
  output$interaction.group <- renderUI({
    df <-dataInput()
    if (is.null(df)) return(NULL)
    if (input$group==' ') return(NULL)
    checkboxInput("interaction.group", "within Treatment 2", TRUE)
  })
  
  # populate random intercept input
  output$covariates <- renderUI({
    df <-dataInput()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("covariates", "Covariate(s)",items, multiple=TRUE)
  })
  
  # populate random intercept input
  output$random_intercept <- renderUI({
    df <-dataInput()
    if (is.null(df) | input$model != 'lmm') return(NULL)
    items=names(df)
    names(items)=items
    selectInput("random_intercept", "Random intercept",items, multiple=TRUE)
  })
  
  # populate random slope input
  output$random_slope <- renderUI({
    df <-dataInput()
    if (is.null(df) | input$model != 'lmm') return(NULL)
    items=names(df)
    names(items)=items
    selectInput("random_slope", "Random slope",items, multiple=TRUE)
  })
  
  # populate glm response input
  output$glm_response <- renderUI({
    df <-dataInput()
    if (is.null(df)) return(NULL)
    if (input$model == 'lm') return(NULL)
    if (input$model == 'lmm') return(NULL)
    items=c("binary", "count", "continuous")
    names(items)=items
    selectInput("glm_response", "Response type", items, selected="count")
  })
  
  # # populate glm distribution input
  output$glm_distribution <- renderUI({
    df <-dataInput()
    if (is.null(df)) return(NULL)
    if (input$model == 'lm') return(NULL)
    if (input$model == 'lmm') return(NULL)
    if(input$glm_response=="binary"){
      items=c("binomial")
    }
    if(input$glm_response=="count"){
      items=c("negative binomial", "poisson")
    }
    if(input$glm_response=="continuous"){
      items=c("log", "gamma")
    }
    names(items)=items
    selectInput("glm_distribution", "distribution", items, selected=1)
  })
  
  output$reml <- renderUI({
    df <-dataInput()
    if (is.null(df)) return(NULL)
    if (input$model=='lm') return(NULL)
    if (input$model=='glm') return(NULL)
    checkboxInput("reml", "REML (otherwise, ML)", TRUE)
  })
  
  # render data_table
  output$data_table = DT::renderDataTable(dataInput())

  round_df <- function(df, digits){
    df_names <- colnames(df)
    df <- data.frame(lapply(df, function(y) if(is.numeric(y)) round(y, digits) else y)) 
    colnames(df) <- df_names
    return(df)
  }

  # 
  # contrast.groups <- function(contrast_matrix, grouping, add_interaction){
  #   split1 <- data.frame(t(do.call("cbind", strsplit(as.character(contrast_matrix$contrast)," - "))))
  #   if(grouping == TRUE & add_interaction==TRUE){
  #     split2a <- data.frame(t(do.call("cbind", strsplit(as.character(split1$X1),","))))
  #     colnames(split2a) <- c('X1','G1')
  #     split2b <- data.frame(t(do.call("cbind", strsplit(as.character(split1$X2),","))))
  #     colnames(split2b) <- c('X2','G2')
  #     group_names <- data.table(split2a, split2b)
  #   }else{
  #     group_names <- data.table(split1)
  #   }
  #   return(group_names)
  # }
  # 

  plotInput <- reactive({
    df <- dataInput()
    # exit if there are not correct model inputs
    if(is.null(input$response)==TRUE | is.null(input$treatment)==TRUE){
      return(NULL)
    }
    if(input$model=='lmm' & is.null(input$random_intercept)==TRUE){
      return(NULL)
    }
    
    # model
    fit.model <- input$model
    if(fit.model=='lmm'){
      REML <- input$reml
    }
    #    error <- input$error
    error <- 'Normal'
    x <- input$treatment
    y <- input$response
    g <- input$group
    if(g==' '){g <- NULL}
    covcols <- input$covariates
    
    rintcols <- input$random_intercept
    rslopecols <- input$random_slope
    # make sure that random effects are Null if model is lm
    if(fit.model=='lm'|fit.model=='glm'){
      rintcols <- NULL
      rslopcols <- NULL
    }
    glm_family <- input$glm_distribution
    if(glm_family=="negative binomial"){glm_family <- "nb"}
    if(fit.model=='lm'|fit.model=='lmm'){
      glm_family <- "gaussian"
    }
    
    #set inputs to non-shiny variables
    # confidence limits for contrasts and means
    conf <- c(0.99, 0.95, 0.9)
    conf.contrast <- conf[as.numeric(as.character(input$conf.contrast))]
    conf.mean <- conf[as.numeric(as.character(input$conf.mean))]
    
    # contrasts
    # method for computation of treatment CI
    # control or pairwise contrasts
    contrast_array <- c('coefficients','trt.vs.ctrl1', 'revpairwise') # c("Control", "Pairwise") in menu 
    contrasts.method <- contrast_array[as.numeric(as.character(input$contrasts.method))]
    contrast_scaling_array <- c('raw','percent', 'standardized')
    contrasts.scaling <- contrast_scaling_array[as.numeric(as.character(input$contrasts.scaling))]
    # adjust for multiple tests
    adjust <- ifelse(input$adjust==TRUE, TRUE, FALSE)
    # interaction?
    if(is.null(input$interaction)){
      add_interaction <- FALSE
    }else{
      add_interaction <- ifelse(input$interaction==TRUE, TRUE, FALSE)
    }
    interaction.group <- input$interaction.group
    interaction.treatment <- input$interaction.treatment
    
    # treatments
    input_array <- c('box','ci')
    display.treatment <- input_array[as.numeric(as.character(input$display.treatment))]
    # method for computation of treatment CI
    interval_array <- c('raw','lm','boot','bayes')
    mean_intervals.method <- interval_array[as.numeric(as.character(input$mean_intervals.method))]
    #    mean_intervals.method <- input$mean_intervals.method
    
    # plot
    color_array <- c("black","ggplot", "Greys", "Blues", "Accent", "Dark2", "Set1", "Set2", "Set3")
    color_array <- c("ggplot", "greys", "npg", "aaas", "nejm", "lancet", "jama", "jco")
    color_palette <- color_array[as.numeric(as.character(input$colors))]
    theme_array <- c("gray", "bw", "classic", "minimal", "cowplot")
    jtheme <- theme_array[as.numeric(as.character(input$theme))]
    show.treatments <- ifelse(input$hide.treatments==FALSE, TRUE, FALSE)
    show.contrasts <- ifelse(input$hide.contrasts==FALSE, TRUE, FALSE)
    show.mean <- ifelse(input$mean==TRUE, TRUE, FALSE)
    show.dots <- ifelse(input$dots==TRUE, TRUE, FALSE)
    horizontal <- ifelse(input$horizontal==TRUE, TRUE, FALSE)
    zero <- ifelse(input$zero==TRUE, TRUE, FALSE)
    
    if(input$short==FALSE){short <- FALSE}else{short <- TRUE}
    
    rel_height_array <- c(0, 1/2, 2/3, 1, 3/2, 2)
    rel_height <- rel_height_array[as.numeric(as.character(input$relheight))]
    #    rel_height_ch_array <- c('auto', '1:2', '2:3', '1:1', '3:2', '2:1')
    # rel_height_input <- input$relheight
    # rel_height <- rel_height_array[which(rel_height_ch_array==rel_height_input)]
    
    res <- harrellplot(x, y, g, covcols, rintcols, rslopecols, df, fit.model, REML, error="normal", add_interaction, interaction.group, interaction.treatment, mean_intervals.method, conf.mean, contrasts.method, contrasts.scaling, conf.contrast, adjust, show.contrasts, show.treatments, display.treatment, short, show.mean, show.dots, zero, horizontal, color_palette, jtheme, rel_height)
    
    #res <- harrellplot(x, y, g, covcols, rintcols, rslopecols, df, fit.model, glm_family, REML, add_interaction, interaction.group, interaction.treatment, mean_intervals.method, conf.mean, contrasts.method, contrasts.scaling, conf.contrast, adjust, show.contrasts, show.treatments, display.treatment, short, show.mean, show.dots, zero, horizontal, color_palette, jtheme, rel_height)
    
    res
  })
  
  
  # output model text
  output$model_text <- renderText({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    
    # model
    fit.model <- input$model
    x <- input$treatment
    if(is.null(x)){x=''}
    y <- input$response
    if(is.null(y)){y=''}
    g <- input$group
    if(g==' '){
      xcols <- x
    }else{
      xcols <- c(x, g)
    }
    covcols <- input$covariates
    rintcols <- input$random_intercept
    rslopecols <- input$random_slope
    # # make sure that random effects are Null if model is lm
    if(fit.model=='lm'){
      rintcols <- NULL
      rslopcols <- NULL
    }
    if(is.null(input$interaction) | g==' '){
      add_interaction <- FALSE
    }else{
      add_interaction <- ifelse(input$interaction==TRUE, TRUE, FALSE)
    }
    if(add_interaction==TRUE){
      icols <- c(x,g)
    }else{
      icols <- NULL
    }
    model_formula <- make_formula_str(y, xcols, rintcols, rslopecols, icols, covcols)
    
    model_formula
  })
  
  # output contrast interval method
  output$interval_text <- renderText({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    
    # contrast method
    if(input$model=='bayes'){ #bootstrap or bayes
      adjust_text <- 'no'
    }else{
      if(input$adjust==FALSE){adjust_text <- 'none'}else{
        adjust_text <- ifelse(input$contrasts.method==2, 'Dunnet t approximation','Tukey HSD')
      }
    }
    conf <- c(0.99, 0.95, 0.9)
    conf.contrast <- as.integer(conf[as.numeric(as.character(input$conf.contrast))]*100)
    out_text <- paste('contrasts with ',conf.contrast,'% CI using adjustment: ',adjust_text, sep='')
    
    out_text
  })
  
  output$formulaCaption <- renderText({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    "The Model"
  })
  
  output$modelFormula <- renderPrint({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    tables <- plotInput()$tables
    print(tables$form_str, showEnv=FALSE)
  })
  
  output$contrastsCaption <- renderText({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    contrast_axis_name <- 'Model Contrasts'
    if(input$contrasts.scaling==2){contrast_axis_name <- 'Model Contrasts (percent)'}
    if(input$contrasts.scaling==3){contrast_axis_name <- 'Model Contrasts (standardized)'}
    contrast_axis_name
  })
  
  output$modelContrasts <- renderPrint({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    tables <- plotInput()$tables
    
    if(input$notPretty == TRUE){
      # print(tables$contrasts)
      print(tables$contrasts.raw)
    }else{ # pretty
      print(round_df(tables$contrasts, 3), row.names=FALSE)
    }
  })
  
  output$coefCaption <- renderText({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    "Model Coefficients"
  })
  
  output$modelCoefficients <- renderPrint({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    tables <- plotInput()$tables
    if(input$notPretty == TRUE){
      print(tables$coeffs)
    }else{ # pretty
      coef_dt <- data.table(Coeffcient=row.names(tables$coeffs), tables$coeffs)
      print(round_df(coef_dt, 3), row.names=FALSE)
    }
  })
  
  output$sumCaption <- renderText({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    "Model Summary"
  })
  
  output$modelSummary <- renderPrint({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    tables <- plotInput()$tables
    if(input$notPretty == TRUE){
      print(tables$summary.raw, row.names=FALSE)
    }else{ # pretty
      print(round_df(tables$summary, 3), row.names=FALSE)
    }
  })
  
  output$meansCaption <- renderText({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    "Model Means"
  })
  
  output$modelMeans <- renderPrint({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    tables <- plotInput()$tables
    if(input$notPretty == TRUE){
      print(tables$means.raw)
    }else{ # pretty
      print(round_df(tables$means, 3), row.names=FALSE)
    }
  })
  
  output$anovaCaption <- renderText({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    "Model ANOVA (type I, II, III)"
  })
  
  output$modelAnova1 <- renderPrint({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    tables <- plotInput()$tables
    tables$anova.1
  })
  
  output$modelAnova2 <- renderPrint({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    tables <- plotInput()$tables
    tables$anova.2
  })
  
  output$modelAnova3 <- renderPrint({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    tables <- plotInput()$tables
    tables$anova.3
  })
  
  output$modelCode <- renderPrint({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    codes <- plotInput()$codes
    str1 <- 'library(ggplot2)'
    str2 <- 'library(lsmeans)'
    str3 <- 'library(broom)'
    str4 <- 'library(data.table)'
    h <- paste(str1, str2, str3, str4, sep = '<br/>')
    h <- c(h, '<br/>', paste(codes$data, collapse='<br/>'))
    h <- c(h, '<br/>', paste(codes$fit, collapse='<br/>'))
    HTML(h)
  })
  
  # download
  # output$downloadPlot <- downloadHandler(
  #   filename = 'temp.png',
  #   content = function(file) {
  #     device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
  #     ggsave(file, plot = plotInput(), device = device)
  #   }
  # )
  
  # plot to the model panel
  output$hDotPlot.model <- renderPlot({
    df <-dataInput()
    if (is.null(df)) return(NULL)
    if(input$treatment != ' ' & 
       is.numeric(df[[input$response]])){

      print(plotInput()$gg)
    }
  })  
  
  # plot to the plot panel
  output$hDotPlot.plot <- renderPlot({
    df <-dataInput()
    if (is.null(df)) return(NULL)
    if(input$treatment != ' ' & 
       is.numeric(df[[input$response]])){
      print(plotInput()$gg)
    }
  })  
  
  output$downloadPlot = downloadHandler(
    filename = function() {"HarrellPlot.pdf"},
    
    content = function(filename) {
      df <-dataInput()
      if (is.null(df)) return(NULL)
      ggsave(filename, plot = plotInput()$gg, width=input$width, height=input$height, device = "pdf")
    }
    
  )
  
  
  output$download_gg_contrasts <- downloadHandler(
    filename = function() {
      'contrasts.RDS'
    },
    content = function(con) {
      df <-dataInput()
      if (is.null(df)) return(NULL)
      res <- plotInput()
      saveRDS(res$gg_contrasts, con)
    }
  )
  
  output$download_gg_treatments <- downloadHandler(
    filename = function() {
      'treatments.RDS'
    },
    content = function(con) {
      df <-dataInput()
      if (is.null(df)) return(NULL)
      res <- plotInput()
      saveRDS(res$gg_treatments, con)
    }
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)

