# author: jtmccorm

# Shiny
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(fresh)
library(shinyjs)
library(DT)
# AWS
library(aws.s3)
library(aws.signature)
# Analysis
library(tidyverse)
library(plotly)

# 0. Load Data From AWS ===============================================

# TODO: Must be changed based on NETCOM's access keys
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZYX2477HJNE7XMUG",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-2"
)

## A. Helper fxns ---------------------------------------
read_all_csvs <- function(bucket, folder){
  # This is a simple function to pull all the csv's from a folder in an S3 bucket 
  files <- vector("character", length(bucket))
  # loop through all files
  for (b in 1:length(bucket)){
    # pull the objects name
    object_key <- bucket[[b]]$Key
    # check if object is in directory
    if (str_detect(object_key, str_c("^",folder,"/."))){
      # pull the objects 'true' name
      name <- object_key %>% 
        str_replace(., str_c("^", folder,"/"),"") %>%
        str_replace(., ".csv$","")
      # assign that name to the overall environment
      assign(name, s3read_using(FUN = read_csv,
                                object = str_c("s3://xstream-capstone/",object_key)),
             inherits = TRUE)
    }
  }
}

normalize_SHAP <- function(scores, SHAP_df){
  # Normaize (as in fits to normal distribution centered at zero) SHAP values
  assertthat::are_equal(mean(scores), SHAP_df$baseline[1], tol=0.0001)
  stn_dev <- sd(scores)
  
  SHAP_df %>%
    mutate(across(.cols=everything(),
                  .fns= ~ (.x/stn_dev))) %>%
    select(-baseline)
}

## B. Download files of interest --------------------
### I. Access Bucket -----------------------------
bucket <- get_bucket(bucket="xstream-capstone")
### II. Download all '/data' files in the bucket ----
read_all_csvs(bucket, "data") 

## C. Clean and Prep data -------------------------
### I. Create 'Display' Dataset ------------------
# clean the primary 'display' data
display_df <- enriched_altIP %>% select(-`...1`, -`Unnamed: 0`)

# get a list of numeric/ small categoricals
display_feats <- c('iForest', 'xStream', 'lof',
                   'ocsvm','ensemble','virus_total_mal_count',
                   'virus_total_sus_count','VT_SCORE','count',
                   'unique_IP_count','CERT_COUNT','CERT_VALID_DAYS',
                   'NUM_ACTIVE_CERTS','NUM_EXPIRED_CERTS',
                   'CERT_AUTHORITY','WHOIS_CC','WHOIS_DOMAIN_AGE')

#TODO  remove extraneous df's
# remove(enriched_altIP, CleanEnrichedData)

### II. Normalize SHAP Values from all  algorithms ----
SHAPiforest <- normalize_SHAP(EnrichedScoreData$iforest_scores, SHAPiforest)
SHAPlof <- normalize_SHAP(EnrichedScoreData$lof_scores, SHAPlof)
SHAPxstream <- normalize_SHAP(EnrichedScoreData$xstream_scores, SHAPxstream)
SHAPocsvm <- normalize_SHAP(EnrichedScoreData$ocsvm_scores, SHAPocsvm)

#TODO  remove unneeded df 
# remove(EnrichedScoreData)

### III. Assign identifier to all rows of data ----
SHAPiforest['INDICATOR'] <- display_df['INDICATOR']
SHAPxstream['INDICATOR'] <- display_df['INDICATOR']
SHAPlof['INDICATOR']     <- display_df['INDICATOR']
SHAPocsvm['INDICATOR']   <- display_df['INDICATOR']

print('Data Prep Complete')

# 1. User Interface ===================================================

## A. Helper Fxns -----------------------------------------------------

feature_panel <- function(model_title, color){
  # Function to procedurally generate inputs for each model
  # first pull all feature names
  feat_names <- dplyr::select(SHAPiforest, -INDICATOR) %>%
    names() %>% 
    str_remove_all(., "__.*$") %>%
    unique()
  feat_1 <- split(feat_names, rep(c(1,2), 9))$`1`
  feat_2 <- split(feat_names, rep(c(1,2), 9))$`2`
  
  # build the feature panel
  conditionalPanel(
    condition = str_c("input.model == '", model_title,"'"),
    h4(str_c(model_title, ": Feature Weights"), align = 'center'),
    fluidRow(align = 'center',
    actionBttn(str_c(model_title,"_rwt"),
               icon = icon("code", lib = "font-awesome"),
               label = "Calculate Scores",
               color = color,
               size = 'sm',
               style = 'unite'),
    actionBttn(str_c(model_title, "_reset"),
               icon = icon("retweet", lib = "font-awesome"),
               label = "Reset Weights",
               color = color,
               size = 'sm',
               style = 'unite')
    ),
    # create first column of sliders
    column(width=6,
           lapply(feat_1, function(feat) {
             sliderInput(inputId = str_c(model_title, "_", feat),
                         label = feat,
                         min = 0, max = 1.5, value = 1,
                         step = 0.25, ticks=F)
           })),
    # create second column of sliders
    column(width=6,
           lapply(feat_2, function(feat) {
             sliderInput(inputId = str_c(model_title, "_", feat),
                         label = feat,
                         min = 0, max = 1.5, value = 1,
                         step = 0.25, ticks=F)
           })),
    
    
  )
}

model_switch <- function(model, status, icon){
  # creates shinyWidget::materialSwitch for models
  fluidRow(
    icon(icon, lib = "glyphicon"),
    materialSwitch(
      inputId = str_c(model, "_include"),
      label = model, 
      value = TRUE,
      status = status)
  )
}

eda_ui <- function(width){
  # function to allow the EDA UI to dynamically respond to developer mode
  column(width = width,
         fluidRow(
           # Inputs
           box(width=4,
               # Plot type
               selectInput("plotType",
                           "Distribution to Explore",
                           choices = c("Univariate", "Bivariate"),
                           selected = "Univariate"),
               # X Variable Selector
               selectInput("feat_x",
                           "Selected Feature (x)",
                           choices = display_feats,
                           selected = "CERT_AUTHORITY"),
               # Y Variable Selector
               conditionalPanel(
                 condition = "input.plotType == 'Bivariate'",
                 selectInput("feat_y",
                             "Selected Feature (y)",
                             choices = display_feats,
                             selected = "ensemble")
               )
           ),
           # Explanation
           box(width=8, 
               h4(strong("Evaluate and Modify Models"), align = 'center'),
               p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."))
         ),
         # Plot 
         fluidRow(
                 box(width = 12,
                     h4(strong("Explore the Data"), align='center'),
                     plotlyOutput("eda_plot", height = "650px")))
  )
}

anomaly_exp <- function(){
  # SHAP plots
      tabBox(width=12,
             tabPanel(title = p(icon("tree-conifer", lib="glyphicon"), 
                                "iForest"),
                      plotlyOutput("SHAP_iForest",
                                   height = "500px")),
             tabPanel(title = p(icon("random", lib="glyphicon"),
                                'xStream'),
                      plotlyOutput("SHAP_xStream",
                                   height = "500px")),
             tabPanel(title = p(icon("sunglasses", lib="glyphicon"),
                                'LOF'),
                      plotlyOutput("SHAP_lof",
                                   height = "500px")),
             tabPanel(title = p(icon("dashboard", lib="glyphicon"),
                                'OCSVM'),
                      plotlyOutput("SHAP_ocsvm",
                                   height = "500px"))
             )
      
}

action_btns <- function(){
  # Action buttons
  column(width = 12,
      actionBttn('mark', label="Investigate",
                    style = "unite",
                    color = 'primary'),
      p(),
      actionBttn('ignore', label="Ignore",
                    style = "unite",
                    color = 'royal')
  )
}

## B. Create UI Theme -------------------------------------------------
# drawn from example at https://github.com/dreamRs/fresh
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    dark_bg = "#2E3440",
    dark_hover_bg = "#81A1C1",
    dark_color = "#FFF"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)

## C. Application Header-----------------------------------------------
header <- dashboardHeader(title = "Anomaly Detection")

## D. Dashboard Sidebar------------------------------------------------
sidebar <-dashboardSidebar(
  sidebarMenu(
    id='tabs',
    ### I. Tab Selections ------------------------------------------------
    menuItem("Explore Anomalies", icon=icon("eye"), tabName = "ranks"),
    menuItem("Evaluate Models", icon=icon("line-chart"), tabName="eval"),
    menuItem('About', icon = icon("question"), tabName = 'about'),
    
    ### II. Developer Access  -----------------------------------------
    box(width = 12,
        background = 'navy',
        # Header for section
        h4(strong("Developer Access"), align = 'center'),
        
        # credentials text inputs
        p("Enter your credentials below:"),
        
        textInput("user","Username"),
        
        passwordInput("password", "Password"),
        
        actionBttn("dev_cred", "Enter", 
                   style = 'simple'),
        
        # Responds to button via observe event
        fluidRow(align = 'center',
          textOutput("dev_display"),
          # developer mode switch
          conditionalPanel(
            condition = "output.dev_display == 'Access Granted!'",
            switchInput(
              inputId = "dev_access",
              label = strong("Developer Mode"),
              onStatus = "success", 
              offStatus = "danger",
              labelWidth = "100px"
            )
          )
        )
      )
  )
)

## E. Dashboard Body---------------------------------------------------
body <- dashboardBody(use_theme(my_theme),
        tabItems(
          ### I. Explore Anomaly Tab -----------------------------------
          tabItem("ranks",
                  fluidPage(
                    
                    tags$head(tags$style("#main_dt {white-space: nowrap;}")),
                    #### a. Ranked Anomaly Table -------------
                    fluidRow(box(width=10,
                               h4(strong("Ranked Anomalies"), align='center'),
                               DT::dataTableOutput("main_dt")),

                                    #### b. How to Use -------------
                                    box(width=2, align = 'center',
                                        h4(strong("How To Use")),
                                        p("This app provides interactive and interpretable anomaly scoring for new websites visited on the enterprise network. Click on an observation to gain insights on why it may be an anomaly.", align='left'),
                                     #### b. Take Action ------
                                        h5(strong("Mark for Future Action")),
                                        p("If an observation seems suspicious you can mark it for future investigation. If it appears insignificant, you can dismiss it.", align='left'),
                                     uiOutput("mark_btns"))
                             ),
                    fluidRow(
                             #### d. Ensemble score distribution -----
                             column(width = 6,
                                    box(width=12,
                                        h4(strong("Score Distribution"), align = 'center'),
                                        p("This density plot shows the distribution of our final anomaly scores produced by", strong("mean-ensembling") ,"of the four tailored models."),
                                        plotlyOutput("score_dist",
                                                     height = "250px")),
                                    box(width=12,
                                        h4(strong("Mute Features"), align = 'center'))),
                             #### e. SHAP plots ------
                             box(width =6,
                                 h4(strong("Anomaly Explanation"), align='center'), 
                                 p("SHAP plots show the 15 most significant features for each of the models used to score this observation.", strong("Feature Significance "),"in shapley calculations is determined in comparison to a global average (the baseline) of each feature."),
                             uiOutput("anomaly_exp")
                            )
                          ))),
          ### II. Model Evaluation -------------------------------------
          tabItem("eval",
                  fluidPage(
                    #### a. EDA Plots-------------------
                    # dynamically rebuild to provide room for model controls
                    uiOutput("eda_ui"),
                    #### b. Model Controls -------------------
                    # must be conditional panel to ensure inputs always exist (just hidden)
                    conditionalPanel( 
                      condition = "input.dev_access == true",
                      column(width = 5,
                          box(width=12, 
                            h4(strong("Model Controls"), align='center'),
                            ##### i. Ensemble Controls ----
                            p("Choose which models will be retained in the final ensemble:"),
                            fluidRow(
                              column(width=2),
                              column(width = 4, 
                                     model_switch("iForest","success", "tree-conifer"),
                                     model_switch("xStream","danger", "random")),
                              column(width = 4,
                                     model_switch("LOF", "info", "sunglasses"),
                                     model_switch("OCSVM", "warning", "dashboard")),
                              column(width=2)
                            ),
                            ##### ii. Model Selection -------
                            p("Select a model below to modify the weights ansigned to each feature."),
                            fluidRow(align='center',
                              pickerInput('model', 
                                          label = 'Model: ',
                                          choices = c('iForest', 'xStream',
                                                      'LOF', 'OCSVM', 'None'),
                                          selected = 'None',
                                          choicesOpt = list(
                                            icon = c("glyphicon-tree-conifer", 
                                                     "glyphicon-random", 
                                                     "glyphicon-sunglasses", 
                                                     "glyphicon-dashboard", 
                                                     "glyphicon-option-horizontal")
                                          ),
                                          width = '60%',
                                          inline=TRUE)
                              ),
                          ##### iii. Feature Weight Selection ----
                            feature_panel('iForest', "success"),
                            feature_panel('xStream', "danger"),
                            feature_panel('LOF', "primary"),
                            feature_panel('OCSVM', "warning"),
                            setSliderColor(color = c(rep("#00A65A", 18), 
                                                     rep("#DD4B39", 18), 
                                                     rep("#00C0EF", 18), 
                                                     rep("#F39C12", 18)),
                                          sliderId = c(1:72))
                          )
                          )
                    )
                      )
                  ),
          ### III. About -------------------------------------------------
          tabItem("about",
                  box(width =7,
                      h4(strong('About'), align='center'),
                      tabBox(width=12, 
                             tabPanel("Project",
                                      p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")),
                             tabPanel("Contributors",
                                      p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")))
                      )
                  )
        )
)


## F. UI Setup---------------------------------------------------------
ui <- dashboardPage(header, sidebar, body,
                    title = "NETCOM-DSD: Anomaly Detection")

# 2. Server ===============================================

## A. Helper Fxns -----------------------------------------
feat_weights <- function(model_title, input){
  
  # function to extract feature weights from slider Inputs in Model Controls
    feat_dict <- dplyr::select(SHAPiforest, -INDICATOR) %>%
         names() %>% 
         str_remove_all(., "__.*$") %>% tibble(var = factor(.)) %>%
         count(var)
    
    feat_names <- feat_dict$var %>% as.character()
    feat_count <- feat_dict$n
    
    this <- lapply(1:18, function(i){
      rep(input[[str_c(model_title, "_", feat_names[i])]], feat_count[i])
    })
    

    return(unlist(this))
}

reweight_SHAP <- function(SHAP_df, feat_weights){
  # function to produce a re-weighted SHAP data.frame
  rwtd_SHAP_df <- SHAP_df %>%
                dplyr::select(-INDICATOR) %>%
                dplyr::select(order(current_vars())) %>%
                tibble()

  for (i in 1:ncol(rwtd_SHAP_df)){
    rwtd_SHAP_df[i] <- rwtd_SHAP_df[i] * feat_weights[i]
  }

  rwtd_SHAP_df['INDICATOR'] <- SHAP_df['INDICATOR']
  return(rwtd_SHAP_df)
}

shap_plotly <- function(SHAP_df, ind_select){
  SHAP_df <- SHAP_df %>%
    dplyr::filter(INDICATOR == ind_select) %>%
    dplyr::select(-INDICATOR) 
  
  value <- SHAP_df %>% rowSums()
  
  p <- SHAP_df %>%
    pivot_longer(cols = everything(), 
                 names_to = "variable", 
                 values_to = "shap_value") %>%
    arrange(desc(abs(shap_value))) %>% head(15) %>%
    ggplot(aes(y = fct_reorder(variable, abs(shap_value)), 
               x = shap_value, 
               fill = factor(sign(shap_value)))) +
    geom_vline(xintercept = 0, lty=2) +
    geom_col() +
    scale_fill_manual(values = c("-1"='#3384E5', "1"='#F62E56')) +
    scale_y_discrete(labels = function(x) {
        x %>% str_replace_all(., "_", " ") %>%
              str_replace(., "  ", ": ") %>%
              str_wrap(., width=30)
      }) +
    labs(y="",
         x="SHAP Values") + 
    theme_classic(base_size = 10) + 
    theme(legend.position = "none",
          plot.title = element_text(hjust=-1)) 
  
  ggplotly(p, tooltip = 'x') %>%
    config(displayModeBar = FALSE) %>%
    layout(title = list(text = str_c("<b>",ind_select,": ",round(value, 4),"</b>")),
                        font = list(size = 10))
}

reset_weights <- function(session, model_title){
  # function to reset the weights of a specific model
  # first pull all feature names
  feat_names <- dplyr::select(SHAPiforest, -INDICATOR) %>%
    names() %>% 
    str_remove_all(., "__.*$") %>%
    unique()
  
  # update slider inputs
  for (feat in feat_names){
    updateSliderInput(session, str_c(model_title, "_", feat), value=1)
  }
}

## B. Main Fxn --------------------------------------------
server <- function(input, output, session) {
  ### I. Data Reactive -------------------
  #### a. Re-weighted SHAP df's ----------
  rwtd_SHAPiForest <- eventReactive(input$iForest_rwt,
                                    ignoreNULL = F,{
    reweight_SHAP(SHAPiforest,
                  feat_weights("iForest", input))
  })
  
  rwtd_SHAPxStream <- eventReactive(input$xStream_rwt,
                                    ignoreNULL = F, {
    reweight_SHAP(SHAPxstream,
                  feat_weights("xStream", input))
  })
  
  rwtd_SHAPlof <- eventReactive(input$LOF_rwt, 
                                ignoreNULL = F, {
    reweight_SHAP(SHAPlof,
                  feat_weights("LOF", input))
  })
  
  rwtd_SHAPocsvm <- eventReactive(input$OCSVM_rwt,
                                  ignoreNULL = F, {
    reweight_SHAP(SHAPocsvm,
                  feat_weights("OCSVM", input))
  })
  
  #### b. Main 'Display' Data Frame ----------
  main_df <- reactive({
    # get scores from reweighted SHAP values
    iForest <- dplyr::select(rwtd_SHAPiForest(), -INDICATOR) %>% rowSums()
    xStream <- dplyr::select(rwtd_SHAPxStream(), -INDICATOR) %>% rowSums()
    lof <- dplyr::select(rwtd_SHAPlof(), -INDICATOR) %>% rowSums()
    ocsvm <- dplyr::select(rwtd_SHAPocsvm(), -INDICATOR) %>% rowSums()
    
    # build the ensemble score
    ensemble <- (iForest * input$iForest_include +
                       xStream * input$xStream_include +
                       lof * input$LOF_include +
                       ocsvm * input$OCSVM_include) / sum(input$iForest_include,
                                                          input$xStream_include,
                                                          input$LOF_include,
                                                          input$OCSVM_include)
    
    # combine with display_df
    display_df %>%
      add_column(xStream,
                 iForest,
                 lof,
                 ocsvm,
                 ensemble) %>%
      arrange(desc(ensemble))
  })
  
  ### II. UI Outputs ---------------------
  #### a. Dynamic UI Structure ----------
  # render UI for eda plots
  output$eda_ui <- renderUI({
    if (input$dev_access){
      eda_ui(7)
    } else{
      eda_ui(10)
    }
  })
  
  # Has the user selected an item in the Data table
  select_ind <- reactive({
    main_df()[input$main_dt_rows_selected, ] %>% .[['INDICATOR']]
  })
  
  # Render UI for 
  output$anomaly_exp <- renderUI({
      if (length(input$main_dt_rows_selected)){ anomaly_exp() }
    })
  output$mark_btns <- renderUI({
    if (length(input$main_dt_rows_selected)){ action_btns() }
    })
  
  # Reset feature weights
  observeEvent(input$iForest_reset, {
      reset_weights(session, "iForest")
  })
  observeEvent(input$xStream_reset, {
    reset_weights(session, "xStream")
  })
  observeEvent(input$LOF_reset, {
    reset_weights(session, "LOF")
  })
  observeEvent(input$OCSVM_reset, {
    reset_weights(session, "OCSVM")
  })
  
  #### b. Developer Access ---------------
  # Are the credentials correct?
  dev_cred <- eventReactive(input$dev_cred,{
    input$user == "slartibartfast" & input$password == "42"
  })
  
  # If so, update the text.
  output$dev_display <- renderText({
    if (dev_cred()){
      "Access Granted!"
    } else{
      "Access Denied."
    }
  })
  
  #### c. Data Tables ----------------
  output$main_dt <- DT::renderDataTable({
    main_df() %>% 
      select(INDICATOR, iForest, xStream, 
             lof, ocsvm, ensemble, everything()) %>%
      select(-CERT, -GN_IP_SRC) %>%
      mutate(across(where(is.numeric), round, 4))
  }, selection = 'single',
     options   = list(scrollX = TRUE,
                      scrollY = TRUE))
  
  #### d. Plots ------------------
  ##### i . SHAP plots ----
  output$SHAP_iForest <- renderPlotly({
    shap_plotly(rwtd_SHAPiForest(), select_ind())
  })
  output$SHAP_xStream <- renderPlotly({
    shap_plotly(rwtd_SHAPxStream(), select_ind())
  })
  output$SHAP_lof <- renderPlotly({
    shap_plotly(rwtd_SHAPlof(), select_ind())
  })
  output$SHAP_ocsvm <- renderPlotly({
    shap_plotly(rwtd_SHAPocsvm(), select_ind())
  })
  
  ##### ii. Score Distribution -----
  output$score_dist <- renderPlotly({
    p <- ggplot(data = main_df(),
                aes(x = ensemble))+
            geom_density(col = "#2E3440",
                         fill = "#B0BED9", adjust=0.5) +
            theme_bw()
    
    if (length(input$main_dt_rows_selected)){
        val <- main_df() %>% filter(INDICATOR == select_ind()) %>%
          .[['ensemble']]
        
        p <- p + geom_vline(xintercept = val, col = 'firebrick', lty=2)
    }
    
    ggplotly(p)%>%
      config(displayModeBar = FALSE)
  })
  
  ##### iii. EDA plot ------
  output$eda_plot <- renderPlotly({
    # manipulate the data to only plottable features
    this <- main_df() %>%
          select(c(display_feats, INDICATOR)) %>%
          mutate(CERT_AUTHORITY = factor(str_wrap(CERT_AUTHORITY, 5)),
                 WHOIS_CC = factor(str_wrap(str_replace(WHOIS_CC,",.*$",""), 5)))
  
    
    if (input$plotType == "Univariate"){
      # if Univariate continuous produce histogram
      if (input$feat_x %in% c("CERT_AUTHORITY", "WHOIS_CC")){
        p<-ggplot(data=this, aes_string(x = str_c("`",input$feat_x,"`"), 
                                        text = "INDICATOR")) +
          geom_histogram(stat='count', fill = "#434C5E", bins = 22) + theme_bw()
      } else {
        p<-ggplot(data=this, aes_string(x = str_c("`",input$feat_x,"`"), 
                                 text = "INDICATOR")) +
            geom_histogram(fill = "#434C5E", bins = 22) + theme_bw()
      }
    } else{
      # if bivariate produce jitter
      p<-ggplot(data=this, aes_string(x = str_c("`",input$feat_x,"`"), 
                                      y = str_c("`",input$feat_y,"`"), 
                                      text = "INDICATOR")) +
            geom_jitter(color = "#434C5E") + theme_bw()
    }
    
    # turn into plotly
    ggplotly(p, tooltip = 'text') %>%
      config(displayModeBar = FALSE) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
