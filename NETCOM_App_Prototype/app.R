# author: jtmccorm

# Shiny
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(fresh)
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

feature_panel <- function(model_title){
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
           box(width=4, selectInput("plotType",
                                    "Distribution to Explore",
                                    choices = c("Univariate", "Bivariate"),
                                    selected = "Univariate"),
           ),
           # Explanation
           box(width=8, 
               h4(strong("Evaluate and Modify Models"), align = 'center'),
               p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."))
         ),
         # Plot 
         fluidRow(
           box(width = 12,
               h4(strong("Some Plot"), align='center')))
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
                    #### a. Ranked Anomaly Table -------------
                    column(width =7,
                           box(width=12,
                               h4(strong("Ranked Anomalies"), align='center'))
                    ),
                    column(width=5,
                     
                      #### b. SHAP plots -----------------------
                      box(width =12,
                          h4(strong("Feature Significance"), align='center'), 
                          tabBox(width=12,
                             tabPanel(title = p(icon("tree-conifer", lib="glyphicon"), 
                                        "iForest")),
                             tabPanel('xStream')
                                )
                          ),
                      #### c. Action buttons --------------------------
                      box(width=12, align = 'center',
                          h4(strong("Action Steps"), align ='center'),
                          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."),
                          actionBttn('mark', label="Investigate",
                                     style = "unite",
                                     color = 'danger'),
                          actionBttn('ignore', label="Ignore",
                                     style = "unite",
                                     color = 'warning')
                          )
                        )
                      )
                    ),
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
                            feature_panel('iForest'),
                            feature_panel('xStream'),
                            feature_panel('LOF'),
                            feature_panel('OCSVM'),
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
server <- function(input, output) {
  # EDA User Interface updates
  output$eda_ui <- renderUI({
    if (input$dev_access){
      eda_ui(7)
    } else{
      eda_ui(10)
    }
  })
  
  dev_cred <- eventReactive(input$dev_cred,{
    input$user == "slartibartfast" & input$password == "42"
  })
  
  output$dev_display <- renderText({
    if (dev_cred()){
      "Access Granted!"
    } else{
      "Access Denied."
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
