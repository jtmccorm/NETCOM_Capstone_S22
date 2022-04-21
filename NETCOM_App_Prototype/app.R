# author: jtmccorm

# Shiny
library(shiny)
library(shinydashboard)
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

## B. Download files of interest --------------------
### I. Access Bucket -----------------------------
bucket <- get_bucket(bucket="xstream-capstone")
### II. Download all '/data' files in the bucket ----
read_all_csvs(bucket, "data") 

## C. Clean and Prep data -------------------------



# 1. User Interface ===================================================

## A. Create UI Theme -------------------------------------------------
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

## B. Application Header-----------------------------------------------
header <- dashboardHeader(title = "Anomaly Dashboard")

## C. Dashboard Sidebar------------------------------------------------
sidebar <-dashboardSidebar(
  sidebarMenu(
    id='tabs',
    ### I. Menu Items ------------------------------------------------
    menuItem("Explore Anomalies", icon=icon("eye"), tabName = "ranks"),
    menuItem("Evaluate Models", icon=icon("line-chart", tabName="models")),
    menuItem("Full Dataset", icon=icon("table"), tabName = "table")
  )
)

## D. Dashboard Body---------------------------------------------------
body <- dashboardBody(use_theme(my_theme),
             )

## E. UI Setup---------------------------------------------------------
ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {


}

# Run the application 
shinyApp(ui = ui, server = server)
