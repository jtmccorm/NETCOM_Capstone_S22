library(aws.s3)
library(aws.signature)
library(tidyverse)
library(plotly)

# AWS ================================================
# TODO: Must be changed based on NETCOM's access keys
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZYX2477HJNE7XMUG",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-2"
)
# helper fxns ---------------------------------------
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

# download files of interest --------------------
# 1. Access Bucket
bucket <- get_bucket(bucket="xstream-capstone")
# 2. Download all '/data' files in the bucket
read_all_csvs(bucket, "data")

### I. Clean Raw Data for Display -----------------------------------
# clean the primary 'display' data
display_df <- enriched_altIP %>% select(-`...1`, -`Unnamed: 0`)
# remove extraneous df's
remove(enriched_altIP, CleanEnrichedData)

### II. Normalize SHAP values------------------------
normalize_SHAP <- function(scores, SHAP_df){
  assertthat::are_equal(mean(scores), SHAP_df$baseline[1], tol=0.0001)
  stn_dev <- sd(scores)
  
  SHAP_df %>%
    mutate(across(.cols=everything(),
                  .fns= ~ (.x/stn_dev))) %>%
    select(-baseline)
}

# apply to four algorithms
SHAPiforest <- normalize_SHAP(EnrichedScoreData$iforest_scores, SHAPiforest)
SHAPlof <- normalize_SHAP(EnrichedScoreData$lof_scores, SHAPlof)
SHAPxstream <- normalize_SHAP(EnrichedScoreData$xstream_scores, SHAPxstream)
SHAPocsvm <- normalize_SHAP(EnrichedScoreData$ocsvm_scores, SHAPocsvm)

# remove unneeded df
remove(EnrichedScoreData)

### III. Assign identifier to all rows of data
SHAPiforest['INDICATOR'] <- display_df['INDICATOR']
SHAPxstream['INDICATOR'] <- display_df['INDICATOR']
SHAPlof['INDICATOR']     <- display_df['INDICATOR']
SHAPocsvm['INDICATOR']   <- display_df['INDICATOR']

# Display Data ==============================================

# helper fxn for the shapley plots
shap_plot <- function(SHAP_df, ind_select){
  SHAP_df <- SHAP_df %>%
              dplyr::filter(INDICATOR == ind_select) %>%
              dplyr::select(-INDICATOR)
  
  p <- SHAP_df %>%
    pivot_longer(cols = everything(), 
             names_to = "variable", 
            values_to = "shap_value") %>%
    arrange(desc(abs(shap_value))) %>% head(20) %>%
    ggplot(aes(y = fct_reorder(variable, abs(shap_value)), 
               x = shap_value, 
            fill = shap_value>0)) +
      geom_vline(xintercept = 0, lty=2) +
      geom_col() +
      scale_fill_manual(values = c('#3384E5', '#F62E56')) +
      labs(y="Top Features",
           x="SHAP Values") + 
      theme_classic() + theme(legend.position = "none") 
  
  return(p)
}

# Test change to ensure connection
