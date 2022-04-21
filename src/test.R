library(aws.s3)
library(aws.signature)
library(tidyverse)

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

# Display SHAP Values =====================================


### I. Display Data ---------------------
# clean the primary 'display' data
display_df <- enriched_altIP %>% select(-`...1`, -`Unnamed: 0`)
# remove extraneous df's
remove(enriched_altIP, CleanEnrichedData)

### II. Normalize SHAP values
normalize_SHAP <- function(scores, SHAP_df){
  print(mean(scores))
  print(SHAP_df$baseline[1])
  assertthat::are_equal(mean(scores), SHAP_df$baseline[1], tol=0.01)
}

### III. Assign identifier to all rows of data
