## get unique reporting de

library(dhisextractr)
library(plyr)
library(RCurl)
library(rjson)
library(ggplot2)

## Hypothesis : data element is only reported as part of data set

load_env()

data_elements_sets <- read.csv(paste0(snis_data_dir, '/data_elements_sets.csv'))
org_units_report <- read.csv(paste0(snis_data_dir, '/org_units_report.csv'))
data_sets <- read.csv(paste0(snis_data_dir, '/data_sets.csv'))
data_elements_list <- read.csv(paste0(snis_data_dir, '/data_elements_list.csv'))
hierarchy <- read.csv(paste0(snis_data_dir, '/hierarchy.csv'))

start <- Sys.time()
data <- extract_all_data(base_url = snis_url,
                         org_units = unique(org_units_report$id),
                         data_sets = 'pePpSnKtAh3' ,
                         period = c('201701','201805'),
                         pace = 100 ,cl
                         userID = snis_login, password = snis_password ,
                         type_extract = 'ds', period_type = 'month'
)
stop <- Sys.time()

write.csv(data,file = paste0(snis_data_dir, '/data.csv'))