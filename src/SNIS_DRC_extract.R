
  library(devtools)
  install_github("grlurton/dhisextractr")
  
  library(dhisextractr)
  ls("package:dhisextractr")
  library(dplyr)
  library(stringr)
  library(data.table)
  library(ggplot2)
  
  #################################################################### METADATA
  
  ## LOAD ENVIRONMENT (Set working directory)
  
  load_env("env1")
  
  ## EXTRACT LIST OF METADATA FROM API
  
  d <- extract_metdata(url=BASE_URL, userID = USERID, password = PASSWORD)

  ## EXTRACT DEG AND DS METADATA
  
  DEG_metadata <- extract_metadata_DEG(d)
  DS_metadata <-  extract_metadata_DS(d)
  
  ## EXTRACT CAT COMBO METADATA
  
  CC_metadata <- extract_metadata_CC(d)
  
  ## EXTRACT ORGUNIT METADATA FROM API
  
  OU_metadata <- extract_metadata_OrgUnit(url=BASE_URL, userID = USERID, password = PASSWORD, list_metdata = d)
  OU_metadata_flat <- flatten_hierarchy(OU_metadata)

  
  ## BUILD TABLE WITH DATASETS AVAILABLE FOR EACH ORGUNIT
  
  OU_metadata_DSinfo <- extract_metadata_DS_OrgUnit(d, OU_metadata)
  
  
  #################################################################### DATA VALUES
  
  ## EXTRACT DATA VALUES
  
  OU_metadata_level5 <- OU_metadata %>% filter(level==5)
  DATA_DIR <- "/Users/adminbluesquarehub.com/Desktop/Rcode/SNIS_DRC/data_SIGL1/"
  extract_all_data(base_url=BASE_URL,
                           data_element_groups="hpAmMbUweNu",
                           org_units=OU_metadata_level5$id, 
                           period=c("201701", "201801"),
                           pace = 30, 
                           userID=USERID, password=PASSWORD,
                           update_date = NULL , type_extract = 'ds',
                           period_type = 'month',
                           data_dir = DATA_DIR)
  
  DATA_DIR <- "/Users/adminbluesquarehub.com/Desktop/Rcode/SNIS_DRC/data_SIGL1/"
  
  ## LOAD DATA VALUES (for a specific dataset)
  
  temp <- list.files(path=DATA_DIR, pattern="*.csv")
  data_2 <- read.csv(paste0(DATA_DIR, temp[290]))
  for(i in 291:length(temp)){
    print(temp[i])
    tmp <- read.csv(paste0(DATA_DIR, temp[i]))
    tmp <- tmp %>% select(X, dataElement, period, orgUnit, categoryOptionCombo, attributeOptionCombo, value, storedBy, created, lastUpdated, followUp)
    data_2 <- rbind(data_2, tmp)
    tmp <- NULL
  }

  saveRDS(data_2, file=paste0(DATA_DIR, "SIGL1_bis.rds"))
  saveRDS(DEG_metadata, file=paste0(DATA_DIR, "DEG_metadata.rds"))
  saveRDS(DS_metadata, file=paste0(DATA_DIR, "DS_metadata.rds"))
  saveRDS(CC_metadata, file=paste0(DATA_DIR, "CC_metadata.rds"))
  saveRDS(OU_metadata, file=paste0(DATA_DIR, "OU_metadata.rds"))
  saveRDS(OU_metadata_DSinfo, file=paste0(DATA_DIR, "OU_metadata_DSinfo.rds"))

  
  temp[290]
    