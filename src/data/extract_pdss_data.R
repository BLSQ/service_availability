library(dhisextractr)
library(plyr)
library(RCurl)
library(jsonlite)

load_env()

data_elements <- read.csv(paste0(pdss_data_dir, '/data_elements_metadata.csv'))
org_units_reports <- read.csv(paste0(pdss_data_dir, '/org_units_report.csv'))
data_elements_sets <- read.csv(paste0(pdss_data_dir, '/data_elements_sets.csv'))
org_units_metadata <- read.csv(paste0(pdss_data_dir, '/org_units_description.csv'))


## DEFINE WHAT WE ARE LOOKING FOR
data_elements_vaccins <- look_up_data_element_term('13.1')
data_elements_bcg <- look_up_data_element_term('bcg')
data_sets_vaccins <- look_up_data_set_term('13.1')
data_sets_bcg <- look_up_data_set_term('bcg')
org_units_bcg <- get_de_reporting_facilities(data_sets_bcg)

## Extracting the data
data <- extract_all_data(pdss_url, data_elements_bcg, org_units_bcg, '201601', '201712', 
                         pdss_login, pdss_password, 
                         type_extract = 'de', pace = 100)

data_full <- merge(data, data_elements, by.x = 'data_element_ID', by.y =  'id', all.x = TRUE , all.y = FALSE  )
data_full$name <- as.character(data_full$name)
data_full$value <- as.numeric(as.character(data_full$value))

write.csv('data/raw/pdss_bcg_data.csv')

### Aggregating Fosa at facility level for future reference
nat <- 'pL5A7C1at1M'
prov <- as.character(org_units_metadata$id[org_units_metadata$parent == nat])
zone <-  as.character(org_units_metadata$id[(org_units_metadata$parent %in% prov) &
                                              !is.na(org_units_metadata$parent)])

data_zone <- data.frame(zone=c(), units = c())
for(i in seq(1, length(zone))){
  unit <- zone[i]
  n2 <- 0
  n1 <- 1
  while(n2 < n1){
    n2 <- n1
    sub_unit <- as.character(org_units_metadata$id[org_units_metadata$parent %in% unit])
    unit <- unique(c(unit, sub_unit))
    n1 <- length(unit)
  }
  if(length(unit) > 1){
    units_zone <- data.frame(zone = zone[i], units = unique(unit))
  }
  if(length(unit) == 1){

    units_zone <- data.frame(zone = zone[i], units = NA)
  }
  data_zone <- rbind(data_zone, units_zone)
}

write.csv(data_zone, 'data/references/pdss_zones.csv', row.names = FALSE)
