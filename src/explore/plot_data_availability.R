## First try at plotting data availability along differnet lines

## get unique reporting de
library(dhisextractr)
library(plyr)
library(RCurl)
library(rjson)
library(ggplot2)
library(zoo)
library(stringr)
## Hypothesis : data element is only reported as part of data set

load_env()

data_elements_sets <- read.csv(paste0(snis_data_dir, '/data_elements_sets.csv'))
org_units_report <- read.csv(paste0(snis_data_dir, '/org_units_report.csv'))
data_sets <- read.csv(paste0(snis_data_dir, '/data_sets.csv'))
data_elements_list <- read.csv(paste0(snis_data_dir, '/data_elements_list.csv'))
hierarchy <- read.csv(paste0(snis_data_dir, '/hierarchy.csv'))
data <- read.csv(paste0(snis_data_dir, '/data/data.csv'))

colnames(data)[which(colnames(data) %in% c('dataElement', 'period', 'orgUnit'))] <- c('data_element_id', 
                                                                                      'period', 'org_unit_id')

## Standardize column names => move in extraction
org_units_ex <- org_units_report[(org_units_report$id_report == 'pePpSnKtAh3') & 
                                   (org_units_report$id %in% data$org_unit_id) , ]
colnames(org_units_ex) <- c('org_unit_id' , 'data_set_id')
colnames(data_elements_sets) <- c('data_element_id' , 'data_set_id')


# Make the expected data set => can be stored in a file
data_set <- data_elements_sets[data_elements_sets$data_set_id == 'pePpSnKtAh3' , ]

## build expectations should come from dhisextractr
to_get_full <- build_expectations(org_units_ex, data_set, c(201701,201805))
to_get_full <- merge(to_get_full, hierarchy, by.x = 'org_unit_id', by.y = 'id' , all.x = TRUE)

# Pour in reported data
availability_check <- check_expectations(to_get_full, data)

## Evolution of reporting completeness by organisation unit
report_completeness_prov <- ddply(availability_check , .(period, level_2_name, data_set_id), 
                                  check_report_completeness , .progress="text")

## Order province factors
order_output <- function(data, order_variable, additional_variables, aggr_function){
  aggregated_data <- ddply(data, .variables = c(order_variable, additional_variables), 
                           aggr_function , .progress="text")
  ordered_variable <- aggregated_data[order(aggregated_data$V1), order_variable]
  return(ordered_variable)
}

ordered_province <- order_output(availability_check, 'level_2_name', 'data_set_id', check_report_completeness)

province_completeness <- ddply(availability_check, .(level_2_name, data_set_id), 
                               check_report_completeness , .progress="text")
ordered_province <- province_completeness$level_2_name[order(province_completeness$V1)]
report_completeness_prov$level_2_name <- factor(report_completeness_prov$level_2_name, 
                                                levels = ordered_province, 
                                                ordered = TRUE)


report_completeness_prov$period <- as.yearmon(report_completeness_prov$period, "%Y%m")
province_completeness$period <- report_completeness_prov$period[1]

ggplot(report_completeness_prov, aes(x = period, y = V1)) +
  geom_rect(data = province_completeness, aes(fill = V1), xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 0.3) +
  geom_point(shape = 1) + 
  facet_wrap( ~ level_2_name) +
  theme_bw() + scale_x_yearmon() +
  ylab('Mean Completeness for SIGL Reports') + xlab('') +
  scale_fill_gradient(name = "Mean Province level completeness",
                      low = '#ef0404', high = '#6ddb3f',
                      space = "Lab",
                      na.value = "grey50", guide = "colourbar",
                      limits=c(0,1))+ 
  theme(legend.position = "bottom")


## Check data element availability by org_unit

interesting_cols <- c("data_element_id","period","org_unit_id","data_set_id", "name", "level_2_name", 
                      "level_2_id", "level_3_name", "level_3_id", "level_4_name", "level_4_id", 
                      "value", "created", "lastUpdated")

availability_check <- availability_check[,interesting_cols]
lab_availability_check <- merge(availability_check , 
                            data_elements_list[,c('id','displayName')] ,
                            by.x = 'data_element_id', by.y = 'id' , all = FALSE)

data_element_availability <- ddply(lab_availability_check , 
                                   .(displayName, level_2_name), 
                                   get_reporting_facilities , 
                                   .progress='text')


## Splitting indicator names
data_element_availability$displayName <- str_replace_all(data_element_availability$displayName, '- Poudre', ', Poudre')
data_element_availability$displayName <- str_replace_all(data_element_availability$displayName, '- pièce', ', pièce')
split_names <- str_split_fixed(data_element_availability$displayName, ' - ', 2)
data_element_availability$input <- split_names[,1]
data_element_availability$quantity <- split_names[,2]

dat_sel <- unique(data_element_availability$input[data_element_availability$domain == 'Paludisme'])[1:5]
data_plot <- data_element_availability[data_element_availability$input %in% dat_sel, ]

## Province completeness for graph ordering
province_completeness <- ddply(data_element_availability , 
                               .(level_2_name), 
                               function(x) mean(x$V1),
                               .progress = 'text')
ordered_province <- province_completeness$level_2_name[order(province_completeness$V1)]
data_plot$level_2_name <- factor(data_plot$level_2_name, 
                                                levels = ordered_province, 
                                                ordered = TRUE)

## Making full data set to plot benchmark
province_completeness_full <- province_completeness[rep(rownames(province_completeness), 
                                                   length(unique(data_plot$input)) * 
                                                     length(unique(data_plot$quantity))) ,]
province_completeness_full$input <- rep(unique(data_plot$input), 
                                      length(unique(data_plot$quantity))*nrow(province_completeness))
province_completeness_full$quantity <- sort(rep(unique(data_plot$quantity), 
                                           length(unique(data_plot$input))*nrow(province_completeness)))
province_completeness_full$level_2_name <- factor(province_completeness_full$level_2_name, 
                                 levels = ordered_province, 
                                 ordered = TRUE)

ggplot(data_plot, aes(x = level_2_name, y = V1)) +
  geom_point(data = province_completeness_full, aes(x = level_2_name, y = V1), alpha = .2) +
  geom_point(aes(fill = level_2_name,  col = level_2_name)) +
  facet_grid(quantity ~ input) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        strip.text.x = element_text(size = 6),
        legend.position = "bottom", legend.title = element_text('Provinces')) +
  ylab('Mean Completeness for Data Elements') + xlab('') 



ordered_province <- order_output(availability_check , 
                                 'level_2_name', c(), 
                                 check_data_element_availability)
data_rupture$level_2_name <- factor(data_rupture$level_2_name,
                                    levels = ordered_province,
                                    ordered = TRUE)



de_completeness <- ddply(availability_check, .(data_element_id), 
                         check_data_element_availability)
de_completeness <- merge(de_completeness, data_elements_list[,c('id','displayName2')] ,
                         by.x = 'data_element_id', by.y = 'id' , all = FALSE)
ordered_de <- de_completeness$displayName2[order(de_completeness$V1 , decreasing = TRUE)]

data_rupture <- data_rupture[,c('data_element_id'       ,        'level_2_name'    ,    'V1' )]
data_rupture <- merge(data_rupture, data_elements_list[,c('id','displayName2')] ,
                      by.x = 'data_element_id', by.y = 'id' , all = FALSE)
data_rupture$displayName <- factor(data_rupture$displayName2,
                                    levels = ordered_de,
                                    ordered = TRUE )


ggplot(data_rupture, aes(x = level_2_name, y = V1 )) +
  geom_col(aes( fill = level_2_name)) +
  facet_wrap( ~ displayName) +
  ylab('Mean province reporting for Data Element') +
  theme(legend.position='bottom',
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        strip.text.x = element_text(size = 8))


## Check global data element availability
data_element_availability_global <- ddply(availability_check , 
                                   .(data_element_id), 
                                   check_data_element_availability)

ordered_availability <- data_element_availability_global[order(data_element_availability_global$V1, 
                                                               decreasing = TRUE), ]

ordered_availability <- merge(ordered_availability, data_elements_list,
                              by.x = 'data_element_id' , by.y = 'id' ,
                              all = FALSE, sorted = FALSE)

ordered_de <- unique(data_element_availability$data_element_id[order(data_element_availability$data_set_id)])
data_element_availability$data_element_id = factor(data_element_availability$data_element_id, 
                                                   ordered_de)

ggplot(data_element_availability, aes(x = data_element_id , y = org_unit_id, fill = V1)) + 
         geom_bin2d( ) + 
         scale_fill_gradient(#name = "Mean reported stockout days",
           high = '#ef0404', low = '#6ddb3f',
           space = "Lab",
           na.value = "grey50", guide = FALSE)





