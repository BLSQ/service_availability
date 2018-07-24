  library(dplyr)
  library(stringr)
  library(data.table)

  
aggregate_time_space <- function(data_df, DE_id_vect, period_vect, OU_metadata_DSinfo_df, DS_metadata_df, CC_metadata_df, level="province") {
  
  data_df <- as.data.table(data_df)
  
  data_out_province <- NULL
  data_out_district <- NULL
  
  for(id in DE_id_vect) {   
    
    OU_DE_period_exp <- build_OU_period_exp_DElevel(id, OU_metadata_DSinfo_df, DS_metadata_df, CC_metadata_df, period_vect)
    OU_DE_period_exp <- as.data.table(OU_DE_period_exp)
    
    ## COMPARE AVAILABLE DATA WITH EXPECTATION (DATAELEMENT lEVEL)
    
    data_DE <- data_df[dataElement==id, c("period", "orgUnit", "categoryOptionCombo", "value")]
    data_DE$period <- as.character(data_DE$period)
    
    OU_DE_period_expvsdata <- merge(OU_DE_period_exp, data_DE, by.x = c("OU_id", "period", "CatComboOpt_id"), by.y = c("orgUnit", "period", "categoryOptionCombo"), all.x = T)
    
    
    
    #################################################################### DATAELEMENT COMPLETENESS AND ANALYSIS
    
    
    ## Summary over time (facility level) --> sum aggregation
    
    # Aggregate Cat combos
    OU_DE_period_expvsdata_agg <- OU_DE_period_expvsdata[, .(value_sum = sum(value, na.rm = T),
                                                             all_NA = all(is.na(value))),
                                                         by=c("DE_id", "DE_name", "categoryCombo.id", "OU_id","OU_name", "OU_level", "parent.name", "parent.id",
                                                              "parent.parent.name", "parent.parent.id", "parent.parent.parent.name", "parent.parent.parent.id", 
                                                              "parent.parent.parent.parent.name", "parent.parent.parent.parent.id", "period")]
    OU_DE_period_expvsdata_agg$value_sum[OU_DE_period_expvsdata_agg$all_NA] <- NA
    
    # Aggregate periods
    summary_t_OUlevel <- OU_DE_period_expvsdata_agg[, .(periods = length(value_sum), 
                                                        periods_nonNA = length(value_sum[which(!all_NA)]), 
                                                        periods_nonzero = length(value_sum[which(value_sum!=0)]),
                                                        mean = mean(value_sum, na.rm=T),
                                                        mean_zeroexcl = mean(value_sum[which(!is.na(value_sum) & value_sum!=0)])),
                                                    by=c("DE_id", "DE_name", "categoryCombo.id", "OU_id","OU_name", "OU_level", "parent.name", "parent.id",
                                                         "parent.parent.name", "parent.parent.id", "parent.parent.parent.name", "parent.parent.parent.id", 
                                                         "parent.parent.parent.parent.name", "parent.parent.parent.parent.id")]
    
    summary_t_OUlevel$mean[is.nan(summary_t_OUlevel$mean)] <- NA
    summary_t_OUlevel$mean_zeroexcl[is.nan(summary_t_OUlevel$mean_zeroexcl)] <- NA
    
    
    
    ## Summary (province level for FOSAs)
    
    summary_t_OUlevel_FOSAs <- summary_t_OUlevel[OU_level==5]
    
    summary_t_Provincelevel <- summary_t_OUlevel_FOSAs[, .(periods = mean(periods, na.rm=T), 
                                                           periods_nonNA = round(mean(periods_nonNA[periods_nonNA!=0], na.rm=T),2), 
                                                           periods_nonzero = round(mean(periods_nonzero[periods_nonzero!=0], na.rm=T),2),
                                                           facilities = length(periods),
                                                           facilities_available_data = length(periods_nonNA[which(periods_nonNA>0)]),
                                                           facilities_nonzero_data = length(periods_nonzero[which(periods_nonzero>0)]),
                                                           facilities_atleast3_nonzero_data = length(periods_nonzero[which(periods_nonzero>2)]),
                                                           sum_patients = as.integer(round(sum(mean, na.rm = T))),
                                                           mean_patients = round(mean(mean, na.rm=T),2),
                                                           mean_patients_zeroexcl = round(mean(mean_zeroexcl, na.rm=T), 2)),
                                                       by=c("DE_id", "DE_name", "categoryCombo.id", "parent.parent.parent.name", "parent.parent.parent.id")]
    
    summary_t_Provincelevel <- summary_t_Provincelevel %>% rename(Province = "parent.parent.parent.name", Province_id = "parent.parent.parent.id")
    
    
    ## Summary (district level for FOSAs)
    
    summary_t_Districtlevel <- summary_t_OUlevel_FOSAs[, .(periods = mean(periods, na.rm=T), 
                                                           periods_nonNA = round(mean(periods_nonNA[periods_nonNA!=0], na.rm=T),2), 
                                                           periods_nonzero = round(mean(periods_nonzero[periods_nonzero!=0], na.rm=T),2),
                                                           facilities = length(periods),
                                                           facilities_available_data = length(periods_nonNA[which(periods_nonNA>0)]),
                                                           facilities_nonzero_data = length(periods_nonzero[which(periods_nonzero>0)]),
                                                           facilities_atleast3_nonzero_data = length(periods_nonzero[which(periods_nonzero>2)]),
                                                           sum_patients = as.integer(round(sum(mean, na.rm = T))),
                                                           mean_patients = round(mean(mean, na.rm=T),2),
                                                           mean_patients_zeroexcl = round(mean(mean_zeroexcl, na.rm=T), 2)),
                                                       by=c("DE_id", "DE_name", "categoryCombo.id", "parent.parent.parent.name", "parent.parent.parent.id", "parent.parent.name", "parent.parent.id")]
    
    summary_t_Districtlevel <- summary_t_Districtlevel %>% rename(Province = "parent.parent.parent.name", Province_id = "parent.parent.parent.id",
                                                                  District = "parent.parent.name", District_id = "parent.parent.id")
    
    
    data_out_province <- rbind(data_out_province, summary_t_Provincelevel)
    data_out_district <- rbind(data_out_district, summary_t_Districtlevel)
  }
  
  if(level=="province"){return(data_out_province)}
  if(level=="district"){return(data_out_district)}
}