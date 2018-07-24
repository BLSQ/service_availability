
  library(dplyr)
  library(stringr)
  library(data.table)
  library(ggplot2)
  library(rgeos)
  
  project_folder <- "/Users/adminbluesquarehub.com/Desktop/Rcode/SNIS_DRC/"
  
  data_1 <- readRDS(file=paste0(project_folder, "SIGL1.rds"))
  data_2 <- readRDS(file=paste0(project_folder, "SIGL1_bis.rds"))
  data_1 <- rbind(data_1, data_2)
  data_1 <- as.data.table(data_1)
  
  DS_metadata <- readRDS(file=paste0(project_folder, "metadata/DS_metadata.rds"))
  DEG_metadata <- readRDS(file=paste0(project_folder, "metadata/DEG_metadata.rds"))
  CC_metadata <- readRDS(file=paste0(project_folder, "metadata/CC_metadata.rds"))
  OU_metadata <- readRDS(file=paste0(project_folder, "metadata/OU_metadata.rds"))
  OU_metadata_flat <- flatten_hierarchy(OU_metadata)
  OU_metadata_DSinfo <- readRDS(file=paste0(project_folder, "metadata/OU_metadata_DSinfo.rds"))
  

    
    #################################################################### EXPECTED VS DATA (DATAELEMENT LEVEL)
  
    ## SELECT DATAELEMENTS
  
    dataElements <- DEG_metadata %>% filter(grepl("Consultation prenatale", DEG_name), grepl("A 2.1 CPN", DE_name))
    dataElements <- DEG_metadata %>% filter(grepl("SIGL 2", DEG_name), grepl("ABC|AZT|Kit test|Test Kit", DE_name))
    
    
    
    ## BUILD EXPECTATIONS DATAFRAME (DATAELEMENT lEVEL)

    data_out_province <- NULL
    data_out_district <- NULL
    
  for(id in dataElements$DE_id) {   
    
    OU_DE_period_exp <- build_OU_period_exp_DElevel(id, OU_metadata_DSinfo, DS_metadata, CC_metadata, c("201710", "201712"))
    OU_DE_period_exp <- as.data.table(OU_DE_period_exp)
    
    ## COMPARE AVAILABLE DATA WITH EXPECTATION (DATAELEMENT lEVEL)
                                                            
    data_DE <- data_1[dataElement==id, c("period", "orgUnit", "categoryOptionCombo", "value")]
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
    
  #################################################################### BUILD COMPLETENESS INDICATORS
  
    data_out_province$spatial_completeness <- data_out_province$facilities_available_data / data_out_province$facilities
    data_out_district$spatial_completeness <- data_out_district$facilities_available_data / data_out_district$facilities
    
    data_out_province$time_completeness <- data_out_province$periods_nonNA / data_out_province$periods
    data_out_district$time_completeness <- data_out_district$periods_nonNA / data_out_district$periods
    
  #################################################################### BUILD WIDE TABLE
    
    data_out_province_tmp <- data_out_province %>% select(DE_name, Province, Province_id, sum_patients, mean_patients)
    data_out_province_wide <- reshape(data_out_province_tmp, direction = "wide",
                          idvar = c("Province", "Province_id"), timevar = "DE_name")
    data_out_province_wide$CPN4_CPN1 <- data_out_province_wide$`sum_patients.A 2.1 CPN 4`/data_out_province_wide$`sum_patients.A 2.1 CPN 1`
    data_out_province_wide$CPN4_ontime <- data_out_province_wide$`sum_patients.A 2.1 CPN 4 à la 36ème semaine` / data_out_province_wide$`sum_patients.A 2.1 CPN 4`
    
    data_out_district_tmp <- data_out_district %>% select(DE_name, Province, Province_id, District, District_id, sum_patients, mean_patients)
    data_out_district_wide <- reshape(data_out_district_tmp, direction = "wide",
                                      idvar = c("Province", "Province_id", "District", "District_id"), timevar = "DE_name")
    data_out_district_wide$CPN4_CPN1 <- data_out_district_wide$`sum_patients.A 2.1 CPN 4`/data_out_district_wide$`sum_patients.A 2.1 CPN 1`
    data_out_district_wide$CPN4_ontime <- data_out_district_wide$`sum_patients.A 2.1 CPN 4 à la 36ème semaine` / data_out_district_wide$`sum_patients.A 2.1 CPN 4`

  #################################################################### PLOTS 
    
    # ORDER PROVINCES
    ordered_provinces <- data_out_province %>% filter(DE_id == dataElements$DE_id[1]) %>% arrange(facilities_available_data)
    ordered_provinces <- ordered_provinces$Province
    
    # SELECT PROVINCES
    selected_provinces <- c("tn Tanganyika Province", "sk Sud Kivu Province", "ke Kasai Oriental Province", "ks Kasai Province", "kr Kasai Central Province", "hk Haut Katanga Province")
  
    # ARRANGE PROVINCES
    data_plot <- data_out_province %>% filter(Province %in% ordered_provinces)
    data_plot$Province <- factor(data_plot$Province, 
                                 levels = ordered_provinces, # ordered_provinces / selected_provinces
                                 ordered = TRUE)
    
    # COMPLETENESS
    
    data_plot1 <- as.data.table(data_plot)
    data_plot1 <- data_plot1[, .(spatial_completeness = mean(spatial_completeness, na.rm=T),
                                 time_completeness = mean(time_completeness, na.rm=T)),
                                      by=c("Province")]
    
    data_plot1 <- data_plot1 %>% select("Province", "spatial_completeness", "time_completeness")
    data_plot1 <- reshape(data_plot1, direction = "long", varying = list(names(data_plot1)[2:3]),
            idvar = c("Province"), timevar = "Type", times = names(data_plot1)[2:3])
  
    
    ggplot(data_plot1, aes(x = Province, y = spatial_completeness, fill = Type)) + 
      geom_bar(width=0.4, position = position_dodge(width=0.5), stat="identity", aes(fill = Type)) + 
      scale_fill_brewer(palette="Paired") + 
      theme_bw() + 
      theme(axis.title.x=element_blank(),
            axis.text.x= element_text(angle = 90, hjust = 0.5),
            legend.position = "bottom") +
      guides(fill=guide_legend(nrow=2,byrow=TRUE, title = "")) +
      ylab('Completeness')
    
  
    # CPNs
  
    data_plot2 <- data_plot
    
    ggplot(data_plot2, aes(x = Province, y = sum_patients, fill = DE_name)) + 
      geom_bar(width=0.7, position = position_dodge(width=0.8), stat="identity", aes(fill = DE_name)) + 
      #facet_grid(. ~ DE_name) +
      scale_fill_brewer(palette="Paired") + 
      theme_bw() + 
      theme(axis.title.x=element_blank(),
          axis.text.x= element_text(angle = 0, hjust = 0.5),
          legend.position = "bottom") +
      guides(fill=guide_legend(nrow=2,byrow=TRUE, title = "")) +
      ylab('Total number of CPNs')
    
    
    # MAP (provinces)
    
    data_plot3 <- data_plot %>% filter(DE_name == "A 2.1 CPN 1")
    
    shapefile_data <- readOGR("/Users/adminbluesquarehub.com/Desktop/Rcode/service_availability/data/map_polygons.shp")
    coordinates <- fortify(shapefile_data, region="org_unit_I")
    coordinates <- coordinates %>% filter(id %in% OU_metadata_flat$level_2_id)
    
    map <- merge(coordinates , data_plot3, by.x = 'id', by.y = "Province_id", all.x=T) %>% arrange(group, order)
 
    ggplot(map)+
      geom_polygon(aes(x  = long, y = lat , 
                       group=id, 
                       fill = sum_patients,
                       linetype = "black")) +
      theme_minimal() +
      scale_fill_gradient(name = "Total reported CPN1",
                          high = '#009900', low = '#bbddbb',
                          space = "Lab",
                          na.value = "grey70", guide = "colourbar") # + coord_map()
      
    

    # MAP (districts)
    
    data_plot4 <- data_out_district %>% filter(Province %in% selected_provinces)
    
    data_plot4 <- data_plot4 %>% filter(DE_name == "A 2.1 CPN 1")
    
    shapefile_data <- readOGR("/Users/adminbluesquarehub.com/Desktop/Rcode/service_availability/data/map_polygons.shp")
    coordinates <- fortify(shapefile_data, region="org_unit_I")
    coordinates <- coordinates %>% filter(id %in% OU_metadata_flat$level_3_id)
    
    map <- merge(coordinates , data_plot4, by.x = 'id', by.y = "District_id", all.x=T) %>% arrange(group, order)

    ggplot(map)+
      geom_polygon(aes(x  = long, y = lat , 
                       group=id, 
                       fill = spatial_completeness)) +
      theme_minimal() +
      scale_fill_gradient(name = "Spatial Completeness",
                          high = '#008800', low = '#cceecc',
                          space = "Lab",
                          na.value = "grey70", guide = "colourbar") # + coord_map()
    
    
    # MAP (districts) --> Ratios
    
    
    data_plot5 <- data_out_district_wide %>% filter(Province %in% selected_provinces)

    shapefile_data <- readOGR("/Users/adminbluesquarehub.com/Desktop/Rcode/service_availability/data/map_polygons.shp")
    coordinates <- fortify(shapefile_data, region="org_unit_I")
    coordinates <- coordinates %>% filter(id %in% OU_metadata_flat$level_3_id)
    
    map <- merge(coordinates , data_plot5, by.x = 'id', by.y = "District_id", all.x=T) %>% arrange(group, order)

    ggplot(map)+
      geom_polygon(aes(x  = long, y = lat , 
                       group=id, 
                       fill = CPN4_CPN1)) +
      theme_minimal() +
      scale_fill_gradient(name = "CPN4 / CPN1",
                          high = '#008800', low = '#cceecc',
                          space = "Lab",
                          na.value = "grey70", guide = "colourbar") # + coord_map()
    
    ggplot(map)+
      geom_polygon(aes(x  = long, y = lat , 
                       group=id, 
                       fill = CPN4_ontime)) +
      theme_minimal() +
      scale_fill_gradient(name = "% of CPN4 done at week 36 ",
                          high = '#008800', low = '#cceecc',
                          space = "Lab",
                          na.value = "grey70", guide = "colourbar") # + coord_map()
    
    
    # MAP (districts) --> Ratios + Number of CPN1
    
    map <- map %>% rename(CPN1 = "sum_patients.A 2.1 CPN 1")
    
    ggplot(map)+
      geom_polygon(aes(x  = long, y = lat , 
                       group=id, 
                       fill = CPN4_CPN1,
                       alpha = CPN1)) +
      theme_minimal() +
      scale_fill_gradient(name = "CPN4 / CPN1",
                          high = '#00aa00', low = '#aa0000',
                          space = "Lab",
                          na.value = "grey70", guide = "colourbar")
    
    

  #################################################################### TIME SERIES
    
    
    data_out_province <- NULL
    data_out_district <- NULL
    
    for(id in dataElements$DE_id) {   
      
      OU_DE_period_exp <- build_OU_period_exp_DElevel(id, OU_metadata_DSinfo, DS_metadata, CC_metadata, c("201710", "201712"))
      OU_DE_period_exp <- as.data.table(OU_DE_period_exp)
      
      ## COMPARE AVAILABLE DATA WITH EXPECTATION (DATAELEMENT lEVEL)
      
      data_DE <- data_1[dataElement==id, c("period", "orgUnit", "categoryOptionCombo", "value")]
      data_DE$period <- as.character(data_DE$period)
      
      OU_DE_period_expvsdata <- merge(OU_DE_period_exp, data_DE, by.x = c("OU_id", "period", "CatComboOpt_id"), by.y = c("orgUnit", "period", "categoryOptionCombo"), all.x = T)
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
        
    
    
    
    
    
    
    # TO DO --> time series, ratio with population 
    
    ggplot(meltedJoinsByWeek, aes(x = week, y = value, colour = variable)) + 
      geom_line() + 
      ylab(label="Number of new members") + 
      xlab("Week Number") + 
      scale_colour_manual(values=c("grey", "blue"))
    
    ggplot(test_data, aes(date)) + 
      geom_line(aes(y = var0, colour = "var0")) + 
      geom_line(aes(y = var1, colour = "var1"))
    
    library("reshape2")
    library("ggplot2")
    
    test_data_long <- melt(test_data, id="date")  # convert to long format
    
    ggplot(data=test_data_long,
           aes(x=date, y=value, colour=variable)) +
      geom_line()
    

  
    