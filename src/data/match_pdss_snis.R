library(dhisextractr)

load_env()

pdss_org <- read.csv(paste0(pdss_data_dir,'/org_units_description.csv'))
snis_org <- read.csv(paste0(snis_data_dir,'/org_units_description.csv'))

fac_zones_snis <- read.csv('data/references/snis_fosas_zones.csv')
fac_zones_pdss <- read.csv('data/references/pdss_fosas_zones.csv')

pdss_org$name <- gsub('é', 'e' ,trimws(tolower(as.character(pdss_org$name)), 'right'))
snis_org$name <- gsub('é', 'e' ,trimws(tolower(as.character(snis_org$name)), 'right'))

pdss_org$name <- gsub('gethy' , 'gety', pdss_org$name)
pdss_org$name <- gsub('kiyambi' , 'kiambi', pdss_org$name)
pdss_org$name <- gsub('mongbalu' , 'mongbwalu', pdss_org$name)
pdss_org$name <- gsub('kisandji' , 'kisanji', pdss_org$name)


snis_matched <- snis_org[snis_org$name %in% pdss_org$name,]
pdss_matched <- pdss_org[pdss_org$name %in% snis_org$name,]

missing_snis_zone <- unique(as.character(fac_zones_snis$zone[!(fac_zones_snis$zone %in% snis_matched$id)]))
missing_pdss_zone <- unique(as.character(fac_zones_pdss$zone[!(fac_zones_pdss$zone %in% pdss_matched$id)]))

missing_snis_zone <- snis_org[snis_org$id %in% missing_snis_zone , ]
missing_pdss_zone <- pdss_org[pdss_org$id %in% missing_pdss_zone , ]

col_to_match <- c('id', 'name')
snis_side <- snis_matched[snis_matched$id %in% fac_zones_snis$zone, col_to_match]
pdss_side <- pdss_matched[pdss_matched$id %in% fac_zones_pdss$zone, col_to_match]

out <- merge(snis_side, pdss_side, by = 'name' , suffixes = c('_snis', '_pdss'))

write.csv(out, 'data/references/matched_zones.csv')
