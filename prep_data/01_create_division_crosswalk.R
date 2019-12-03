library(sf)
library(dplyr)
library(ggplot2)
library(sp)
# library(rgeos)

setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/data")
source("prep_data/data_utils.R")

PRESENT_VINTAGE <- "201911"
divs_present <- st_read(sprintf("gis/warddivs/%s/Political_Divisions.shp", PRESENT_VINTAGE))
divs_present <- st_transform(divs_present, 2272)

format_div <- function(df){
  if(!`&`(
    "WARD" %in% names(df),
    "DIVSN" %in% names(df)
  )){
    df <- df %>% mutate(
      WARD = substr(DIVISION_N, 1, 2),
      DIVSN = substr(DIVISION_N, 3, 4)
    )
  }
  
  df %>% mutate(
    WARD = sprintf("%02d", asnum(WARD)),
    DIVSN = sprintf("%02d", asnum(DIVSN))
  )
} 

divs_present <- format_div(divs_present)

# library(tigris)
# options(tigris_use_cache = TRUE)
# 
# state_blocks <- readOGR("../gis/census/tl_2010_42_tabblock10.shp")
# phila_blocks <- state_blocks[state_blocks$COUNTYFP10 == "101",]
# phila_blocks <- st_as_sf(phila_blocks)
# phila_blocks <- st_transform(phila_blocks, 2272)
# 
# block_pops_2010 <- read.csv("../census/block_pops_census10_pa_socialexp.csv")
# block_pops_2010 <- block_pops_2010 %>%
#   filter(Geo_COUNTY == "101") %>%
#   rename(pop = SF1_P0010001) %>%
#   mutate(GEOID = as.character(Geo_FIPS))
# 
# phila_blocks <- phila_blocks %>% 
# 	left_join(
# 		block_pops_2010 %>% select(GEOID, pop), 
# 		by = c("GEOID10" = "GEOID")
# 	)
# 	
# block_centroids <- st_transform(phila_blocks,crs=2272) %>% st_centroid
# write.csv("census/block10_centroid_pops.csv")

block_cents <- read.csv("census/block10_centroid_pops.csv")
block_cents <- block_cents %>% filter(COUNTYFP10 == '101')
block_cents <- SpatialPointsDataFrame(
  block_cents[,c("INTPTLON10", "INTPTLAT10")], 
  proj4string=CRS("+init=EPSG:4326"),
  data = block_cents
)
block_cents <- st_as_sf(block_cents) %>% st_transform(2272)

get_block_div_crosswalk <- function(
	divs_shp, 
	blocks = block_cents
){
	blocks_to_divs <- st_intersects(blocks, divs_shp)

	print("count of blocks matched to divs.")
	print(table(sapply(blocks_to_divs, length)))
	if (any(sapply(blocks_to_divs, length)) > 1){
		stop("Block match not unique. Something is wrong")
	}

	blocks$div_row <- sapply(blocks_to_divs, function(x) ifelse(is.null(x), NA, x))
	
	rematched <- blocks[is.na(blocks$div_row),] %>%
	  st_distance(divs_shp) %>% 
	  apply(1, which.min)

	blocks$div_row[is.na(blocks$div_row)] <- rematched

	blocks$WARD <- divs_shp$WARD[blocks$div_row]
	blocks$DIV <- divs_shp$DIVSN[blocks$div_row]

	blocks_to_divisions <- blocks %>% 
	  as.data.frame() %>%
		select(GEOID10, WARD, DIV, pop10) 
	
	print("pops of matched blocks:")
	print(sum(blocks_to_divisions$pop10))
	print("pops of all blocks")
	print(sum(blocks$pop10))
	if(sum(blocks_to_divisions$pop10) != sum(blocks$pop10)){
		stop("Population was lost or doubled. Something's wrong!")
	}
	return(blocks_to_divisions)
}


blocks_to_divs <- get_block_div_crosswalk(divs_present) 
# saveRDS(blocks_to_divs, file = sprintf("../gis_crosswalks/blocks10_to_divs_%s.Rds", PRESENT_VINTAGE))
blocks_to_divs_present <- blocks_to_divs

original_gis_paths <- c(
	`2005` = "2005/2005_Ward_Divisions.shp",
	`2008` = "2008/2008_12_Ward_Divisions.shp",
	`2009` = "2009/2009_Ward_Divisions.shp",
	`2010` = "2010/2010_Ward_Divisions.shp",
	`2011` = "2011/2011_Ward_Divisions.shp",
	`2012` = "2012/2012_09_Ward_Divisions.shp", 
	`2013` = "2013/2013_02_Ward_Divisions.shp",
	`2016` = "2016/2016_Ward_Divisions.shp",
	`2019` = "2019/Political_Divisions.shp"
)

for(i in 1:length(original_gis_paths)){
	vintage <- names(original_gis_paths)[i]
	file <- sprintf("gis/warddivs/%s", original_gis_paths[i])

	print("##############################")
	print(vintage)
	print("##############################")

	out_dir <- "gis_crosswalks"
	
	divs <- st_read(file, quiet = TRUE)
	divs <- divs %>% format_div()
	divs <- st_transform(divs, 2272)
	blocks_to_divs <- get_block_div_crosswalk(divs)

	print("Count blocks that stayed in Division. Few TRUEs is a bad sign")
	print(
		left_join(
			blocks_to_divs,
			blocks_to_divs_present,
			by = "GEOID10"
		) %>% with(table(WARD.x == WARD.y & DIV.x == DIV.y))
	)
	
	if(
	  with(
	    blocks_to_divs %>% filter(is.na(WARD) | is.na(DIV)), 
	    sum(pop10) > 0
	   )
	) stop("wards lost pop")
	
	crosswalk_to_present <- left_join(
		blocks_to_divs %>% filter(!is.na(WARD)),
		blocks_to_divs_present %>% select(-pop10),
		by='GEOID10',
		suffix=c('.old', '.present')
	) %>% group_by(WARD.old, DIV.old, WARD.present, DIV.present) %>%
		summarise(pop10 = sum(pop10)) %>%
		group_by(WARD.old, DIV.old) %>%
		mutate(weight_to_present = if(sum(pop10) > 0) pop10 / sum(pop10) else 1/n())
	
	if(
	  sum(crosswalk_to_present$weight_to_present) != 
	  nrow(unique(as.data.frame(divs)[,c("WARD","DIVSN")]))
	 ){
		stop("weight should sum to n_divs")
	}
	saveRDS(blocks_to_divs, file = sprintf("%s/blocks10_to_divs_%s.Rds", out_dir, vintage))
	saveRDS(crosswalk_to_present, file = sprintf("%s/div_crosswalk_%s_to_%s.Rds",out_dir, vintage, PRESENT_VINTAGE))	
	
	print(i)
}

###### Block Groups
files <- list.files(pattern = "blocks10_to_divs_.*Rds")

out_path <- "."
for(file in files){
	blocks_to_divs <- readRDS(file)
	vintage <- gsub("blocks10_to_divs_|\\.Rds", "", file)
	bgs_to_divs <- blocks_to_divs %>% 
	  mutate(bg_fips = substr(GEOID10, 1, 12)) %>%
		group_by(bg_fips, WARD, DIV) %>% 
	  summarise(pop10 = sum(pop10)) %>%
		group_by(bg_fips) %>% 
		mutate(
			weight = pop10 / sum(pop10),
			weight = ifelse(is.na(weight), 1/n(), weight)
		)

	saveRDS(bgs_to_divs, file = sprintf("%s/bgs10_to_divs_%s.Rds",out_path, vintage))
	print(file)
}

