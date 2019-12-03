library(dplyr)
library(sf)
library(magrittr)

setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/data")
source("prep_data/data_utils.R")

out_dir <- "processed_data"
crosswalk_dir <- "processed_data/crosswalked_results/"

###########################
## Crosswalk election results
###########################
PRESENT_VINTAGE <- 201911
divs <- st_read(
  sprintf("gis/warddivs/%s/Political_Divisions.shp", PRESENT_VINTAGE)
) %>%
  rename(warddiv=DIVISION_N)

use_crosswalk <- tribble(
  ~year, ~election, ~vintage,
  2002, "primary", 2005,
  2002, "general", 2005,
  2003, "primary", 2005,
  2003, "general", 2005,
  2004, "primary", 2005,
  2004, "general", 2005,
  2005, "primary", 2005,
  2005, "general", 2005,
  2006, "primary", 2005,
  2006, "general", 2005,
  2007, "primary", 2005,
  2007, "general", 2005,
  2008, "primary", 2008,
  2008, "general", 2008,
  2009, "primary", 2008, # broken
  2009, "general", 2008, # broken
  2010, "primary", 2009,
  2010, "general", 2011,
  2011, "primary", 2011,
  2011, "general", 2011,
  2012, "primary", 2011,
  2012, "general", 2012,
  2013, "primary", 2013,
  2013, "general", 2013,
  2014, "primary", 2016,
  2014, "general", 2016,
  2015, "primary", 2016,
  2015, "general", 2016,
  2016, "primary", 2016,
  2016, "general", 2016,
  2017, "primary", 2016,
  2017, "general", 2016,
  2018, "primary", 2019,
  2018, "general", 2019,
  2019, "primary", 2019,
  2019, "general", 201911
)

fix_colnames <- function(df){
  ## 2018 general results changed all the column names...
  replace_name <- function(df, oldname, newname){
    names(df)[names(df) == oldname] <- newname
    return(df)
  }
  
  df <- df %>%
    replace_name("CATEGORY", "OFFICE") %>%
    replace_name("SELECTION", "CANDIDATE") %>%
    replace_name("VOTE.TYPE", "TYPE") %>%
    replace_name("VOTE.COUNT", "VOTES") %>%
    replace_name("?..WARD", "WARD") %>%
    replace_name("ï..WARD", "WARD")
  return(df)
}


ALLOW_MISTAKES <- TRUE
VERBOSE <- TRUE
elections_with_mistakes <- data.frame()

for(i in 1:nrow(use_crosswalk)){
  if(VERBOSE) print(use_crosswalk[i,] %>% unlist)
  
  year <- use_crosswalk$year[i]
  election <- use_crosswalk$election[i]
  vintage <- use_crosswalk$vintage[i]
  
  election_df <- read.csv(sprintf("raw_election_data/%s_%s.csv", year, election))
  election_df <- fix_colnames(election_df)
  election_df <- election_df %>%
    mutate(warddiv = paste0(sprintf("%02d", WARD), sprintf("%02d", DIVISION)))
  
  if(vintage == PRESENT_VINTAGE){
    ## HERE
    election_df_present <- election_df %>% 
      select(TYPE, OFFICE, CANDIDATE, PARTY, warddiv, VOTES)
    
    missing_g1 <- unique(election_df$warddiv)[!(unique(election_df$warddiv) %in% divs$warddiv)]
    missing_g2 <- divs$warddiv[!(divs$warddiv %in% unique(election_df$warddiv))]
  } else {
    
    crosswalk <- readRDS(
      sprintf(
        "gis_crosswalks/div_crosswalk_%s_to_%s.Rds", 
        vintage, 
        PRESENT_VINTAGE
      )
    ) %>% mutate(
      warddiv.old = paste0(WARD.old, DIV.old),
      warddiv.present = paste0(WARD.present, DIV.present)
    )

    missing_g1 <- unique(election_df$warddiv)[!(unique(election_df$warddiv) %in% unique(crosswalk$warddiv.old))]
    missing_g2 <- unique(crosswalk$warddiv.old)[!(unique(crosswalk$warddiv.old) %in% unique(election_df$warddiv))]
    
    election_df_present <- election_df %>%
      left_join(crosswalk, by=c("warddiv"="warddiv.old")) %>%
      group_by(TYPE, OFFICE, CANDIDATE, PARTY, warddiv.present) %>%
      summarise(VOTES = sum(VOTES * weight_to_present)) %>%
      rename(warddiv = warddiv.present)
  }
  
  
  if(length(missing_g1) > 0 || length(missing_g2) > 0){
    if(!ALLOW_MISTAKES) stop("Votes mismatch. Something went wrong.")
    elections_with_mistakes <- bind_rows(
      elections_with_mistakes,
      data.frame(
        year=year,
        election=election,
        lost_votes=sum(election_df$VOTES) - sum(election_df_present$VOTES),
        total_votes = sum(election_df$VOTES),
        g1_missing_from_crosswalk=paste(missing_g1, collapse=","),
        crosswalk_missing_from_g1=paste(missing_g2, collapse=",")
      )
    )
  }
  
  write.csv(
    election_df_present,
    file=sprintf("%s/%s_%s_crosswalked_to_%s.csv", crosswalk_dir, year, election, PRESENT_VINTAGE),
    row.names=FALSE
  )
}

print(elections_with_mistakes)


####################
## format data
####################

files <- list.files(crosswalk_dir, pattern=sprintf("[0-9]{4}_[a-z]+_crosswalked_to_%s\\.csv", PRESENT_VINTAGE), full.names=FALSE)

df_list <- list()
for(file in files) {
  year_char <- substr(file, 1, 4)
  election_char <- gsub("^[0-9]{4}_([a-z]+)_.*$", "\\1", file)
  df_list[[file]] <- readr::read_csv(sprintf("%s/%s", crosswalk_dir, file), col_types="cccccdcc") %>%
    mutate(year = year_char, election=election_char) 
}
df <- bind_rows(df_list)
names(df) <- tolower(names(df))

replace_value <- function(value, old, new){
  ifelse(value == old, new, value)
}

df$party <- replace_value(df$party, "DEMOCRAT", "DEMOCRATIC")

## remove party info in office
df$office <- gsub("-\\s?(D|R|DEM|REP)$", "", df$office)
df$office <- gsub("-(D|R)-", "-", df$office)

## remove trailing spaces
df$office <- gsub("\\s+$", "", df$office)

## fix office names

df %<>% mutate(
  office = replace_value(office, 'PRESIDENT AND VICE PRESIDENT OF THE UNITED STATES', 'PRESIDENT OF THE UNITED STATES'),
  office = replace_value(office, 'GOVERNOR AND LIEUTENANT GOVERNOR', 'GOVERNOR'),
  office = replace_value(office, 'COUNCIL AT-LARGE', 'COUNCIL AT LARGE'),
  office = replace_value(office, 'CONTROLLER', 'CITY CONTROLLER')
)
df$office <- gsub("^REPRESENTATIVE IN THE UNITED STATES CONGRESS(.*)", "REPRESENTATIVE IN CONGRESS\\1", df$office)

df$candidate <- gsub("\\s+", " ", df$candidate)
df$candidate <- gsub("\\s+$", "", df$candidate)


## remove district numbers
## EG STATE REP-9TH DISTRICT
district_re <- "(.*[^0-9])([0-9]+)[A-Z]+(-|\\s|SENATORIAL|CONGRESSIONAL)*DIST(RICT)?$"
has_district <- grepl(district_re, df$office)
district <- gsub(district_re, "\\2", df$office)

df$district <- NA
df$district[has_district] <- district[has_district]
table(df$district)

df$office[has_district] <- gsub(district_re, "\\1", df$office[has_district])
df$office <- gsub("(-|\\s)+$", "", df$office)

df$is_question <- grepl("QUESTION", df$office)


## RETENTIONS
retention_re <- "(RETENTION - (JUDGE|JUSTICE) OF THE (SUPERIOR COURT|SUPREME COURT|COURT OF COMMON PLEAS|MUNICIPAL COURT)) - (.*)?$"
is_retention <- grepl(retention_re, df$office)

table(paste(df$year, df$election), is_retention)

retention_candidate <- gsub(retention_re, "\\4", df$office[is_retention])
table(retention_candidate)

df$district[is_retention] <- retention_candidate
df$office[is_retention] <- gsub(retention_re, "\\1", df$office[is_retention])


## SOME RETENTIONS DIDN'T HAVE RETENTION IN THE OFFICE
yes_or_no <- substr(
  df$candidate, 
  nchar(df$candidate)-4,
  nchar(df$candidate)
) %in% c("NO NO", "ES SI", "YES S")
# yes_or_no <- grepl("NO NO$", df$candidate) | grepl("YES S(I)?$", df$candidate)

fix_retention <- yes_or_no & (substr(df$office, 1, 9) != "RETENTION") & !df$is_question
table(paste(df$year, df$election), fix_retention)

df$office[fix_retention] <- paste0("RETENTION - ", df$office[fix_retention])

df$ward <- substr(df$warddiv, 1, 2)

df_major <- df %>% 
  ungroup() %>%
	filter(
    !grepl("QUESTION", office),
    !grepl("^SPECIAL ELECTION", office),
    !grepl("STATE COMMITTEE", office),
    !grepl("NATIONAL CONVENTION", office),
    !grepl("^RETENTION", office),
    office != "No Vote"
	) %>%
  mutate(
    is_topline_office = office %in% c("PRESIDENT OF THE UNITED STATES", "MAYOR", "DISTRICT ATTORNEY", "GOVERNOR")
  ) %>%
  group_by(office, candidate, party, warddiv, year, election, district, ward, is_topline_office) %>%
  summarise(votes = sum(votes)) %>%
  ungroup()
	
saveRDS(df_major, file = dated_stem("df_major", path=out_dir, fileext="Rds"))
saveRDS(ungroup(df), file = dated_stem("df_all", path=out_dir, fileext="Rds"))

