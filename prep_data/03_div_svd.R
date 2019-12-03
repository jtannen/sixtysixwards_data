library(tidyverse)
library(MASS)
library(sf)

select <- dplyr::select

setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/data")
out_dir <- "processed_data"

source("../posts/election_night_needle/svd_for_turnout_and_pvote.R", chdir=TRUE)

SVD_METHOD = "shrinkage"

PRESENT_VINTAGE <- "201911"
USE_LOG <- TRUE

df_past <- most_recent_file("processed_data/df_major_") %>%
  readRDS() %>%
  unite("election", year, election) %>%
  mutate(warddiv = paste0(substr(warddiv,1,2),"-",substr(warddiv,3,4))) %>%
  filter(candidate != "Write In")

divs <- st_read(sprintf("gis/%s/Political_Divisions.shp", PRESENT_VINTAGE)) %>%
  mutate(
    warddiv = pretty_div(DIVISION_N)
  )


pvote_svd <- get_pvote_svd(
  df_past,
  use_primary=TRUE, 
  use_general=TRUE, 
  primary_party_regex = "^DEM",
  use_log=USE_LOG
)

turnout_svd_general <- get_turnout_svd(
  result_df=df_past,
  election_type="general", 
  use_log=USE_LOG
)

turnout_svd_primary_dem <- get_turnout_svd(
  df_past,
  election_type="primary", 
  party_grep="^DEM",
  use_log=USE_LOG
)

turnout_svd_primary_rep <- get_turnout_svd(
  df_past,
  election_type="primary", 
  party_grep="^REP",
  use_log=USE_LOG
)

pvote_diagnostics(pvote_svd, divs)
turnout_diagnostics(turnout_svd_general, divs, "General")
turnout_diagnostics(turnout_svd_primary_dem, divs, "Democratic Primary")
turnout_diagnostics(turnout_svd_primary_rep, divs, "Republican Primary")

#########################
## div cats
#########################

## Div Cats only use recent years

div_cats <- pvote_svd@row_scores %>%
  rename(warddiv=row) %>%
  select(-mean)

div_cats %<>% left_join(
  st_centroid(divs) %>%
    mutate(
      x = mapply(function(g) unlist(g)[1], geometry),
      y = mapply(function(g) unlist(g)[2], geometry)
    ) %>% as.data.frame() %>% select(warddiv, x, y)
)

div_cats$cat <- NA

div_cats$cat[div_cats$score.2 > 0.005] <- "Black Voters"
div_cats$cat[
  div_cats$score.3 > 0.005 & div_cats$x < -75.1 &
    is.na(div_cats$cat)
] <- "Wealthy Progressives"
div_cats$cat[
  div_cats$score.4 < -0.01 & div_cats$y > 39.95 & 
    div_cats$y < 40.06 & is.na(div_cats$cat)
] <- "Hispanic North Philly"
div_cats$cat[is.na(div_cats$cat)] <- "White Moderates"     

###############
## PLots
###############

ggplot(
  divs %>% left_join(div_cats) %>% filter(cat=="Hispanic North Philly")
) + geom_sf()

ggplot(divs %>% left_join(div_cats)) + 
  geom_sf(aes(fill=cat), color=NA)

turnout_df <- readRDS(most_recent_file("processed_data/df_major_")) %>%
  filter(is_primary_office, election=="general") %>%
  group_by(year, warddiv) %>%
  summarise(turnout=sum(votes)) %>%
  mutate(warddiv=pretty_div(warddiv))

turnout_divcat <- turnout_df %>%
  left_join(div_cats) %>%
  group_by(cat, year) %>%
  summarise(turnout=sum(turnout)) %>%
  group_by(year) %>%
  mutate(pct_of_turnout=turnout/sum(turnout))

ggplot(turnout_divcat, aes(x=year, y=pct_of_turnout)) +
  geom_line(aes(group=cat, color=cat)) +
  theme_sixtysix()


div_cats$cat <- factor(
  div_cats$cat,
  levels = c(
    "Black Voters",
    "Wealthy Progressives",
    "White Moderates",
    "Hispanic North Philly"
  )
)

saveRDS(pvote_svd, dated_stem(paste0(out_dir, "/pvote_svd"), fileext="RDS"))
saveRDS(turnout_svd_general, dated_stem(paste0(out_dir, "/turnout_svd_general"), fileext="RDS"))
saveRDS(turnout_svd_primary_dem, dated_stem(paste0(out_dir, "/turnout_svd_primary_dem"), fileext="RDS"))
saveRDS(turnout_svd_primary_rep, dated_stem(paste0(out_dir, "/turnout_svd_primary_rep"), fileext="RDS"))
saveRDS(div_cats, dated_stem(paste0(out_dir, "/div_cats"), fileext="RDS"))
