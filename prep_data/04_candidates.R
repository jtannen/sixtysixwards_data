library(tidyverse)

setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/data")
source("../admin_scripts/util.R")
out_dir <- "processed_data"

df_major <- readRDS(
  most_recent_file(paste0(out_dir, "/df_major_"))
)

candidates_to_races <- df_major %>%
  filter(!substr(candidate, nchar(candidate)-5, nchar(candidate)) %in% c(" NO NO", "YES SI")) %>%
  filter(!candidate %in% c("NO NO","YES SI","No Vote","WRITE IN","Write In")) %>%
  group_by(year, election, office, district, party, candidate) %>%
  summarise(votes = sum(votes)) %>%
  ungroup()

names <- candidates_to_races$candidate

fix_spaces <- function(x) {
  x <- gsub("|'|\\.|,","", x)
  x <- gsub("\\s+", " ", x)
  x <- gsub("^\\s", "", x)
  x <- gsub("\\s$", "", x)
  x
}

suffix_re <- "(.*) \\b(SR|JR|(I|V)+)$"
has_suffix <- grepl(suffix_re, names)
candidates_to_races$suffix <- ""
candidates_to_races$suffix[has_suffix] <- gsub(suffix_re, "\\2", names[has_suffix])
names <- gsub(suffix_re, "\\1", names) %>% fix_spaces

last_name_re <- "^(.*) (\\S+)$"
candidates_to_races$last_name <- gsub(last_name_re, "\\2", names)
names <- gsub(last_name_re, "\\1", names) %>% fix_spaces()

mc_re <- "(*) (MC)$" 
is_mc <- grepl(mc_re, names)
lastname_prefix <- gsub(mc_re, "\\2", names)
candidates_to_races$last_name[is_mc] <- paste0(lastname_prefix[is_mc], candidates_to_races$last_name[is_mc])
names <- gsub(mc_re, "\\1", names) %>% fix_spaces()

first_name_re <- "^(\\S+)\\s?(.*)$"
candidates_to_races$first_name <- gsub(first_name_re, "\\1", names)
names <- gsub(first_name_re, "\\2", names) %>% fix_spaces()

candidates_to_races$middle_name <- names

candidate_fixes <- read_csv("../admin_scripts/prep_data/tmp/candidate_fixes_2019_10_13.csv") %>%
  mutate_at(.vars = vars(ends_with("_fix")), .funs=list(~ ifelse(is.na(.), "", .)))

fix_merge <- candidates_to_races %>% 
  left_join(
    candidate_fixes %>% 
      select(year, office, candidate, ends_with("_fix")) %>% 
      mutate(year = as.character(year))
  ) 

if(nrow(fix_merge) != nrow(candidates_to_races)) stop("JOIN SHOULD BE 1:1")

for(var in c("last_name", "first_name", "middle_name", "suffix")){
  var_fix <- paste0(var, "_fix")
  candidates_to_races[[var]] <- ifelse(
    !is.na(fix_merge[[var_fix]]), 
    fix_merge[[var_fix]], 
    candidates_to_races[[var]]
  )
}

candidates <- candidates_to_races %>% 
  select(first_name, middle_name, last_name, suffix) %>% 
  unique

## WRITE OUT THE RESULTS. GO THROUGH TO MAKE SURE A SINGLE PERSON DOESN'T HAVE DUPLICATES
## ADD THEM TO candidate_fixes
if(FALSE){
  write_csv(
    candidates_to_races %>% 
      select(year, election, office, candidate, first_name, middle_name, last_name, suffix) %>%
      unique(),
    path=dated_stem("../admin_scripts/prep_data/tmp/candidates_to_races", fileext="csv")
  )
}

## Emilio Vazquez in 2004/06 and 2018 was really two different people
candidates %>% group_by(first_name, last_name) %>% 
  count() %>%
  filter(n>1) %>%
  arrange(desc(last_name)) %>%
  as.data.frame()

candidates_to_races %>% 
  select(year, office, candidate, first_name, middle_name, last_name, suffix) %>%
  inner_join(
    candidates %>% group_by(first_name, last_name) %>% 
      count() %>%
      filter(n>1)
  )  %>%
  arrange(desc(last_name)) %>%
  as.data.frame()

dim(candidates)

## For the time being, we can give candidates a unique id for first+middle+last+suffix
## It's possible that these may not be unique in the future, but for now this works
candidates$candidate_id <- gsub("\\s+", " ", with(candidates, paste(first_name, middle_name, last_name, suffix)))
candidates$candidate_id <- gsub("\\s$", "", candidates$candidate_id)
head(candidates)

candidates_to_races <- left_join(
  candidates_to_races,
  candidates
) %>%
  select(year, election, office, district, party, candidate, candidate_id)


saveRDS(candidates, file=dated_stem("processed_data//candidates", fileext="Rds"))
saveRDS(candidates_to_races, file=dated_stem("processed_data//candidates_to_races", fileext="Rds"))


candidate_fixes$candidate_id <- gsub("\\s+", " ", with(candidate_fixes, paste(first_name_fix, middle_name_fix, last_name_fix, suffix_fix)))
candidate_fixes$candidate_id <- gsub("\\s$", "", candidate_fixes$candidate_id)

saveRDS(
  bind_rows(
    candidates %>%
      select(candidate_id) %>%
      mutate(ballot_name=candidate_id),
    candidate_fixes  %>%
      rename(ballot_name = candidate) %>%
      select(candidate_id, ballot_name) %>%
      unique()
  ),
  file=dated_stem("processed_data/candidate_names", fileext="Rds")
)

