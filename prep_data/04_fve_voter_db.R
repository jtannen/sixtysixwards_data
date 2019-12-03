library(tidyverse)
library(dbplyr)

setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/admin_scripts/prep_data/")
source("../util.R")

fve_date <- "20190624"
county <- "PHILADELPHIA"

folder <- sprintf("../../data/voter_registration/%s/", fve_date)
FVE_FILE <- sprintf("%s/%s FVE %s.txt", folder, county, fve_date)
fve_colnames <- read_csv("../../data/voter_registration/20180806/col_names.csv")

elections <- read_tsv(
  sprintf("%s/%s Election Map %s.txt", folder, county, fve_date),
  col_names = c("county", "election_id", "election_desc", "election_date")
) %>%
  mutate(
    is_special = grepl("SPECIAL", election_desc, ignore.case = TRUE),
    is_general = !is_special & substr(election_date,1,2) == "11",
    is_primary = grepl("PRIMARY", election_desc, ignore.case = TRUE),
    year = substr(election_date, 7, 10)
  )

N_ELECTIONS <- max(elections$election_id)

load_chunk <- function(skip, n_max){
  fve <- read_tsv(
    FVE_FILE,  
    skip=skip,
    n_max=n_max,
    col_names=fve_colnames$name,
    col_types=paste(rep('c', nrow(fve_colnames)), collapse="")
  ) %>%
    rename(
      voter_id = `ID Number`,
      warddiv = `Precinct Code`
    )
  
  if(nrow(fve) == 0) return(list(fve=data.frame(), voters_to_elections=data.frame()))
  
  voter_to_elections <- mapply(
    function(e) data.frame(
      voter_id = fve$voter_id,
      vote_method = fve[[sprintf("Election %s Vote Method", e)]], 
      party = fve[[sprintf("Election %s Party", e)]], 
      election_id = e
    ),
    1:N_ELECTIONS,
    SIMPLIFY=FALSE
  ) %>% bind_rows
  
  fve <- fve %>% select(-starts_with("Election "))
  
  return(list(fve=fve, voters_to_elections=voter_to_elections))
}

load_all <- function(n_max = 100e3){
  i <- 1
  results <- list(load_chunk(skip=0, n_max=n_max))
  while(nrow(results[[i]]$fve) == n_max){
    print(i * n_max)
    i <- i + 1
    results[[i]] <- load_chunk(skip=(i-1)*n_max, n_max=n_max)
  }
  return(list(
    fve = bind_rows(lapply(results, function(x) x$fve)),
    voters_to_elections = bind_rows(lapply(results, function(x) x$voters_to_elections))
  ))
}

x <- load_all()

fve <- x$fve
voters_to_elections <- x$voters_to_elections

names(fve) <- gsub(" ", "_", tolower(names(fve)))

date_cols <- c("dob", "registration_date", "status_change_date", "last_vote_date", "date_last_changed")
for(col in date_cols) fve[[col]] <- lubridate::mdy(fve[[col]])

elections$election_date <- lubridate::mdy(elections$election_date)
elections$cycle <- ifelse(
  elections$is_special, NA,
  c(
    "President", "District Attorney", "Governor", "Mayor"
  )[(asnum(elections$year) %% 4) + 1]
)
elections <- arrange(elections, election_date)

## data only valid after 2006
elections <- elections %>% filter(year >= 2006)
voters_to_elections <- voters_to_elections %>% 
  inner_join(elections %>% select(election_id))

elections$election_type <- with(
  elections,
  ifelse(is_special, "special", ifelse(is_primary, "primary", ifelse(is_general, "general", NA)))
)

# saveRDS(fve, "../../data/processed_data/voter_db/fve.Rds")
# saveRDS(voters_to_elections, "../../data/processed_data/voter_db/voters_to_elections.Rds")
# saveRDS(elections, "../../data/processed_data/voter_db/elections.Rds")


##############
### DATABASE
##############

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = "C:/Users/Jonathan Tannen/Dropbox/sixty_six/data/database/sixtysixwardsdb.sqlite"
)

# con <- dbConnect(
#   pg, dbname = "sixtysix_db",
#   host = "localhost", port = 5432,
#   user = "postgres", password = rstudioapi::askForPassword("Database password")
# )

OVERWRITE <- TRUE

# RSQLite::dbRemoveTable(con, "fve_elections_")

for(col in date_cols) fve[[col]] <- as.character(fve[[col]])

for(tablename in c("fve", sprintf("fve_%s", fve_date))){
  fve <- fve %>% mutate(fve_date=!!fve_date)
  dbWriteTable(con, tablename, fve, row.names=FALSE)
}

fve_tbl <- tbl(con, "fve")

dbListTables(con)

elections$election_date <- as.character(elections$election_date)

for(tablename in c("fve_elections", sprintf("fve_elections_%s", fve_date))){
  dbWriteTable(con, tablename, elections %>% mutate(fve_date=!!fve_date), row.names=FALSE)
}

for(tablename in c("voters_to_elections", sprintf("voters_to_elections_%s", fve_date))){
  dbWriteTable(con, tablename, voters_to_elections %>% mutate(fve_date=!!fve_date), row.names=FALSE)
}

