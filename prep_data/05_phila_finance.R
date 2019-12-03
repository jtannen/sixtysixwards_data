library(tidyverse)
library(magrittr)

setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/admin_scripts/prep_data/")
source("../../admin_scripts/util.R")

standardize_text <- function(x){
  x <- str_to_upper(x)
  x <- gsub(""|'|\\.|,","", x)
  x <- gsub("\\s+"," ", x)
  x <- gsub("(^\\s)|(\\s$)", "", x)
  return(x)
}

standardize_address <- function(x){
  x <- standardize_text(x)
  x <- gsub("\\bSTREET\\b", "ST", x)
  x <- gsub("\\bAVENUE\\b", "AVE", x)
  x <- gsub("\\bBOULEVARD\\b", "BLVD", x)
  x <- gsub("\\bROAD\\b", "RD", x)
  x <- gsub("\\bLANE\\b", "LN", x)
  return(x)
}

col_types = cols(
  FilerName = col_character(),
  Year = col_character(),
  Cycle = col_character(),
  DocType = col_character(),
  EntityName = col_character(),
  EntityAddressLine1 = col_character(),
  EntityAddressLine2 = col_character(),
  EntityCity = col_character(),
  EntityState = col_character(),
  EntityZip = col_character(),
  Occupation = col_logical(),
  EmployerName = col_character(),
  EmployerAddressLine1 = col_character(),
  EmployerAddressLine2 = col_character(),
  EmployerCity = col_character(),
  EmployerState = col_character(),
  EmployerZip = col_double(),
  Date = col_character(),
  Amount = col_double(),
  Description = col_character(),
  Amended = col_character(),
  SubDate = col_character()
)

dfs <- list()
for(year in 2011:2019){
  path <- list.files(
    "../../data/campaign_finance/philadelphia/", 
    pattern=sprintf("Report-%s", year), 
    full.names = T
  )  
  if(year == 2019) {
    dfs[[as.character(year)]] <- read_csv(path, col_types=col_types) 
  } else {
    dfs[[as.character(year)]] <- read_tsv(path, col_types=col_types) 
  }
}

head(dfs[['2014']])
head(dfs[["2019"]])

df <- bind_rows(dfs, .id = "year")
head(df)

valid_year <- function(y) grepl("^[0-9]{4}$", y)
bad_rows <- df %>% filter(!valid_year(Year))
df %<>% filter(valid_year(Year))

nona <- function(x) ifelse(is.na(x), "", x)
df %<>% mutate(
  add1_std = standardize_address(EntityAddressLine1),
  address = standardize_address(
    paste(nona(EntityAddressLine1), nona(EntityAddressLine2))
  ),
  entity_name_std = standardize_text(EntityName),
  filer_name_std = standardize_text(FilerName)
)

##################################
## REPLACE FILERS
##################################

entities_to_names <- read_csv(
  most_recent_file("tmp/entities_to_names"),
  col_types = "cccc"
) %>%
  filter(entity_cat != "auto-generated-filer")


entities_to_names_noaddr <- entities_to_names %>%
  select(-address_std) %>%
  unique()

entities_to_names_noaddr %>% group_by(name_std) %>% count()  %>% arrange(desc(n)) %>% as.data.frame() %>% head(20)
entities_to_names_noaddr %>% group_by(name_std) %>% filter(n()>1)


## Don't attempt the dups if they are voters
entities_to_names_noaddr <- entities_to_names_noaddr %>% 
  group_by(name_std) %>%
  filter(!(n() > 1 & entity_cat == "voter"))

## Should be zero
entities_to_names_noaddr %>% group_by(name_std) %>% filter(n()>1) %>% ungroup %>% with(table(entity_cat))

candidates <- readRDS(most_recent_file("../../data/processed_data/candidates"))
candidates_to_races <- readRDS(most_recent_file("../../data/processed_data/candidates_to_races"))

## SCRIPT TO REPLACE CANDIDATES THAT DON'T MATCH
# matched <- rep(NA, nrow(cats))
# 
# check_replace <- function(matched, oldname, newname){
#   try1 <- match(
#     gsub(oldname, newname, cats$TypeDetail),
#     paste(candidates$first_name, candidates$last_name)
#   )
#   try2 <- match(
#     gsub(oldname, newname, cats$TypeDetail),
#     paste(candidates$first_name, candidates$middle_name, candidates$last_name)
#   )
#   try3 <- match(
#     gsub(oldname, newname, cats$TypeDetail),
#     paste(candidates$first_name, candidates$middle_name, candidates$last_name, candidates$suffix)
#   )
#   try4 <- match(
#     gsub(oldname, newname, cats$TypeDetail),
#     paste(candidates$first_name, candidates$last_name, candidates$suffix)
#   )
#   try <- ifelse(!is.na(try1), try1, ifelse(!is.na(try2), try2, ifelse(!is.na(try3), try3, try4)))
#   replace <- !is.na(try) & is.na(matched)
#   print(table(replace))
#   matched[replace] <- candidates$candidate_id[try[replace]]
#   return(matched)
# }
# matched %<>% check_replace("", "")
# matched %<>% check_replace("WILLIAM", "BILL")
# matched %<>% check_replace("BILL(Y?)", "WILLIAM")
# matched %<>% check_replace("MATTHEW", "MATT")
# matched %<>% check_replace("VINCE", "VINCENT")
# matched %<>% check_replace("VERN", "VERNON")
# matched %<>% check_replace("BOB", "ROBERT")
# matched %<>% check_replace("MIKE", "MICHAEL")
# matched %<>% check_replace("DAVID", "DAVE")
# matched %<>% check_replace("JOE", "JOSEPH")
# matched %<>% check_replace("KEN", "KENNETH")
# matched %<>% check_replace("RON", "RONALD")
# matched %<>% check_replace("DAN", "DANIEL")
# matched %<>% check_replace("DOUGLAS", "DOUG")
# matched %<>% check_replace("EMANUEL", "MANNY")
# matched %<>% check_replace("DENNY", "DENNIS")
# matched %<>% check_replace("ANDY", "ANDREW")
# matched %<>% check_replace("CHRIS", "CHRISTOPHER")
# matched %<>% check_replace("ALMIRON", "ALMIRÓN")
# matched %<>% check_replace("LARRY", "LAWRENCE")
# matched %<>% check_replace("JENN","JENNIFER")
# matched %<>% check_replace("TERRY","TERRENCE")
# matched %<>% check_replace("TERRANCE","TERRENCE")
# matched %<>% check_replace("JOSH","JOSHUA")
# matched %<>% check_replace("MARTY","MARTIN")
# matched %<>% check_replace("RONALD","RON")
# matched %<>% check_replace("DELANEY","DELANY")
# matched %<>% check_replace("BRWONLEE","BROWNLEE")
# matched %<>% check_replace("BROWNEE","BROWNLEE")
# matched %<>% check_replace("ROBERT","BOBBY")
# matched %<>% check_replace("TOM","THOMAS")
# matched %<>% check_replace("FERNANDO TREVI.*O", "FERNANDO TREVIÑO")
# matched %<>% check_replace("SETH","R SETH")
# matched %<>% check_replace("\\bO ","O")
# matched %<>% check_replace("-"," ")
# matched %<>% check_replace("ADRIAN RIVERA ", "ADRIÁN RIVERA-")
# 
# 
# ## delete middle
# matched %<>% check_replace("^(.+)\\b [A-Z]+ \\b(.+)$", "\\1 \\2")
# 
# cats$TypeDetail[!is.na(matched)] <- matched[!is.na(matched)]
# write_csv(cats, "outputs/filers.csv")


NON_CANDIDATES <- c(
  "ARCHYE LEACOCK", "ANTHONY BURTULATO", "CHARLIE HILLS", "BRIAN JAEGER",
  "CHARLES CARLIN", "JOHN YUDICHAK", "MIKE BOYLE", "STEVEN VAUGHN-LEWIS", "CHRISTOPHER HAYES", "KEITH GOODMAN",
  "JOSEPH SCARNATI", "FRANCESCA IACOVANGELO", "JIM BREWSTER", "LILLIAN TURNER", "TIM SOLOBAY", "WAYNE FONTANA",
  "NYDIA VELAZQUEZ", "CRAIG WASHINGTON", "DENEEN WILKERSON", "DIANE MONROE", "DENIS COHEN", "FLORENCE COHEN",
  "JUDY SCHWANK", "ANDREW MCGINLEY", "DAPHNE GOGGINS", "ED MARSICO", "ED PAWLOWSKI", "EDWARD THORNTON",
  "FRANK DERMODY", "GEORGE MATYSIK", "GLENN DAWSON", "JACK STOLLSTEIMER", "JEFF DENCE", "JOE COX",
  "JOE GUERRA", "JOHN MEYERS", "KARLA CRUEL", "KEN LAWRENCE", "KEN TRUJILLO", "KENDRA BROOKS",
  "LISA BOSCOLA", "LUCINA LITTLE", "MADELEINE DEAN", "MARY JO DALEY", "MARY-JO DALEY", "MATT BRADFORD",
  "MICHAEL WILLIAMS", "MICHELE HANGLEY", "MIKE GERBER", "MIKE J MORMELLO", "MIKE SCHLOSSBERG", "NIC OROURKE",
  "PATRICK JONES", "PATTY KIM", "PETER SCHWEYER", "PHIL ROSENZWEIG", "RICHARD DEMARCO", "ROB TEPLITZ",
  "SEAN WILEY", "SEAN MCMONAGLE", "JOHN BLAKE", "TINA DAVIS", "TODD EACHUS", "WADE ALBERT",
  "BARBARA GLOVER", "BARBARA GONZALEZ", "BARBARA GOODMAN", "HARRISK HARRISK", "JARRETT ANDERSON", "JAY COSTA",
  "JEANNETTE GETER", "JOANN BELL", "JOHN DANFORD", "JOHN GIORDANO", "JOHN HENNINGER", "JOHN MILLIGAN",
  "BLANK GONZALEZ", "JOHN SMITH", "JOSEPH GUERRA", "KAPLIN STEWART", "KENNETH TRUJILLO", "KERENSA SOL FIGUEROA",
  "LAURI KAVULICH", "LEAH WRIGHT", "JENNIFER LENTZ", "LISA GUTH", "MAGGIE MARCH", "MARK WULLER",
  "MARY JONES", "MAUREEN BACHICH", "MICHAEL J CUNNINGHAM", "MORENE FREEDMAN", "MYLES GORDON", "NA'IM MINCY",
  "OLA BAILEY", "OMAR WOODARD", "OSCAR PAYNE", "PAMALA WILLIAMS", "PAMELA GRONDSKI", "PAT MEEHAN",
  "PAUL COSTA", "TONYA BAH", "PERRY MCGOWAN", "PETER ANDERSON", "PETER WILSON", "PRIYA SHAH",
  "RICHARD HAYDEN", "RICHARD GMEREK", "ROBERT CUMMINGS", "ROBERT TUREK", "ROBERT HERSHMAN", "RONDAL COUSER",
  "SABRIYA BILAL", "SCOTT TARBUTTON", "SHAWN MURPHY", "STACY SHIELDS", "STEFANIE CARNEY", "STEVE CHERNIAVSKY",
  "STEVE SANTARSIERO", "TARA QUAY", "TIMOTHY SPAIN", "VIRNELDIA HAINES", "WILLIAM COBB",
  "NAIM MINCY", "CHRISSY HOULAHAN"
)

## fix accents...
for(pattern in sprintf(c("ERIKA ALMIR%sN", "ADRI%sN RIVERA.?REYES", "FERNANDO TREVI%sO"), "(.{1}|.{2})")){
  print("Replacing")
  print(entities_to_names$entity_id[grepl(pattern, entities_to_names$entity_id)])
  print("with")
  print(candidates$candidate_id[grepl(pattern, candidates$candidate_id)])
  print("")
  entities_to_names$entity_id[grepl(pattern, entities_to_names$entity_id)] <- candidates$candidate_id[grepl(pattern, candidates$candidate_id)]
}

matched <- entities_to_names$entity_id %in% candidates$candidate_id

entities_to_names$entity_id[
  !matched & !is.na(entities_to_names$entity_cat) & entities_to_names$entity_cat == "candidate" & 
    !is.na(entities_to_names$entity_cat) & !entities_to_names$entity_id %in% NON_CANDIDATES
  ] %>%
  unique() %>% length

entities_to_names$entity_id[
  !matched & !is.na(entities_to_names$entity_cat) & entities_to_names$entity_cat == "candidate" & 
    !is.na(entities_to_names$entity_cat) & !entities_to_names$entity_id %in% NON_CANDIDATES
  ] %>%
  unique()  #%>%
  # sprintf(fmt='"%s"') %>% paste(collapse=", ") %>% cat


left_join_1to1 <- function(df0, df1, ...){
  df <- left_join(df0, df1, ...)
  if(nrow(df0) != nrow(df)) stop("rows were added or dropps")
  return(df)
}

df <- left_join_1to1(
  df, 
  entities_to_names_noaddr,
  by=c("filer_name_std"="name_std")
)

df <- df %>% rename(filer_cat=entity_cat, filer_id=entity_id)
auto_gen <- is.na(df$filer_id) & !is.na(df$filer_name_std)
df$filer_cat[auto_gen] <- "auto-generated-filer"
df$filer_id[auto_gen] <- df$filer_name_std[auto_gen] 

## HAND CATEGORIZE, then add fixes to entities.csv
write.csv(
  df %>% 
    rename(entity_id=filer_id, entity_cat=filer_cat) %>%
    group_by(filer_name_std, entity_cat, entity_id) %>% 
    summarise(years=paste(unique(Year), collapse=", ")), 
  dated_stem("tmp/filers_to_entities", fileext="csv"),
  row.names=FALSE,
  na=""
)

## Update entities
new_entities <- df %>% 
  filter(filer_cat == "auto-generated-filer") %>%
  select(filer_name_std, filer_cat, filer_id) %>%
  rename(entity_cat=filer_cat, entity_id=filer_id, name_std=filer_name_std) %>%
  unique() %>%
  mutate(address_std="")

table(new_entities$name_std %in% entities_to_names$name_std)

entities_to_names <- bind_rows(entities_to_names, new_entities)
write_csv(
  entities_to_names, 
  path=dated_stem("tmp/entities_to_names", fileext="csv"),
  na=""
)

df %>% 
  filter(filer_cat == "candidate") %>%
  as.data.frame()

doctypes <- tribble(
  ~DocType, ~doc_cat,
  "Campaign Finance Statement", "statement", 
  "CFR - Schedule I - Contributions and Receipts", "receipt", # less than 50
  "CFR - Schedule I - Part A - Contributions Received From Political Committees ($50.01 to $250.00)","receipt",
  "CFR - Schedule I - Part B - All Other Contributions ($50.01 - $250.00)","receipt",
  "CFR - Schedule I - Part C - Contributions Received From Political Committees (Over $250.00)","receipt",
  "CFR - Schedule I - Part D - All Other Contributions (Over $250.00)","receipt",       
  "CFR - Schedule I - Part E - Other Receipts","receipt",
  "CFR - Schedule II - In-Kind Contributions and Valuable Things Received","inkind",
  "CFR - Schedule II - Part F - In-Kind Contributions Received (Value of $50.01 to $250.00)","inkind",   
  "CFR - Schedule II - Part G - In-Kind Contributions Received (Value Over $250.00)","inkind",
  "CFR - Schedule III - Statement of Expenditures","expenditure",
  "CFR - Schedule IV - Statement of Unpaid Depts", "debt",                                                
  "Independent Expenditures","indpendent expenditure",                                             
  "Late Contributions","late contributions"
)

df %<>% left_join(doctypes)

df %>%
  filter(filer_cat == "candidate", doc_cat=="expenditure", year==2019) %>%
  group_by(filer_id, year) %>%
  summarise(amt=sum(Amount, na.rm=T)) %>%
  arrange(desc(amt)) %>% 
  as.data.frame()

#################################
## MATCH DONORS TO PARTY IDS
#################################

df %>% filter(grepl("jonathan tannen", EntityName, ignore.case=T))

fve <- readRDS("../../data/processed_data/voter_db/fve.Rds")
fve %>% filter(last_name == "TANNEN") %>% as.data.frame()

fve$address <- with(
  fve, 
  paste(nona(house_number), nona(house_number_suffix), nona(street_name))
)
fve$address <- standardize_address(fve$address)

fve$name <- paste(fve$first_name, fve$last_name)
fve$name_address <- paste(fve$name, fve$address, sep=":::")


streets <- fve %>% group_by(street_name) %>% summarise()


if(FALSE){
  donors <- df %>%
    filter(!is.na(entity_name), address != "") %>%
    group_by(entity_name, address) %>% 
    summarise() %>%
    ungroup() %>%
    mutate(
      name = str_to_upper(entity_name), 
      upper_address = str_to_upper(address)
    )
  
  donors$upper_address <- gsub("\\bSTREET\\b", "ST", donors$upper_address)
  donors$upper_address <- gsub("\\bAVENUE\\b", "AVE", donors$upper_address)
  donors$upper_address <- gsub("\\bBOULEVARD\\b", "BLVD", donors$upper_address)
  
  ## unorder so the fraction matched is informative
  set.seed(215)
  donors <- sample_frac(donors, 1)
  
  ids <- rep(NA, nrow(donors))
  chunk_size <- 100
  continue <- TRUE
  i <- 0
  while(continue){
    rows <- i*chunk_size + (1:chunk_size)
    
    if(max(rows) > nrow(donors)){
      rows <- rows[rows <= nrow(donors)]
      continue <- FALSE
    }
    
    donors_chunk <- donors[rows,]
    dists <- adist(
      donors_chunk$upper_address, 
      streets$street_name
    )
    closest_streets <- apply(
      dists,
      1, 
      function(x) which(rank(x, ties.method = "first") <= 5)
    )
    
    name_address <- with(donors_chunk, paste(name, upper_address, sep=":::"))
    for(j in seq_along(rows)){
      try_streets <- streets[closest_streets[,j],]
      try_voters <- inner_join(fve, try_streets, by="street_name")
      voter_guess <- agrep(name_address[j], try_voters$name_address)
      if(length(voter_guess)>0) {
        ids[rows[j]] <- try_voters$voter_id[voter_guess]
      } else {
        ids[rows[j]] <- NA
      }
    }
    i <- i+1
    print(sprintf("%s/%s, %s matched", i*chunk_size, nrow(donors), sum(!is.na(ids))))
  }
  
  # saveRDS(ids, "outputs/id_matches.RDS")
  ids[!is.na(ids)] %>% unique %>% length
  donors$voter_id <- ids
  # saveRDS(donors, "outputs/donors.Rds")
} else {
  donors <- readRDS("tmp/donors.Rds")
}

donors %<>% mutate(
  entity_name_std = standardize_text(entity_name),
  address=standardize_address(address)
)

donors %>% filter(grepl("TANNEN", entity_name_std)) %>% as.data.frame()

df %<>% left_join(
  donors %>% select(entity_name_std, address, voter_id),
  by=c("address", "entity_name_std")
)

carrtannens <- tribble(
  ~voter_id, ~name,
  '014583972-51', 'CATHERINE CARR',
  '014583975-51', 'LOUIS TANNEN',
  '020813260-51', 'ETHAN TANNEN',
  '015389287-51', 'JONATHAN B TANNEN'
)

## TODO:
## Remove street types ("Ave", "St", etc), see Louis Tannen below
## Confirm if amended is duplicates

######################
## VALIDATE ENTITIES_TO_NAMES
######################

entities_to_names <- read_csv(most_recent_file("tmp/entities_to_names"), col_types = "cccc") 

## name_std + address_std should be unique
entities_to_names %>% group_by(name_std, address_std) %>% count() %>% filter(n>1)

## WHICH name_std MAP TO MULTIPLE entity_id? (THESE SHOULD LARGELY BE DIFF PEOPLE)
dups <- entities_to_names %>% 
  group_by(name_std) %>% 
  mutate(n_ids=length(unique(entity_id))) %>%
  ungroup() %>%
  filter(n_ids > 1) %>% 
  arrange(name_std)
nrow(dups)

## MAKE SURE NO CANDIDATES ALSO APPEAR AS VOTERS
dups %>% group_by(name_std) %>%
  filter(any(entity_cat == "candidate"))

dups %>% group_by(name_std) %>%
  filter(any(entity_cat == "company"))

dups %>% group_by(name_std) %>%
  filter(any(entity_cat == "pac"))

dups %>% group_by(name_std) %>%
  filter(any(entity_cat == "individual"))%>% as.data.frame()


##########################
## CLEAN UP ENTITIES
##########################

## Guess the entity
df$entity_cat <- NULL
df$entity_id <- NULL

## By name and address
df %<>% 
  left_join_1to1(
    entities_to_names,
    by=c("entity_name_std" = "name_std", 'address' = "address_std")
  )

table(is.na(df$entity_id))

## By name and address1
df %<>% 
  left_join_1to1(
    entities_to_names,
    by=c("entity_name_std" = "name_std", 'add1_std' = "address_std"),
    suffix=c("", ".y")
  )

maybe_replace_ent <- function(df, entity_cat, entity_id){
  replace_rows <- is.na(df$entity_cat)
  
  print(sprintf("Replacing %s out of %s NA rows", sum(replace_rows & !is.na(entity_cat)), sum(replace_rows)))
  
  df$entity_cat[replace_rows] <- entity_cat[replace_rows]
  df$entity_id[replace_rows] <- entity_id[replace_rows]
  
  return(df)
}

df %<>% maybe_replace_ent(df$entity_cat.y, df$entity_id.y)
df$entity_cat.y <- NULL; df$entity_id.y <- NULL

## By name only
## If there is a unique name match in entities, use that
uniquely_named_entities <- entities_to_names %>% 
  group_by(name_std, entity_cat, entity_id) %>% 
  summarise() %>%
  group_by(name_std) %>%
  filter(n()==1) %>%
  ungroup()

df <- left_join_1to1(
  df, 
  uniquely_named_entities, 
  by=c("entity_name_std" = "name_std"),
  suffix=c("", ".y")
)
df %<>% maybe_replace_ent(df$entity_cat.y, df$entity_id.y)
df$entity_cat.y <- NULL; df$entity_id.y <- NULL

## Try without the last address word (e.g. rd->st)
df$add1_cut <- gsub("^(.*)\\s\\S+$", "\\1", df$add1_std)
entities_to_names$add1_cut <- gsub("^(.*)\\s\\S+$", "\\1", entities_to_names$address_std)
entities_to_names_cut <- entities_to_names %>% 
  select(name_std, add1_cut, entity_cat, entity_id) %>%
  unique

## should be zero...
entities_to_names_cut %>% 
  group_by(name_std, add1_cut) %>%
  filter(n() > 1)

df %<>% 
  left_join_1to1(
    entities_to_names_cut,
    by=c("entity_name_std" = "name_std", 'add1_cut' = "add1_cut"),
    suffix=c("", ".y")
  )

df %<>% maybe_replace_ent(df$entity_cat.y, df$entity_id.y)
df$entity_cat.y <- NULL; df$entity_id.y <- NULL
df$add1_cut <- NULL


entities_to_check <- df %>% 
  filter(!is.na(EntityName)) %>%
  group_by(entity_name_std, add1_std, entity_id, entity_cat, filer_id, doc_cat) %>%
  summarise(Amount = sum(Amount, na.rm=T)) %>%
  group_by(entity_name_std, add1_std, entity_id, entity_cat) %>%
  summarise(
    top_filers = paste(filer_id[order(Amount, decreasing=TRUE)[1:5]], collapse=" || "),
    receipt=sum(Amount * (doc_cat == "receipt")),
    expenditure=sum(Amount * (doc_cat == "expenditure")),
    total=sum(Amount)
  )


## auto-generated matches with EntityNames should probably be explicitly added to entities_to_names with real entity_cat
autogens <- df %>% filter(entity_cat == "auto-generated-filer") %>% 
  group_by(entity_id, add1_std) %>% 
  summarise()
autogens

write_csv(autogens, path = "tmp/autogens.csv")

## GO THROUGH TOP ENTITIES BY HAND 
## ADD correct entity_ids TO entities_to_names

write.csv(
  entities_to_check, 
  file=dated_stem("tmp/entities_to_check", fileext = "csv"),
  row.names = FALSE,
  na=""
)


write.csv(
  entities_to_check %>%
    filter(is.na(entity_id)) %>% 
    group_by(entity_name_std) %>% 
    summarise(total = sum(total)), 
  file=dated_stem("tmp/entities_to_check", fileext = "csv"),
  row.names = FALSE,
  na=""
)

auto_gen <- is.na(df$entity_cat)
df$entity_cat[auto_gen] <- "auto-generated-entity"
df$entity_id[auto_gen] <- df$entity_name_std[auto_gen]


df %>% 
  select(entity_name_std, entity_cat, entity_id) %>%
  unique() %>%
  arrange(entity_name_std) %>%
  group_by(entity_name_std) %>%
  filter(
    n() > 1, 
    any(
      entity_cat %in% c("company", "candidate", "organization", "union")
    )
  )



df %>% inner_join(carrtannens, by=c(entity_id="voter_id")) %>% as.data.frame()
df %>% filter(grepl("louis.*tannen", entity_id, ignore.case = T)) %>% as.data.frame()

df %>% 
  filter(year == 2019, doc_cat=="expenditure") %>%
  mutate(filer_shortname = substr(FilerName, 1, 20)) %>%
  group_by(filer_shortname, filer_cat, filer_id) %>%
  summarise(amt=sum(Amount)) %>%
  mutate(filer_id = substr(filer_id,1,20)) %>%
  arrange(desc(amt)) %>% 
  as.data.frame()

## Allan Domb is duplicated since he donated to himself
df %>% 
  filter(year == 2019, doc_cat=="expenditure", filer_id=="ALLAN DOMB") %>%
  mutate(filer_shortname = substr(FilerName, 1, 50)) %>%
  group_by(filer_shortname, EntityName) %>%
  summarise(amt=sum(Amount)) %>%
  arrange(desc(amt)) %>% 
  as.data.frame()

df %>% 
  filter(year == 2019, filer_id == "ISAIAH THOMAS", doc_cat=="expenditure") %>%
  group_by(EntityName) %>% 
  summarise(Amount = sum(Amount)) %>% 
  arrange(desc(Amount)) %>%
  as.data.frame()

df %>% 
  filter(year == 2019, filer_id == "HELEN GYM", doc_cat=="receipt") %>%
  mutate(
    entity_shortid = substr(entity_id, 1, 20),
    entity_shortname = substr(EntityName, 1, 20)
  ) %>%
  group_by(entity_shortname, entity_cat, entity_shortid) %>% 
  summarise(Amount = sum(Amount)) %>% 
  arrange(desc(Amount)) %>%
  as.data.frame()


## who is Forward together philadelphia? Super PAC for Kenney
## pft gave 100,000
df %>% 
  filter(year == 2019, EntityName == "Forward Together Philadelphia", doc_cat=="expenditure") %>%
  arrange(desc(Amount)) %>%
  as.data.frame()

## They spent entirely on GPS Impact, seems like for Kenney
df %>% 
  filter(year == 2019, FilerName == "Forward Together Philadelphia", doc_cat=="expenditure") %>%
  arrange(desc(Amount)) %>%
  as.data.frame()


df %>% 
  filter(FilerName == "Forward Together Philadelphia", doc_cat=="expenditure") %>%
  arrange(desc(Amount)) %>%
  as.data.frame()

## A BUNCH OF THESE ARE CANDIDATES. SHOULD BE FIXED
df %>% 
  filter(year == 2019, doc_cat=="receipt") %>%
  inner_join(fve %>% select(voter_id, first_name, last_name), by=c("entity_id"="voter_id")) %>%
  group_by(entity_id, first_name, last_name) %>%
  summarise(amt=sum(Amount)) %>% 
  arrange(desc(amt)) %>% as.data.frame()

df %>% 
  filter(year == 2018:2019, cat=="expenditure") %>%
  group_by(EntityName) %>%
  summarise(amt=sum(Amount)) %>% 
  arrange(desc(amt)) %>% as.data.frame()

df %>% 
  filter(year == 2019, cat=="receipt", EntityName == "Jeff Yass") %>%
  group_by(FilerName) %>%
  summarise(amt=sum(Amount)) %>% 
  arrange(desc(amt)) %>% as.data.frame()

df %>% 
  filter(year %in% 2018:2019, cat=="receipt", EntityName == "CDS") %>%
  group_by(FilerName) %>%
  summarise(amt=sum(Amount)) %>% 
  arrange(desc(amt)) %>% as.data.frame()
