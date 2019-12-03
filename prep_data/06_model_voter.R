library(tidyverse)
library(sf)
library(magrittr)

setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/admin_scripts/prep_data")
source("../../admin_scripts/util.R")

######################
## PROCESS DF
######################

processed_data_dir <- "../../data/processed_data/voter_db"
# fve <- readRDS(sprintf("%s/fve.Rds", processed_data_dir))
# voters_to_elections <- readRDS(sprintf("%s/voters_to_elections.Rds", processed_data_dir))
# elections <- readRDS(sprintf("%s/elections.Rds", processed_data_dir))

con <- DBI::dbConnect(
  RSQLite::SQLite(), 
  dbname = "C:/Users/Jonathan Tannen/Dropbox/sixty_six/data/database/sixtysixwardsdb.sqlite"
)

fve <- tbl(con, "fve")
voters_to_elections <- tbl(con, "voters_to_elections")
elections <- tbl(con, "fve_elections")
turnout <- tbl(con, "turnout")
div_turnout <- tbl(con, "div_turnout")
div_turnout_smoothed <- tbl(con, "div_turnout_smoothed")
turnout_svd_div <- tbl(con, "turnout_svd_div")
turnout_svd_election <- tbl(con, "turnout_svd_election")

fve <- fve %>% mutate(
  party = ifelse(
    is.na(party_code), "U",
    ifelse(
      party_code=="D", "D", 
      ifelse(
        party_code == "R", "R", 
        "U"
      ))))


carrtannens <- tribble(
  ~name, ~voter_id,
  "jonathan", "015389287-51", 
  "ethan", "020813260-51", 
  "louis", "014583975-51",
  "cathy", "014583972-51"
)
carrtannens <- copy_to(con, carrtannens, name="carrtannens", temp=FALSE, overwrite=TRUE)
# voters_to_elections <- voters_to_elections %>% inner_join(carrtannens)

voter_stats <- voters_to_elections %>%
  mutate(
    voted = !is.na(vote_method) & vote_method != "P"  #provisional
  ) %>%
  filter(voted) %>%
  inner_join(elections %>% filter(!is_special) %>% select(election_id, election_date, is_general, is_primary)) %>%
  group_by(voter_id) %>%
  summarise(
    first_election = min(election_date, na.rm=TRUE),
    last_election = max(election_date, na.rm=TRUE),
    votes_primaries = sum(is_primary, na.rm=TRUE),
    votes_generals = sum(is_general, na.rm=TRUE),
    n_elections = n()
  )

###############
## fit a model
###############

elections <- elections %>% mutate(year = as.numeric(year))

years <- 2015:2019

get_v2e0_df <- function(years){
  min_year <- min(years)
  max_year <- max(years)
  
  filter_to_relevant_elections <- function(df) df %>% filter(
    (year <= max_year),
    year >= (!!min_year-4)
  )
  
  election_stats <- elections %>% 
    filter(is_general | is_primary) %>%
    filter_to_relevant_elections() %>%
    left_join(turnout, by=c("year"="year", "election_type"="election"))   %>%
    left_join(
      turnout_svd_election %>% 
        rename(
          election_log_mean = log_mean, 
          election_score.1 = score.1,
          election_score.2 = score.2,
          election_score.3 = score.3,
          election_score.4 = score.4,
          election_score.5 = score.5
        ),
      by=c("year"="year", "election_type"="election")
    )
  
  fve_counts <- fve %>% 
    group_by(warddiv) %>% 
    summarise(count_voters=n(), count_active_voters=sum(voter_status=="A"))

  voter_stats <- fve %>% 
    select(voter_id, registration_date, dob, warddiv, voter_status) %>%
    left_join(
      turnout_svd_div %>% 
        rename(
          div_log_mean = log_mean, 
          div_score.1 = score.1,
          div_score.2 = score.2,
          div_score.3 = score.3,
          div_score.4 = score.4,
          div_score.5 = score.5
        ), 
      by="warddiv"
    ) %>% 
    left_join(fve_counts, by="warddiv")
  
  v2e <- voters_to_elections %>%
    # inner_join(carrtannens) %>%
    inner_join(
      election_stats %>% 
        select(
          election_id, election_date, turnout, cycle, 
          election_type, year, 
          election_log_mean, election_score.1:election_score.5
        ),
      by="election_id"
    ) %>%
    left_join(
      voter_stats,
      by="voter_id"
    )
  
  v2e <- v2e %>% 
    left_join(
      div_turnout_smoothed %>% rename(div_turnout=turnout), 
      by=c('warddiv'='warddiv', "election_type"='election', "year"="year")
    )

  v2e <- v2e %>% 
    mutate(
      was_here = (election_date >= registration_date),
      age = year - as.numeric(substr(dob, 1, 4)),
      voted = !is.na(vote_method) & vote_method != "P",  #provisional
      turnout_per_voter = div_turnout / count_active_voters,
      is_primary = (election_type == "primary"),
      is_active = (voter_status == "A")
    )
  
  return(v2e)
}

if(FALSE){  
  v2e <- get_v2e0_df(years)
  print("computing")
  v2e0 <- compute(v2e, name=dated_stem("v2e0"), temporary=FALSE)
  print("done computing")  
} 

v2e0 <- most_recent_tbl(con, "v2e0") %>%
  select(
    voter_id, election_id,
    voted,
    age, voter_status,
    turnout_per_voter, is_primary,
    div_score.1:div_score.5, election_score.1:election_score.5,
    year, was_here
  ) %>%
  collect()

v2e0$age_bin <- cut(v2e0$age, breaks = c(17, 18, 19, seq(20,40,5), seq(50,90,10),200))
v2e0$is_active <- v2e0$voter_status == "A"


#####################################
## Level 1: Predict for a single year
######################################
set.seed(215)
unique_voters <- unique(v2e0$voter_id)
holdout_voters <- sample(unique_voters, size=round(length(unique_voters)*0.8))
v2e0$holdout <- v2e0$voter_id %in% holdout_voters
v2e0$subset <- rbernoulli(nrow(v2e0), p=0.05)
v2e0_subset <- filter(v2e0, subset)

age_vars <- c("age", "I(age^2)", "I(age==18)", "I(age > 90)")
covars <- c("turnout_per_voter", "is_primary", paste0("div_score.", 1:5), paste0("election_score.", 1:5))

all_vars <- expand.grid(a=age_vars, b=covars) %>% with(paste(a, b, sep="*", collapse =" + "))
print(all_vars)

if(FALSE){
  fit0 <- glm(
    as.formula(paste0("voted ~ ", all_vars)),
    family=binomial(link="logit"),
    data=v2e0 %>% filter(!holdout & year >= "2015" & was_here & is_active)
  )
  
  # fit0 <- lm(
  #   as.formula(paste0("voted ~ ", all_vars)),
  #   data=v2e0 %>% 
  #     filter(!holdout & year >= "2015" & was_here & is_active)
  # )
  
  saveRDS(
    fit0, 
    file=dated_stem("tmp/fit0", fileext="Rds")
  )
} else{
  fit0 <- readRDS(most_recent_file("tmp/fit0"))
}
summary(fit0)

# save preds, but too big

max_year <- max(v2e0$year)
inactive_vote_rate <- v2e0 %>% filter(year==max_year, !is_active) %>%
  with(mean(voted))

predict_with_inactive <- function(fit, newdata, type="response"){
  p <- predict(fit, newdata=newdata, type=type)
  p[!newdata$is_active] <- inactive_vote_rate
  p[p<0] <- 0
  p[p>1] <- 1
  return(p)
}

if(FALSE){
  fit0_preds <- rep(NA, nrow(v2e0))
  chunk_size <-1e6
  continue <- TRUE
  i <- 0
  while(continue){
    rows <- i * chunk_size + (1:chunk_size)
    if(!all(rows <= nrow(v2e0))) continue <- FALSE
    rows <- rows[rows <= nrow(v2e0)]
    fit0_preds[rows] <- predict_with_inactive(
      fit0, newdata=v2e0[rows,], type="response"
    )
    i <- i+1
    print(max(rows))
  }
  v2e0$fit0_pred <- fit0_preds
  v2e0_subset$fit0_pred <- fit0_preds[v2e0$subset]

  copy_to(
    con, 
    v2e0[,c("voter_id", "election_id", "fit0_pred")],
    name=dated_stem("fit0_pred"),
    overwrite=TRUE,
    temp=FALSE
  )
} else {
  fit0_pred <- most_recent_tbl(con, "fit0_pred") %>% collect() 
  v2e0 <- v2e0 %>% left_join(fit0_pred)
  v2e0_subset <- v2e0_subset %>% left_join(fit0_pred)
  rm(fit0_pred)
}

election_df <- elections %>% 
  filter(is_general|is_primary) %>%
  select(election_id, election_type, cycle, election_date, year) %>%
  arrange(election_date) %>%
  collect() %>%
  mutate(order = 1:n())

ggplot(
  v2e0_subset %>% 
    filter(holdout & year >= "2015" & was_here & is_active) %>%
    mutate(
      resid = voted - fit0_pred,
      pred_round = round(fit0_pred, 2)
    ),
  aes(x=pred_round)
) +
  geom_histogram(binwidth = 0.01, boundary=0) +
  facet_grid(year ~ is_primary)

ggplot(
  v2e0_subset %>% 
    filter(holdout & year >= "2015" & was_here & is_active) %>%
    mutate(
      resid = voted - fit0_pred,
      pred_round = round(fit0_pred, 2),
      election_type = ifelse(is_primary, "primary", "general")
    ) %>%
    group_by(pred_round, election_type, year) %>%
    summarise(mean_voted = mean(voted, na.rm=T), n=n()),
  aes(x=pred_round, y=mean_voted)
) +
  geom_point(aes(size=n), alpha = 0.2, pch=16) +
  geom_abline(slope=1, intercept=0) +
  facet_wrap(election_type~year)

xvar <- quo(div_score.1)
digits <- 2
ggplot(
  v2e0_subset %>% 
    filter(holdout & year >= "2015" & was_here & is_active) %>%
    mutate(
      resid = voted - fit0_pred,
      x=round(!!xvar, digits=digits),
      election_type = ifelse(is_primary, "primary", "general")
    ) %>%
    group_by(x, election_type, year) %>%
    summarise(mean_resid = mean(resid, na.rm=T), n=n()),
  aes(x=x, y=mean_resid)
) +
  geom_point(aes(size=n), alpha = 0.2, pch=16) +
  geom_hline(yintercept=0)+
  facet_wrap(~year + election_type) +
  xlab(rlang::as_name(xvar))

entropy <- function(p) -ifelse(p<=0 | p>=1, 0, p * log(p) + (1-p) * log(1-p))

v2e0_subset %>%
  filter(holdout, year >= "2015", was_here, is_active) %>%
  mutate(entropy=entropy(fit0_pred)) %>%
  group_by(year, is_primary) %>%
  summarise(
    pvoted = mean(voted),
    model_entropy = mean(entropy)
  ) %>%
  mutate(base_entropy = entropy(pvoted))

v2e0 %>% group_by(year, is_primary) %>%
  summarise(
    voted=sum(voted,na.rm=T),
    pred = sum(fit0_pred,na.rm=T)
  )

#############################
## Lagged model
#############################


if(FALSE){
  signed_surprisal <- function(x, p) ifelse(x==1, -log(p), log(1-p))
  
  v2e0 %<>% 
    mutate(
      fit0_resid = voted - fit0_pred,
      fit0_surprisal = signed_surprisal(voted, fit0_pred)
    )
  
  v2e0_subset %<>% 
    mutate(
      fit0_resid = voted - fit0_pred,
      fit0_surprisal = signed_surprisal(voted, fit0_pred)
    )
  
  v2e1 <- v2e0 %>% 
    select(
      voter_id, subset, holdout, is_active, 
      election_id, voted, was_here, age, age_bin,
      fit0_pred, fit0_resid, fit0_surprisal
    ) %>%
    left_join(election_df, by="election_id")
  
  merge_dates <- data.frame(year=years) %>%
    left_join(
      election_df %>% select(election_id, year, order)
    ) %>%
    left_join(
      expand.grid(order=election_df$order, comp_order=election_df$order) %>%
        mutate(lag = order - comp_order) %>%
        filter(lag >= 0, lag <= 8)
    ) 
  
  ## Push everything to sql
  copy_to(con, v2e1, name=dated_stem("v2e1"), temp=FALSE, overwrite=T)
  copy_to(con, merge_dates, name=dated_stem("merge_dates"), overwrite=TRUE, temp=FALSE)
}

v2e1 <- most_recent_tbl(con, "v2e1")
merge_dates <- most_recent_tbl(con, "merge_dates")

v2e_comp <- v2e1 %>% 
  select(voter_id, order, fit0_pred, fit0_resid, fit0_surprisal, was_here, voted) %>%
  rename(comp_order=order) %>%
  inner_join(
    merge_dates %>% select(order, comp_order, lag),
    by=c("comp_order")
  ) %>%
  select(voter_id, order, lag, fit0_pred, fit0_resid, fit0_surprisal, was_here, voted) %>%
  collect()

# v2e_comp_wide <- v2e1 %>%
#   select(voter_id, order, fit0_pred, fit0_resid, fit0_surprisal, was_here, voted) %>%
#   inner_join(
#     merge_dates %>% group_by(order) %>% summarise(),
#     by=c("order")
#   ) %>%
#   collect()

v2e_comp <- v2e_comp %>%
  pivot_wider(
    id_cols=c(voter_id, order),
    names_from=lag,
    values_from=fit0_pred:voted
  )

v2e_comp <- left_join(
  v2e_comp, 
  fve %>% mutate(is_active = (voter_status=="A")) %>% select(voter_id, is_active) %>% collect(),
  by="voter_id"
)
  
# v2e_comp <- left_join(v2e_comp_wide, v2e_comp, by=c("voter_id","order"))

vars_for_lag <- function(lag) sprintf("fit0_pred_%1$i * was_here_%1$i + election_type * fit0_resid_%1$i * was_here_%1$i", lag)

logit <- function(p) log(p) - log(1-p)

USE_GLM <- TRUE

lag_form <- as.formula(
  paste(
    "voted_0 ~ ",
    ifelse(USE_GLM, "logit(fit0_pred_0) +", "fit0_pred_0 +"),
    paste(vars_for_lag(1:8), collapse=" + ")
  )
)

v2e_comp$holdout <- v2e_comp$voter_id %in% holdout_voters

v2e_comp <- v2e_comp %>% 
  left_join(election_df %>% select(order, election_type, cycle, year))

if(FALSE){
  USE_GLM <- TRUE
  if(USE_GLM) {
    fit_fn <- function(form, data, ...) glm(form, family=binomial(link="logit"), data, ...)
  } else {
    fit_fn <- function(form, data, ...) lm(form, data, ...)
  }
  
  fit_lag <- fit_fn(
    lag_form,
    v2e_comp %>% filter(!holdout, is_active)
  )
  
  saveRDS(fit_lag, file=dated_stem("tmp/fit_lag",fileext="Rds"))
} else {
  fit_lag <- readRDS(most_recent_file("tmp/fit_lag"))  
}

summary(fit_lag)


get_coefs <- function(lag){
  s1 <- sprintf("fit0_resid_%i", lag)
  s2 <- sprintf("was_here_%1$i:fit0_resid_%1$i", lag)
  surprisal_total <- sum(coef(fit_lag)[c(s1, s2)])
  p1 <- sprintf("fit0_pred_%i", lag)
  p2 <- sprintf("fit0_pred_%1$i:was_here_%1$i", lag)
  pred_total <- sum(coef(fit_lag)[c(p1, p2)])
  data.frame(
    lag=lag,
    surprisal=surprisal_total,
    pred=pred_total
  )
}

ggplot(
  bind_rows(lapply(1:8, get_coefs)) %>% gather("key", "value", -lag),
  aes(x=lag, y=value, color=key)
) +
  geom_point() +
  geom_line(aes(group=key))


v2e_comp$pred <- predict_with_inactive(
  fit_lag, 
  newdata=v2e_comp,
  type = "response"
)

pdf <- v2e_comp %>% 
  filter(holdout) %>% 
  group_by(order) %>% 
  summarise(
    voted=sum(voted_0), 
    pred0 = sum(fit0_pred_0, na.rm=T),
    pred=sum(pred, na.rm=T) 
  )
pdf

ggplot(
  v2e_comp %>% 
    filter(holdout) %>%
    mutate(pred_round = round(pred, 2)) %>%
    group_by(pred_round, year, election_type) %>%
    summarise(
      voted = mean(voted_0),
      count=n()
    ),
    aes(x=pred_round, y=voted, size=count)
  ) + 
  geom_point(alpha = 0.4) +
  geom_abline(slope=1, intercept=0)+
  scale_size_area() +
  facet_grid(election_type ~ year)+
  coord_fixed()

ggplot(
  v2e_comp %>% 
    filter(holdout),
  aes(x=pred)
) + 
  geom_histogram(boundary=0, binwidth=0.05) +
  facet_grid(election_type ~ year)


entropy_df <- v2e_comp %>%
  filter(holdout, was_here_0) %>%
  mutate(
    entropy=entropy(pred),
    fit0_entropy = entropy(fit0_pred_0)
  ) %>%
  group_by(order) %>%
  summarise(
    pvoted = mean(voted_0),
    model_entropy = mean(entropy),
    fit0_entropy=mean(fit0_entropy)
  ) %>%
  mutate(base_entropy = entropy(pvoted)) %>%
  left_join(election_df %>% select(order, year, election_type, cycle))

entropy_df


xcol <- sym("fit0_pred_1")
was_here <- sym("was_here_1")
ggplot(
  v2e_comp %>% 
    filter(holdout) %>%
    mutate(x_round=round(!!xcol,2)) %>%
    group_by(x_round, !!was_here, order) %>%
    summarise(
      resid = mean(voted_0 - pred),
      count=n()
    ) %>% arrange(desc(!!was_here)),
  aes(x=x_round, y=resid, size=count, color=as.logical(!!was_here))
)+ 
  geom_point(alpha = 0.5) +
  geom_hline(yintercept=0)+
  scale_size_area() +
  facet_wrap(~ order)+
  xlab(rlang::as_name(xcol)) +
  theme_minimal()

if(FALSE){
  copy_to(
    con, 
    v2e_comp %>% 
      inner_join(
        election_df %>% select(election_id, order)
      ) %>%
      select(voter_id, election_id, pred),
    dated_stem("fit_lag"),
    temporary=FALSE
  )
}



