library(tidyverse)
library(sf)
library(magrittr)

setwd("C:/Users/Jonathan Tannen/Dropbox/sixty_six/admin_scripts/prep_data/")
source("../../admin_scripts/util.R")

######################
## PROCESS DF
######################

# fve <- readRDS("outputs/fve.Rds")
# voters_to_elections <- readRDS("outputs/voters_to_elections.Rds")
# elections <- readRDS("outputs/elections.Rds")

logit <- function(p) log(p) - log(1-p)

con <- DBI::dbConnect(
  RSQLite::SQLite(), 
  dbname = "C:/Users/Jonathan Tannen/Dropbox/sixty_six/data/database/sixtysixwardsdb.sqlite"
)

fve <- tbl(con, "fve")
# voters_to_elections <- tbl(con, "voters_to_elections")
elections <- tbl(con, "fve_elections")
turnout <- tbl(con, "turnout")
# div_turnout <- tbl(con, "div_turnout")
# div_turnout_smoothed <- tbl(con, "div_turnout_smoothed")
turnout_svd_div <- tbl(con, "turnout_svd_div")
turnout_svd_election <- tbl(con, "turnout_svd_election")


v2e0_pred <- most_recent_tbl(con, "v2e0") %>%
  filter(year == 2019, is_primary) %>%
  select(
    voter_id,
    warddiv,
    voted,
    age, 
    voter_status
  ) %>%
  collect() %>%
  mutate(is_active=(voter_status=="A"))

inactive_vote_rate <- v2e0_pred %>% 
  filter(is_active) %>%
  with(mean(voted))

predict_with_inactive <- function(fit, newdata, type="response"){
  p <- predict(fit, newdata=newdata, type=type)
  p[!newdata$is_active] <- inactive_vote_rate
  p[p<0] <- 0
  p[p>1] <- 1
  return(p)
}


## PICK VALUES FOR THIS ELECTION
ggplot(
  turnout %>% collect(),
  aes(
    x=year, 
    y=turnout, 
    group=as.numeric(year) %% 4
  )
) +
  geom_line(
    size=2, 
    aes(color=as.numeric(year) %% 4)
  ) +
  facet_wrap(~election) +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5))

ggplot(
  turnout_svd_election %>% inner_join(
    elections %>% select(year, election_type, cycle),
    by=c("year"="year","election"="election_type")
  ) %>% 
    collect() %>%
    gather("key","value",log_mean:score.5),
  aes(x=year, y=value, group=cycle, color=cycle)
) + 
  geom_point() + 
  geom_line(size=2) + 
  facet_grid(election~key) +
  theme_minimal() +
  geom_hline(yintercept=0)


## Change necessary details
pred_election_stats <- data.frame(
  turnout = 220e3,
  election_date = "2019-11-05",
  year=2019,
  is_primary=FALSE,
  election_type="general",
  election_score.1=-5.0,
  election_score.2=5.0,
  election_score.3=-2.5,
  election_score.4=-1.25,
  election_score.5=1.25
)

# pred_election_stats <- data.frame(
#   turnout = 220e3,
#   election_date = "2019-05-21",
#   year=2019,
#   is_primary=TRUE,
#   election_score.1=-7.2,
#   election_score.2=5.5,
#   election_score.3=1.5,
#   election_score.4=2.3,
#   election_score.5=2.7
# )


pred_div <- turnout_svd_div %>%
  collect() %>%
  rename(
    div_score.1 = score.1,
    div_score.2 = score.2,
    div_score.3 = score.3,
    div_score.4 = score.4,
    div_score.5 = score.5
  ) %>%
  mutate(
    score = log_mean + 
      div_score.1 * pred_election_stats$election_score.1 +
      div_score.2 * pred_election_stats$election_score.2 +
      div_score.3 * pred_election_stats$election_score.3 +
      div_score.4 * pred_election_stats$election_score.4 +
      div_score.5 * pred_election_stats$election_score.5,
    prop_turnout = exp(score)
  )

mean_factor <- pred_election_stats$turnout / sum(pred_div$prop_turnout)
pred_election_stats$log_mean <- log(mean_factor)

pred_div$div_turnout <- pred_div$prop_turnout * mean_factor

fve_counts <- fve %>% 
  filter(voter_status=="A") %>%
  group_by(warddiv) %>%
  summarise(active_voters=n()) %>%
  collect()

pred_div <- pred_div %>%
  left_join(
    fve_counts %>% select(warddiv, active_voters), 
    by="warddiv"
  ) %>%
  mutate(turnout_per_voter=div_turnout/active_voters)


v2e0_pred <- cbind(v2e0_pred, pred_election_stats)
v2e0_pred <- left_join(v2e0_pred, pred_div, by="warddiv")
v2e0_pred$is_active <- v2e0_pred$voter_status=="A"


fit0 <- readRDS(most_recent_file("tmp/fit0"))
v2e0_pred$pred <- predict_with_inactive(
  fit0, 
  newdata=v2e0_pred %>% mutate(is_primary=as.numeric(is_primary))
)

sum(v2e0_pred$pred)

max_order <- 27

v2e1 <- most_recent_tbl(con, "v2e1")
merge_dates <- most_recent_tbl(con, "merge_dates")

v2e_comp_pred <- v2e1 %>% 
  select(voter_id, order, fit0_pred, fit0_resid, fit0_surprisal, was_here, voted) %>%
  rename(comp_order=order) %>%
  inner_join(
    merge_dates %>% filter(order == max_order) %>% select(order, comp_order, lag),
    by=c("comp_order")
  ) %>%
  select(voter_id, order, lag, fit0_pred, fit0_resid, fit0_surprisal, was_here, voted) %>%
  collect()

v2e_comp_pred <- v2e_comp_pred %>%
  pivot_wider(
    id_cols=c(voter_id, order),
    names_from=lag,
    values_from=fit0_pred:voted
  )

v2e_comp_pred <- left_join(
  v2e_comp_pred, 
  fve %>% mutate(is_active = (voter_status=="A")) %>% select(voter_id, is_active) %>% collect(),
  by="voter_id"
)

v2e_comp_pred <- cbind(v2e_comp_pred, pred_election_stats)

for(var in c("fit0_pred", "fit0_resid", "fit0_surprisal", "was_here", "voted")){
  for(i in 8:1){
    old_var_name <- paste0(var,"_",i-1)
    new_var_name <- paste0(var,"_",i)
    v2e_comp_pred[[new_var_name]] <- v2e_comp_pred[[old_var_name]]
    v2e_comp_pred[[old_var_name]] <- NULL
  }
}

v2e_comp_pred <- v2e_comp_pred %>%
  left_join(v2e0_pred[,c("voter_id", "pred")] %>% rename(fit0_pred_0=pred))


fit_lag <- readRDS(most_recent_file("tmp/fit_lag"))
v2e_comp_pred$pred <- predict_with_inactive(fit_lag, newdata=v2e_comp_pred)

sum(v2e_comp_pred$pred, na.rm=TRUE)

logit <- function(x) log(x) - log(1-x)
inv_logit <- function(x) 1/(1+exp(-x))
adj_logit <- function(a, p) inv_logit(logit(p) + a)

logit_error <- function(a, pred=v2e_comp_pred$pred){
  new_pred <- adj_logit(a, pred)
  total_voters <- sum(new_pred, na.rm=TRUE)
  return(
    (220e3 - total_voters)^2
  )
}

adj <- optimize(logit_error, c(-10, 10))

logit_error(adj$minimum) %>% sqrt()

v2e_comp_pred$pred <- adj_logit(adj$minimum, v2e_comp_pred$pred)

copy_to(
  con,
  df=v2e_comp_pred %>% 
    select(
      voter_id,
      year,
      election_type,
      pred
    ),
  name=paste(
    "voter_pred", 
    pred_election_stats$year, 
    pred_election_stats$election_type, 
    sep="_"
  ) %>% dated_stem(),
  temporary=FALSE,
  overwrite=TRUE
)

carrtannens <- tbl(con, "carrtannens")
v2e_comp_pred %>% inner_join(carrtannens %>% collect) %>%
  as.data.frame()


table(is.na(v2e_comp_pred$pred))

entropy <- function(p) -ifelse(p<=0 | p>=1, 0, p * log(p) + (1-p) * log(1-p))
entropy(v2e_comp_pred$pred) %>% mean(na.rm=TRUE)
sum(v2e_comp_pred$pred, na.rm=TRUE)

ggplot(v2e_comp_pred, aes(x=pred)) +
  geom_histogram(boundary=0, binwidth=0.05) +
  scale_y_continuous(labels = scales::comma)

ggplot(
  v2e_comp_pred %>%
    mutate(pred_bin = cut(pred, seq(0,1,0.05))) %>%
    group_by(pred_bin) %>%
    summarise(voters = sum(pred, na.rm=TRUE)),  
  aes(x=pred_bin, y=voters)
) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::comma)


fve <- collect(fve)
v2e_comp_pred %>% filter(pred > 0.5) %>%
  sample_n(100) %>%
  inner_join(fve) %>%
  # with(hist(as.numeric(substr(dob,1,4))))
  as.data.frame()

