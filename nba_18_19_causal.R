library(tidyverse)
library(lubridate)
library(Matching)
library(MatchIt)
library(grf)


### Work for Propensity Score Model ###


nba5 <- read_csv("nba_18_19_analysis.csv")

### Create variables of proensity score model ###

nba5 <- nba5 %>%
 mutate(
  rating_H_max = pmax(p1_H_rating, p2_H_rating, p3_H_rating, p4_H_rating, p5_H_rating),
  rating_A_max = pmax(p1_A_rating, p2_A_rating, p3_A_rating, p4_A_rating, p5_A_rating),
  rating_H_mean = (p1_H_rating + p2_H_rating + p3_H_rating + p4_H_rating + p5_H_rating) / 5,
  rating_A_mean = (p1_A_rating + p2_A_rating + p3_A_rating + p4_A_rating + p5_A_rating) / 5
  )


nba_prop <- nba5 %>%
 dplyr::select(idGame, numberPeriod, time_left_sec, poss_af, scoreMarginHome, scoreMarginAway, tfo_opp, tfo_att, tfo_natt, pts_diff, rating_H_max, rating_A_max, rating_H_mean, rating_A_mean, Total, HSpread, VSpread) %>%
 filter((tfo_att == 1 | tfo_natt == 1)) %>%
 mutate(
  scoreMargin = ifelse(poss_af == "H", scoreMarginHome, scoreMarginAway),
  rating_max = ifelse(poss_af == "H", rating_H_max, rating_A_max),
  rating_max_opp = ifelse(poss_af == "H", rating_A_max, rating_H_max),
  rating_mean = ifelse(poss_af == "H", rating_H_mean, rating_A_mean),
  rating_mean_opp = ifelse(poss_af == "H", rating_A_mean, rating_H_mean),
  spread = ifelse(poss_af == "H", HSpread, VSpread),
  total_score = Total,
  scoreMarginsq = scoreMargin^2,
  numberPeriod1 = ifelse(numberPeriod == 1, 1, 0),
  numberPeriod2 = ifelse(numberPeriod == 2, 1, 0),
  team = ifelse(poss_af == "H", home_team.x, away_team.x)
 )

nba_prop <- nba_prop %>%
 filter((!is.na(spread)) & (!is.na(total_score)))




### Fit a logistic regression model ### 

prop_model <- stats::glm(tfo_att ~ as.factor(numberPeriod) + time_left_sec + scoreMargin + rating_max + rating_max_opp + rating_mean + rating_mean_opp + spread + total_score, data = nba_prop, family = "binomial")
nba_prop[["fitted_values"]] <- prop_model$fitted.values

nba_prop %>%
 ggplot() +
 geom_freqpoly(mapping = aes(x = fitted_values, color = as.factor(tfo_att)))

nba_prop %>%
 ggplot() +
 geom_density(mapping = aes(x = fitted_values, color = as.factor(tfo_att)))


match_model <- Match(Tr = as.logical(nba_prop[["tfo_att"]]), X = nba_prop[["fitted_values"]])
summary(match_model)
summary.Match(match_model)


matchbal_model <- MatchBalance(as.logical(tfo_att) ~  as.factor(numberPeriod) + time_left_sec + scoreMargin + rating_max + rating_max_opp + rating_mean + rating_mean_opp + spread + total_score, data = nba_prop, match.out = match_model)
plot(match_model)



match_model <- Match(Tr = as.logical(nba_prop[["tfo_att"]]), X = nba_prop[["fitted_values"]], Y = nba_prop[["pts_diff"]])
summary(match_model, full = TRUE)
summary.Match(match_model)


### Using MatchIt
library(MatchIt)

matchit.mod <- matchit(tfo_att ~ as.factor(numberPeriod) + time_left_sec + scoreMargin + rating_max + rating_max_opp + rating_mean + rating_mean_opp + spread + total_score, 
                       data=nba_prop, distance = "glm", method = "nearest", replace = TRUE, verbose = TRUE)

summary(matchit.mod)
m.data1 <- match.data(matchit.mod)
head(m.data1)
plot(matchit.mod)
plot(summary(matchit.mod))

matchit.lm <- lm(pts_diff~tfo_att + as.factor(numberPeriod) + time_left_sec + scoreMargin + rating_max + rating_max_opp + rating_mean + rating_mean_opp, data=m.data1, weights=weights)
summary(matchit.lm)
coeftest(matchit.lm, vcov. = vcovCL)


### Propensity Score Weighting 

psweight.form <- tfo_att ~ as.factor(numberPeriod) + time_left_sec + scoreMargin + rating_max + rating_max_opp + rating_mean + rating_mean_opp + spread + total_score

#using WeightIt
library(WeightIt)
library(cobalt)
bal.tab(psweight.form, data=nba_prop, estimand="ATT", thresholds = c(m = .05))
W.out <- weightit(psweight.form,
                  data = nba_prop, estimand = "ATT", method = "cbps")
summary(W.out)
bal.tab(W.out, stats = c("m", "v"), thresholds = c(m = .05))
covnames <- c("prop.score"="prop.score", "as.factor(numberPeriod)_1"="Period 1", "as.factor(numberPeriod)_2"="Period 2",
              "as.factor(numberPeriod)_3"="Period 3", "time_left_sec"="Time Left (Sec)", "scoreMargin"="Score Margin",
              "rating_max"="Max Player Rating", "rating_max_opp"="Max Player Rating (Opp)",
              "rating_mean"= "Avg Player Rating", "rating_mean_opp"="Avg Player Rating (Opp)",
              "spread"="Betting Spread", "total_score"="Betting Total")
love1 <- love.plot(W.out, stats = c("mean.diffs", "ks.statistics"), binary = "std", thresholds = c(m = .05), drop.distance = TRUE,
          var.names=covnames, position="top", title=element_blank())

ggsave(filename="love1.png", plot=love1, height=6, width=4)


library(survey)
d.w <- svydesign(~1, weights = W.out$weights, data = nba_prop)
fit <- svyglm(pts_diff ~ tfo_opp, design = d.w)
coef(fit)
summary(fit)
confint(fit)



### Heterogenous Treatment Effect - Team Specific Effect ###



covariate_names <- c("numberPeriod1", "numberPeriod2", "time_left_sec", "scoreMargin", "scoreMarginsq", "rating_max", "rating_max_opp", "rating_mean", "rating_mean_opp", "spread", "total_score")

X <- nba_prop %>%
 select(numberPeriod1, numberPeriod2, time_left_sec, scoreMargin, scoreMarginsq, rating_max, rating_max_opp, rating_mean, rating_mean_opp, spread, total_score)

Y <- nba_prop[["pts_diff"]]

W <- nba_prop[["tfo_att"]]

cf <- causal_forest(X, Y, W, num.trees = 4000)
cf

estimated_aipw_ate <- lapply(
  unique(nba_prop$team), function(w) {
  ate <- average_treatment_effect(cf, subset = nba_prop$ntile == w)
})
estimated_aipw_ate <- data.frame(do.call(rbind, estimated_aipw_ate))
estimated_aipw_ate$team <- unique(nba_prop$team)

estimated_aipw_ate %>%
 mutate(
    lb = -estimate - std.err,
    ub = -estimate + std.err,
    teams_n = fct_reorder(team, -estimate, min)
    ) %>%
 ggplot() +
 geom_point(aes(x = -estimate, y = teams_n), shape = 24, fill="blue", color="darkred", size=3) +
 geom_point(aes(x = lb, y = teams_n)) +
 geom_point(aes(x = ub, y = teams_n)) +
 geom_segment(aes(x = -estimate, y = teams_n, xend = lb, yend = teams_n)) +
 geom_segment(aes(x = -estimate, y = teams_n, xend = ub, yend = teams_n)) +
 geom_vline(aes(xintercept = 0), linetype = "dotted") +
 labs(x = "Average Treatment Effect", y = "Teams") +
 theme_bw()
