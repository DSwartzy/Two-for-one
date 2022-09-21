library(tidyverse)

### Read NBA play-by-play data ###
nba <- read_csv("nbapbp2018.csv")

### Sort Data ###
nba <- nba %>%
 arrange(idGame, numbernumberPeriod, desc(timeQuarter))

### Create new variables ###

nba <- nba %>%
 mutate(descriptionPlayHome = replace_na(descriptionPlayHome, "cricket")) %>%
 mutate(descriptionPlayVisitor = replace_na(descriptionPlayVisitor, "cricket")) %>%
 mutate(
  time_left_sec = (minuteRemainingQuarter * 60) + secondsRemainingQuarter,
  shot_home = as.numeric(str_detect(descriptionPlayHome, 
    regex("Jump Shot|Layup|Hook Shot|Jumper|Dunk|Tip Shot|Free Throw|Bank Shot|Fadeaway Shot|Fadeaway", ignore_case = T)))) %>%
 mutate(
  shot_away = as.numeric(str_detect(descriptionPlayVisitor,
    regex("Jump Shot|Layup|Hook Shot|Jumper|Dunk|Tip Shot|Free Throw|Bank Shot|Fadeaway Shot|Fadeaway", ignore_case = T)))) %>%
 mutate(
  shot = shot_home + shot_away,
  free_throw_home = as.numeric(str_detect(descriptionPlayHome, "Free Throw")),
  free_throw_away = as.numeric(str_detect(descriptionPlayVisitor, "Free Throw")),
  free_throw = free_throw_home + free_throw_away,
  miss_home = as.numeric(str_detect(descriptionPlayHome, "MISS")),
  miss_away = as.numeric(str_detect(descriptionPlayVisitor, "MISS")),
  miss = miss_home + miss_away,
  made_home = as.numeric(shot_home == 1 & miss_home == 0),
  made_away = as.numeric(shot_away == 1 & miss_away == 0),
  made = made_home + made_away,
  pts_attempt_home = ifelse(str_detect(descriptionPlayHome, " 3PT "), 3, ifelse(shot_home == 1, 2, 0)),
  pts_attempt_home = ifelse(str_detect(descriptionPlayHome, "Free Throw"), 1, pts_attempt_home),
  pts_attempt_away = ifelse(str_detect(descriptionPlayVisitor, " 3PT "), 3, ifelse(shot_away == 1, 2, 0)),
  pts_attempt_away = ifelse(str_detect(descriptionPlayVisitor, "Free Throw"), 1, pts_attempt_away),
  pts_attempt = pts_attempt_home + pts_attempt_away,
  pts_made_home = pts_attempt_home * (1 - miss_home),
  pts_made_away = pts_attempt_away * (1 - miss_away),
  pts_made = pts_made_home + pts_made_away,
  shot_dist_home = ifelse(!is.na(str_extract(descriptionPlayHome, "\\d\\d'")), str_extract(descriptionPlayHome, "\\d\\d'"), ifelse(!is.na(str_extract(descriptionPlayHome, "\\d'")), str_extract(descriptionPlayHome, "\\d'"), "-1'")),
  shot_dist_away = ifelse(!is.na(str_extract(descriptionPlayVisitor, "\\d\\d'")), str_extract(descriptionPlayVisitor, "\\d\\d'"), ifelse(!is.na(str_extract(descriptionPlayVisitor, "\\d'")), str_extract(descriptionPlayVisitor, "\\d'"), "-1'")),
  shot_dist_home = as.numeric(str_sub(shot_dist_home, 1, str_length(shot_dist_home) - 1)),
  shot_dist_away = as.numeric(str_sub(shot_dist_away, 1, str_length(shot_dist_away) - 1)),
  shot_dist_home = ifelse(shot_dist_home < 0 & shot_home == 1, -5, shot_dist_home),
  shot_dist_away = ifelse(shot_dist_away < 0 & shot_away == 1, -5, shot_dist_away),
  shot_dist = ifelse(shot_dist_home > 0, shot_dist_home, ifelse(shot_dist_away > 0, shot_dist_away, ifelse(shot_dist_home == -5 | shot_dist_away == -5, -5, -1))),
  shot_type_home = str_extract(descriptionPlayHome, "Jump Shot|Layup|Hook Shot|Jumper|Dunk|Tip Shot|Free Throw|Bank Shot|Fadeaway Shot|Fadeaway"),
  shot_type_home = ifelse(!is.na(str_extract(descriptionPlayHome, "Free Throw 1 of 1|Free Throw 1 of 2|Free Throw 1 of 3|Free Throw 2 of 2|Free Throw 2 of 3|Free Throw 3 of 3")), str_extract(descriptionPlayHome, "Free Throw 1 of 1|Free Throw 1 of 2|Free Throw 1 of 3|Free Throw 2 of 2|Free Throw 2 of 3|Free Throw 3 of 3"), shot_type_home),
  shot_type_home = ifelse( !is.na(str_extract(descriptionPlayHome, "Free Throw Technical")), "Free Throw 1 of 1", shot_type_home),
  shot_type_away = str_extract(descriptionPlayVisitor, "Jump Shot|Layup|Hook Shot|Jumper|Dunk|Tip Shot|Free Throw|Bank Shot|Fadeaway Shot|Fadeaway"),
  shot_type_away = ifelse(!is.na(str_extract(descriptionPlayVisitor, "Free Throw 1 of 1|Free Throw 1 of 2|Free Throw 1 of 3|Free Throw 2 of 2|Free Throw 2 of 3|Free Throw 3 of 3")), str_extract(descriptionPlayVisitor, "Free Throw 1 of 1|Free Throw 1 of 2|Free Throw 1 of 3|Free Throw 2 of 2|Free Throw 2 of 3|Free Throw 3 of 3"), shot_type_away),
  shot_type_away = ifelse( !is.na(str_extract(descriptionPlayVisitor, "Free Throw Technical")), "Free Throw 1 of 1", shot_type_away),
  shot_type = ifelse(!is.na(shot_type_home), shot_type_home, shot_type_away),
  start_of_game = ifelse(!is.na(descriptionPlayNeutral) & str_detect(descriptionPlayNeutral, "Start of 1st numberPeriod"), 1, 0),
  scoreAway = ifelse(start_of_game == 1, 0, scoreAway),
  scoreHome = ifelse(start_of_game == 1, 0, scoreHome),
 ) %>%
 fill(scoreAway) %>%
 fill(scoreHome) %>%
 mutate(
  scoreMarginHome = scoreHome - scoreAway,
  scoreMarginAway = - scoreMarginHome,
  scoreHomebf = lag(scoreHome),
  scoreHomebf = ifelse(start_of_game == 1, 0, scoreHomebf),
  scoreAwaybf = lag(scoreAway),
  scoreAwaybf = ifelse(start_of_game == 1, 0, scoreAwaybf),
  scoreMarginHomebf = scoreHomebf - scoreAwaybf,
  scoreMarginAwaybf = - scoreMarginHomebf
  )


write_csv(nba2, file = "nbapbp2018_analysis.csv")
save(nba2, file = "nbapbp2018_analysis.RData")




### Create Variables for Possesion and Shot Clock ###

nba <- nba %>%
 mutate(
  last_free_throw_home = as.numeric(str_detect(descriptionPlayHome, regex('1 of 1|2 of 2|3 of 3|Free throw technical', ignore_case = T))),
  last_free_throw_away = as.numeric(str_detect(descriptionPlayVisitor, regex('1 of 1|2 of 2|3 of 3|Free throw technical', ignore_case = T))),
  last_free_throw = last_free_throw_home + last_free_throw_away,
  t_foul_home = as.numeric(str_detect(descriptionPlayHome, regex(' T.Foul', ignore_case = T))),
  t_foul_away = as.numeric(str_detect(descriptionPlayVisitor, regex(' T.Foul', ignore_case = T))),
  t_foul = t_foul_home + t_foul_away, 
  flagrant_foul_home = as.numeric(str_detect(descriptionPlayHome, regex('flagrant', ignore_case = T))) * (1 - free_throw_home),
  flagrant_foul_away = as.numeric(str_detect(descriptionPlayVisitor, regex('flagrant', ignore_case = T))) * (1 - free_throw_away),
  flagrant_foul = flagrant_foul_home + flagrant_foul_away, 
  t_foul_3s_home = as.numeric(str_detect(descriptionPlayHome, regex(' T.Foul \\(Def. 3 Sec', ignore_case = T))),
  t_foul_3s_away = as.numeric(str_detect(descriptionPlayVisitor, regex(' T.Foul \\(Def. 3 Sec', ignore_case = T))),
  t_foul_3s = t_foul_3s_home + t_foul_3s_away, 
  t_foul_reg_home = t_foul_home * (1 - t_foul_3s_home),
  t_foul_reg_away = t_foul_away * (1 - t_foul_3s_away),
  t_foul_reg = t_foul_reg_home + t_foul_reg_away,
  vil_14_home = as.numeric(str_detect(descriptionPlayHome, regex('Kicked Ball|Delay of game', ignore_case = T))),
  vil_14_away = as.numeric(str_detect(descriptionPlayVisitor, regex('Kicked Ball|Delay of game', ignore_case = T))),
  vil_14 = vil_14_home + vil_14_away,
  event_24_home = as.numeric(str_detect(descriptionPlayHome, regex('Goaltending|Turnover|Steal|STEAL|Rebound|REBOUND', ignore_case = T))),
  event_24_away = as.numeric(str_detect(descriptionPlayVisitor, regex('Goaltending|Turnover|Steal|STEAL|Rebound|REBOUND', ignore_case = T))),
  event_24 = pmin(event_24_home + event_24_away, 1), 
  event_rebound_away = as.numeric(str_detect(descriptionPlayVisitor, regex('Rebound|REBOUND', ignore_case = T))),
  event_rebound_home = as.numeric(str_detect(descriptionPlayHome, regex('Rebound|REBOUND', ignore_case = T))),
  event_rebound = event_rebound_home + event_rebound_away,
  c_foul_home = as.numeric(str_detect(descriptionPlayHome, regex('Foul', ignore_case = T))) * (1 - t_foul_home) * (1 - flagrant_foul_home),
  c_foul_away = as.numeric(str_detect(descriptionPlayVisitor, regex('Foul', ignore_case = T))) * (1 - t_foul_away) * (1 - flagrant_foul_away),
  c_foul = c_foul_home + c_foul_away,
  reset_24 = as.numeric(event_24 == 1 | c_foul == 1 | (made == 1 & free_throw == 0) | (made == 1 & last_free_throw == 1 & free_throw == 1)),
  reset_14 = as.numeric(t_foul_3s == 1 | vil_14 == 1)
 )




### Code for Possesion ###

nba[["poss_at"]] <- rep("T", length(nba2[["numberPeriod"]]))
nba[["poss_af"]] <- rep("T", length(nba2[["numberPeriod"]]))

for (i in 2:length(nba[["numberPeriod"]])) {
  if(i %% 1000 == 0) {
    print(i)
  }
  if (nba[["shot_home"]][i] == 1 && nba[["free_throw_home"]][i] == 0 && nba[["made_home"]][i] == 1) {
    nba[["poss_at"]][i] = "H"
    nba[["poss_af"]][i] = "A"
  }
  else if (nba[["shot_away"]][i] == 1 && nba[["free_throw_away"]][i] == 0 && nba[["made_away"]][i] == 1) {
    nba[["poss_at"]][i] = "A"
    nba[["poss_af"]][i] = "H"
  }
  else if (as.numeric(str_detect(nba[["descriptionPlayHome"]][i], regex('Rebound', ignore_case = T)))) {
    nba[["poss_at"]][i] = nba2[["poss_af"]][i - 1]
    nba[["poss_af"]][i] = "H"
  }
  else if (as.numeric(str_detect(nba[["descriptionPlayVisitor"]][i], regex('Rebound', ignore_case = T)))) {
    nba[["poss_at"]][i] = nba2[["poss_af"]][i - 1]
    nba[["poss_af"]][i] = "A"
  }
  else if (as.numeric(str_detect(nba[["descriptionPlayHome"]][i], regex('Turnover', ignore_case = T)))) {
    nba[["poss_at"]][i] = "H"
    nba[["poss_af"]][i] = "A"
  }
  else if (as.numeric(str_detect(nba[["descriptionPlayVisitor"]][i], regex('Turnover', ignore_case = T)))) {
    nba[["poss_at"]][i] = "A"
    nba[["poss_af"]][i] = "H"
  }
  else if (nba[["made_home"]][i] == 1 && as.numeric(str_detect(nba[["descriptionPlayHome"]][i], regex('Free Throw 1 of 1|Free Throw 2 of 2|Free Throw 3 of 3', ignore_case = T)))) {
    nba[["poss_at"]][i] = "H"
    nba[["poss_af"]][i] = "A"
  }
  else if (nba[["made_away"]][i] == 1 && as.numeric(str_detect(nba[["descriptionPlayVisitor"]][i], regex('Free Throw 1 of 1|Free Throw 2 of 2|Free Throw 3 of 3', ignore_case = T)))) {
    nba[["poss_at"]][i] = "A"
    nba[["poss_af"]][i] = "H"
  }
  else if (as.numeric(str_detect(nba[["descriptionPlayHome"]][i], regex('Free Throw flagrant', ignore_case = T)))) {
    nba[["poss_at"]][i] = "H"
    nba[["poss_af"]][i] = "H"
  }
  else if (as.numeric(str_detect(nba[["descriptionPlayVisitor"]][i], regex('Free Throw flagrant', ignore_case = T)))) {
    nba[["poss_at"]][i] = "A"
    nba[["poss_af"]][i] = "A"
  }
  else if (as.numeric(str_detect(nba[["descriptionPlayHome"]][i], regex('Free Throw technical', ignore_case = T)))) {
    nba[["poss_at"]][i] = "H"
    nba[["poss_af"]][i] = "H"
  }
  else if (as.numeric(str_detect(nba[["descriptionPlayVisitor"]][i], regex('Free Throw technical', ignore_case = T)))) {
    nba[["poss_at"]][i] = "A"
    nba[["poss_af"]][i] = "A"
  }
  else if (nba[["c_foul_home"]][i] == 1) {
    nba[["poss_at"]][i] = "H"
    nba[["poss_af"]][i] = "A"
  }
  else if (nba[["c_foul_away"]][i] == 1) {
    nba[["poss_at"]][i] = "A"
    nba[["poss_af"]][i] = "H"
  }
  else if (nba[["shot_home"]][i] == 1) {
    nba[["poss_at"]][i] = "H"
  }
  else if (nba[["shot_away"]][i] == 1) {
    nba[["poss_at"]][i] = "A"
  }
  else{
    nba[["poss_at"]][i] = nba[["poss_af"]][i - 1]
    nba[["poss_af"]][i] = nba[["poss_af"]][i - 1]
  }
}


### Variables for Shot Clock ###

### Work for Offensive and Defensive Rebound ###

nba <- nba %>%
 mutate(
  free_throw_special_home = as.numeric(str_detect(nba[["descriptionPlayHome"]][i], regex('Free Throw flagrant|Free Throw technical', ignore_case = T))),
  free_throw_special_away = as.numeric(str_detect(nba[["descriptionPlayVisitor"]][i], regex('Free Throw flagrant|Free Throw technical', ignore_case = T))),
  free_throw_special = free_throw_special_home + free_throw_special_away,
  free_throw_special_prev = c(0, free_throw_special[-length(numberPeriod)]),
  poss_at_prev = c("T", poss_at[-length(numberPeriod)]),
  off_rebound_home = as.numeric(str_detect(nba[["descriptionPlayHome"]], regex('Rebound', ignore_case = T)) * as.numeric(poss_at_prev == "H") * as.numeric(poss_af == "H")),
  off_rebound_away = as.numeric(str_detect(nba[["descriptionPlayVisitor"]], regex('Rebound', ignore_case = T)) * as.numeric(poss_at_prev == "A") * as.numeric(poss_af == "A")),
  off_rebound = (off_rebound_home + off_rebound_away) * (1 - free_throw_special),
  def_rebound = event_rebound - off_rebound,
  event_24_home = as.numeric(str_detect(descriptionPlayHome, regex('Goaltending|Turnover|Steal|STEAL', ignore_case = T))),
  event_24_away = as.numeric(str_detect(descriptionPlayVisitor, regex('Goaltending|Turnover|Steal|STEAL', ignore_case = T))),
  event_24 = pmin(event_24_home + event_24_away, 1), 
  reset_24 = as.numeric(event_24 == 1 | c_foul == 1 | (made == 1 & free_throw == 0) | (made == 1 & last_free_throw == 1 & free_throw == 1) | (def_rebound == 1)),
  reset_14 = as.numeric(t_foul_3s == 1 | vil_14 == 1 | off_rebound == 1)
  )


nba <- nba %>%
 mutate(
  reset_24_prev = c(0, reset_24[-length(numberPeriod)]),
  reset_14_prev = c(0, reset_14[-length(numberPeriod)]),  
  time_left_sec_prev = c(0, time_left_sec[-length(numberPeriod)]),  
  numberPeriod_prev = c(0, numberPeriod[-length(numberPeriod)]),
  shot_clock = rep(24, length(numberPeriod))
  )

for (i in 1:length(nba[["numberPeriod"]])) {
  if(i %% 1000 == 0) {
    print(i)
  }
  if (abs(nba[["numberPeriod"]][i] - nba[["numberPeriod_prev"]][i]) > 0.5) {
    nba[["shot_clock"]][i] = 24
  }
  else if (nba[["time_left_sec"]][[i]] < 24) {
    nba[["shot_clock"]][i] = nba[["time_left_sec"]][[i]]
  }
  else if (nba[["reset_24_prev"]][i] == 1) {
    nba[["shot_clock"]][i] = 24 - (nba[["time_left_sec_prev"]][i] - nba[["time_left_sec"]][i])
  }
  else if (nba[["reset_14_prev"]][i] == 1) {
    nba[["shot_clock"]][i] = max(14, nba[["shot_clock"]][i - 1] - (nba[["time_left_sec_prev"]][i] - nba[["time_left_sec"]][i]))
  }
  else{
    nba[["shot_clock"]][i] = nba[["shot_clock"]][i - 1] - (nba[["time_left_sec_prev"]][i] - nba[["time_left_sec"]][i])    
  }
}


### Restrict to only end of period data ###



nba <- nba %>%
 filter(numberPeriod < 4 & time_left_sec < 120) %>%
 mutate(
  off_foul_home = as.numeric(str_detect(descriptionPlayHome, regex('off.foul|offensive charge foul', ignore_case = T))),
  off_foul_away = as.numeric(str_detect(descriptionPlayVisitor, regex('off.foul|offensive charge foul', ignore_case = T))),
  off_foul = off_foul_away + off_foul_home,
  free_throw_one_home = as.numeric(str_detect(descriptionPlayHome, regex('1 of 1', ignore_case = T))),
  free_throw_one_away = as.numeric(str_detect(descriptionPlayVisitor, regex('1 of 1', ignore_case = T))),
  free_throw_one = free_throw_one_home + free_throw_one_away,
  free_throw_tech_home = as.numeric(str_detect(descriptionPlayHome, regex('Free throw technical', ignore_case = T))),
  free_throw_tech_away = as.numeric(str_detect(descriptionPlayVisitor, regex('Free throw technical', ignore_case = T))),
  free_throw_tech = free_throw_tech_home + free_throw_tech_away,
  tfo_exc_home = as.numeric(str_detect(descriptionPlayHome, regex('Goaltending|Delay of game', ignore_case = T))),
  tfo_exc_away = as.numeric(str_detect(descriptionPlayVisitor, regex('Goaltending|Delay of game', ignore_case = T))),
  tfo_exc = tfo_exc_home + tfo_exc_away,
  shot_clock_adj = ifelse(shot == 1 & pts_attempt == 2, shot_clock + 1, ifelse(shot == 1 & pts_attempt == 3, shot_clock + 1.5, shot_clock))
 )

### Create Two for One Opportunity Variables ###



nba[["tfo_opp"]] <- rep(0, length(nba[["idGame"]]))
nba[["rule1"]] <- rep(0, length(nba[["idGame"]]))
nba[["rule2"]] <- rep(0, length(nba[["idGame"]]))
nba[["rule3"]] <- rep(0, length(nba[["idGame"]]))
nba[["rule4"]] <- rep(0, length(nba[["idGame"]]))
nba[["rule5"]] <- rep(0, length(nba[["idGame"]]))

for (i in 1:length(nba[["idGame"]])) {

  if(i %% 1000 == 0) {
    print(i)
  }

  if(nba[["time_left_sec"]][i] < 44 && nba[["time_left_sec"]][i] > 34) {


    # TFO OPP WHEN (LAST FREE THROW AND FREE THROW TECH)
    # AND (NO FREE THROW AT THE SAME TIME) AND (FREE THROW WAS MADE)

    if(nba[["last_free_throw"]][i] == 1 && nba[["free_throw_tech"]][i] == 1) {

        curr_time <- nba[["time_left_sec"]][i]
        only_foul <- 1
        j <- i + 1


        while (nba[["time_left_sec"]][j] == curr_time) {
          if (nba[["free_throw"]][j] == 1) {
            only_foul <- 0
            break
          }

          j <- j + 1
        }

        if (only_foul == 1 && nba[["made"]][i] == 1) {
          nba[["tfo_opp"]][i] <- 1
          nba[["rule1"]][i] <- 1
        }

    }


    # TFO OPP WHEN (LAST FREE THROW AND NO FREE THROW TECH)
    # AND (FREE THROW WAS MADE)    ### CHANGE TO CHECK IF ANOTHER FREE THROW AT THE SAME TIME

    if (nba[["last_free_throw"]][i] == 1 && nba[["made"]][i] == 1 && nba[["free_throw_tech"]][i] == 0) {

        curr_time <- nba[["time_left_sec"]][i]
        only_foul <- 1
        j <- i + 1


        while (nba[["time_left_sec"]][j] == curr_time) {
          if (nba[["free_throw"]][j] == 1) {
            only_foul <- 0
            break
          }

          j <- j + 1
        }

        if (only_foul == 1) {
          nba[["tfo_opp"]][i] <- 1
          nba[["rule1"]][i] <- 1
        }

    }

    # TFO OPP WHEN (MADE NON FREE THROW SHOT AND NO FREE THROW AFTER THIS)

    if (nba[["made"]][i] == 1 && nba[["free_throw"]][i] == 0) {

      curr_time <- nba[["time_left_sec"]][i]
      only_foul <- 1
      j <- i + 1



      while (nba[["time_left_sec"]][j] == curr_time) {
        if (nba[["free_throw"]][j] == 1) {
          only_foul <- 0
          break
        }

        j <- j + 1
      }

      if (only_foul == 1) {
        nba[["tfo_opp"]][i] <- 1
        nba[["rule2"]][i] <- 1
      }

    }

    # TFO OPP WHEN (EVENT_24 AND NO TFO EXCLUSION) AND ((REBOUND and NO FREE THROW) OR (NO REBOUND))

    if (nba[["event_24"]][i] == 1 && nba[["tfo_exc"]][i] == 0) {


      if (as.numeric(str_detect(nba[["descriptionPlayHome"]][i], regex("rebound", ignore_case = T))) || as.numeric(str_detect(nba[["descriptionPlayVisitor"]][i], regex("rebound", ignore_case = T)))) {

        curr_time <- nba[["time_left_sec"]][i]
        only_foul <- 1
        j <- i + 1


        while (nba[["time_left_sec"]][j] == curr_time) {
          if (nba[["free_throw"]][j] == 1) {
            only_foul <- 0
            break
          }

          j <- j + 1
        }

        if (only_foul == 1) {
          nba[["tfo_opp"]][i] <- 1
          nba[["rule3"]][i] <- 1
        }

      }



      else {
        nba[["tfo_opp"]][i] <- 1
        nba[["rule3"]][i] <- 1
      }
    }

    # TFO OPP WHEN (COMMON FOUL AND NO TFO EXCLUSION AND NO OFF FOUL AND NO FREE THROW)  

    if (nba[["c_foul"]][i] == 1 && nba[["off_foul"]][i] == 0 && nba[["tfo_exc"]][i] == 0) {
      curr_time <- nba[["time_left_sec"]][i]
      only_foul <- 1
      j <- i + 1
      while (nba[["time_left_sec"]][j] == curr_time) {
        if (nba[["free_throw"]][j] == 1) {
          only_foul <- 0
          break
        }
     
        j <- j + 1
      }

      if (only_foul == 1) {
        nba[["tfo_opp"]][i] <- 1
        nba[["rule5"]][i] <- 1
      }
    }
  }
}



### Construct Two for One Attempt and No Attempt ###



nba[["next_numberPeriod"]] <- c(nba[["numberPeriod"]][-1], nba[["numberPeriod"]][length(nba[["numberPeriod"]])])
nba[["scoreMarginHome_eop"]] <- ifelse(nba[["next_numberPeriod"]] == nba[["numberPeriod"]], NA, nba[["scoreMarginHome"]])
nba[["scoreMarginAway_eop"]] <- ifelse(nba[["next_numberPeriod"]] == nba[["numberPeriod"]], NA, nba[["scoreMarginAway"]])

nba <- nba %>%
  fill(scoreMarginHome_eop, .direction = "up") %>%
  fill(scoreMarginAway_eop, .direction = "up")


nba[["tfo_att"]] <- rep(0, length(nba[["next_numberPeriod"]]))
nba[["tfo_natt"]] <- rep(0, length(nba[["next_numberPeriod"]]))
nba[["tfo_neither"]] <- rep(0, length(nba[["next_numberPeriod"]]))
nba[["pts_diff"]] <- rep(0, length(nba[["next_numberPeriod"]]))
nba[["tfo_natt_nactr"]] <- rep(0, length(nba[["next_numberPeriod"]]))

activity <- 0

for (i in 1:(length(nba[["next_numberPeriod"]]) - 1)) {

  if (nba[["tfo_opp"]][i] == 1) {
    j <- i + 1

    ## IF NOTHING HAPPENS BY 28 SECONDS THEN IT NO ATTEMPT

    if (nba[["time_left_sec"]][j] < 28) {
      nba[["tfo_natt"]][i] <- 1
      if (nba[["poss_af"]][i] == "H") {
        nba[["pts_diff"]][i] = nba[["scoreMarginHome"]][i] - nba[["scoreMarginHome_eop"]][i]
      }
      else {
        nba[["pts_diff"]][i] = nba[["scoreMarginAway"]][i] - nba[["scoreMarginAway_eop"]][i]
      }
    }
    else {

      activity <- 0

      while (nba[["time_left_sec"]][j] > 27) {

        ## IF THERE IS A SHOT THEN IT IS ATTEMPT

        if (nba[["shot"]][j] == 1) {
          activity <- 1
          nba[["tfo_att"]][i] <- 1

          if (nba[["poss_af"]][i] == "H") {
            nba[["pts_diff"]][i] = nba[["scoreMarginHome"]][i] - nba[["scoreMarginHome_eop"]][i]
          }
          else {
            nba[["pts_diff"]][i] = nba[["scoreMarginAway"]][i] - nba[["scoreMarginAway_eop"]][i]
          }

        }


        ## IF THERE IS A FOUL THEN IT IS AN ATTEMPT  ### Remove t_foul_3s


        if (nba[["t_foul"]][j] == 1 || nba[["flagrant_foul"]][j] == 1 || nba[["t_foul_reg"]][j] == 1 || nba[["c_foul"]][j] == 1) {

          activity <- 1
          nba[["tfo_att"]][i] <- ifelse((nba[["time_left_sec"]][j] - nba[["time_left_sec"]][i]) < 0.5, 0, 1)

          if (nba[["poss_af"]][i] == "H") {
            nba[["pts_diff"]][i] = nba[["scoreMarginHome"]][i] - nba[["scoreMarginHome_eop"]][i]
          }
          else {
            nba[["pts_diff"]][i] = nba[["scoreMarginAway"]][i] - nba[["scoreMarginAway_eop"]][i]
          }
        }

        if (nba[["reset_24"]][j] == 1 || nba[["reset_14"]][j] == 1 || nba[["t_foul_3s"]][j] == 1) {
          activity <- 1
        }

        # BREAK IF THERE IS ANOTHER OPP

        if (nba[["tfo_opp"]][j] > 0) {
          break
        }

        j <- j + 1
      }
    } 

    ## TAKES CARE OF SUBSTITUTIONS/TIME OUTS etc.

    if (activity == 0 && nba[["tfo_natt"]][i] == 0) {
      nba[["tfo_natt"]][i] <- 1
      nba[["tfo_natt_nactr"]][i] <- 1

      if (nba[["poss_af"]][i] == "H") {
        nba[["pts_diff"]][i] = nba[["scoreMarginHome"]][i] - nba[["scoreMarginHome_eop"]][i]
      }
      else {
        nba[["pts_diff"]][i] = nba[["scoreMarginAway"]][i] - nba[["scoreMarginAway_eop"]][i]
      }

    }


    if (nba[["tfo_natt"]][i] == 0 && nba[["tfo_att"]][i] == 0) {
      nba[["tfo_neither"]][i] <- 1
    }
  }
}












## Add betting data to nba data

nba <- read_csv("nba_18_19_analysis_upd_05_10.csv")

nba_betting <- read_csv("nba_odds_18_19.csv")

nba6 <- nba %>%
 left_join(nba_betting, by = "idGame")

write_csv(nba6, file = "nba_18_19_analysis_upd_09_08.csv")

## Win Probability to nba data

load("win_prob_18_19.RData", verbose = T)
dim(win_prob_18_19)

win_prob <- win_prob_18_19 %>%
 group_by(idGame, numberPeriod, timeRemaining, secondsRemaining, minuteRemainingQuarter, secondsRemainingQuarter) %>%
 summarise(
  win_prob_home = mean(pctWinProbHome, na.rm = T),
  win_prob_away = mean(pctWinProbAway, na.rm = T)
  )

win_prob <- win_prob %>%
 select(idGame, numberPeriod, minuteRemainingQuarter, secondsRemainingQuarter, win_prob_home, win_prob_away)

nba7 <- nba6 %>%
 left_join(win_prob, by = c("idGame", "numberPeriod", "minuteRemainingQuarter", "secondsRemainingQuarter"))

write_csv(nba7, file = "nba_18_19_analysis_upd_09_08.csv")

###### checking NA entries

load("win_prob_18_19.RData", verbose = T)

win_prob_18_19 %>%
 dplyr::select(idGame, numberPeriod, minuteRemainingQuarter, secondsRemainingQuarter) %>%
 filter(idGame == "21801200", numberPeriod == 1)


win_prob_18_19 %>%
 summarise(
  idna = mean(is.na(idGame)),
  periodna = mean(is.na(numberPeriod)),
  minutena = mean(is.na(minuteRemainingQuarter)),
  secondna = mean(is.na(secondsRemainingQuarter)),
  timeremna = mean(is.na(timeRemaining)),
  winprobna = mean(is.na(pctWinProbHome))
  )
