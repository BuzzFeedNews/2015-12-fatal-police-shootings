# required packages
library(dplyr)

#####################
# Washington Post data

# load Washington Post data
wapo_complete <- read.csv("wapo.csv")

# remove cases where race not determined
wapo <- wapo_complete %>%
  filter(race != "")
  
# overall percentages of those shot by police, by race
w_race <- wapo %>%
  group_by(race) %>%
  summarize(percent=round(n()/nrow(wapo)*100, digits=1))

w_race
# race percent
# (chr)   (dbl)
# 1     A     1.4
# 2     B    27.1
# 3     H    17.9
# 4     N     1.0
# 5     O     1.5
# 6     W    51.1

# filter by unarmed vs other, create new categorical variable and recombine
w_unarmed <- wapo %>%
  filter(armed == "unarmed") %>%
  mutate(unarmed = "Unarmed")
w_other <- wapo %>%
  filter(armed != "unarmed" | is.na(armed)) %>%
  mutate(unarmed = "Armed/Undetermined")
wapo <- bind_rows(w_unarmed,w_other)

# collapse the race categories into black, white, hispanic, and other
w_main_races <- wapo %>%
  filter(race == "B" | race == "W" | race == "H") %>%
  mutate(race2 = race)
w_other_races <- anti_join(wapo, w_main_races, by="id") %>%
  mutate(race2 = "Other")
wapo <- bind_rows(w_main_races,w_other_races)

# percentages of unarmed suspects shot by police, by race
w_unarmed_race <- wapo %>%
  filter(armed == "unarmed") %>%
  group_by(race2) %>%
  summarize(percent=round(n()/nrow(w_unarmed)*100, digits=1))

w_unarmed_race
# race2 percent
# (chr)   (dbl)
# 1     B    37.8
# 2     H    20.7
# 3 Other     6.1
# 4     W    35.4

# race vs unarmed cross-tab and chi-squared
w_race_unarmed <- table(wapo$race2, wapo$unarmed)
w_chisq_unarmed <- chisq.test(w_race_unarmed)

w_chisq_unarmed
# Pearson's Chi-squared test
# data:  w_race_unarmed
# X-squared = 9.7171, df = 3, p-value = 0.02113

w_race_unarmed
# Armed/Undetermined Unarmed
# B                    204      31
# H                    138      17
# Other                 29       5
# W                    414      29

w_chisq_unarmed$expected
# Armed/Undetermined   Unarmed
# B              212.77393 22.226067
# H              140.34025 14.659746
# Other           30.78431  3.215686
# W              401.10150 41.898501

# filter by whether or not there was any form of attack (as defined by WaPo)
# create new categorical variable
w_attack <- wapo %>%
  filter(threat_level_display == "attack") %>%
  mutate(threat="Attack")
w_non_attack <- wapo %>%
  filter(threat_level_display == "other" | threat_level_display == "undetermined") %>%
  mutate(threat="Other")
wapo <- bind_rows(w_attack, w_non_attack)
  
# race vs attack cross-tab and chi-squared
w_race_attack <- table(wapo$race2, wapo$threat)
w_chisq_attack <- chisq.test(w_race_attack)

w_chisq_attack
# Pearson's Chi-squared test
# data:  w_race_attack
# X-squared = 28.451, df = 3, p-value = 2.921e-06

w_race_attack
# Attack Other
# B        166    69
# H         91    64
# Other     24    10
# W        355    88

w_chisq_attack$expected
# Attack      Other
# B     172.38754  62.612457
# H     113.70242  41.297578
# Other  24.94118   9.058824
# W     324.96886 118.031142

# percentages by race, no attack in progress
w_non_attack_race <- w_non_attack %>%
  group_by(race2) %>%
  summarize(percent=round(n()/nrow(w_non_attack)*100, digits=1))

w_non_attack_race
# race2 percent
# (chr)   (dbl)
# 1     B    29.9
# 2     H    27.7
# 3 Other     4.3
# 4     W    38.1

#####################
# Guardian data

# load Guardian data
guardian <- read.csv("guardian.csv")

# filter for those shot by police, and remove cases where race Unknown
g_shot <- guardian %>%
  filter(classification=="Gunshot", raceethnicity != "Unknown")

# overall percentages of those shot by police, by race
g_race <- g_shot %>%
  group_by(raceethnicity) %>%
  summarize(percent=round(n()/nrow(g_shot)*100, digits=1))

g_race 
# raceethnicity percent
# (chr)   (dbl)
# 1          Arab-American     0.5
# 2 Asian/Pacific Islander     1.6
# 3                  Black    25.5
# 4        Hispanic/Latino    17.5
# 5        Native American     1.1
# 6                  Other     0.1
# 7                  White    53.7

# collapse the race categories into black, white, hispanic, and other
g_main_races <- g_shot %>%
  filter(raceethnicity == "Black" | raceethnicity == "White" | raceethnicity == "Hispanic/Latino") %>%
  mutate(race2 = raceethnicity)
g_other_races <- anti_join(g_shot, g_main_races, by="uid") %>%
  mutate(race2 = "Other")
g_shot <- bind_rows(g_main_races,g_other_races)

# filter by unarmed vs armed/unknown/disputed, create new categorical variable and recombine
g_unarmed <- g_shot %>%
  filter(armed=="No") %>%
  mutate(armed2="Unarmed")
g_other <- g_shot %>%
  filter(armed != "No") %>%
  mutate(armed2="Other")
g_shot <- bind_rows(g_unarmed,g_other)

# race vs armed cross-tab and chi-squared
g_race_armed <- table(g_shot$race2, g_shot$armed2)
g_chisq_armed <- chisq.test(g_race_armed)

g_chisq_armed
# Pearson's Chi-squared test
# data:  g_race_armed
# X-squared = 8.0864, df = 3, p-value = 0.04426

g_race_armed
# Other Unarmed
# Black             192      33
# Hispanic/Latino   136      18
# Other              26       3
# White             436      37

g_chisq_armed$expected
# Other  Unarmed
# Black           201.75936 23.24064
# Hispanic/Latino 138.09308 15.90692
# Other            26.00454  2.99546
# White           424.14302 48.85698

# percentages by race, unarmed
g_unarmed_race <- g_unarmed %>%
  group_by(race2) %>%
  summarize(percent=round(n()/nrow(g_unarmed)*100, digits=1))

g_unarmed_race
# race2 percent
# (chr)   (dbl)
# 1           Black    36.3
# 2 Hispanic/Latino    19.8
# 3           Other     3.3
# 4           White    40.7

#####################
# FBI active shooter summary data 

active <- read.ftable("active_shooter.txt")
chisq_active <- chisq.test(active)

chisq_active
# Pearson's Chi-squared test with Yates' continuity correction
# data:  active2
# X-squared = 0.92965, df = 1, p-value = 0.335

active
# outcome shot_police other
# race                           
# Black                  10    24
# Other                  25   101

chisq_active$expected
# [,1]    [,2]
# [1,]  7.4375 26.5625
# [2,] 27.5625 98.4375


