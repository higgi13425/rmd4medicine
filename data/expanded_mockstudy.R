#### expanding mockstudy
library(arsenal)
library(randomNames)
library(tidyverse)
library(here)
data(mockstudy)
head(mockstudy)

# note 7 missing race, assign to "Other"
mockstudy$race[is.na(mockstudy$race)] <- "Other"

mockstudy$race_num <- factor(case_when(
  mockstudy$race == "African-Am" ~ 3,
  mockstudy$race =="Asian" ~ 2,
  mockstudy$race =="Caucasian" ~ 5,
  mockstudy$race =="Hawaii/Pacific" ~ 2,
  mockstudy$race =="Hispanic" ~ 4,
  mockstudy$race =="Native-Am/Alaska" ~ 1,
  mockstudy$race =="Other" ~ 6
))


mockstudy$gender_num <- factor(case_when(
  mockstudy$sex == "Male" ~ 0,
  mockstudy$sex =="Female" ~ 1
))

mockstudy$alive <- factor(case_when(
  mockstudy$fu.stat == 2 ~ 0,
  mockstudy$fu.stat == 1 ~ 1
))

mockstudy$firstname <- 
  randomNames(ethnicity=mockstudy$race_num, 
              gender = mockstudy$gender_num,
              which.names = "first",
              sample.with.replacement = TRUE)

mockstudy$lastname <- 
  randomNames(ethnicity=mockstudy$race_num, 
              gender = mockstudy$gender_num,
              which.names = "last",
              sample.with.replacement = TRUE)

mockstudy %>% 
  mutate(ae_random = runif(n=nrow(.), min=0, max=1)) %>% 
  mutate(ae_random = ifelse(arm=="G: IROX", 
                ae_random+0.12, ae_random)) %>% 
  mutate(ae_random = ifelse(arm=="A: IFL", 
                ae_random+ 0.07, ae_random)) %>% 
  mutate(low_wbc = ifelse(ae_random>0.9, 1, 0)) %>% 
  mutate(neuropathy = ifelse(ae_random >0.97, 1, 0)) %>% 
  mutate(diarrhea = ifelse(ae_random >0.95, 1, 0)) %>%
  mutate(vomiting = ifelse(ae_random >0.88, 1, 0)) %>%
  mutate(blood_clot = ifelse(ae_random >0.99, 1, 0)) %>% 
  rename(mdquality = mdquality.s) %>% 
  select(case, firstname, lastname, age, age.ord, sex, race, bmi, hgb, alk.phos, 
         ast, arm, mdquality, fu.time, alive, low_wbc:blood_clot) ->
mockstudy

write.csv(mockstudy, here("data/mockstudy2.csv"))

# next - eliminate G:, F:, I: at beginning of names
# next - diff random for each of 5 AEs
# next - differential different between arms for each of 5 AEs
#

#next - add random adverse events for FOLFOX, IROX, IFL
# FOLFOX more than 10%
# infection
# low WBC 23%
# fever 5%
# SOB
# Anemia
# bruising, bleeding
# fatigue
# neuropathy 18%
# nausea
# diarrhea
# mouth sores
# 1-10%
# peeling skin on hands
# runny nose
# sunburn
# hair loss 
# chipped nails 
# blood clot 
# 
# less than 1%
# difficulty swallowing
# tinnitus 

#IROX
# low WBC 30%
# fever 29%
# diarrhea 24%
# n/V 12%%
# neuropathy 18%

#IFL
# vomiting 11%
# diarrhea 16%
#mouth sores 3%
# low WBC 26%
# fever 5%


