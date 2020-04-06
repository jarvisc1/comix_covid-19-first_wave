## Descriptive values for the paper
library(socialmixr)
library(data.table)
library(ggplot2)



part <- readRDS('data/clean_participants.rds')
contacts <- readRDS('data/clean_contacts.rds')
household <- readRDS('data/clean_households.rds')

## Mean cases per person

## Number of participants
nrow(part)

## Number of contacts
nrow(contacts)

## Average age
part[ ,  .(avg = median(part_age), sd = sd(part_age), max = max(part_age))]

# Gender
part[ ,  .(avg = mean(part_gender == "Female"), X = sum(part_gender == "Female"), N = .N)]

## Households
part[ ,  .(avg = mean(hh_size, na.rm = T), sd = sd(hh_size, na.rm = T), max = max(hh_size, na.rm = T))]

## Regions
part[!is.na(regions_large) ,  Total := .N]
part[ ,  N := .N, by = regions_large]
part[,.( N = min(N), Total = max(Total), per = max(N/Total)) , by = regions_large]


## Behaviours and attitudes

## Household member isolating
part_hhm_isolate <- household[, .(isolate = hhm_isolate == "Yes"), by = part_id]
part_hhm_isolate <- part_hhm_isolate[, .(isolate = max(isolate)), by = part_id]
part_hhm_isolate[ ,  .(per = mean(isolate), X = sum(isolate), N = .N)]

## Household member quarantine
part_hhm_quar <- household[, .(quarantine = hhm_quarantine == "Yes"), by = part_id]
part_hhm_quar <- part_hhm_quar[, .(quarantine = max(quarantine)), by = part_id]
part_hhm_quar[ ,  .(per = mean(quarantine), X = sum(quarantine), N = .N)]

## Tests and contacts
table(part$part_covid_test_result)
table(part$part_covid_contact)

## Serious and likely disease
part[ ,  .(per = mean(part_att_likely %in% c("Strongly agree", "Tend to agree")), 
                      X = sum(part_att_likely %in% c("Strongly agree", "Tend to agree")),
                      N = .N)]

part[ ,  .(per = mean(part_att_serious %in% c("Strongly agree", "Tend to agree")), 
                      X = sum(part_att_serious %in% c("Strongly agree", "Tend to agree")),
                      N = .N)]

## Hand washing
## In the table in the paper. 


### Effectiveness of interventions
n <- nrow(part)
part[ , .(.N, .N/n), by = part_att_eff_reduce_contacts]
part[ , .(.N, .N/n), by = part_att_eff_stay_home7_mild]
part[ , .(.N, .N/n), by = part_att_eff_stay_home7_severe]
part[ , .(.N, .N/n), by = part_att_eff_stay_home14_severe_not_you]
part[ , .(.N, .N/n), by = part_att_eff_crowd_places]
part[ , .(.N, .N/n), by = part_att_isolate_problems]
part[ , .(.N, .N/n), by = part_att_isolate_enough_food]


## Number of contacts in different locations

pmodpc <- readRDS('data/polymod_contacts_part.rds')


mean(contacts_part[ cnt_home == "No", .N,  by = part_id]$N) 
mean(contacts_part[ , .N,  by = part_id]$N) 




