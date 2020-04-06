## Setup contact matrix data


library(socialmixr)
library(data.table)
library(ggplot2)

source('r/functions/cm_filter.R')
source('r/functions/create_scaling_matrices.R')


## Requires participants and contacts data
part <- readRDS('data/clean_participants.rds')
contacts <- readRDS('data/clean_contacts.rds')

## Create survey object

## Reduce the number of variables going into the data
contacts_m <- contacts[, .(
  part_id, 
  cont_id,
  cnt_gender,
  cnt_age_est_min,
  cnt_age_est_max,
  phys_contact, 
  cnt_home,
  cnt_work,
  cnt_school,
  cnt_supermarket,
  cnt_shop,
  cnt_inside,
  cnt_outside,
  hhm_pregnant,
  hhm_isolate,
  hhm_quarantine,
  hhm_limit_work,
  hhm_limit_school
)
]

part_m <- part[, .(
  part_id,
  part_gender,
  part_age,
  date,
  country,
  part_isolate,
  part_quarantine,
  part_limit_work,
  part_limit_school,
  part_work_closed,
  part_covid_test_result,
  part_covid_contact
)
]


h2020 <- survey(part_m, contacts_m)


## Save the survey objects
saveRDS(h2020, file = "data/contact_matrices/h2020_survey.rds")


## Create a single matrix 
create_scaling_matrices_observed(nboots = 1, file_name = "h2020_cms.RData")

load('data/contact_matrices/h2020_cms.RData')


h2020_cm
# Save the h2020_cm and polymod seperately
saveRDS(h2020_cm, file = "data/contact_matrices/h2020_cm.rds")
saveRDS(polymod_cm, file = "data/contact_matrices/polymod_cm.rds")

create_scaling_matrices_phys(nboots = 1, file_name = "h2020_phys_cms.RData")

load('data/contact_matrices/h2020_phys_cms.RData')

saveRDS(h2020_cm_phys, file = "data/contact_matrices/h2020_cm_phys.rds")
saveRDS(polymod_cm_phys, file = "data/contact_matrices/polymod_cm_phys.rds")


## Takes a long time to run

# create_scaling_matrices(nboots = 10000, file_name = "boots_cms.RData")
# create_scaling_matrices_phys(nboots = 10000, file_name = "boots_phys_cms.RData")



  
