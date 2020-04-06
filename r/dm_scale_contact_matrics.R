### We need to impute the missing age contacts in the H2020 data.

## This script will take a long time to run it create 10,000 bootstrapped contact
## Matrices for each location for Polymod and H2020

library(data.table)

## Set up the observed contact matrix

load('data/contact_matrices/h2020_cms.RData')
source('r/functions/matrix_scaling.r')

image(h2020_cm_imputed)

h2020_cm_imputed <- scale_matrix_single(polymod_cm, h2020_cm, polymod_cm)

saveRDS(h2020_cm_imputed, file = 'data/contact_matrices/h2020_cm_imputed.rds')


load('data/contact_matrices/h2020_phys_cms.RData')


h2020_cm <- h2020_cm_phys
h2020_cm_school <- h2020_cm_school_phys
h2020_cm_home <- h2020_cm_home_phys
h2020_cm_work <- h2020_cm_work_phys
h2020_cm_other <- h2020_cm_other_phys
polymod_cm <- polymod_cm_phys
polymod_cm_school <- polymod_cm_school_phys
polymod_cm_home <- polymod_cm_home_phys
polymod_cm_work <- polymod_cm_work_phys
polymod_cm_other <- polymod_cm_other_phys




h2020_cm_phys_imputed <- scale_matrix_single(polymod_cm_phys, h2020_cm_phys, polymod_cm_phys)


saveRDS(h2020_cm_phys_imputed, file = 'data/contact_matrices/h2020_cm_phys_imputed.rds')

## Of each matrix