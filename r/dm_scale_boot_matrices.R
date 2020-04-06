
library(socialmixr)
library(data.table)
library(ggplot2)



load('data/contact_matrices/boots_cms.RData')
source('r/functions/matrix_scaling.r')

boots <- length(h2020_boot_cm)
eigen_scale <- numeric(boots)

for (i in 1:boots){
  ## Scaling for each matrix
  eigen_scale[i] <- scale_factor_R(h2020_boot_cm, polymod_boot_cm, i = i)
  
}

saveRDS(eigen_scale, file = "data/contact_matrices/eigen_all.rds")



## Repeat for physical


remove(list = ls()[(grepl("_", ls()))])

load('data/contact_matrices/boots_phys_cms.RData')
source('r/functions/matrix_scaling.r')

h2020_boot_cm <- h2020_cm_phys
h2020_boot_cm_school <- h2020_cm_school_phys
h2020_boot_cm_home <- h2020_cm_home_phys
h2020_boot_cm_work <- h2020_cm_work_phys
h2020_boot_cm_other <- h2020_cm_other_phys
polymod_boot_cm <- polymod_cm_phys
polymod_boot_cm_school <- polymod_cm_school_phys
polymod_boot_cm_home <- polymod_cm_home_phys
polymod_boot_cm_work <- polymod_cm_work_phys
polymod_boot_cm_other <- polymod_cm_other_phys


boots <- length(h2020_boot_cm)
eigen_scale_phys <- numeric(boots)

## Scale Bootstrapped matrices

for (i in 1:boots){
  ## Scaling for each matrix
  eigen_scale_phys[i] <- scale_factor_R(h2020_boot_cm, polymod_boot_cm, i = i)
  
}

saveRDS(eigen_scale_phys, file = "data/contact_matrices/eigen_physical.rds")


## Repeated for matrix scaled to reduce contacts in young people by 50%


remove(list = ls()[(grepl("_", ls()))])


### Reduce contacts for 5-18 by 50%

## Changed to scale all by 50%
load('data/contact_matrices/boots_cms.RData')
source('r/functions/matrix_scaling.r')

scale_list <- function(mat_list, row = 2, col = 2, scale = 0.5) {
  mat_list$matrix[row, col] <- mat_list$matrix[row, col]*scale
  mat_list
}


polymod_boot_cm <- lapply(polymod_boot_cm, scale_list)
polymod_boot_cm_home <- lapply(polymod_boot_cm_home, scale_list)
polymod_boot_cm_work <- lapply(polymod_boot_cm_work, scale_list)
polymod_boot_cm_other <- lapply(polymod_boot_cm_other, scale_list)


boots <- length(h2020_boot_cm)
eigen_scale_scaled <- numeric(boots)

for (i in 1:boots){
  ## Scaling for each matrix
  eigen_scale_scaled[i] <- scale_factor_R(h2020_boot_cm, polymod_boot_cm, i = i)
  
}

saveRDS(eigen_scale_scaled, file = "data/contact_matrices/eigen_all_polymod_scaled.rds")




## Repeat for physical


remove(list = ls()[(grepl("_", ls()))])

load('data/contact_matrices/boots_phys_cms.RData')
source('r/functions/matrix_scaling.r')

h2020_boot_cm <- h2020_cm_phys
h2020_boot_cm_school <- h2020_cm_school_phys
h2020_boot_cm_home <- h2020_cm_home_phys
h2020_boot_cm_work <- h2020_cm_work_phys
h2020_boot_cm_other <- h2020_cm_other_phys
polymod_boot_cm <- polymod_cm_phys
polymod_boot_cm_school <- polymod_cm_school_phys
polymod_boot_cm_home <- polymod_cm_home_phys
polymod_boot_cm_work <- polymod_cm_work_phys
polymod_boot_cm_other <- polymod_cm_other_phys


scale_list <- function(mat_list, row = 2, col = 2, scale = 0.5) {
  mat_list$matrix[row, col] <- mat_list$matrix[row, col]*scale
  mat_list
}

polymod_boot_cm <- lapply(polymod_boot_cm, scale_list)
polymod_boot_cm_home <- lapply(polymod_boot_cm_home, scale_list)
polymod_boot_cm_work <- lapply(polymod_boot_cm_work, scale_list)
polymod_boot_cm_other <- lapply(polymod_boot_cm_other, scale_list)


boots <- length(h2020_boot_cm)
eigen_scale_phys_scaled <- numeric(boots)

## Scale Bootstrapped matrices

for (i in 1:boots){
  ## Scaling for each matrix
  eigen_scale_phys_scaled[i] <- scale_factor_R(h2020_boot_cm, polymod_boot_cm, i = i)
  
}

saveRDS(eigen_scale_phys_scaled, file = "data/contact_matrices/eigen_physical_polymod_scaled.rds")








