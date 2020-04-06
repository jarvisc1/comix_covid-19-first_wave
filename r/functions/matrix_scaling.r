## Functions for scaling the bootstrapped matrices


## These functions a helper functions that feed into scale_factor_R
## scale_factor_R takes the matrices elements of two contact matrices
## from the social mixR package, the matrices need to be bootstrapped ones
## as social mixr calls the element matrices when bootstrapped and matrix 
## when just one value is created. 

## mat1 and mat2 are the matrices objects



# Split contact matrix to only the required rows and columns
## This is because we do not have data for participants <18
split_cm <- function(mat, i = 1, row = 3:8, col = 3:8, ...){
  mat[[i]]$matrix[row,col]
}
split_cm_single <- function(mat, i = 1, row = 3:8, col = 3:8, ...){
  mat[row,col]
}

## Make the matrix symmetric
symm_mat <- function(x){
  (x + t(x))/ 2  
}

## Calculate ratio of max eigen values 
# This gives a sense of different magnitudes of the matrices
max_eigen_ratio <- function(x, y){
  max(eigen(x, only.values = TRUE)$values)/max(eigen(y, only.values = TRUE)$values)
  
}


# Take two matrices and calculate the scale factor for them
## Repeats the above functions to get the ratio of the max eigen values
## Of each matrix
scale_factor <- function(mat1, mat2, ... ){
  #x1 <- split_cm(mat1, ...)
  x2 <- split_cm(mat2, ...)
  x1 <- symm_mat(mat1)
  x2 <- symm_mat(x2)
  max_eigen_ratio(x1, x2)
}

# Take a full matrix and scale it based on ratio of mat1 and mat2
scale_matrix <- function(fullmat, mat1, mat2, i = 1, ...){
  mat1 <- mat1[[i]]$matrix
  
  fullmat[[i]]$matrix * scale_factor(mat1, mat2, i = i, ...)
  
}

## Specific for this analysis get imputed values for H2020 matrix
impute_values <- function(i = 1, school = FALSE, ...){
  imputed_values <- scale_matrix(
    polymod_boot_cm_home, 
    h2020_boot_cm_home, 
    polymod_boot_cm_home, 
    i = i,
    ...
  ) +
    scale_matrix(
      polymod_boot_cm_work, 
      h2020_boot_cm_work, 
      polymod_boot_cm_work, 
      i = i,
      ...
    ) +
    scale_matrix(
      polymod_boot_cm_other, 
      h2020_boot_cm_other, 
      polymod_boot_cm_other, 
      i = i,
      ...
    ) 
  
  if(school){
    ## Schools were closed so can not scale by this so just add
    imputed_values <- imputed_values +
      polymod_boot_cm_school[[i]]$matrix 
  }
  return(imputed_values)
  
}


update_cm <- function(
  mat1, mat2, 
  impute_rows = 1:2, impute_cols = 1:8, 
  i = 1,  
  observed_rows = 2:8, 
  observed_col = 1:8,
  school = FALSE,
  ...
){
  
  mat1 <- mat1[[i]]$matrix
  mat2 <- mat2[[i]]$matrix
  
  update_mat <- matrix(0, nrow = nrow(mat1), ncol = ncol(mat1))
  row.names(update_mat) <- colnames(mat1)
  colnames(update_mat)  <- colnames(mat1)
  
  #non_imputed_values <- mat1[row, col]
  update_mat <- mat1
  
  imputed_values <- impute_values(i = i, school = school, ... )[impute_rows, impute_cols]
  update_mat[impute_rows, impute_cols] <- imputed_values
  
  symm_mat(update_mat)
}


### Final function which calcualte the scaling factors for R.

scale_factor_R <- function(mat1, mat2, i = 1, ...){
  x1 <- update_cm(mat1, mat2, i = i, ...)
  x2 <- mat2[[i]]$matrix
  x1 <- symm_mat(x1)
  x2 <- symm_mat(x2)
  max_eigen_ratio(x1, x2)
}




### Repeat these for a single matrix objects. 

##################



scale_factor_single <- function(mat1, mat2, ... ){
  x1 <- split_cm_single(mat1, ...)
  x2 <- split_cm_single(mat2, ...)
  x1 <- symm_mat(x1)
  x2 <- symm_mat(x2)
  max_eigen_ratio(x1, x2)
}

# Take a full matrix and scale it based on ratio of mat1 and mat2
scale_matrix_single <- function(fullmat, mat1, mat2, i = 1, ...){
  mat1 <- mat1
  
  fullmat * scale_factor_single(mat1, mat2, i = i, ...)
  
}

## Specific for this analysis get imputed values for H2020 matrix
impute_values_single <- function(i = 1, school = FALSE, ...){
  imputed_values <- scale_matrix_single(
    polymod_cm_home, 
    h2020_cm_home, 
    polymod_cm_home, 
    i = i,
    ...
  ) +
    scale_matrix_single(
      polymod_cm_work, 
      h2020_cm_work, 
      polymod_cm_work, 
      i = i,
      ...
    ) +
    scale_matrix_single(
      polymod_cm_other, 
      h2020_cm_other, 
      polymod_cm_other, 
      i = i,
      ...
    ) 
  
  if(school){
    ## Schools were closed so can not scale by this so just add
    imputed_values <- imputed_values +
      polymod_cm_schoolmatrix 
  }
  return(imputed_values)
  
}