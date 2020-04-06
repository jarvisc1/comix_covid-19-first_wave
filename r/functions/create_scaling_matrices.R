## Impute the ages for the contact matrices for bootstrap and for just 1. 

create_scaling_matrices <- function(nboots = 1, 
                        age_limits = c(0, 5, 18, 30, 40, 50, 60, 70),
                        age_limits_sym = c(18, 30, 40, 50, 60, 70),
                        file_name = "default.RData") {
  
  ## We do not have data on participants below 18 so need to have lower limit
  ## Of 18 to do the inputed and have a symmetric matrix
  
  
  h2020_cm <- cm_filter(h2020, boots = nboots)
  polymod_cm <- cm_filter(polymod, symmetric = T, boots = nboots)
  
  
  
  ## Create location specific contact matrices
  
  ## Need to run the cm_filter function multiple times so wrap in a specific
  ## function with the same defaults
  
  h2020_cm_scaling <- function(...){
    cm_filter(df = h2020, boots = nboots,
              age_limits = age_limits_sym,
              filter = TRUE, 
              symmetric = TRUE, 
              ...
    )
  }
  
  h2020_cm_school <- h2020_cm_scaling(school = "Yes")
  h2020_cm_home <- h2020_cm_scaling(home = "Yes")
  h2020_cm_work <- h2020_cm_scaling(work = "Yes")
  h2020_cm_other <- h2020_cm_scaling(work = "No", school = "No", home = "No")
  
  ## Repeat for Polymod
  
  polymod_cm_scaling <- function(...){
    cm_filter(df = polymod, boots = nboots,
              age_limits = age_limits,
              filter = TRUE,
              symmetric = TRUE, 
              ...
    )
  }
  
  ## Polymod has different values for it's defaults
  polymod_cm_school <- polymod_cm_scaling(school = 1)
  polymod_cm_home <- polymod_cm_scaling(home = 1)
  polymod_cm_work <- polymod_cm_scaling(work = 1)
  polymod_cm_other <- polymod_cm_scaling(work = 0, school = 0, home = 0)
  
  save(
    h2020_cm,
    h2020_cm_school,
    h2020_cm_home,
    h2020_cm_work,
    h2020_cm_other,
    polymod_cm,
    polymod_cm_school,
    polymod_cm_home,
    polymod_cm_work,
    polymod_cm_other,
    file = paste0("data/contact_matrices/", file_name))
  
}




create_scaling_matrices_phys <- function(h2020,
                                    nboots = 1, 
                                    age_limits = c(0, 5, 18, 30, 40, 50, 60, 70),
                                    age_limits_sym = c(18, 30, 40, 50, 60, 70),
                                    file_name = "default_phys.RData"){


  
  ## We do not have data on participants below 18 so need to have lower limit
  ## Of 18 to do the inputed and have a symmetric matrix
  
  
  h2020_cm_phys <- cm_filter(h2020, boots = nboots, filter = TRUE, phys_contact = 1)
  polymod_cm_phys <- cm_filter(polymod, symmetric = T, boots = nboots,filter = TRUE, phys_contact = 2)
  
  
  
  ## Create location specific contact matrices
  
  ## Need to run the cm_filter function multiple times so wrap in a specific
  ## function with the same defaults
  
  h2020_cm_scaling <- function(...){
    cm_filter(df = h2020, boots = nboots,
              age_limits = age_limits_sym,
              filter = TRUE, 
              symmetric = TRUE, 
              phys_contact = 1,
              ...
    )
  }
  
  h2020_cm_school_phys <- h2020_cm_scaling(school = "Yes")
  h2020_cm_home_phys <- h2020_cm_scaling(home = "Yes")
  h2020_cm_work_phys <- h2020_cm_scaling(work = "Yes")
  h2020_cm_other_phys <- h2020_cm_scaling(work = "No", school = "No", home = "No")
  
  ## Repeat for Polymod
  
  polymod_cm_scaling <- function(...){
    cm_filter(df = polymod, boots = nboots, 
              age_limits = age_limits,
              filter = TRUE,
              symmetric = TRUE, 
              phys_contact = 2,
              ...
    )
  }
  
  ## Polymod has different values for it's defaults
  polymod_cm_school_phys <- polymod_cm_scaling(school = 1)
  polymod_cm_home_phys <- polymod_cm_scaling(home = 1)
  polymod_cm_work_phys <- polymod_cm_scaling(work = 1)
  polymod_cm_other_phys <- polymod_cm_scaling(work = 0, school = 0, home = 0)
  
  save(
    h2020_cm_phys,
    h2020_cm_school_phys,
    h2020_cm_home_phys,
    h2020_cm_work_phys,
    h2020_cm_other_phys,
    polymod_cm_phys,
    polymod_cm_school_phys,
    polymod_cm_home_phys,
    polymod_cm_work_phys,
    polymod_cm_other_phys,
    file = paste0("data/contact_matrices/", file_name))


}





create_scaling_matrices_observed <- function(nboots = 1, 
                                    age_limits = c(0, 5, 18, 30, 40, 50, 60, 70),
                                    age_limits_sym = c(18, 30, 40, 50, 60, 70),
                                    file_name = "default.RData") {
  
  ## We do not have data on participants below 18 so need to have lower limit
  ## Of 18 to do the inputed and have a symmetric matrix
  
  
  h2020_cm <- cm_filter(h2020, boots = nboots)
  polymod_cm <- cm_filter(polymod, symmetric = T, boots = nboots)
  
  
  
  ## Create location specific contact matrices
  
  ## Need to run the cm_filter function multiple times so wrap in a specific
  ## function with the same defaults
  
  h2020_cm_scaling <- function(...){
    cm_filter(df = h2020, boots = nboots,
              age_limits = age_limits,
              filter = TRUE, 
              symmetric = FALSE, 
              ...
    )
  }
  
  h2020_cm_school <- h2020_cm_scaling(school = "Yes")
  h2020_cm_home <- h2020_cm_scaling(home = "Yes")
  h2020_cm_work <- h2020_cm_scaling(work = "Yes")
  h2020_cm_other <- h2020_cm_scaling(work = "No", school = "No", home = "No")
  
  ## Repeat for Polymod
  
  polymod_cm_scaling <- function(...){
    cm_filter(df = polymod, boots = nboots,
              age_limits = age_limits,
              filter = TRUE,
              symmetric = TRUE, 
              ...
    )
  }
  
  ## Polymod has different values for it's defaults
  polymod_cm_school <- polymod_cm_scaling(school = 1)
  polymod_cm_home <- polymod_cm_scaling(home = 1)
  polymod_cm_work <- polymod_cm_scaling(work = 1)
  polymod_cm_other <- polymod_cm_scaling(work = 0, school = 0, home = 0)
  
  save(
    h2020_cm,
    h2020_cm_school,
    h2020_cm_home,
    h2020_cm_work,
    h2020_cm_other,
    polymod_cm,
    polymod_cm_school,
    polymod_cm_home,
    polymod_cm_work,
    polymod_cm_other,
    file = paste0("data/contact_matrices/", file_name))
  
}









