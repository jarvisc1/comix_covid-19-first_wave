### Create a contact matrix with filters

## Put filter = TRUE
## Then put the value to filter by in the various filter variables

cm_filter <- function(
  df  = h2020,
  matrix_output = TRUE,
  country = "United Kingdom",
  age_limits = c(0, 5, 18, 30, 40, 50, 60, 70),
  symmetric = FALSE,
  filter = FALSE,
  phys_contact = NULL,
  home  = NULL,
  other_house = NULL,
  work = NULL,
  school = NULL,
  gender = NULL,
  hhm_quarantine = NULL,
  hhm_isolate = NULL,
  hhm_limit_work = NULL,
  hhm_limit_school = NULL,
  hhm_work_closed = NULL,
  part_quarantine = NULL,
  part_isolate = NULL,
  part_limit_work = NULL,
  part_limit_school = NULL,
  part_work_closed = NULL,
  part_covid_test_results = NULL,
  part_covid_contact = NULL,
  boots = 1
  ){
  
  filter_text <- list()
  
  if(filter){
    filter_text$phys_contact <- phys_contact
    filter_text$cnt_home <- home
    filter_text$cnt_other_house <- other_house
    filter_text$cnt_work <- work
    filter_text$cnt_school <- school
    filter_text$cnt_gender <- gender
    filter_text$hhm_quarantine <- hhm_quarantine
    filter_text$hhm_isolate <- hhm_isolate
    filter_text$hhm_limit_work <- hhm_limit_work
    filter_text$hhm_limit_school <- hhm_limit_school
    filter_text$hhm_work_closed <- hhm_work_closed
    filter_text$part_quarantine <-  part_quarantine
    filter_text$part_isolate <-  part_isolate
    filter_text$part_limit_work <-  part_limit_work
    filter_text$part_work_closed <- part_work_closed
    filter_text$part_limit_school <-  part_limit_school
    filter_text$part_covid_test_results <-  part_covid_test_results
    filter_text$part_covid_contact <-  part_covid_contact
  }
  
  x <- contact_matrix(
    df, 
    countries = country, 
    age.limits = age_limits, 
    symmetric = symmetric,
    filter = filter_text,
    n = boots
  )
  if(matrix_output){
     if(boots == 1){
       return(x$matrix)
     } else {
       return(x$matrices)
    }  
  } else {
  
  return(x)
  }  
}
