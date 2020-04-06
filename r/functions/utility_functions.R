# Contains functions and commonly used objects, open to re-arrange later


## AGE

age_bins <- c(0,1,5*(1:14))
age_bin_levels <- c(
  "<=1",
  "1-5",
  "6-10",
  "11-15",
  "16-20",
  "21-25",
  "26-30",
  "31-35",
  "36-40",
  "41-45",
  "46-50",
  "51-55",
  "56-60",
  "61-65",
  "66+"
)

# WIll remove when data received
rnorm_age_bins <- function(RESP_YEARS) {
  x <- rtruncnorm(1, 0,70, 
                  mean = RESP_YEARS, sd = RESP_YEARS * 0.8)
  # bins <- c(0,1,5*(1:14))
  x <- cut(x, age_bins, labels = age_bin_levels)
  return(x)
}

numeric_age_col <- function(age_bin, level) {
  #level must be "low" or "high"
  age_bin <- as.character(age_bin)
  regx = "-| |\\+|<|>|="
  age_split <- as.numeric(strsplit(age_bin, split = regx)[[1]])
  i <- case_when(level == "low" ~ 1, 
                 level == "high" ~ as.numeric(length(age_split)))
  age <- as.numeric(age_split[i])
  return(age)
}

## Belief Scale

likert_scale_levels = 1:6
likert_scale_labels <- c("Strongly agree",
                  "Tend to agree",
                  "Neither agree nor disagree",
                  "Tend to disagree",
                  "Strongly disagree",
                  "Don’t know")

matrix_age_bins <- c(0,1,5,15,60,75)




## For plots
age_labs <- c("0-4",
              "5-17", 
              "18-29",
              "30-39",
              "40-49",
              "50-59",
              "60-69",
              "70+")
