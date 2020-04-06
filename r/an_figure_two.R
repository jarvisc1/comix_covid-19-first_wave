### Figure Contact matrices different settings. 

library(socialmixr)
library(data.table)
library(ggplot2)
library(patchwork)
library(ggthemr)



## Create Figure of Contact Matrices 

load('data/contact_matrices/h2020_cms.RData')

source('r/functions/sm_to_gg_matrix.R')
source(('r/functions/utility_functions.R'))

h2020_cm <- readRDS('data/contact_matrices/h2020_cm.rds')
polymod_cm <- readRDS('data/contact_matrices/polymod_cm.rds')
h2020_cm_phys <- readRDS('data/contact_matrices/h2020_cm_phys.rds')
polymod_cm_phys <- readRDS('data/contact_matrices/polymod_cm_phys.rds')

cmatrices <- list(
  "CoMix - All" = h2020_cm,
  "POLYMOD - All" = polymod_cm,
  "CoMix - Physical" = h2020_cm_phys,
  "POLYMOD - Physical" = polymod_cm_phys,
  "CoMix - Home" = h2020_cm_home,
  "POLYMOD - Home" = polymod_cm_home,
  "CoMix - Work" = h2020_cm_work,
  "POLYMOD - Work" = polymod_cm_work,
  "CoMix - Other" = h2020_cm_other,
  "POLYMOD - Other" = polymod_cm_other,
  "CoMix - School" = h2020_cm_school,
  "POLYMOD - School" = polymod_cm_school
)

cm_dt <- sm_to_gg_matrix(cmatrices) 


cm_dt


gg_matrix <- function(dt, breaks = c(0,0.5, 1), age_lab = FALSE) {
  ct_age_unique <- unique(dt[,contact_age])
  
  gplot <- ggplot(dt,
                  aes(x = factor(participant_age,  ct_age_unique),
                      y = factor(contact_age,  ct_age_unique),
                      fill = contacts
                  )
  ) +
    facet_wrap(. ~ factor(study, levels = names(cmatrices)), ncol = 4, nrow = 3) +
    geom_tile() +
    labs(
      x="Age of participant",
      y="Age of contact",
      fill="Contacts"
    ) + 
    scale_fill_gradientn(
      colors=c("#0D5257","#00BF6F", "#FFB81C"),
      na.value = "#EEEEEE",
      values = c(0, 1, 3, 5, 12)/12,
      breaks= breaks,
      limits = c(0, 9)
      
    )  +
    coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = FALSE, clip = "off")
  
  if(length(age_lab) > 2){
    gplot <- gplot +
      scale_x_discrete(
        labels=age_lab
      ) +
      scale_y_discrete(
        labels=age_lab
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  }
  gplot
  
}


x1 <- gg_matrix(cm_dt , 
          breaks = c(0, 2,4,6, 8), 
          age_lab = age_labs
) 

x1


ggsave(filename = "outputs/figure2.png", x1, width = 8, height = 8)


