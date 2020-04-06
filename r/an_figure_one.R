### Figure 1. 

library(patchwork)
library(ggthemr)
library(socialmixr)
library(data.table)

ggthemr("fresh")
## Create Figure of Contact Matrices

source('r/functions/sm_to_gg_matrix.R')
source('r/functions/utility_functions.R')

h2020_cm <- readRDS('data/contact_matrices/h2020_cm.rds')
h2020_cm_imputed <- readRDS('data/contact_matrices/h2020_cm_imputed.rds')
polymod_cm <- readRDS('data/contact_matrices/polymod_cm.rds')


rowSums(polymod_cm)/rowSums(h2020_cm)
colnames(polymod_cm)

cmatrices <- list(
  "POLYMOD" = polymod_cm,
  "CoMix" = h2020_cm_imputed
)

## Transform data
cm_dt <- sm_to_gg_matrix(cmatrices)

cm_dt$contact_age

## Create the plot
matrix_plot <- gg_matrix(
  cm_dt, 
  breaks = c(0, 2,4,6, 8),
  age_lab = age_labs
) + 
  ggtitle("A") 

matrix_plot


## B change in R plot

changes_inR <-  readRDS("data/contact_matrices/rds_eigen.rds")


changes_inR <- changes_inR[type %in% c("All", "Physical"), ]

ggthemr("fresh")
r_plot <- ggplot(changes_inR) +
  geom_density(aes(newR, fill= type), 
                  alpha = 0.8, position = "identity") +
  labs(
    x = expression("Estimate of" ~ R[0]),
    y = "Density",
    title = "B") +
  scale_fill_manual(
    name="Type of Contact",
    values=c("#0D5257","#6d979a")
                      ) +
  scale_x_continuous(
    limits = c(0.0, 1.25),
    breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25)
  ) +
  geom_vline(xintercept = 1, col = "black") +
  theme(
    legend.position = c(0.6, 0.9),
    legend.box.background = element_blank(),
    legend.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) 



r_plot
## Combine the plots
x1 <- (matrix_plot / r_plot )
x1

ggsave(filename = "outputs/figure1.png", x1, width = 6, height = 6.5)
