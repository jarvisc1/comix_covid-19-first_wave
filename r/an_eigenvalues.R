library(socialmixr)
library(data.table)
library(ggplot2)

eigen_all <- readRDS('data/contact_matrices/eigen_all.rds')
eigen_phys <- readRDS('data/contact_matrices/eigen_physical.rds')
eigen_all_scaled <- readRDS('data/contact_matrices/eigen_all_polymod_scaled.rds')
eigen_phys_scaled <- readRDS('data/contact_matrices/eigen_physical_polymod_scaled.rds')



type <- rep(c("All", "Physical", "All_scaled", "Phys_scaled"), each = boots)

eigens <- c(eigen_all, eigen_phys, eigen_all_scaled, eigen_phys_scaled)

eigen_df <- data.table(type, eigens)



previousR <- rnorm(nrow(eigen_df), mean = 2.6, sd = 0.54)

eigen_df <- data.table(type, eigens, previousR, newR = eigens*previousR)

saveRDS(eigen_df, file = "data/contact_matrices/rds_eigen.rds")


eigen_df[,.(
  mean = mean(newR),
  lci = quantile(newR, probs = 0.025),
  uci = quantile(newR, probs = 0.975),
  eigen_mean = mean(eigens),
  eigen_lci = quantile(eigens, probs = 0.025),
  eigen_uci = quantile(eigens, probs = 0.975),
  eigen_lci = min(eigens),
  eigen_uci = max(eigens)
  ),
  by = type
  ]


changes_inR <-  readRDS("data/contact_matrices/rds_eigen.rds")



     
