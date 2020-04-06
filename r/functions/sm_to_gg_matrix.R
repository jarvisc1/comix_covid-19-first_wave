## Convert socialmixR matrix into a dataframe for plotting


sm_to_gg_matrix <- function(cmatrices) {
  cmatrices_dt <- list()
  # Convert matrces into to format for plot
  for(m in 1:length(cmatrices)){
    
    cmatrix <- as.data.table(cmatrices[[m]])
    
    cmatrix[, "participant_age"] <- colnames(cmatrix)
    
    cmatrix <- melt(
      cmatrix, 
      id.vars = "participant_age",
      variable.factor = F,
      value.factor = F,
      variable.name = "contact_age",
      value.name = "contacts"
    )
    
    cmatrix[, "study"] <- names(cmatrices)[m]
    
    cmatrices_dt[[length(cmatrices_dt)+1]] <- cmatrix
  }
  
  cmatrices_dt <- rbindlist(cmatrices_dt)
  cmatrices_dt
}


gg_matrix <- function(dt, breaks = c(0,0.5, 1), age_lab = FALSE) {
  ct_age_unique <- unique(dt[,contact_age])
  
  gplot <- ggplot(dt,
         aes(x = factor(participant_age,  ct_age_unique),
             y = factor(contact_age,  ct_age_unique),
             fill = contacts
         )
  ) +
    facet_wrap(. ~ study) +
    geom_tile() +
    labs(
      x="Age of participant",
      y="Age of contact",
      fill="Contacts"
    ) + 
    scale_fill_gradientn(
      colors=c("#0D5257","#00BF6F", "#FFB81C"),
      na.value = "#FFFFFF",
      values = c(0, 1, 3, 5, 12)/12,
      breaks= breaks,
      limits = c(0, 9)
    ) +
    theme(
      legend.position = "right"
    ) 
  
  
  if(length(age_lab) > 2){
    print("Yes")
    gplot <- gplot +
      scale_x_discrete(
        labels=age_lab
      ) +
      scale_y_discrete(
        labels=age_lab
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.background = element_blank())
  }
  gplot
  
}
  

  

