# Contact survey during COVID-19  [![DOI](https://zenodo.org/badge/253430563.svg)](https://zenodo.org/badge/latestdoi/253430563)

Analyses of the first wave of data from the CoMix social contact survey conducted in the UK during COVID-19 outbreak


## Packages
data.table
ggplot2
socialmixr
patchwork
ggthemr
```
remotes::install_github("Rdatatable/data.table")
install.packages("ggplot2)
remotes::install_github("sbfnk/socialmixr")
install.packages("patchwork/ggplot2)
devtools::install_github('cttobin/ggthemr')
```

## Folder structure

data/ - for the data
data/contact_matrices - For contact matrices
r/ - for r scripts
r/functions/ - for functions created for this analysis
reports/ - For markdown documents
outputs/ - For any graphs or output of data for public use
admin/ - documents such as the survey protocol 

## Outline of scripts

scripts with dm_ are for data management
scripts with an_ are for analysis and creating figures

* dm_setup_contact_matrix - Create contact matrices based on the survey data and polymod and save in data/contact_matrices/
* dm_scale_contact_matrix - Scale the contact matrices to impute age and reduction in R. 
* dm_scale_boot_matrices - repeat what dm_scale_contact_matrix does but for bootstrapped matrices
* an_figure_one - Figure for paper
* an_figure_two - Figure of contact matrices for different locations
* an_descriptive_stats_tables -  descriptive stats that go into tables in the analysis.
* an_table_two - Table overview of the characterstics of participants and contacts
* an_eigenvalues - Create data and look at change in R based on the eigenvalue scalings. 


 
 



