###############################################################################################################
## itn_equity_exploratory.r
## Amelia Bertozzi-Villa
## December 2023
## 
## Begin exploring the ITN equity DHS data shared by Camilo Vargas of
## MAP on December 1, 2023 via Google Drive
##############################################################################################################

library(data.table)
library(ggplot2)

rm(list=ls())

# filepaths
parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/primary_data"
raw_data_fname <- file.path(parent_dir, "itn_with_housing-0-1-0.csv")

# load data from Camilo
data_raw <- fread(raw_data_fname)

# start by isolating only the itn-specific data & doing some data checks
itn_cols <- names(data_raw)[1:30]
itn_data_raw <- data_raw[, ..itn_cols]

# this dataset looks complete, except for some lat-longs... great! 
summary(itn_data_raw)

# try to infer what "itn_theoretical_capacity" is: just n_itn*2, or does it take household size into account?
itn_data_raw[itn_theoretical_capacity!=(n_itn*2), list(n_defacto_pop, n_itn, itn_theoretical_capacity)]

# ok, is it our old friend pmin(n_defacto_pop, n_itn*2)?
itn_data_raw[itn_theoretical_capacity!=pmin(n_itn*2, n_defacto_pop),
             list(n_defacto_pop, n_itn, itn_theoretical_capacity)]

# yes. this is the column the stockflow data calls n_with_access.

# so then we should be able to calculate access via itn_theoretical_capacity/n_defacto_pop.


