###############################################################################################################
## clean_itn_distribution_data.r
## Amelia Bertozzi-Villa
## January 2024
## 
## Clean and prep the ITN distribution data from 2000-2020 pulled from the google cloud
##############################################################################################################

library(data.table)
library(ggplot2)

rm(list = ls())

parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/primary_data"
raw_data_fname <- file.path(parent_dir, "input_itn_distributions.csv")

dist_raw <- fread(raw_data_fname)

# set nets distributed to the sum of itn and llin
dist_raw[, tot_nets:= ifelse(is.na(ITN), LLIN, ITN + LLIN)]

# now take the greater of this sum or the TOT column
dist_raw[, tot_nets:= pmax(tot_nets, TOT, na.rm = T)]

dist_clean <- dist_raw[, list(iso3=ISO3, 
                              name=MAP_Country_Name, 
                              year,
                              nets_distributed=tot_nets)]

# see if we can find mass campaigns via a cutoff
dist_clean[, mean:= mean(nets_distributed, na.rm=T), by="name"]

# looks like this mean value is a little better at capturing mass dists than a linear regression
ggplot(dist_clean, aes(x=year, y=nets_distributed)) +
  geom_line() +
  geom_point() +
  geom_hline(aes(yintercept=mean), color="blue") +
  facet_wrap(~iso3, scales="free_y") 
  geom_smooth(method = "lm", na.rm = T, se=F)

# wow, this does quite well! 
dist_clean[, mass_campaign:=nets_distributed>mean]
ggplot(dist_clean, aes(x=year, y=nets_distributed)) +
  geom_line() +
  geom_point(aes(color=mass_campaign)) +
  facet_wrap(~iso3, scales="free_y") 

# save
write.csv(dist_clean, file=file.path(parent_dir, "../cleaned_input_data/itn_dists_cleaned.csv"), row.names=F)



