## clean_itn_provenance_data.r
## Amelia Bertozzi-Villa
## January 2024
## 
## Clean and prep the ITN provenance data shared by Camilo Vargas of
## MAP on January 18, 2024 via Google Drive
##############################################################################################################

library(data.table)
library(ggplot2)

rm(list = ls())

parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/primary_data"
raw_data_fname <- file.path(parent_dir, "itn_nets-1-0-0.csv")

prov_raw <- fread(raw_data_fname)
setnames(prov_raw, "householdid", "hhid")
prov_svyids <- unique(prov_raw$surveyid)

# also read household data to merge on survey ids
hh_data <- fread(file.path(parent_dir, "../cleaned_input_data/itn_equity_cleaned.csv"))

hh_svyids <- unique(hh_data[hh_has_entry_in_net_tbl==1]$surveyid)

# looks like we should have at least some provenance data for all the surveys we're analyzing 
length(intersect(hh_svyids, prov_svyids)) == length(hh_svyids)

# what proportion of each survey collected itn data?
prop.table(table(hh_data$surveyid, hh_data$hh_has_entry_in_net_tbl), margin=1)
# all of them, except for 338!

# go ahead and subset the provenance data down to just these surveys
prov_raw <- prov_raw[surveyid %in% hh_svyids]

# there are 11 cases with an NA clusterid... can we rediscover the clusterid
# using the hh data?
na_clusters <- prov_raw[is.na(clusterid), list(surveyid, hhid, netid)]

# maybe not
merge(na_clusters, hh_data[, list(surveyid, clusterid, hhid)], all.x=T)

# check manually... no
hh_data[surveyid==468 & startsWith(hhid, "1 "), list(surveyid, clusterid, hhid)]
hh_data[surveyid==474 & startsWith(hhid, "001"), list(surveyid, clusterid, hhid)]
hh_data[surveyid==546 & startsWith(hhid, "1 "), list(surveyid, clusterid, hhid)]
hh_data[surveyid==562 & startsWith(hhid, "4 "), list(surveyid, clusterid, hhid)]
hh_data[surveyid==574 & startsWith(hhid, "1 "), list(surveyid, clusterid, hhid)]

# ok, drop those households
prov_raw <- prov_raw[!is.na(clusterid)]


####### do a survey by survey check-- is there data in prov_raw for every household 
####### in hh_data with hh_has_entry_in_net_tbl==1?

hhs_with_net_info <- hh_data[hh_has_entry_in_net_tbl==1,
                         list(surveyid,
                              clusterid,
                              hhid,
                              n_itn)]

unique_prov_hhs <- prov_raw[, list(surveyid,
                                   clusterid,
                                   hhid,
                                   netid)]

find_joint <- merge(hhs_with_net_info, unique_prov_hhs, all=T)

# looks like we have discrepancies both ways. 
# start with the places where we have prov_raw entries but no hh entries.
# are they actually in hh_data, with a miscoded hh_has_entry_in_net_tbl?
missing_hhs <- find_joint[is.na(n_itn)]
missing_hhs[, n_itn:=NULL]

# this is only two surveys
unique(missing_hhs$surveyid)
table(missing_hhs$surveyid)

hhs_without_net_info <- hh_data[hh_has_entry_in_net_tbl==0,
                             list(surveyid,
                                  clusterid,
                                  hhid,
                                  n_itn)]
miscoded_hhs <- merge(missing_hhs, hhs_without_net_info, all.x=T)

# looks like there were some mis-labeled households in survey 338, but how many?
find_338 <- merge(missing_hhs[surveyid==338], hhs_without_net_info[surveyid==338], all=T)

# all of them! the only na's are in households with zero nets, which we tackle below
summary(find_338)

#that deals with survey 338, but not 549:
table(miscoded_hhs[is.na(n_itn)]$surveyid)

# looks like the provenance data includes 22 extra clusters not in the main dataset? 
# follow up with camilo
unique(miscoded_hhs[is.na(n_itn)]$clusterid)
unique((hh_data[surveyid==549]$clusterid))


# next look at places where we have hh entries but missings in prov_raw:
missing_prov <- find_joint[is.na(netid)]

# looks like these are exclusively places where there were no nets in the home,
# so it's appropriate to have NAs here
unique(missing_prov$n_itn)==0

####### end exploratory section
####### 


## ok, let's start again here. 
# first, recode all surveyid 338 surveys to have an hh_has entry_in_net_table of 1
hh_data[surveyid==338 & hh_has_entry_in_net_tbl==0, hh_has_entry_in_net_tbl:=1]

# now every household should have net data:
unique(hh_data$hh_has_entry_in_net_tbl)

# now, merge on the net table to check if questions were asked about every single net
check_counts <- merge(prov_raw
                      # [, list(surveyid,
                      #                     clusterid,
                      #                     hhid,
                      #                     netid)]
                      ,
                          hh_data[,
                                  list(surveyid,
                                       clusterid,
                                       hhid,
                                       n_itn)],
                          all=T)

# make sure all netid NAs are zeros
unique(check_counts[is.na(netid)]$n_itn)

# make sure all n_itn==0 has a netid of NA
# oh, these are untreated nets
untreated_nets <- check_counts[n_itn==0 & !is.na(netid)]

# how many in each survey? quite a few! 
prop.table(table(prov_raw$surveyid, prov_raw$itn), margin=1)*100

# ok, for now to check numbers, let's keep just the itns
# note that this will also drop cases where n_itn==0 
check_counts <- check_counts[itn=="yes"]
summary(check_counts)

# count the itns per household, see if it sums to n_itn
count_itns <- check_counts[, list(prov_net_count=.N),
                           by=list(surveyid, 
                                   clusterid,
                                   hhid,
                                   n_itn)]
# wow, we really do account for every net. how about that.
count_itns[n_itn!=prov_net_count]

#######################################################
# ok, now that we know this... for how many of these nets do we
# *actually* have provenance data?
# make a guess here until we get docs from camilo

prov_raw[, source_1_is_missing:= 
           (source_1_reclass %in% c("not applicable", "missing"))]
prop.table(table(prov_raw$surveyid, prov_raw$source_1_is_missing), margin=1) * 100

prov_raw[, source_2_is_missing:= 
           (source_2_reclass %in% c("do not know", "not applicable", "missing", "other"))]
prop.table(table(prov_raw$surveyid, prov_raw$source_2_is_missing), margin=1) * 100

# hmm, I'll need more info from camilo before I can continue.



