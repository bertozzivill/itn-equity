## clean_itn_provenance_data.r
## Amelia Bertozzi-Villa
## January 2024
## 
## Clean and prep the ITN provenance data shared by Camilo Vargas of
## MAP on January 18, 2024 via Google Drive
##############################################################################################################

library(data.table)
library(ggplot2)
library(RColorBrewer)

rm(list = ls())

parent_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_general/itn_equity/primary_data"
raw_data_fname <- file.path(parent_dir, "itn_nets-1-0-1.csv")

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

# for now, drop the places with na n_itn (not present in hh surveys)
check_counts <- check_counts[!is.na(n_itn)]

# make sure all netid NAs are zeros
unique(check_counts[is.na(netid)]$n_itn)

# ok, then drop those as well for now
check_counts <- check_counts[!is.na(netid)]

# make sure all n_itn==0 has a netid of NA
# oh, these are untreated nets
untreated_nets <- check_counts[n_itn==0 & !is.na(netid)]
unique(untreated_nets$itn)

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

prov_raw[, source_1_check_missingness:= 
           (ifelse(source_1_reclass %in% c("", "not applicable", "missing"),
                   source_1_reclass, 
                   "present"))]
prov_raw[source_1_check_missingness=="", source_1_check_missingness:="not in survey"]

missing_props <- as.data.table(prop.table(table(prov_raw$surveyid,
                                                prov_raw$source_1_check_missingness), 
                                          margin=1) * 100)
names(missing_props) <- c("surveyid", "source_1_type", "prop")
missing_props[, surveyid:=as.integer(surveyid)]
missing_props <- merge(unique(hh_data[, list(surveyid, dhs_survey_id, country_name, year)]),
      missing_props, all=T)
# ok, there are only three surveys that have any variation in missingness type, 
# and all three of those are 99% present. 
missing_props[!prop %in% c(0, 100)]

# let's just round everything to an integer value to have one value per survey
missing_props[, prop:=round(prop)]
missing_props <- missing_props[prop==100]
table(missing_props$source_1_type)

# aha, ok, we start to get info about net provenance in 2015
ggplot(missing_props, aes(x=year, fill=source_1_type)) +
  geom_bar() +
  theme_minimal() 

# let's subset down to the 44 surveys that have responses and take a look at the contents

present_surveys <- missing_props[source_1_type=="present"]$surveyid

prov_subset <- prov_raw[surveyid %in% present_surveys]

# let's *also* subset down to only itns, though we might revise this in the future??
# prov_subset <- prov_subset[itn=="yes"]

prop.table(table(prov_subset$surveyid, prov_subset$source_1_reclass), margin=1)*100

prop.table(table(prov_subset$source_1_reclass))*100

# confirm that source 2 is only filled when source 1 is "no"-- whoops, it's not
unique(prov_subset$source_2_reclass)
table(prov_subset[source_1_reclass!="no"]$source_2_reclass)

# hmm, let's explore these one by one
# chw looks like an added detail on top of routine distribution
prov_subset[source_1_reclass!="no" & source_2_reclass=="community health worker"]

# govt facility similar to chw looks like an added detail on top of routine distribution
unique(prov_subset[source_1_reclass!="no" & source_2_reclass=="government facility"]$source_1_reclass)

# same with ngo
prov_subset[source_1_reclass!="no" & source_2_reclass=="ngo"]

# same with pharmacy
prov_subset[source_1_reclass!="no" & source_2_reclass=="pharmacy"]

# same with religious institution
prov_subset[source_1_reclass!="no" & source_2_reclass=="religious institution"]

# same with private facility
unique(prov_subset[source_1_reclass!="no" & source_2_reclass=="private facility"]$source_1_reclass)

# same with school
unique(prov_subset[source_1_reclass!="no" & source_2_reclass=="school"]$source_1_reclass)

# shop is confusing, give source 1 priority
prov_subset[source_1_reclass!="no" & source_2_reclass=="shop"]

# family & friends is confusing, but only 3 nets-- go w/ source 1
prov_subset[source_1_reclass!="no" & source_2_reclass=="family or friends"]

# don't know is pretty straightforward & rare
prov_subset[source_1_reclass!="no" & source_2_reclass=="do not know"]

# think we can safely ignore "other"
prov_subset[source_1_reclass!="no" & source_2_reclass=="other"]

#### ok, we can always ignore source 2 when source 1 is not "no".
# when source 1 *is* "no", how do we decide when someone paid for it?
table(prov_subset[source_1_reclass=="no"]$source_2_reclass)

# clearly unpaid: chw, distribution campaign, ngo, school, community association,
#                 govt facility, 
# clearly paid: shop
# unclear: family/friends, pharmacy, private facility, religious institution
# null/undefined: do not know, missing, other

# I'm going to put family/friends and religious institution (0.5% of total) into "free", and 
# pharmacy and private facility (1% of total) into "paid"

### construct a new variable that combines both of these variables since there's overlap
source_1_vals <- unique(prov_subset$source_1_reclass)
source_2_vals <- unique(prov_subset$source_2_reclass)
intersect(source_1_vals, source_2_vals)
setdiff(source_1_vals, source_2_vals)
setdiff(source_2_vals, source_1_vals)

# make sure that distribution campaigns have the same value in both
prov_subset[source_2_reclass=="distribution campaign", source_2_reclass:="campaign"]

prov_subset[, joint_source:= ifelse(source_1_reclass=="no", 
                                    source_2_reclass,
                                    source_1_reclass)]

prop.table(table(prov_subset$joint_source))*100
free_vals <- c("antenatal care",
               "at birth",
               "campaign",
               "community association",
               "community health worker",
               "family or friends",
               "government facility",
               "immunization visit",
               "ngo",
               "religious institution",
               "school")

paid_vals <- c("pharmacy",
               "private facility",
               "shop")

unknown_vals <- c("do not know",
                  "missing",
                  "other")

map_vals <- c(rep("free", length(free_vals)),
              rep("paid", length(paid_vals)),
              rep("unknown", length(unknown_vals)))


joint_vals_map <- data.table(joint_source=c(free_vals,
                                            paid_vals,
                                            unknown_vals),
                             joint_source_recode=map_vals)

prov_subset <- merge(prov_subset, joint_vals_map, all.x=T)

prop.table(table(prov_subset$surveyid, prov_subset$joint_source_recode), margin=1)*100

# randomly distribute the unknown values into free or paid, weighted by
# the frequency of free/paid in the dataset, by survey
free_prop <- prop.table(table(prov_subset[joint_source_recode!="unknown"]$surveyid,
                              prov_subset[joint_source_recode!="unknown"]$joint_source_recode),
                        margin=1)
free_prop_dt <- data.table(surveyid=as.integer(rownames(free_prop)),
                           free_prop=free_prop[,1])

prov_subset <- merge(prov_subset, free_prop_dt, by="surveyid", all.x=T)

set.seed(684)
prov_subset$rand <- runif(nrow(prov_subset))
prov_subset[joint_source_recode=="unknown", joint_source_recode_2:= ifelse(rand<free_prop, "free", "paid")]
prov_subset[joint_source_recode!="unknown", joint_source_recode_2:=joint_source_recode]

new_free_prop <- prop.table(table(prov_subset$surveyid,
                 prov_subset$joint_source_recode_2),
           margin=1)
# good agreement
round(free_prop, 2)==round(new_free_prop, 2)


############ final cleaning and household-level aggregation
# ok, finally, let's clean this, make plots, and bring up to the household level 

prov_final <- prov_subset[, list(surveyid,
                                 clusterid,
                                 hhid, 
                                 netid,
                                 itn,
                                 joint_source,
                                 paid_type=joint_source_recode_2)]

prov_final <- merge(unique(hh_data[, list(surveyid,
                                          dhs_survey_id,
                                          country_name,
                                          year)],),
                    prov_final,
  all.y=T)

prov_final <- prov_final[order(dhs_survey_id)]
prov_final[, survey_label:= paste(country_name, year)]


prov_final_props <- prov_final[, list(type_count=.N),
                               by=list(dhs_survey_id,
                                       country_name, 
                                       year,
                                       paid_type)][, 
                                                   type_prop:=type_count/sum(type_count)*100,
                                                   by=list(dhs_survey_id, 
                                                           country_name,
                                                           year)]
prov_final_props[, survey_label:= paste(country_name, year)]

ggplot(prov_final_props, aes(x=survey_label, y=type_prop, fill=paid_type)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=round(type_prop, 0)), 
            position=position_stack(vjust=0.5),
            size=3) +
  theme_minimal() +
  scale_fill_manual(values=c("#4dac26", "#d01c8b"), name="Payment Type") + 
  theme(axis.text.x = element_text(hjust=1, angle=45)) +
  labs(x="", 
       y="Percentage")


# what proportion of these are untreated?
untreated_final_props <- prov_final[, list(type_count=.N),
                               by=list(dhs_survey_id,
                                       country_name, 
                                       year,
                                       itn)][, 
                                                   itn_prop:=type_count/sum(type_count)*100,
                                                   by=list(dhs_survey_id, 
                                                           country_name,
                                                           year)]
untreated_final_props[, survey_label:= paste(country_name, year)]

ggplot(untreated_final_props, aes(x=survey_label, y=itn_prop, fill=itn)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=round(itn_prop, 0)),
            position=position_stack(vjust=0.5),
            size=3) +
  theme_minimal() +
  scale_fill_manual(values=c( "#F8766D","#00BFC4"), name="Is ITN") + 
  theme(axis.text.x = element_text(hjust=1, angle=45)) +
  labs(x="", 
       y="Percentage")


# compare paid vs unpaid itns
joint_final_props <- prov_final[, list(type_count=.N),
                                    by=list(dhs_survey_id,
                                            country_name, 
                                            year,
                                            paid_type,
                                            itn)][, 
                                                  itn_prop:=type_count/sum(type_count)*100,
                                                  by=list(dhs_survey_id, 
                                                          country_name,
                                                          year)]
joint_final_props[, survey_label:= paste(country_name, year)]
joint_final_props[, net_type:= ifelse(itn=="yes", "ITN", "non-ITN")]
joint_final_props[, full_type := paste(paid_type, net_type)]

pal <- brewer.pal(4, "PiYG")
ggplot(joint_final_props, aes(x=survey_label, y=itn_prop, fill=full_type)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=round(itn_prop, 0)),
            position=position_stack(vjust=0.5),
            size=3) +
  theme_minimal() +
  scale_fill_manual(values = c( pal[4], pal[3], pal[1], pal[2]), name="Net Category") + 
  theme(axis.text.x = element_text(hjust=1, angle=45)) +
  labs(x="", 
       y="Percentage")


