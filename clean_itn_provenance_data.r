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

# subset the provenance data down to just these surveys
prov_raw <- prov_raw[surveyid %in% hh_svyids]

# drop the 11 cases with an NA clusterid
prov_raw <- prov_raw[!is.na(clusterid)]

#######################################################
# for how many of these nets do we
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

# let's subset down to the 44 surveys that have responses 

present_surveys <- missing_props[source_1_type=="present"]$surveyid

prov_subset <- prov_raw[surveyid %in% present_surveys]

# let's *also* subset down to only itns, though we might revise this in the future??
prov_subset <- prov_subset[itn=="yes"]


#### we can always ignore source 2 when source 1 is not "no".
# when source 1 *is* "no", how do we decide when someone paid for it?
table(prov_subset[source_1_reclass=="no"]$source_2_reclass)

# clearly unpaid: chw, distribution campaign, ngo, school, community association,
#                 govt facility, 
# clearly paid: shop
# unclear: family/friends, pharmacy, private facility, religious institution
# null/undefined: do not know, missing, other

# I'm going to put family/friends and religious institution into "free", and 
# pharmacy and private facility into "paid"

### construct a new variable that combines both of these variables since there's overlap

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

############ test use counts
# do the number of people using nets in the provenance data equal that in the 
# household data?
prov_subset[, n_slept_under_net_numeric:= ifelse(n_slept_under_net=="more than 5",
                                                 5, as.integer(n_slept_under_net))]
# make sure we understand the original data
use_subset <- prov_subset[, list(surveyid, clusterid, hhid, netid, n_slept_under_net, n_slept_under_net_numeric)]


prov_hh_use <- prov_subset[, list(net_table_n_itn= .N,
                                  net_table_n_slept_under_itn=sum(n_slept_under_net_numeric)),
                           by=list(surveyid, clusterid, hhid)]

prov_hh_use <- merge(prov_hh_use, 
                     hh_data[surveyid %in% unique(prov_hh_use$surveyid),
                             list(surveyid, dhs_survey_id, country_name,clusterid, hhid, 
                                  n_defacto_pop,
                                  hh_table_n_itn=n_itn,
                                  hh_table_n_slept_under_itn=n_slept_under_itn)],
                     by=c("surveyid", "clusterid", "hhid"),
                     all=T)

# provenance data is only na when there were zero nets to sleep under
summary(prov_hh_use[is.na(net_table_n_slept_under_itn)])
prov_hh_use <- prov_hh_use[!is.na(net_table_n_slept_under_itn)]

# remove uganda rows that aren't in hh data-- these are  refugee numbers that we don't include
prov_hh_use <- prov_hh_use[!is.na(hh_table_n_itn)]

#net counts are exactly right
prov_hh_use[net_table_n_itn!=hh_table_n_itn]

# sometimes net_table_n_slept_under_itn is greater than n_defacto_pop...
# defer to n_defacto_pop, as per the logic of clean_itn_equity_data.r
prov_hh_use[net_table_n_slept_under_itn>n_defacto_pop, net_table_n_slept_under_itn:=n_defacto_pop]

# when are the sleeping numbers different?
prov_hh_use[, diff:= net_table_n_slept_under_itn-hh_table_n_slept_under_itn]

# it's almost always greater in the net file, not less
ggplot(prov_hh_use[diff!=0], aes(x=diff)) + geom_histogram()

# different in 2.3% of cases
nrow(prov_hh_use[diff!=0])/nrow(prov_hh_use)

unique(prov_hh_use[diff!=0]$surveyid)

# it's only less than 1 in survey 515, in 13 households that log more than 5 people sleeping under a net

# how many of these examples feature a surprisingly large use rate?
test_discrepancy <- prov_hh_use[diff>0]
test_discrepancy[, prov_use_rate:= net_table_n_slept_under_itn/net_table_n_itn]

write.csv(prov_hh_use, file = "~/Desktop/net_vs_hh_table_use_discrepancy.csv", row.names = F)

############ final cleaning and household-level aggregation
# ok, finally, let's clean this, make plots, and bring up to the household level 

prov_final <- prov_subset[, list(surveyid,
                                 clusterid,
                                 hhid, 
                                 netid,
                                 itn,
                                 net_age_months,
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


# save this
write.csv(prov_final, file=file.path(parent_dir, "../cleaned_input_data/provenance_by_net.csv"), 
          row.names = F)

## collapse down to the household level, retaining the paid/unpaid disaggregation
prov_hh_final <- prov_final[, list(net_count=.N),
                            by=list(surveyid,
                                    dhs_survey_id,
                                    survey_label,
                                    country_name,
                                    year,
                                    clusterid,
                                    hhid,
                                    itn,
                                    paid_type)]

#save this
write.csv(prov_hh_final, file=file.path(parent_dir, "../cleaned_input_data/provenance_by_hh.csv"), 
          row.names = F)


# how many hh's have both paid and free nets?
prov_hh_final[, type_count:=.N, by=list(surveyid,
                                       dhs_survey_id,
                                       survey_label,
                                       country_name,
                                       year,
                                       clusterid,
                                       hhid,
                                       itn)]

