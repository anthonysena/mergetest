library("data.table")
library("lubridate")
library("magrittr")

# Read in the data.
dat <- fread("../../data/dqcdm-temporal-summary/dqcdm_temporal_summary_subset_2.txt")
analyses <- fread("../../data/dqcdm-temporal-summary/dqcdm_analyses.csv")

# Light munging. 
dat[ , prevalence := as.double(prevalence)]
dat[ , time_period := paste0(time_period, "01")]
dat[ , time_period := ymd(time_period)]
dat[ , month := month(time_period)]
dat[ , year := year(time_period)]


# Exploratory data analysis.
# ggplot(subset(dat, concept_id==312664), aes(x=time_period , y=prevalence)) +
#   geom_line(aes(group=1)) +
#   geom_point() +
#   facet_wrap(~ source_name) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   labs(title="Influenza by Database")
#
# ggplot(subset(dat, concept_id==312664),
#        aes(x=time_period , y=prevalence, colour=source_name)) +
#   geom_line(aes(group=1)) +
#   geom_point() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   labs(title="Influenza by Database")
