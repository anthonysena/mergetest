# Scrath work. 

library("data.table")
library("magrittr")

dimset <- fread("data/CSV/synpuf_dq/dimension_set.csv")
measure <- fread("data/CSV/synpuf_dq/measure.csv")
result <- fread("data/CSV/synpuf_dq/result.csv")


# Merge the data together. 
dat <- merge(dimset, result, by="set_id")
dat <- merge(dat, measure, by="measure_id")

# Exploratory analysis. 
dat[ , .N, by=.(name, description, measure_id)]

# Dim_set_set in Sigfried's terminology. 
dat[ , .(dim_name_1, dim_name_2, dim_name_3, dim_name_4, dim_name_4, dim_name_4, dim_name_6)] %>% 
  unique()
