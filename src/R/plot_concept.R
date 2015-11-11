# Given a concept_id, do a cross-db plot of prevalence
# source("C:/Users/pardre1/Documents/vdw/dq/r/plot_concept.R")
# 201826 is DM2
plot_db_concept <- function(dat, concept_id) {
  sub <- dat[dat$concept_id == concept_id,]
  concept_name <- sub[1,]$concept_name
  p <- ggplot(sub, aes(x = time_period, y = prevalence))
  p + geom_point() + facet_grid(. ~ source_name) + ggtitle(concept_name)
}
