# Analyze by year. 

library("data.table")
library("lubridate")
library("magrittr")
library("testthat")


# Function to compare a single year in a database for a condition to all other years. 
compareYearCondition <- function(dat, sn, year, cid){
  # Subset to the database, year, and concept_id. 
  dat <- copy(dat)  # do not modify the original dat
  dat_year <- subset(dat, source_name==sn & 
                       year(time_period)==year &
                       concept_id==cid)
  dat_control <- subset(dat, source_name==sn & 
                          year(time_period) != year &
                          concept_id==cid)
  dat_control_mean <- summarySE(dat_control, measurevar="prevalence", groupvars="month") %>% 
    data.table()
  dat_control_mean[ , upper := prevalence + se]
  dat_control_mean[ , lower := prevalence - se]
  browser()
  return(list(dat_year=dat_year, dat_control_mean=dat_control_mean))
}

# Function to compute flags for being "anomalous". 
computeFlags <- function(dat_year, dat_control_mean){
  dat <- merge(res$dat_year, res$dat_control_mean, by="month")
  setnames(dat, "prevalence.x", "prevalence_year")
  setnames(dat, "prevalence.y", "prevalence_control")
  dat[ , flag := prevalence_year > prevalence_control + upper | 
         prevalence_year < prevalence_control - lower]
  
}

plotComparison <- function(res){
  g <- ggplot(res, aes(x=month, y=prevalence_year)) + 
    geom_point(aes(color=flag)) + 
    geom_line() + 
    scale_colour_manual(values=c("black", "red")) + 
    geom_point(data=res, aes(x=month, y=prevalence_control)) +
    geom_line(data=res, aes(x=month, y=prevalence_control)) + 
    geom_line(data=res, aes(x=month, y=prevalence_control + upper), linetype=9) +
    geom_line(data=res, aes(x=month, y=prevalence_control - lower), linetype=9)
  browser()
  return(g)
}

plotCondition <- function(cid){
  g <- ggplot(subset(dat, concept_id==cid), 
              aes(x=time_period , y=prevalence, colour=source_name)) + 
    geom_line(aes(group=1)) + 
    geom_point() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    labs(title="Influenza by Database")
  return(g)
}

runComparison <- function(dat, sn, year, cid){
  if (!is.null(cid) & is.null(sn) & is.null(year)){
    g <- plotCondition(cid)
  } else if (!is.null(cid) & !is.null(sn) & !is.null(year)){
    res <- compareYearCondition(dat, sn, year, cid)
    res <- computeFlags(res$dat_year, res$dat_control_mean)
    g <- plotComparison(res)
  }
  return(g)
}


runComparison(dat, "JMDC", 2009, 312664)


