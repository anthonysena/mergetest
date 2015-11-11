# Detects whether the functional form of an annual time series is primarily seasonal, trend or remainder.

# source("C:/Users/pardre1/Documents/vdw/dqcdm-temporal/src/R/detect_form.R")

fit_models <- function(dat) {
  # fit a series of simple lms for each concept_name, return the RMSE for each one, and pick a winner.
  concepts <- as.data.frame(table(dat$concept_name, dat$source))
  concepts$iqrr_seasonal <- NA
  concepts$iqrr_trend <- NA
  concepts$iqrr_remainder <- NA
  concepts$winner <- '?'

  names(concepts) <- c('concept', 'source', 'freq', 'iqrr_seasonal', 'iqrr_trend', 'iqrr_remainder', 'winner')
  for (con in as.data.frame(table(concepts$concept))[, 1]) {
    for (src in as.data.frame(table(concepts$source))[, 1]) {
      print(paste("working on concept", con, "for source", src))
      # subset of the input data for this source & concept
      subs <- dat[dat$concept_name == con & dat$source == src,]
      # print(summary(subs))
      # print(dim(subs))

      # Do we have any actual rows?
      if (dim(subs)[1] > 0) {
        dt_min <- min(subs$time_period)
        dt_max <- max(subs$time_period)

        tsas <- ts(subs$prevalence, start=c(year(dt_min), month(dt_min)), end=c(year(dt_max), month(dt_max)), frequency = 12)
        # Gotta have more than 2 full cycles (years) or else stl() will bark at us.
        if (length(tsas) > 24) {
          # print(tsas)
          stl <- stl(tsas, s.window="periodic")

          conrows <- (concepts$concept == con & concepts$source == src)
          # See https://stats.stackexchange.com/questions/82048/how-to-interpret-r-stl-output
          concepts[conrows,]$iqrr_seasonal   <- round(IQR(stl$time.series[, "seasonal" ]) / IQR(tsas) * 100, 1)
          concepts[conrows,]$iqrr_trend      <- round(IQR(stl$time.series[, "trend"    ]) / IQR(tsas) * 100, 1)
          concepts[conrows,]$iqrr_remainder  <- round(IQR(stl$time.series[, "remainder"]) / IQR(tsas) * 100, 1)

          # there has got to be a better way...
          best <- max(concepts[conrows, c('iqrr_seasonal', 'iqrr_trend', 'iqrr_remainder')])
          if (best == concepts[conrows, c('iqrr_seasonal'  )]) {concepts[conrows, ]$winner <- 'seasonal'}
          if (best == concepts[conrows, c('iqrr_trend'     )]) {concepts[conrows, ]$winner <- 'trend'}
          if (best == concepts[conrows, c('iqrr_remainder' )]) {concepts[conrows, ]$winner <- 'remainder'}
        }
      }
    }
  }
  return(concepts)
}

