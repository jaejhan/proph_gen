# Functions used in ProphGen app

# df_for_plotting ---------------------------------------------------------
df_for_plotting <- function(m, fcst) {
  # Make sure there is no y in fcst
  fcst$y <- NULL
  df <- m$history %>%
    dplyr::select(ds, y) %>%
    dplyr::full_join(fcst, by = "ds") %>%
    dplyr::arrange(ds)
  return(df)
}


# plot_trend --------------------------------------------------------------
plot_trend <- function(df, uncertainty = TRUE, plot_cap = TRUE) {
  df.t <- df[!is.na(df$trend),]
  gg.trend <- ggplot2::ggplot(df.t, ggplot2::aes(x = ds, y = trend)) +
    ggplot2::geom_line(color = "#0072B2", na.rm = TRUE)
  if (exists('cap', where = df.t) && plot_cap) {
    gg.trend <- gg.trend + ggplot2::geom_line(ggplot2::aes(y = cap),
                                              linetype = 'dashed',
                                              na.rm = TRUE)
  }
  if (uncertainty) {
    gg.trend <- gg.trend +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = trend_lower,
                                        ymax = trend_upper),
                           alpha = 0.2,
                           fill = "#0072B2",
                           na.rm = TRUE)
  }
  return(gg.trend)
}


# plot_yearly -------------------------------------------------------------
plot_yearly <- function(m, uncertainty = TRUE, yearly_start = 0) {
  # Compute yearly seasonality for a Jan 1 - Dec 31 sequence of dates.
  df.y <- data.frame(
    ds=seq.Date(zoo::as.Date('2017-01-01'), by='d', length.out=365) +
      yearly_start, cap=1.)
  df.y <- setup_dataframe(m, df.y)$df
  seas <- predict_seasonal_components(m, df.y)
  seas$ds <- df.y$ds
  
  gg.yearly <- ggplot2::ggplot(seas, ggplot2::aes(x = ds, y = yearly,
                                                  group = 1)) +
    ggplot2::geom_line(color = "#0072B2", na.rm = TRUE) +
    ggplot2::scale_x_date(labels = scales::date_format('%B %d')) +
    ggplot2::labs(x = "Day of year")
  if (uncertainty) {
    gg.yearly <- gg.yearly +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = yearly_lower,
                                        ymax = yearly_upper),
                           alpha = 0.2,
                           fill = "#0072B2",
                           na.rm = TRUE)
  }
  return(gg.yearly)
}


# plot_weekly -------------------------------------------------------------
plot_weekly <- function(m, uncertainty = TRUE, weekly_start = 0) {
  # Compute weekly seasonality for a Sun-Sat sequence of dates.
  df.w <- data.frame(
    ds=seq.Date(zoo::as.Date('2017-01-01'), by='d', length.out=7) +
      weekly_start, cap=1.)
  df.w <- setup_dataframe(m, df.w)$df
  seas <- predict_seasonal_components(m, df.w)
  seas$dow <- factor(weekdays(df.w$ds), levels=weekdays(df.w$ds))
  
  gg.weekly <- ggplot2::ggplot(seas, ggplot2::aes(x = dow, y = weekly,
                                                  group = 1)) +
    ggplot2::geom_line(color = "#0072B2", na.rm = TRUE) +
    ggplot2::labs(x = "Day of week")
  if (uncertainty) {
    gg.weekly <- gg.weekly +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = weekly_lower,
                                        ymax = weekly_upper),
                           alpha = 0.2,
                           fill = "#0072B2",
                           na.rm = TRUE)
  }
  return(gg.weekly)
}


# plot_holidays -----------------------------------------------------------
plot_holidays <- function(m, df, uncertainty = TRUE) {
  holiday.comps <- unique(m$holidays$holiday) %>% as.character()
  df.s <- data.frame(ds = df$ds,
                     holidays = rowSums(df[, holiday.comps, drop = FALSE]),
                     holidays_lower = rowSums(df[, paste0(holiday.comps,
                                                          "_lower"), drop = FALSE]),
                     holidays_upper = rowSums(df[, paste0(holiday.comps,
                                                          "_upper"), drop = FALSE]))
  df.s <- df.s[!is.na(df.s$holidays),]
  # NOTE the above CI calculation is incorrect if holidays overlap in time.
  # Since it is just for the visualization we will not worry about it now.
  gg.holidays <- ggplot2::ggplot(df.s, ggplot2::aes(x = ds, y = holidays)) +
    ggplot2::geom_line(color = "#0072B2", na.rm = TRUE)
  if (uncertainty) {
    gg.holidays <- gg.holidays +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = holidays_lower,
                                        ymax = holidays_upper),
                           alpha = 0.2,
                           fill = "#0072B2",
                           na.rm = TRUE)
  }
  return(gg.holidays)
}


# setup_dataframe ---------------------------------------------------------
setup_dataframe <- function(m, df, initialize_scales = FALSE) {
  if (exists('y', where=df)) {
    df$y <- as.numeric(df$y)
  }
  df$ds <- zoo::as.Date(df$ds)
  if (anyNA(df$ds)) {
    stop('Unable to parse date format in column ds. Convert to date format.')
  }
  
  df <- df %>%
    dplyr::arrange(ds)
  
  if (initialize_scales) {
    m$y.scale <- max(abs(df$y))
    m$start <- min(df$ds)
    m$t.scale <- as.numeric(max(df$ds) - m$start)
  }
  
  df$t <- as.numeric(df$ds - m$start) / m$t.scale
  if (exists('y', where=df)) {
    df$y_scaled <- df$y / m$y.scale
  }
  
  if (m$growth == 'logistic') {
    if (!(exists('cap', where=df))) {
      stop('Capacities must be supplied for logistic growth.')
    }
    df <- df %>%
      dplyr::mutate(cap_scaled = cap / m$y.scale)
  }
  return(list("m" = m, "df" = df))
}



# predict_seasonal_components ---------------------------------------------
predict_seasonal_components <- function(m, df) {
  seasonal.features <- make_all_seasonality_features(m, df)
  lower.p <- (1 - m$interval.width)/2
  upper.p <- (1 + m$interval.width)/2
  
  # Broken down into components
  components <- dplyr::data_frame(component = colnames(seasonal.features)) %>%
    dplyr::mutate(col = 1:n()) %>%
    tidyr::separate(component, c('component', 'part'), sep = "_delim_",
                    extra = "merge", fill = "right") %>%
    dplyr::filter(component != 'zeros')
  
  if (nrow(components) > 0) {
    component.predictions <- components %>%
      dplyr::group_by(component) %>% dplyr::do({
        comp <- (as.matrix(seasonal.features[, .$col])
                 %*% t(m$params$beta[, .$col, drop = FALSE])) * m$y.scale
        dplyr::data_frame(ix = 1:nrow(seasonal.features),
                          mean = rowMeans(comp, na.rm = TRUE),
                          lower = apply(comp, 1, stats::quantile, lower.p,
                                        na.rm = TRUE),
                          upper = apply(comp, 1, stats::quantile, upper.p,
                                        na.rm = TRUE))
      }) %>%
      tidyr::gather(stat, value, mean, lower, upper) %>%
      dplyr::mutate(stat = ifelse(stat == 'mean', '', paste0('_', stat))) %>%
      tidyr::unite(component, component, stat, sep="") %>%
      tidyr::spread(component, value) %>%
      dplyr::select(-ix)
    
    component.predictions$seasonal <- rowSums(
      component.predictions[unique(components$component)])
  } else {
    component.predictions <- data.frame(seasonal = rep(0, nrow(df)))
  }
  return(component.predictions)
}


# make_all_seasonality_features -------------------------------------------
make_all_seasonality_features <- function(m, df) {
  seasonal.features <- data.frame(zeros = rep(0, nrow(df)))
  if (m$yearly.seasonality) {
    seasonal.features <- cbind(
      seasonal.features,
      make_seasonality_features(df$ds, 365.25, 10, 'yearly'))
  }
  if (m$weekly.seasonality) {
    seasonal.features <- cbind(
      seasonal.features,
      make_seasonality_features(df$ds, 7, 3, 'weekly'))
  }
  if(!is.null(m$holidays)) {
    # A smaller prior scale will shrink holiday estimates more than seasonality
    scale.ratio <- m$holidays.prior.scale / m$seasonality.prior.scale
    seasonal.features <- cbind(
      seasonal.features,
      make_holiday_features(m, df$ds))
  }
  return(seasonal.features)
}


# make_seasonality_features -----------------------------------------------
make_seasonality_features <- function(dates, period, series.order, prefix) {
  features <- fourier_series(dates, period, series.order)
  colnames(features) <- paste(prefix, 1:ncol(features), sep = '_delim_')
  return(data.frame(features))
}


# fourier_series ----------------------------------------------------------
fourier_series <- function(dates, period, series.order) {
  t <- dates - zoo::as.Date('1970-01-01')
  features <- matrix(0, length(t), 2 * series.order)
  for (i in 1:series.order) {
    x <- as.numeric(2 * i * pi * t / period)
    features[, i * 2 - 1] <- sin(x)
    features[, i * 2] <- cos(x)
  }
  return(features)
}


# make_holiday_features ---------------------------------------------------
make_holiday_features <- function(m, dates) {
  scale.ratio <- m$holidays.prior.scale / m$seasonality.prior.scale
  wide <- m$holidays %>%
    dplyr::mutate(ds = zoo::as.Date(ds)) %>%
    dplyr::group_by(holiday, ds) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::do({
      if (exists('lower_window', where = .) && !is.na(.$lower_window)
          && !is.na(.$upper_window)) {
        offsets <- seq(.$lower_window, .$upper_window)
      } else {
        offsets <- c(0)
      }
      names <- paste(
        .$holiday, '_delim_', ifelse(offsets < 0, '-', '+'), abs(offsets), sep = '')
      dplyr::data_frame(ds = .$ds + offsets, holiday = names)
    }) %>%
    dplyr::mutate(x = scale.ratio) %>%
    tidyr::spread(holiday, x, fill = 0)
  
  holiday.mat <- data.frame(ds = dates) %>%
    dplyr::left_join(wide, by = 'ds') %>%
    dplyr::select(-ds)
  
  holiday.mat[is.na(holiday.mat)] <- 0
  return(holiday.mat)
}