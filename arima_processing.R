libs = c("quantmod", "astsa", "dplyr", "ggplot2", "forecast", "PerformanceAnalytics")
sapply(libs, require, character.only = TRUE)

extend_with_prediction = function(ts, extend_by=1, freq="days", model_order = c(0, 0, 0), seasonal_order = c(0, 0, 0), seasonal_period = NA) {
  # create model from <ts> and extend data with its prediction
  model = arima(ts, order = model_order, seasonal = list(order = seasonal_order, period = seasonal_period))
  new_index = seq(as.Date(index(tail(ts, n=1))), length=extend_by, by=freq)
  
  predictions = predict(model, n.ahead=extend_by) %>% as.data.frame() %>% xts(order.by = new_index)
  
  list(original = ts, predictions = predictions, model = model)
}

# extended = extend_with_prediction(UNEMPLOY, extend_by = 90, freq = "months", model_order = c(2, 0, 5))

plot_extended = function(extended_ts) {
  # plot the output of a model and predictions
  # <extended_ts> should be a list with $original and $predictions attributes
  original_dates = index(extended_ts$original)
  prediction_dates = index(extended_ts$predictions)
  
  extended_ts$predictions = as.data.frame(extended_ts$predictions)
  extended_ts$original = as.data.frame(extended_ts$original)
  
  colnames(extended_ts$original) = "price"
  colnames(extended_ts$predictions)[colnames(extended_ts$predictions) == "pred"] = "price"
  
  extended_ts$predictions$date = prediction_dates
  extended_ts$original$date = original_dates
  
  ggplot(NULL, aes(date, price)) +
    geom_line(data = extended_ts$original) +
    geom_ribbon(data = extended_ts$predictions, aes(ymin = price - se, ymax = price + se), fill = "red", alpha = 0.3) +
    geom_line(data = extended_ts$predictions, aes(col = se))
}

# plot_extended(extended_ts = extended)

ndiff = function(ts, n = 1) {
  # apply diff <n> times to <df>
  
  while (n > 0) {
    df = diff(df)
    n = n - 1
  }
  
  df
}

grid_eval = function(ts, p = c(0, 0), d = c(0, 0), q = c(0, 0)) {
  # fit ts to various models and create a grid with their results and return it with the coordinates for the best
  
  grid = rep(list(rep(list(rep(list(0), q+1)), d+1)), p+1)
  best = list(model = NULL, result = Inf, order = NULL)
  
  curr_p = p[1]
  curr_d = d[1]
  curr_q = q[1]
  
  repeat {
    repeat {
      repeat {
        model = arima(ts, order = c(curr_p, curr_d, curr_q))
        result = mean(BIC(model), AIC(model))
        
        if (result < best$result) {
          best$model = model
          best$result = result
          best$order = c(curr_p, curr_d, curr_q)
        }
        
        grid[[curr_p+1]][[curr_d+1]][[curr_q+1]]  = result
        
        if (curr_q == q[2]) { break }
        curr_q = curr_q + 1
      }
      if (curr_d == d[2]) { break }
      curr_d = curr_d + 1
    }
    if (curr_p == p[2]) { break }
    curr_p = curr_p + 1
  }
  
  list(grid = grid, best = best)
}

# res = grid_eval(UNEMPLOY, p = 4, d = 4, q = 4)

xts_to_ts = function(xts, base_u = "years", freq = "months") {
  # turns an xts time series into ts given a periodicity
  
  form_b = str_to_form(base_u)
  form_f = str_to_form(freq)
  
  ndays_b = str_to_ndays(base_u)
  ndays_f = str_to_ndays(freq)
  
  if (is.null(form_b) | is.null(form_f)) {
    errorCondition("freq and base_u must be either days, weeks months or Years") 
    }
  
  ts(as.vector(xts),
     start = extract_time(xts, start, c(form_b, form_f)),
     end = extract_time(xts, end, c(form_b, form_f)),
     frequency = ceiling(ndays_b / ndays_f))
}

extract_time = function(xts, fun, forms) {
  c(as.numeric(strftime(fun(xts), format = forms[1])), as.numeric(strftime(fun(xts), format = forms[2])))
}

str_to_form = function(str) {
  ifelse(str == "days", "%j", 
         ifelse(str == "weeks", "%w", 
                ifelse(str == "months", "%m", 
                       ifelse(str == "years", "%Y", NULL))))
}

str_to_ndays = function(str) {
  ifelse(str == "days", 1, 
         ifelse(str == "weeks", 7, 
                ifelse(str == "months", 31, 
                       ifelse(str == "years", 366, NULL))))
}

u = xts_to_ts(UNEMPLOY)

ts_overview = function(ts, h = 1, thorough = FALSE) {
  lambda = BoxCox.lambda(ts)
  
  fit = auto.arima(ts, lambda = lambda, stepwise = !thorough)
  
  checkresiduals(fit)
  forecast(fit) %>% autoplot()
}

harmonic_reg = function(ts, K = 1, h = 1) {
  harmonic1 = fourier(ts, K = K)
  harmonic2 = fourier(ts, K = K, h = h)
  
  model = auto.arima(ts, xreg = harmonic1, seasonal = FALSE)
  
  checkresiduals(model)
  forecast(model, xreg = harmonic2) %>% autoplot()
  
  model
}