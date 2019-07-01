libs = c("quantmod", "astsa", "dplyr", "ggplot2")
sapply(libs, require, character.only = TRUE)

extend_with_prediction = function(ts, extend_by=1, freq="days", model_order = c(0, 0, 0)) {
  # create model from <ts> and extend data with its prediction
  model = arima(ts, order = model_order)
  new_index = seq(as.Date(index(tail(ts, n=1))), length=extend_by, by=freq)
  
  predictions = predict(model, n.ahead=extend_by) %>% as.data.frame() %>% xts(order.by = new_index)
  
  list(original = ts, predictions = predictions, model = model)
}

extended = extend_with_prediction(UNEMPLOY, extend_by = 90, freq = "months", model_order = c(2, 0, 5))

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

plot_extended(extended_ts = extended)

ndiff = function(df, n = 1) {
  # apply diff <n> times to <df>
  
  while (n > 0) {
    df = diff(df)
    n = n - 1
  }
  
  df
}
