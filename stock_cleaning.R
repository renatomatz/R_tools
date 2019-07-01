merge_and_index = function(ts) {
  # merge an array of time series and index them to 100
  # <ts> is a list of time series objects
  # only the product of an inner join between the arrays will remain
  
  # merged = ts[[1]]
  # for (i in 2:length(ts)) {
  #   merged = merge(merged, ts[[i]], join = "inner")
  # }
  
  merged = do.call(merge, ts, join = "inner")
  
  for (i in 1:length(colnames(merged))) {
    merged[, i] = merged[, i] / as.numeric(merged[1, i]) * 100
  }
  
  merged
}

amplify = function(ts, magnitude=1) {
  # amplify stock movements of a one-column time series dataset
  
  amp_pct_diff = (diff(ts[, 1]) / ts[, 1] * magnitude) + 1
  amplified = as.numeric(ts[1, 1])
  
  for (i in 2:length(ts)) {
    amplified = c(amplified, (tail(amplified, n = 1) * as.numeric(amp_pct_diff[i])))
  }
  
  xts(amplified, order.by = index(ts))
}

