merge_and_index = function(ts, only_cl = TRUE) {
  # merge list of xts objects and index them to 100
  # <ts> is a list of xts objects
  # only the product of an inner join between the arrays will remain
  
  if (!is.list(ts)) {
    stop("<ts> must be a list")
  }
  
  if (length(ts) == 1) {
    if (only_cl) { 
      return(Cl(ts[[1]]))
    } else {
      return(ts[[1]])
    }
  }
  
  if (only_cl) {
    for (i in seq_along(ts)) {
      ts[[i]] = Cl(ts[[i]])
    }
  }
  
  merged = ts[[1]]
  for (i in 2:length(ts)) {
    merged = merge(merged, ts[[i]], join = "inner")
  }
  
  for (i in 1:length(colnames(merged))) {
    merged[, i] = merged[, i] / as.numeric(merged[1, i]) * 100
  }
  
  colnames(merged) = sub(".Close", "", colnames(merged))
  
  merged
}

amplify = function(ts, magnitude=1, only_cl = TRUE) {
  # amplify stock movements of a one-column time series dataset
  if (only_cl) { ts = Cl(ts) }
  
  amp_pct_diff = as.numeric(diff(ts[, 1]) / ts[, 1])
  m_vec = vapply(amp_pct_diff, FUN = function(x) { ifelse(x > 0, magnitude**(1-x), magnitude**(1+x)) }, FUN.VALUE = numeric(1))
  amp_pct_diff = (amp_pct_diff * m_vec)
  amp_pct_diff[1] = 1
  # amplify differences only if number is positive
  
  cumsum(amp_pct_diff * ts[, 1])
}

