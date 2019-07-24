library(Quandl)

bond_price = function(fv, r, ttm, y) {
  # if y is a character, it should refer to the bond's Moody's rating
  if (is.character(y)) {
    y = get_moodys_yield(y)
  }
  
  cash_flow = c(rep(fv*r, ttm-1), fv*(1+r))
  
  sum(cash_flow / (1 + y)^seq(1, length(cash_flow), 1))
}

duration = function(fv, r, ttm, y, y_ch) {
  # using approximate duration function
  # if y is a character, it should refer to the bond's Moody's rating
  if (is.character(y)) {
    y = get_moodys_yield(y)
  }
  
  price = c(curr = bond_price(fv, r, ttm, y), 
            up = bond_price(fv, r, ttm, y + y_ch),
            down = bond_price(fv, r, ttm, y - y_ch))
  
  (price$down - price$up) / (2 * price$curr * y_ch)
}

convexity = function(fv, r, ttm, y, y_ch) {
  # using approximate complexity function
  # if y is a character, it should refer to the bond's Moody's rating
  
  if (is.character(y)) {
    y = get_moodys_yield(y)
  }
  
  price = c(curr = bond_price(fv, r, ttm, y), 
            up = bond_price(fv, r, ttm, y + y_ch),
            down = bond_price(fv, r, ttm, y - y_ch))
  
  (price$up + price$down - 2*price$curr)/(price$curr * y_ch^2)
}

yield_change = function(fv, r, ttm, y, y_ch) {
  # Price change from a yield change
  dur_p_ch = -duration(fv, r, ttm, y) * y_ch
  cnv_p_ch = 0.5 * convexity(fv, r, ttm, y) * y_ch^2
  
  (dur_p_ch + cnv_p_ch) * bond_price(fv, r, ttm, y)
}

get_moodys_yield = function(rating) {
  y = Quandl(paste("FED/RIMLP", m_rating, "R_N_M", sep = ""),
             start_date=Sys.Date(), end_date=Sys.Date())
  
  as.numeric(y$Value) / 100
}

log_return = function(ts) { diff(log(ts))[-1] }