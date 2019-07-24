libraries = c("QRM", "ggplot2", "PerformanceAnalytics", "PortfolioAnalytics", "tseries")
lapply(libraries, require, character.only = TRUE)

student_t = function(arr) {
  arr = as.vector(arr)
  
  tfit = fit.st(arr)
  tpars = tfit$par.ests
  
  mu = tpars["mu"]
  nu = tpars["nu"]
  sigma = tpars["sigma"]
  
  tdist = dt((arr - mu)/sigma, df=nu)/sigma
  
  ggplot(mapping = aes(x = arr)) +
    geom_histogram(aes(y = ..density..)) +
    geom_line(aes(y = tdist), col="red")
  
}

rate_to_price = function(x, r_col, period = 365) {
  # Turn a time series of rates located in <r_col> into a relative price with the first value of 100
  # - and compounded by <period>
  
  # Returns the time series with a new $Close column
  
  x$Close = c(100, rep(NA, length(x[, r_col])-1))
  
  for (i in 2:length(x[, r_col])) {
    x[i, "Close"] = x[i-1, "Close"] * (1+(x[i-1, r_col]/100))**(1/period)
  }
  
  x
}

daily_return_growth = function(x) {
  # Uses a time series <x> containing period return data and returns a copy showing the growth of those returns 
  # at a starting value of 100
  
  for (c in 1:ncol(x)) {
    x[1, c] = 100
    for (i in 2:length(x[, c])) {
      x[[i, c]] = x[[i-1, c]] * (1 + x[[i, c]])
    }
  }
  
  x
}

annualized_to_period = function(r, period = 365) {
  (1+r)**(1/period) - 1
}

zip_weights = function(assets, weights) {
  
  if (length(assets) != length(weights)) { stop("<assets> and <weights> do not match length") }
  
  zipped = c()
  
  for (i in seq_along(assets)) {
    zipped[assets[i]] = weights[i]
  }
  
  zipped
}

portfolio_summary = function(prices, weights = NULL, rf = 0.0, hm = list(pm = NULL, reslow = NULL, reshigh = NULL)) {
  # summarize key portfolio statistics for a given set of weights
  # perform Harry Markowitz analysis to decide on portfolio weights if <weights == "hm">
  
  returns = Return.calculate(prices)[-1]
  port = list()
  
  if (is.null(weights)) {
    port$pw = rep(1/ncol(prices), ncol(prices))
  } else if (weights == "hm") {
    if (is.null(hm$pm)) { hm$pm = mean(returns) }
    port = portfolio.optim(returns, rf = rf, pm = hm$pm, reslow = hm$reslow, reshigh = hm$reshigh)
    port$pw = round(port$pw, digits = 4)
  } else if (is.numeric(weights)) {
    port$pw = weights 
  } else {
    stop("Invalid <weights> argument")
  }
  
  port$pw = zip_weights(colnames(prices), port$pw)
   
  port$cor = cor(prices)
  port$returns = returns
  port$px = Return.portfolio(returns, weights = port$pw)
  port$pm = Return.annualized(port$px)
  port$ps = StdDev.annualized(port$px)
  port$sharpe = SharpeRatio.annualized(port$px, Rf = rf)
  port$kurtosis = kurtosis(port$px)[1]
  port$skewness = skewness(port$px)[1]
  port$ES = ES(port$px)
  port$VaR = VaR(port$px)
  port$asset_risk_contrib = StdDev(returns, portfolio_method = "component", weights = port$pw)$pct_contrib_StdDev
  
  port$charts = list()
  
  port$charts$performance = autoplot(all, facets = FALSE) + 
    geom_line(data = daily_return_growth(port$px), aes(x = Index, y = portfolio.returns), size = 1.3, alpha = 0.6)
  
  port$charts$histogram = ggplot(port$px, aes(x = portfolio.returns)) + 
    geom_histogram(col = "white") +
    geom_density(col = "blue") +
    stat_function(fun = dnorm,
                  args = with(hm_default$px, c(mean = mean(portfolio.returns), sd = sd(portfolio.returns))),
                  col = "green")
  
  port$charts$drawdown = ggplot(Drawdowns(port$px), aes(x = Index, y = portfolio.returns)) + geom_bar(stat = "identity")

  port_chart_summary(port)
  
  port
}

port_chart_summary = function(port) {
  # generate a summary of a portfolio sumarry's graphs
  grid.arrange(port$charts$performance + ggtitle(paste(as.character(port$pw), collapse = " ")), 
               port$charts$histogram, 
               port$charts$drawdown, 
               layout_matrix = rbind(c(1, 1), c(2, 3)))
}
