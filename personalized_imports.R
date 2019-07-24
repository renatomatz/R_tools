investing.com.clean = function(dir, name, ext = ".csv") {
  dir = paste0(dir, "/", name, ext)
  
  data = read.csv(dir)
  data$Date = as.Date(data$Date, "%b %d, %Y")
  data$Close = data$Price
  data$Price = NULL
  
  data = xts(data[, -c(which(colnames(data) %in% c("Date", "Change..", "Vol.")))], order.by = data$Date)
  
  colnames(data) = paste0(name, ".", colnames(data))
  
  data
}


B3.clean = function(dir, name, ext = ".xlsx") {
  dir = paste0(dir, "/", name, ext)
  
  data = read_excel(dir)
  data = data[, c("Data", "Volume", "Média")]
  
  data = xts(data[, -c(which(colnames(data) == "Data"))], order.by = data$Data)
  colnames(data) = paste0(name, ".", colnames(data))
  
  data
}