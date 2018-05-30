#function for convert ts in df
convert_ts_to_df <- function(df){
  if(!is.ts(df)) stop('\"df\" must be class ts', call. = T)
  data.frame(Date = as.numeric(time(df)), value = as.numeric(df))
}
df <- convert_ts_to_df(AirPassengers)
plot(df$Date, df$value, type = 'l', xlab = "Time")

#function for book Advanced R by Hadley
rollmean <- function(x, n) {
  out <- rep(NA, length(x))
  
  offset <- trunc(n / 2)
  for (i in (offset + 1):(length(x) - n + offset + 1)) {
    out[i] <- mean(x[(i - offset):(i + offset - 1)])
  }
  out
}
lines(df$Date, rollmean(df$value, 5), col = 'red')
