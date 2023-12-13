#前置作業----
library(readr)
library(dplyr)
data <- jsonlite::fromJSON("data.json")

#命名Values----
on_stop <- data$on_stop
off_stop <- data$off_stop
sum_of_txn_times <- data$sum_of_txn_times
data_arranged<- dplyr::arrange(data,"on_stop")

#改變data$sum_of_txn_times屬性----
data$sum_of_txn_times <- as.numeric(data$sum_of_txn_times)

#sum up交易次數by on/off stop----
sum_of_txn_times_numbered_on_stop <- 
  data |> dplyr::group_by(on_stop) |>
  dplyr::summarise(
sum_of_txn_times = sum(sum_of_txn_times)
  ) 

sum_of_txn_times_numbered_off_stop <- 
  data |> dplyr::group_by(off_stop) |>
  dplyr::summarise(
    sum_of_txn_times = sum(sum_of_txn_times)
  ) 

#gg----



