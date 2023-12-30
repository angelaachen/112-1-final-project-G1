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
class(data$sum_of_txn_times)

#sum up交易次數by on/off stop----
sum_of_txn_times_numbered_on_stop <- 
  data |> dplyr::group_by(on_stop) |>
  dplyr::summarise(
    sum_of_txn_times = sum(sum_of_txn_times)
  )  |> arrange(desc(sum_of_txn_times))

sum_of_txn_times_numbered_off_stop <- 
  data |> dplyr::group_by(off_stop) |>
  dplyr::summarise(
    sum_of_txn_times = sum(sum_of_txn_times)
  )  |> arrange(desc(sum_of_txn_times))

#sum up交易次數by district_origin/district_destination----
sum_of_txn_times_numbered_district_origin <- 
  data |> dplyr::group_by(district_origin) |>
  dplyr::summarise(
    sum_of_txn_times = sum(sum_of_txn_times)
  )  |> arrange(desc(sum_of_txn_times))

sum_of_txn_times_numbered_district_destination <- 
  data |> dplyr::group_by(district_destination) |>
  dplyr::summarise(
    sum_of_txn_times = sum(sum_of_txn_times)
  )  |> arrange(desc(sum_of_txn_times))

#加總----
SDS <- sum(sum_of_txn_times_numbered_district_destination$sum_of_txn_times)
SOS <- sum(sum_of_txn_times_numbered_district_origin$sum_of_txn_times)

#前十站點借還比例----
站點還前十加總<-sum_of_txn_times_numbered_off_stop$sum_of_txn_times[c(1,2,3,4,5,6,7,8,9,10)] |> sum()
站點還前十比例<-站點還前十加總/SDS
站點借前十加總<-sum_of_txn_times_numbered_on_stop$sum_of_txn_times[c(1,2,3,4,5,6,7,8,9,10)]|> sum()
站點借前十比例<-站點借前十加總/SOS

#後十站點借還比例----
站點還後十加總<-sum_of_txn_times_numbered_off_stop$sum_of_txn_times[c(2155,2156,2157,2158,2159,2160,2161,2162,2163,2164)]|>sum()
站點還後十比例<-站點還後十加總/SDS
站點借後十加總<-sum_of_txn_times_numbered_on_stop$sum_of_txn_times[c(1280,1281,1282,1283,1284,1285,1286,1287,1288,1289)]|>sum()
站點借後十比例<-站點借後十加總/SOS

#行政區比例----
借車最多之行政區 <- sum_of_txn_times_numbered_district_origin$district_origin[[1]]
還車最多之行政區 <- sum_of_txn_times_numbered_district_destination$district_destination[[1]]
借車最多之行政區之比例 <- sum_of_txn_times_numbered_district_origin$sum_of_txn_times[[1]]/SOS
還車最多之行政區之比例 <- sum_of_txn_times_numbered_district_destination$sum_of_txn_times[[1]]/SDS

#大安區的借車去哪裡----
#抓出所需資料
大安區資料 <- subset(data_arranged, district_origin == "大安區")

#改變屬性
factor(data_arranged$district_origin)
class(data_arranged$district_origin)
大安區資料$sum_of_txn_times <- as.numeric(大安區資料$sum_of_txn_times)
class(大安區資料$sum_of_txn_times)

#新建表格
sum_of_txn_times_numbered_district_destination_大安區<- 
  大安區資料 |> dplyr::group_by(off_stop) |>
  dplyr::summarise(
    sum_of_txn_times = sum(sum_of_txn_times)
  )  |> arrange(desc(sum_of_txn_times))
#檢查
sum(大安區資料$sum_of_txn_times)
sum(sum_of_txn_times_numbered_district_destination_大安區$sum_of_txn_times)
#第一名占比
在大安區借車的還車還在哪最多 <- sum_of_txn_times_numbered_district_destination_大安區$sum_of_txn_times[[1]] / sum(sum_of_txn_times_numbered_district_destination_大安區$sum_of_txn_times)
