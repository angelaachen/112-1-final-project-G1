library(readr)
data <- jsonlite::fromJSON("data.json")
class(data)
on_stop_id <- data$on_stop_id
class(on_stop_id)
table(on_stop_id)
data$on_stop_id[[527]]
library(dplyr)