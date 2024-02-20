library(jsonlite)
library(tidyverse)

url <- "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"
dest_file <- "downloaded_file.json"
download.file(url, destfile = dest_file, method="auto")

system('linux shell command')
getwd()
download.file(url, destfile = dest_file, method ="auto")


mylist <- fromJSON("dates.json")
mydf <- bind_rows(mylist$result[-1])
class(mydf$date)
[1] "character"
n<-3
head(mydf,n)
# A tibble: 3 × 6
date  description                                                                  lang  categ…¹ categ…² granu…³
<chr> <chr>                                                                        <chr> <chr>   <chr>   <chr>  
  1 1     Tiberius, under order of Augustus, quells revolts in Germania (1–5).         en    By pla… Roman … year   
2 1     Gaius Caesar and Lucius Aemilius Paullus are appointed consuls.              en    By pla… Roman … year   
3 1     Gaius Caesar marries Livilla, daughter of Antonia Minor and Nero Claudius D… en    By pla… Roman … year   
# … with abbreviated variable names ¹​category1, ²​category2, ³​granularity