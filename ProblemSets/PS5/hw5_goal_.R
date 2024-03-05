library(tidyverse)
library(rvest)

goal <- read_html("https://en.wikipedia.org/wiki/List_of_footballers_with_100_or_more_Premier_League_goals")

goal_table <- goal %>%
  html_element("#mw-content-text > div.mw-content-ltr.mw-parser-output > table") %>%
  html_table()


goal_data <- goal_table[[1]]

head(goal_data)

write.csv(goal_data, "goal_data.csv")

goal_table
write.csv(goal_table,file="goal_2_data.csv")







library(rvest)

response <- httr::GET("https://www.google.com/finance/quote/.INX:INDEXSP")

html_content <- httr::content(response, "text", encoding = "UTF-8")
SP500 <- read_html(html_content)


Current_Index <- SP500 %>%
  html_node(".YMlKec.fxKbKc") %>%
  html_text()

SP500_data <- data.frame(Current_Index_0305 = Current_Index)

print(SP500_data)




