#Problem 3
library(rvest)
webpage <- read_html("http://www.espn.com/soccer/blog/the-toe-poke/65/post/2962023/zlatan-ibrahimovic-35-best-quotes-as-manchester-united-release-him")
webpage
results <- webpage %>% html_nodes("strong")
results
xml_contents(results)
library(dplyr)


install.packages("quantmod")
library(quantmod)
getSymbols('TWTR')
head(TWTR)
tail(TWTR)
chartSeries(TWTR)
getSymbols('AMZN')
chartSeries(AMZN)
chartSeries(AMZN, subset = 'last 3 months')
