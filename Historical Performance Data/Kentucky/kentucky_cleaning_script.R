library(tidyverse)
library(RSelenium)
library(rvest)
library(webshot)

setwd("C:\\Users\\reb\\OneDrive\ -\ Colostate\\Thesis\\Historical\ Performance\ Data\\Kentucky")

urls <- read.csv("Urls.csv", header = 
                   T)

#rawdata <- lapply(urls[[1]], function(url){
#  l <- read_html(url)
#  data.frame(txt = html_text(html_element(l,"#meetResultsBody"))) |> 
#    separate_rows(txt, sep="\r\n")
#}) |> 
#  bind_rows(.id="df")

url<-"https://ky.milesplit.com/meets/351220-class-aa-khsaa-state-track-meet-2019/results/657016/raw"
#h<-read_html(url)

ptm <- phantom()
rd <- remoteDriver(browserName = 'phantomjs')
rd$open()

rd$navigate(url)
html <- rd$getPageSource()[[1]]

# clean up
rd$close()
ptm$stop()

r <- h |> 
   |>
   ) #|> 
  
