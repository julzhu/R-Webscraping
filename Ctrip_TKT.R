library(RSelenium)
library(rvest)
library(stringr)
library(openxlsx)

remDr <- remoteDriver(remoteServerAddr = "127.0.0.1" ,
                      port = 4444,
                      browserName = "chrome"
)
remDr$open()
remDr$maxWindowSize()

wb <- createWorkbook()

Departure <- "SHA"
Destination <- "BJS"
Date <- Sys.Date()

DateList <- seq(from=Date, to=Date+5, by=1)

url <- paste("https://flights.ctrip.com/online/list/oneway-", Departure, "-", Destination, "?_=1&depdate=", Date, sep = "")

remDr$navigate(url)

Sys.sleep(3) #delay 3 sec

try(
  {
    Alert <- remDr$findElement("xpath", '//*[@id="outerContainer"]/div/div[3]/div/button')
    Alert$clickElement()
    #close alert box
  }, silent = TRUE
)  

try(
  {
    FlightFilter <- remDr$findElement("xpath",'//*[@id="hp_container"]/div[1]/div/div[3]/div[2]/div/ul[1]/li[1]/div/span/i')
    FlightFilter$clickElement()
  }, silent = TRUE
)   #test if checkbox


###################################################select only direct flights####################################################


remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
Sys.sleep(3)
remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
Sys.sleep(3)
#scroll down
#can also use ###remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")

FlightList <- remDr$findElements("class",'flight-box')

FlightDataFunc <- function(FlightList)lapply(FlightList,function(i){
  
  doc <- i$getElementAttribute("outerHTML")[[1]]%>%
    read_html()
  #read web script
  
  name <- doc%>%
    html_nodes("div.airline-name")%>%
    html_text()
  #read airline name
  
  plane <- doc%>%
    html_nodes("span.plane-No")%>%
    html_text()
  #read plane info
  
  depart_time <- doc%>%
    html_nodes("div.depart-box")%>%
    html_nodes("div.time")%>%
    html_text()
  #read depart time
  
  depart_airport <- doc%>%
    html_nodes("div.depart-box")%>%
    html_nodes("div.airport")%>%
    html_text()
  #read depart airport
  
  arrive_time <- doc%>%
    html_nodes("div.arrive-box")%>%
    html_nodes("div.time")%>%
    html_text()
  #read depart time
  
  arrive_airport <- doc%>%
    html_nodes("div.arrive-box")%>%
    html_nodes("div.airport")%>%
    html_text()
  #read arrive airport  
  
  price <- doc%>%
    html_nodes("span.price")%>%
    html_text()
  #read price 
  
  name <- ifelse(identical(name,character(0)),NA,name)
  plane <- ifelse(identical(plane,character(0)),NA,plane)
  depart_time <- ifelse(identical(depart_time,character(0)),NA,depart_time)
  depart_airport <- ifelse(identical(depart_airport,character(0)),NA,depart_airport)
  arrive_time <- ifelse(identical(arrive_time,character(0)),NA,arrive_time)
  arrive_airport <- ifelse(identical(arrive_airport,character(0)),NA,arrive_airport)
  price <- ifelse(identical(price,character(0)),NA,price)
  
  return(c(Date, name, plane, depart_time, depart_airport, arrive_time, arrive_airport, price))
})

FlightData <- FlightDataFunc(FlightList)


##########################################################scrape next 5 days#######################################################


for (i in 2:6){
  
  url <- paste("https://flights.ctrip.com/online/list/oneway-", Departure, "-", Destination, "?_=1&depdate=", DateList[i], sep = "")
  
  Date <- DateList[i] %>% format("%Y-%m-%d")
  
  remDr$navigate(url)
  
  Sys.sleep(3)
  
  tryNext_1 <- try(
    {
      
      Alert <- remDr$findElement("xpath", '//*[@id="outerContainer"]/div/div[3]/div/button')
      Alert$clickElement()
      
      tryNext_2 <- try(
        {
          FlightFilter <- remDr$findElement("xpath",'//*[@id="hp_container"]/div[1]/div/div[3]/div[2]/div/ul[1]/li[1]/div/span/i')
          FlightFilter$clickElement()
          
          webElem <- remDr$findElement("css", "body")
          webElem$sendKeysToElement(list(key = "end"))
          Sys.sleep(3)
          webElem <- remDr$findElement("css", "body")
          webElem$sendKeysToElement(list(key = "end"))
          Sys.sleep(3)
          #scroll down
          
          FlightList <- remDr$findElements("class",'flight-box')
          FlightData <- c(FlightData, FlightDataFunc(FlightList)) 
          
        }, silent = TRUE
      ) 
      
      if('try-error' %in% class(tryNext_2)) {
      
      webElem <- remDr$findElement("css", "body")
      webElem$sendKeysToElement(list(key = "end"))
      Sys.sleep(3)
      webElem <- remDr$findElement("css", "body")
      webElem$sendKeysToElement(list(key = "end"))
      Sys.sleep(3)
      #scroll down
      
      FlightList <- remDr$findElements("class",'flight-box')
      FlightData <- c(FlightData, FlightDataFunc(FlightList)) 
      
    }}
  )   #test if alert box, then close
  if('try-error' %in% class(tryNext_1)) {
    tryNext_2 <- try(
      {
        FlightFilter <- remDr$findElement("xpath",'//*[@id="hp_container"]/div[1]/div/div[3]/div[2]/div/ul[1]/li[1]/div/span/i')
        FlightFilter$clickElement()
        
        webElem <- remDr$findElement("css", "body")
        webElem$sendKeysToElement(list(key = "end"))
        Sys.sleep(3)
        webElem <- remDr$findElement("css", "body")
        webElem$sendKeysToElement(list(key = "end"))
        Sys.sleep(3)
        #scroll down
        
        FlightList <- remDr$findElements("class",'flight-box')
        FlightData <- c(FlightData, FlightDataFunc(FlightList)) 
        
      }, silent = TRUE
    ) 
    
    if('try-error' %in% class(tryNext_2)) {
      
      webElem <- remDr$findElement("css", "body")
      webElem$sendKeysToElement(list(key = "end"))
      Sys.sleep(3)
      webElem <- remDr$findElement("css", "body")
      webElem$sendKeysToElement(list(key = "end"))
      Sys.sleep(3)
      #scroll down
      
      FlightList <- remDr$findElements("class",'flight-box')
      FlightData <- c(FlightData, FlightDataFunc(FlightList)) 
      
    }
  }
} #scrape next 5 days

###Another solution
#tryCatch(
#  {
#    remDr$findElement("xpath", '//*[@id="outerContainer"]/div/div[3]/div/button')
#  }, warning = function(w){
#    Alert <- remDr$findElement("xpath", '//*[@id="outerContainer"]/div/div[3]/div/button')
#    Alert$clickElement()
#  }, error = function(e){
#    FlightList <- remDr$findElements("class",'flight-box')
#    FlightData <- c(FlightData, FlightDataFunc(FlightList)) 
#  },
#  finally = {
#    Alert <- remDr$findElement("xpath", '//*[@id="outerContainer"]/div/div[3]/div/button')
#    Alert$clickElement()
#    FlightList <- remDr$findElements("class",'flight-box')
#    FlightData <- c(FlightData, FlightDataFunc(FlightList)) 
#  }
#)

FlightData_direct <- sapply(FlightData,c) %>%  t() %>% as.data.frame
colnames(FlightData_direct) <- c("date", "name", "plane", "depart_time", "depart_airport", "arrive_time", "arrive_airport", "price")


###################################################select only transfer flights####################################################


remDr$navigate(url)

TransFilter <- remDr$findElement("xpath",'//*[@id="filter_item_trans_count"]/div')
TransFilter$clickElement()
TransFilterBox <- remDr$findElement("xpath",'//*[@id="domestic_filter_group_trans_and_train__trans_count"]/li/span/i')
TransFilterBox$clickElement()
#select "transfer" button

FlightList <- remDr$findElements("class",'flight-box')

FlightDataFunc <- function(FlightList)lapply(FlightList,function(i){
  
  doc <- i$getElementAttribute("outerHTML")[[1]]%>%
    read_html()
  #read web script
  
  name <- doc%>%
    html_nodes("div.ariline-name")%>%
    html_text() %>% paste()
  #read airline name
  
  groupNodes <-  doc %>% html_nodes("div.ariline-name")
  plane <-lapply(groupNodes, function(node){
    results <- node %>% html_nodes("div.plane") %>% html_text()
  }) %>% paste()
  #read plane info
  
  depart_time <- doc%>%
    html_nodes("div.depart-box")%>%
    html_nodes("div.time")%>%
    html_text()
  #read depart time
  
  depart_airport <- doc%>%
    html_nodes("div.depart-box")%>%
    html_nodes("div.airport")%>%
    html_text()
  #read depart airport
  
  arrive_time <- doc%>%
    html_nodes("div.arrive-box")%>%
    html_nodes("div.time")%>%
    html_text()
  #read depart time
  
  arrive_airport <- doc%>%
    html_nodes("div.arrive-box")%>%
    html_nodes("div.airport")%>%
    html_text()
  #read arrive airport  
  
  price <- doc%>%
    html_nodes("span.price")%>%
    html_text()
  #read price 
  
  return(c(name, plane, depart_time, depart_airport, arrive_time, arrive_airport, price))
})

FlightData <- FlightDataFunc(FlightList)

FlightData <- sapply(FlightData,c) %>% t() %>% as.data.frame

num_trans <- (ncol(FlightData)-5)/2 #count how many connections

for(i in (1:num_trans)){
  colnames(FlightData)[i] <- paste("name", as.character(i))
}
for(i in (1:num_trans)){
  colnames(FlightData)[i+num_trans] <- paste("plane", as.character(i))
}
colnames(FlightData)[(ncol(FlightData)-4):ncol(FlightData)] <- c("depart_time", "depart_airport", "arrive_time", "arrive_airport", "price")
#rename columns


###########################################################output to csv###########################################################


addWorksheet(wb, "SHA-BJS")

writeData(wb, sheet = "SHA-BJS", FlightData)

saveWorkbook(wb, "###")

remDr$quit()


