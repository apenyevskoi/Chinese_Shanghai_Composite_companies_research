#-----step 0---Preparetion code--------------------------------
library(tidyverse); 
library(dplyr); 
library(tidyr)
library(writexl); 
library(readxl);
#library(lobstr); 
library(stringi)
#library("jsonlite"); 
#library("rjson")
#library(RMariaDB)
#library(googledrive)
library(dplyr)

#setwd("C:/Users/INSAGNIFICANT/Downloads/R")
#rm(list = ls())

#---::STEP 1::::::::::::::::::::::::::::::::::::::::::::::::::::::-----
#------------STEP1------Download DB from server----------------------------
#No access to DB server. Commented
'''

download.quotesdb <- function(){
  library(RMariaDB)
  finDb <- dbConnect(RMariaDB::MariaDB(), user='ext', password='OtawCeSZEl9AsJyf', dbname='fin', host='209.97.167.11')
  dbListTables(finDb)
  #:::::::::::::::::::: QUOTES TABLE
  rs = dbSendQuery(finDb,"select * from quotes")
  quotesdb <- dbFetch(rs)
  dbDisconnect(finDb)
  quotesdb
}
download.chartdb <- function() {
  #:::::::::::::::::::: CHART TABLE
  library(RMariaDB)
  finDb <- dbConnect(RMariaDB::MariaDB(), user='ext', password='OtawCeSZEl9AsJyf', dbname='fin', host='209.97.167.11')
  dbListTables(finDb)
  chartRS = dbSendQuery(finDb, "select * from chart")
  chartdb<-dbFetch(chartRS)
  dbDisconnect(finDb)
  chartdb
}
download.financedb <- function() {
  #:::::::::::::::::::: FINANCIAL TALBE
  library(RMariaDB)
  finDb <- dbConnect(RMariaDB::MariaDB(), user='ext', password='OtawCeSZEl9AsJyf', dbname='fin', host='209.97.167.11')
  dbListTables(finDb)
  #---partial table
  financeRS2 = dbSendQuery(finDb, "select * from fins where 
                                 name = \"annualTotalRevenue\" or
                                 name = \"annualNetIncomeContinuousOperations\" or
                                 name = \"annualTotalIssued\" or
                                 name = \"annualNetIncome\" or
                                 name = \"annualOperatingRevenue\" or
                                 name = \"annualTotalCapitalization\" or
                                 name = \"annualOperatingCashFlow\" or
                                 name = \"annualShareIssued\" or
                                 name = \"annualOrdinarySharesNumber\"")
  financedb2 <- dbFetch(financeRS2)
  #---full table
  #financedb <- dbFetch(financeRS)
  #financeRS = dbSendQuery(finDb, "select * from fins")
  dbDisconnect(finDb)
  financedb2
}
download.calendardb <- function() {
  #:::::::::::::::::::: ECONOMIC CALENDAR
  library(RMariaDB)
  finDb <- dbConnect(RMariaDB::MariaDB(), user='ext', password='OtawCeSZEl9AsJyf', dbname='fin', host='209.97.167.11')
  dbListTables(finDb)
  calendarRS = dbSendQuery(finDb, "select * from economicCalendar")
  calendardb <- dbFetch(calendarRS)
  dbDisconnect(finDb)
  calendardb
}


quotesdb <- download.quotesdb()
chartdb <- download.chartdb()
financedb2 <- download.financedb() #NEXT STEP: PREPARE DATA!!!!!!!!!!!!!!!!
#financedb.q <- download.financedb.quarterly()
calendardb <- download.calendardb()

#write.csv(quotesdb, "json/quotesdb.csv")
#write.csv(chartdb, "json/chartdb.csv")
#write.csv(financedb, "json/financedb.csv")
#write.csv(calendardb, "json/calendardb.csv")
'''
#---read DB from file
#See README.ME: You need to download chartdb.csv, financedb.csv, calendardb.csv, quotesdb.csv, links at README.ME
  chartdb <- read.csv("jchartdb.csv")
  chartdb$date <- as.Date(levels(chartdb$date)[chartdb$date])
  chartdb <- chartdb[,c(-1,-2)]
  financedb <- read.csv("financedb.csv")
  financedb$date <- as.Date(levels(financedb$date)[financedb$date])
  calendardb <- read.csv("calendardb.csv")
  calendardb$date <- as.Date(levels(calendardb$date)[calendardb$date])
  quotesdb <- read.csv("quotesdb.csv")

#fields of financial table, 313 fields
unique(financedb[financedb$quote == 1,]$name)

#---::STEP 2::::::::::::::::::::::::::::::::::::::::::::::::::::::-----
#------------STEP 2------Prepare table fields--------------------

#financedb is TOO BIG, cannot pivot_wider. Do query some fields
financedb2 <- financedb[financedb$name == "annualTotalRevenue" |
                        financedb$name == "annualNetIncomeContinuousOperations" |
                        financedb$name == "annualTotalIssued" |
                        financedb$name == "annualNetIncome" |
                        financedb$name == "annualOperatingRevenue" |
                        financedb$name == "annualTotalCapitalization" |
                        financedb$name == "annualOperatingCashFlow" |
                        financedb$name == "annualShareIssued" |
                        financedb$name == "annualOrdinarySharesNumber",]
  uniq.economic.fields <- unique(as.character(calendardb$name))
  short.uniq.economic.fields <- substr(uniq.economic.fields,20, nchar(uniq.economic.fields))
  financedb2 <- financedb2[,c(-1,-2)]
  financedb2 <- pivot_wider(financedb2, names_from = c(name), values_from = c(value))
  financedb2 <- financedb2 %>% group_by(date, quote) %>% summarise_all(funs(mean(.,na.rm = T)))

#------------STEP 3----FUNCTIONS-(do all section)--------------
#draw shanghai composite charts. company numbers from exchange source
#---draw.price.chart(chart-dataset, company-names-datase, company-number)
draw.price.chart <- function(chartdb, quotesdb, company.number) {
  plot_ly(data = chartdb[chartdb$quote == company.number,], 
          x = ~date, 
          y = ~close, 
          type = 'scatter', 
          mode = 'lines',
          text = ~chg,
          hovertemplate = paste(
            "price: %{y}<br>",
            "% change: %{text:,.2f}<br>",
            "%{x}")) %>%
    layout(
      xaxis = list(
        title = "Date"
      ),
      yaxis = list(
        title = "Price"
      ),
      title = list(
        text = quotesdb[quotesdb == company.number,]$name
      )
    ) %>%
    rangeslider()
}
  #example. company 1000
  draw.price.chart(chartdb, quotesdb, 1000)
  

#draw capitalization and PE
#draw.cap.pe(financedb2, chartdb, quotesdb, company's id)
draw.cap.pe <- function(financedb2, chartdb, quotesdb, company.number) {
  #PIVOTED financedb2 and chartdb !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  tmp <- merge(chartdb[chartdb$quote == company.number,c(which(colnames(chartdb) == "date"),
                                               which(colnames(chartdb) == "close"))], 
               financedb2[financedb2$quote == company.number, c(which(colnames(financedb2) == "date"),
                                                      which(colnames(financedb2) == "annualOrdinarySharesNumber"),
                                                      which(colnames(financedb2) == "annualNetIncome"))],
               all = TRUE, 
               by = "date")
  tmp <- fill(tmp, c(annualNetIncome, annualOrdinarySharesNumber))
  tmp <- mutate(tmp, cap = close * annualOrdinarySharesNumber); tmp <- na.omit(tmp)
  tmp <- mutate(tmp, pe = cap/annualNetIncome)
  library(plotly)
  graph_return <- plot_ly(data = tmp, x = ~date, y = ~cap, type = 'scatter', mode = 'lines', name = 'cap') %>%
    add_trace(x = ~date, y = ~pe, yaxis = 'y2', type = 'scatter', mode = 'lines', name = 'pe') %>%
    layout(
      title = list(
        text = paste(quotesdb[quotesdb$id == company.number,]$name, "(",company.number,")",sep = "")
      ),
      yaxis2 = list(
        title = "PE",
        overlaying = 'y',
        side = 'right',
        automargin = T,
        autorange = T
      ),
      legend = list(
        autorange =T,
        automargin = T
      )
    )
  return(graph_return)
}
  #example, company 1232
  draw.cap.pe(financedb2, chartdb, quotesdb, 1232)


#draw price and NetIncome
draw.price.ni <- function(financedb2, chartdb, quotesdb, company.number) {
  #PIVOTED financedb2 and chartdb !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  tmp <- merge(chartdb[chartdb$quote == company.number,c(which(colnames(chartdb) == "date"),
                                            which(colnames(chartdb) == "close"))], 
               financedb2[financedb2$quote == company.number, c(which(colnames(financedb2) == "date"),
                                                   which(colnames(financedb2) == "annualOrdinarySharesNumber"),
                                                   which(colnames(financedb2) == "annualNetIncome"),
                                                   which(colnames(financedb2) == "annualTotalRevenue"))],
               all = TRUE, 
               by = "date")
  tmp <- fill(tmp, c(annualNetIncome, annualOrdinarySharesNumber, annualTotalRevenue))
  tmp <- mutate(tmp, cap = close * annualOrdinarySharesNumber); tmp <- na.omit(tmp)
  tmp <- mutate(tmp, pe = cap/annualNetIncome)
  solution <- glm(close ~ annualTotalRevenue + annualNetIncome, data = tmp)

  library(plotly)
  plot_ly(data = tmp, x = ~date, y = ~close, type = 'scatter', mode = 'lines', name = 'cap') %>%
    add_trace(x = ~date, y = ~annualNetIncome, yaxis = 'y2', type = 'scatter', mode = 'lines', 
              name = paste('NetIncome',
                           '<br>regression coef:',
                           '<br>revenue',
                           '<br>',solution$coefficients[2])) %>%
    layout(
      title = list(
        text = paste(quotesdb[quotesdb$id == company.number,]$name, "(",1,")",sep = "")
      ),
      yaxis2 = list(
        title = "NetIncome",
        overlaying = 'y',
        side = 'right',
        automargin = T,
        autorange = T
      ),
      legend = list(
        autorange =T,
        automargin = T
      )
    )
}
  #example, company 1
  draw.price.ni(financedb2, chartdb, quotesdb, 1)

#compute pe, ps, return
#---financedb2 relativistic structure:
#---quote
#---date
#---name:
#---annualTotalRevenue, annualGainOnSaleOfSecurity, annualNetIncomeContinuousOperations, 
#---annualTotalIssued, annualNetIncome annualOperatingRevenue, annualTotalCapitalization,
#---annualOperatingCashFlow, annualShareIssued
#---value
compute.pivot.multi <- function(financedb2) {
  #tmp <- pivot_wider(financedb2, names_from = name, values_from = value)
  tmp <- fill(financedb2, annualTotalCapitalization)
  tmp <- mutate(tmp, pe = annualTotalCapitalization/annualNetIncome)
  tmp <- mutate(tmp, ps = annualTotalCapitalization/annualTotalRevenue)
  tmp <- mutate(tmp, rent = (annualNetIncome * 100)/annualTotalRevenue)
  tmp
}
#add PE, PS, profitability to finance2 table
financedb_tmp <- compute.pivot.multi(financedb2)
#regression
#---financedb2 relativistic structure:
#---quote
#---date
#---name:
#---annualTotalRevenue, annualGainOnSaleOfSecurity, annualNetIncomeContinuousOperations, 
#---annualTotalIssued, annualNetIncome annualOperatingRevenue, annualTotalCapitalization,
#---annualOperatingCashFlow, annualShareIssued
#---value
draw.regression.Cap.Netincome <- function(financedb2, company.number) {
  library(plotly)
  #financedb2 <- financedb2[,-1]
  #tmp <- pivot_wider(financedb2, names_from = name, values_from = value)
  tmp <- mutate(financedb2, pe = annualTotalCapitalization/annualNetIncome)
  tmp <- mutate(tmp, ps = annualTotalCapitalization/annualTotalRevenue)
  tmp <- mutate(tmp, rent = (annualNetIncome * 100)/annualTotalRevenue)
    #command check NA values and remove it
    tmp.plus.solution <- data.frame(annualTotalCapitalization = tmp[tmp$quote == company.number,]$annualTotalCapitalization,
                                    annualNetIncome = tmp[tmp$quote == company.number,]$annualNetIncome)
    tmp.plus.solution <- na.omit(tmp.plus.solution)
  solution.fitted <- glm(annualTotalCapitalization ~ annualNetIncome,data = tmp.plus.solution) %>% fitted.values()
  solution <- glm(annualTotalCapitalization ~ annualNetIncome,data = tmp[tmp$quote == company.number,])
  plot_ly(data = tmp.plus.solution, 
          x = ~annualNetIncome,
          y = ~annualTotalCapitalization,
          type = 'scatter',
          mode = 'markers',
          name = paste("dots (",nrow(tmp.plus.solution),"<br>years)", sep = "")) %>%
    add_trace(x = ~annualNetIncome,
              y = solution.fitted,
              mode = 'lines',
              name = "regression<br>line") %>%
    layout(
      title = list(
        text = paste("Net Income and Cap relationship (intercept", 
                     format(solution$coefficients[1], digits = 4)," regression ",
                     format(solution$coefficients[2], digits = 4),")", sep = "")
      ),
      xaxis = list(title = "Net Income"),
      yaxis = list(title = "Total Capitalization")
    )
}
  #example, define dependence of company's Capitalization of Net Income, regression
  draw.regression.Cap.Netincome(financedb2, 2)

  
#!!!!!!!!!!!!!!!temporary function!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#EXPEREMENTAL prediction PRICE ~ NetIncome
draw.experemental.prediction <- function(financedb2, company.number) {
  library(plotly)
  #financedb2 <- financedb2[,-1]
  #tmp <- pivot_wider(financedb2, names_from = name, values_from = value)
  tmp <- mutate(financedb2, pe = annualTotalCapitalization/annualNetIncome)
  tmp <- mutate(tmp, ps = annualTotalCapitalization/annualTotalRevenue)
  tmp <- mutate(tmp, rent = (annualNetIncome * 100)/annualTotalRevenue)
  tmp2 <- na.omit(tmp[tmp$quote == company.number,])
  x <- tmp2$annualNetIncome
  y <- tmp2$annualTotalCapitalization
  y <- x^2 + 2*x
  sol <- lm(y~x)
  prediction <- predict(lm(y~x + I(x^2)), data.frame(x = sort(x)))
  plot(y ~ x)
  abline(sol, col = "red")
  lines(sort(x), prediction)
}
  #example, predict price of company by regression function, Price ~ NetIncome
  draw.experemental.prediction(financedb2, 505)
  
#CANNOT ACCESS TO DB
#uniq.economic.fields - char, unique elements from economic calendar table, name column
sqlquery.put.economical.data.to.list <- function(uniq.economic.fields, short.uniq.economic.fields) {
  library(RMariaDB)
  finDb <- dbConnect(RMariaDB::MariaDB(), user='ext', password='OtawCeSZEl9AsJyf', dbname='fin', host='209.97.167.11')
  dbListTables(finDb)
  quote.chg.economic.data.value <- list()
  for(i in 1:length(uniq.economic.fields)) {
    merge.calendar.chartRS = dbSendQuery(finDb, paste("SELECT quote,time,chg,trim('%' from value) as value, date, volume FROM ecoChartJoin 
                                                       inner join economicCalendar on economicCalendar.id=idEco
                                                       inner join chart on chart.id=idChart
                                                       where   name = '",
                                                       uniq.economic.fields[i],
                                                       "' ORDER BY quote, time", sep = ""))
    quote.chg.economic.data.value[[i]] <- dbFetch(merge.calendar.chartRS)
    quote.chg.economic.data.value[[i]]$value <- as.numeric(quote.chg.economic.data.value[[i]]$value)
    quote.chg.economic.data.value[[i]] <- na.omit(quote.chg.economic.data.value[[i]])
    #names(quote.chg.economic.data.value) <- short.uniq.economic.fields[i]
  }
  names(quote.chg.economic.data.value) <- short.uniq.economic.fields
  quote.chg.economic.data.value[[i]] <- na.omit(quote.chg.economic.data.value[[i]])
  dbDisconnect(finDb)
  quote.chg.economic.data.value
}
uniq.economic.fields
#quote.chg.economic.data.value <- sqlquery.put.economical.data.to.list(uniq.economic.fields, short.uniq.economic.fields)
#compute CORRELATION b/n variable CHG and VALUE
#add.corr.to.quote.chg.value <- function(quote.chg.economic.data.value) {
  tmp2 <- list()
  tmp3 <- data.frame()
  for(i in 1:length(quote.chg.economic.data.value)) {
    for(j in 1:last(quote.chg.economic.data.value[[1]]$quote)) {
      #quote.chg.economic.data.value[[i]] <- quote.chg.economic.data.value[[i]][,-which(colnames(quote.chg.economic.data.value[[i]]) == 'corr')]
      tmp3 <- rbind(tmp3, c(j, cor(quote.chg.economic.data.value[[i]][quote.chg.economic.data.value[[i]]$quote == j,]$chg,
                                   quote.chg.economic.data.value[[i]][quote.chg.economic.data.value[[i]]$quote == j,]$value)))
    }
    colnames(tmp3) <- c("quote", "corr")
    tmp2[[i]] <- tmp3
    tmp3 <- data.frame()
  }
  for(i in 1:length(quote.chg.economic.data.value)) {
    quote.chg.economic.data.value[[i]] <- merge(quote.chg.economic.data.value[[i]], tmp2[[i]], by = 'quote', all = T)
  }
  quote.chg.economic.data.value
}
#quote.chg.economic.data.value <- add.corr.to.quote.chg.value(quote.chg.economic.data.value)


#-autoregression gauge
#f <- function(x) (x/4)*exp(-x^2/8)
#f(1)
#integral <- integrate(f, 0, Inf)
#integral$value
#var()
#dnorm(3, mean = 11, sd = 20)
#pnorm(1, sd = 1.64)
#ar.ols(financedb2[financedb2$quote == 1,]$annualNetIncome[1:(length(financedb2[financedb2$quote == 1,]$annualNetIncome)-1)])
#ar.ols(financedb2[financedb2$quote == 1,]$annualNetIncome)
#tail(financedb2[financedb2$quote == 1,])
#:::::::::::::::::::::::::::::::::::::-----



#---::STEP 4::::::::::::::::::::::::::::::::::::::::::::::::::::::-----
#------------Compute regression using RELATIVISTIC table--- STEP 4--------------
#DOESN'T WORK, CANNOT DOWNLOAD DATASET
'''
intercept.tbl <- data.frame()
regr.coeff.tbl <- data.frame()
for(i in 1:length(short.uniq.economic.fields)) {
  for(j in 1:last(quote.chg.economic.data.value[[2]]$quote)) {
    if (j!=1288 && 
        count(quote.chg.economic.data.value[[i]][quote.chg.economic.data.value[[i]]$quote == j,]) > 0) {
      solution <- lm(chg ~ value, data = quote.chg.economic.data.value[[i]][quote.chg.economic.data.value[[i]]$quote == j,c(1,3,4)])
      intercept.tbl[j,i] <- solution$coefficients[1]
      regr.coeff.tbl[j,i] <- solution$coefficients[2]
    }
  }
  colnames(intercept.tbl)[i] <- short.uniq.economic.fields[i]
  colnames(regr.coeff.tbl)[i] <- short.uniq.economic.fields[i]
}

#------------Compute percent in one column-------------------------
pct <- function(x) {x*100/lag(x)-100}
tmp10 <- mutate_each(chartdb.pivot[,-1], funs(pct), c(1:1495))
colnames(tmp10) <- paste(rep("chg",1495),c(1:1495),sep="")
tmp11 <- cbind(chartdb.pivot, tmp10)

#------------Compute MEDIAN of regression-------------------
median.regression <- data.frame(econParam = names(summary(regr.coeff.tbl)[1,]), 
                                median = as.numeric(sub("Median :", 
                                                        "", 
                                                        summary(regr.coeff.tbl)[3,])))
rownames(median.regression) = NULL
median.regression


#---::STEP 5::::::::::::::::::::::::::::::::::::::::::::::::::::::-----
#------------FILTERS !!!!!!!!!! STEP 5 get filter.regr.corr; filter.regr.corr.pe.rent-----------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#---COMPANIES FILTER -- REGRESSION, CORR, PE, RENT
intersect(unique(quote.chg.economic.data.value[[3]][quote.chg.economic.data.value[[3]]$corr > 0.3,]$quote),
          regr.coeff.tbl[regr.coeff.tbl[,3] < 80 &
                           regr.coeff.tbl[,3] > 40,]$quote)
#REGRESSION 20-40, CORR > 0.2, PE <= 4.5, rent >= 17
filter.regr.corr <- na.omit(intersect(unique(quote.chg.economic.data.value[[3]][quote.chg.economic.data.value[[3]]$corr > 0.2,]$quote),
                                      regr.coeff.tbl[regr.coeff.tbl[,3] < 100 &
                                                       regr.coeff.tbl[,3] > 5,]$quote))
filter.regr.corr
#financedb2 <- compute.pivot.multi(financedb2)
filter.pe.rent <- na.omit(financedb2[financedb2$date == "2019-12-31" &
                          financedb2$pe <= 4.5 & 
                          financedb2$rent >= 17,c(1),]$quote)
filter.pe.rent
filter.regr.corr.pe.rent <-intersect(filter.pe.rent, filter.regr.corr); filter.regr.corr.pe.rent
#---::STEP 6::::::::::::::::::::::::::::::::::::::::::::::::::::::-----
#----------------------FUNC DRAW REGRESSION gauge and CORRELATION COEFFICIENT------------------
draw.regression.chg.value <- function(economic.number, 
                                      quote.chg.economic.data.value, 
                                      quotesdb, 
                                      company.number, 
                                      short.uniq.economic.fields, 
                                      uniq.economic.fields) {
  #tmp <- quote.chg.economic.data.value[[economic.number]][quote.chg.economic.data.value[[economic.number]]$quote == company.number,]
  tmp <- quote.chg.economic.data.value[[economic.number]][quote.chg.economic.data.value[[economic.number]]$quote == company.number,]
  sol <- glm(chg ~ value, data = tmp)
  sol.fitted <- sol %>% fitted.values()
  
  library(plotly)
  plot_ly(data = tmp, x = ~value, y = ~chg, type = 'scatter', mode = 'markers', name = paste(count(tmp)," dots,",
                                                                                             "<br><b>correlation of<br>chg and value:</b><br>",
                                                                                             format(tmp$corr, digits = 5)),
          legendgroup = "group1") %>%
    add_trace(x = ~value, y = sol.fitted, type = 'scatter', mode = 'lines', name = paste("<b>regression:</b><br>",
                                                                                         format(sol$coefficients[2], digits = 5),
                                                                                         "<br><b>intercept:</b><br>",
                                                                                         format(sol$coefficients[1], digits = 4)),
              legendgroup = "group1") %>%
    layout(
      title = list(
        text = paste(short.uniq.economic.fields[economic.number], " - ",
                     quotesdb[quotesdb$id == company.number,]$name, " (",company.number,") ", sep = "")
      ),
      legend = list(
        title = list(
          text = "   chart description"
        )
      )
    )
}
#draw PLOT gdp ~ cpi
#x - independant variable, y - dependant -- numbers of columns of calendardb table
draw.regression <- function(calendardb, x, y) {
  calendardb$name <- sub("/economic-calendar/", "", calendardb$name)
  calendar.pivot <- pivot_wider(calendardb[,c(2,3,5)], names_from = name, values_from = value)
  k <- calendar.pivot[,c(1,x)]; p <- calendar.pivot[,c(1,y)]
  k <- na.omit(k); p <- na.omit(p)
  nrow(k); nrow(p)
  m <- merge(k,p, by = "time", all = TRUE); m <- fill(m, colnames(m)[3]); m <- na.omit(m)
  sol <- glm(m[,3] ~ m[,2]) %>% fitted.values()
  sol2 <- glm(m[,3] ~ m[,2])
  plot_ly(data = m, x = ~m[,2], y = ~m[,3], type = 'scatter', mode = 'markers', showlegend = F) %>%
    add_trace(x = ~m[,2], y = sol, type = 'scatter', mode = 'lines', 
              name = paste("regression:",
                           format(sol2$coefficient[2], digits =5),
                           "<br>correlation:",
                           format(cor(m[,2],m[,3]), digits = 5)),
              showlegend = T) %>%
    layout(
      title = list(
        text = paste(colnames(m)[3], "~", colnames(m)[2])
      ),
      showlegend = TRUE,
      xaxis = list(title = colnames(m)[2]),
      yaxis = list(title = colnames(m)[3])
    )
}
#---::GAUGE:::::::::::::::::::::::::::::::::::::::::::::::::::::::-----
#----------------------regression gauge----------------------
sol.mediation <- glm(quote.chg.economic.data.value[[3]][quote.chg.economic.data.value[[3]]$quote == 22,]$chg ~
                       quote.chg.economic.data.value[[12]][quote.chg.economic.data.value[[12]]$quote == 22,]$value *
                       quote.chg.economic.data.value[[3]][quote.chg.economic.data.value[[3]]$quote == 22,]$value)
summary(sol.mediation)
draw.regression.chg.value(2,quote.chg.economic.data.value, quotesdb, 40, short.uniq.economic.fields, uniq.economic.fields)
draw.regression.chg.value(2,quote.chg.economic.data.value, quotesdb, 696, short.uniq.economic.fields, uniq.economic.fields)
draw.regression.chg.value(3,quote.chg.economic.data.value, quotesdb, 594, short.uniq.economic.fields, uniq.economic.fields)
draw.regression.chg.value(12,quote.chg.economic.data.value, quotesdb, 1, short.uniq.economic.fields, uniq.economic.fields)
draw.regression.chg.value(12,quote.chg.economic.data.value, quotesdb, 7, short.uniq.economic.fields, uniq.economic.fields)

draw.regression(calendardb, 4, 9)
draw.regression(calendardb, 5, 9)
draw.regression(calendardb, 17, 9)
draw.regression(calendardb, 18, 9)
draw.regression(calendardb, 9, 18)
draw.regression(calendardb, 30, 7)
#reg 44
draw.regression(calendardb, 7, 30)
#reg 2.25 LOANS ~ CPI
draw.regression(calendardb, 34, 4)
#reg  28
draw.regression(calendardb, 34, 9)
#FILTER COEFFICIENTS
filter.regr.corr
filter.pe.rent
filter.regr.corr.pe.rent
#---CHECK COMPANY: draw price chart, cap chart, PE
draw.price.chart(chartdb,quotesdb,1232) #WHAT TO DO. CHANGE RENT and PE
draw.cap.pe(financedb2,chartdb,quotesdb,1232)
#---gauge 594 company: corr > 0.1, regression 5-100, PE <= 4.5, rent >= 17
draw.price.chart(chartdb,quotesdb,594)
draw.cap.pe(financedb2,chartdb,quotesdb,594)
draw.price.ni(financedb2,chartdb,quotesdb,594)
draw.regression.Cap.Netincome(financedb2, 594)
draw.regression.chg.value(3,quote.chg.economic.data.value, quotesdb, 594, short.uniq.economic.fields, uniq.economic.fields)

#AUTOREGRESSION AND PREDICTION
library(forecast)
fit.ni <- ar.ols(financedb2[financedb2$quote == 1,]$annualNetIncome, order.max = 1, demean = F, intercept = T)
fit.rev <- ar.ols(financedb2[financedb2$quote == 1,]$annualTotalRevenue, order.max = 1, demean = F, intercept = T)
fit$x.intercept + fit$ar[1]*last(financedb2[financedb2$quote == 594,]$annualNetIncome) #test
financedb2[financedb2$quote == 594,]$annualNetIncome #test

new1 <- data.frame("NetIncome" = financedb2[financedb2$quote == 1,]$annualNetIncome)
new2 <- data.frame("NetIncome" = financedb2[financedb2$quote == 1,]$annualTotalRevenue)

forecast.ni <- forecast(fit.ni, newdata = new); forecast.ni <- forecast.ni$lower[,2]
forecast.rev <- forecast(fit.rev, newdata = new2); forecast.rev <- forecast.rev$lower[,2]

predicted.average.price = forecast.rev*(-0.06425) + forecast.ni*2.855
predicted.average.price/10000000000
draw.price.chart(chartdb,quotesdb,1)
#----------------------temporary code---------------------------------
for(i in 1:length(quote.chg.economic.data.value)) {
  quote.chg.economic.data.value[[i]] <- quote.chg.economic.data.value[[i]][,-7]
}
quote.chg.economic.data.value[[1]]

#----------------------regression GDP ~ CPI. NEED: Calendardb------
#x - independant variable, y - dependant -- numbers of columns of calendardb table
regression.gdp.cpi <- function(calendardb, x, y) {
  calendardb$name <- sub("/economic-calendar/", "", calendardb$name)
  calendar.pivot <- pivot_wider(calendardb[,c(2,3,5)], names_from = name, values_from = value)

  k <- calendar.pivot[,c(1,x)]; p <- calendar.pivot[,c(1,y)]
  k <- na.omit(k); p <- na.omit(p)
  m <- merge(k,p, by = "time", all = TRUE); m <- fill(m, colnames(m)[3]); m <- na.omit(m)
  summary(glm(m[,3] ~ m[,2]))
}
regression.gdp.cpi(calendardb,4,9)

'''
#----::PREDICTION:::::::::::::::::::::::::::::::::::::::::::::::::-----
#------PREDICTION, DISPERTION, SIGMA-----------------------------------
#AUTOREGRESSION AND PREDICTION
library(forecast)
fit.ni <- ar.ols(financedb2[financedb2$quote == 1,]$annualNetIncome, order.max = 1, demean = F, intercept = T)
fit.rev <- ar.ols(financedb2[financedb2$quote == 1,]$annualTotalRevenue, order.max = 1, demean = F, intercept = T)

new1 <- data.frame("NetIncome" = financedb2[financedb2$quote == 1,]$annualNetIncome)
new2 <- data.frame("NetIncome" = financedb2[financedb2$quote == 1,]$annualTotalRevenue)

forecast.ni <- forecast(fit.ni, newdata = new); forecast.ni <- forecast.ni$lower[,2]
forecast.rev <- forecast(fit.rev, newdata = new2); forecast.rev <- forecast.rev$lower[,2]

predicted.average.price = forecast.rev*(-0.122066) + forecast.ni*0.5813
predicted.average.price/1000000000
draw.price.chart(chartdb,quotesdb,1)
#START dispertion, q
#price dispertion of calculation
tmp <- merge(chartdb[chartdb$quote == 1,c(which(colnames(chartdb) == "date"),
                                          which(colnames(chartdb) == "close"))], 
             financedb2[financedb2$quote == 1, c(which(colnames(financedb2) == "date"),
                                                 which(colnames(financedb2) == "annualOrdinarySharesNumber"),
                                                 which(colnames(financedb2) == "annualNetIncome"),
                                                 which(colnames(financedb2) == "annualTotalRevenue"))],
             all = TRUE, 
             by = "date")
tmp <- fill(tmp, c(annualNetIncome, annualOrdinarySharesNumber, annualTotalRevenue))
df.predict <- tmp # only one company, #1
df.predict <- na.omit(tmp)
df.grouped <- df.predict %>% group_by(format(date, "%Y-12-31"), .drop = F) %>% summarise(close = mean(close), ni = mean(annualNetIncome), rev = mean(annualTotalRevenue))
df.grouped.only.price <- df.predict %>% group_by(format(date, "%Y-12-31"), .drop = F) %>% 
  summarise(disper = mean(close^2)/length(close)-mean(close)^2/length(close))
df.grouped.only.price <- mutate(df.grouped.only.price, sigma = sqrt(disper))
df.grouped.only.price
#system of equation
predict.coef <- data.frame()
#df.grouped[,c(3,4)] <- df.grouped[,c(3,4)]/1000000000
for(i in 1:nrow(df.grouped)-1) {
  Xrev = (df.grouped$close[i] * df.grouped$ni[i+1] - df.grouped$ni[i] * df.grouped$close[i+1]) / (df.grouped$rev[i]*df.grouped$ni[i+1] - df.grouped$ni[i]*df.grouped$rev[i+1])
  predict.coef <- rbind(predict.coef, Xrev)
  #predict.coef[i,1] <- Xrev
}
df.grouped$close[1]
predict.coef
Xrev
install.packages("optR")
library(optR)
XY <- data.frame()
for(i in 2:nrow(df.grouped)-1) {
  A <- matrix(c(df.grouped$rev[i], df.grouped$ni[i],df.grouped$rev[i+1], df.grouped$ni[i+1]),nrow =2,ncol=2, byrow = TRUE)
  b <- matrix(c(df.grouped$close[i],df.grouped$close[i+1]),nrow =2,ncol=1, byrow = TRUE)
  XY <- rbind(XY,as.data.frame(t(solve(A,b))))
#  Xrev
}
XY <- XY * 1000000000
XY
xmean <- mean(XY$V1)
ymean <- mean(XY$V2)
xmedian <- median(XY$V1)
ymedian <- median(XY$V2)
df.grouped <- df.grouped[-1,]
colnames(df.grouped)[1] <- "date"
df.grouped <- cbind(df.grouped, xmean)
df.grouped <- cbind(df.grouped, ymean)
df.grouped <- cbind(df.grouped, xmedian)
df.grouped <- cbind(df.grouped, ymedian)
df.grouped <- cbind(df.grouped, XY$V1)
df.grouped <- cbind(df.grouped, XY$V2)
df.grouped <- mutate(df.grouped, duplicated.pr = (rev * XY$V1 + ni * XY$V2)/1000000000)
df.grouped <- mutate(df.grouped, pr1mean = (rev * xmean + ni * ymean)/1000000000)
df.grouped <- mutate(df.grouped, pr2med = (rev * xmedian + ni * ymedian)/1000000000)
df.grouped <- mutate(df.grouped, pr3mM = (rev * ((xmean+xmedian)/2) + ni * ((ymean+ymedian)/2))/1000000000)
df.grouped <- mutate(df.grouped, pr4.xy1 = (rev * XY$V1[1] + ni * XY$V2[1])/1000000000)
df.grouped <- mutate(df.grouped, pr4.xy13 = (rev * XY$V1[13] + ni * XY$V2[13])/1000000000)
df.grouped <- mutate(df.grouped, pr4.xy20 = (rev * XY$V1[20] + ni * XY$V2[20])/1000000000)
df.grouped
summary(df.grouped.only.price)
df.grouped <- df.grouped[,-16]
tmp2 <- as.data.frame(cbind(forecast.ni, forecast.rev, xmean, ymean))
mutate(tmp2, price = (forecast.rev * xmean + forecast.ni * ymean)/10000000000)
draw.price.chart(chartdb, quotesdb ,1)

mean(df.predict[df.predict$date >= "2019-01-01" & df.predict$date <= "2019-12-31",]$close)
mean(df.predict[df.predict$date >= "2018-01-01" & df.predict$date <= "2018-12-31",]$close)
df.predict[df.predict$date >= "2012-01-01" & df.predict$date <= "2012-12-30",]$annualTotalRevenue
df.predict[df.predict$date >= "2012-01-01" & df.predict$date <= "2012-12-30",]$annualNetIncome
df.predict[df.predict$date >= "2018-01-01" & df.predict$date <= "2018-12-30",]$annualTotalRevenue
df.predict[df.predict$date >= "2018-01-01" & df.predict$date <= "2018-12-30",]$annualNetIncome

#mean(df.predict[df.predict$date <= "1999-12-31",]$close^2)/length(df.predict[df.predict$date <= "1999-12-31",]$close)
#mean(df.predict[df.predict$date <= "1999-12-31",]$close)^2/length(df.predict[df.predict$date <= "1999-12-31",]$close)
#df.grouped
#disper <- mean(df.predict$annualNetIncome^2) - sapply(df.predict$annualNetIncome, function(x) mean(x))
#END dispertion, q

