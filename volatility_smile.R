rm(list = ls())

library(xml2)
library(rvest)
library(dplyr)
library(magrittr)
library(ggplot2)
library(plot3D)
library(plotly)
library(rgl)
#download option prices
url <- "https://www.biznesradar.pl/gielda/pochodne_opcje"
page <- read_html(url)
rm(url)
table <- html_table(page, fill = TRUE)
data = as.data.frame(table[[1]])
rm(table, page)
## data cleansing
filtr = function(x) {
  substr(x, 1, 2) == "OW"
}
data %<>% filter(filtr(Profil) == 1 & Czas != "")
rm(filtr)
rawdata <- data
last_trans <- function(string) {
  splited = (string %>% strsplit(split = " "))[[1]]
  if (length(splited) == 1) {
    return(as.character(Sys.Date()))
  }
  else{
    day = splited[1]
    month = switch (
      splited[2],
      "sty" = "01",
      "lut" = "02",
      "mar" = "03",
      "kwi" = "04",
      "maj" = "05",
      "cze" = "06",
      "lip" = "07",
      "sie" = "08",
      "wrz" = "09",
      "paź" = "10",
      "lis" = "11",
      "gru" = "12"
    )
    year <- (Sys.Date() %>% substr(1, 4))
    date = as.Date(paste(year, "-", month, "-", day, sep = ""))
    if (date < Sys.Date()) {
      return(as.character(date))
    }
    else{
      year = as.character(as.numeric(year) - 1)
      date = as.Date(paste(year, "-", month, "-", day, sep = ""))
      return(as.character(date))
    }
    
  }
}
last_trans %<>% Vectorize()
data %<>% mutate(Last_Trade = last_trans(Czas))
data$Last_Trade %<>% as.Date()
data %<>% select(Profil, Kurs, Last_Trade)
data %<>% filter((Sys.Date() - Last_Trade) < 10)
data$Kurs %<>% as.numeric()

data %<>% mutate(MonthLetter = substr(Profil, 5, 5),
                 Strike = substr(Profil, 8, 11))
data %<>% mutate(Strike = as.numeric(Strike))

url <- "https://bossa.pl/edukacja/kontrakty-opcje/opcje/nazwy-opcji"
page <- read_html(url)
rm(url)
option_letter <- html_table(page, fill = TRUE)[[1]]
option_letter %<>% select(-Miesiąc)
rm(page)
call_or_put <-
  function(x) {
    return(which(option_letter == x, arr.ind = T)[2] * (-2) + 3)
  }
month_number <-
  function(x) {
    return(which(option_letter == x, arr.ind = T)[1])
  }

call_or_put %<>% Vectorize()
month_number %<>% Vectorize()
data %<>% mutate(Call_or_Put = call_or_put(MonthLetter))
data %<>% mutate(Month_number = month_number(MonthLetter))
rm(call_or_put, month_number)
rm(option_letter)
#obliczenie 3 piatku miesiaca
get_no_trade_days <- function(){
  url <- "https://www.gpw.pl/szczegoly-sesji/"
  page <- read_html(url)
  rm(url)
  #wczytanie lat
  lines = readLines("https://www.gpw.pl/szczegoly-sesji/")
  
  key = 'class=\"margin-top-0\"/>'
  rowind = c()
  for (i in 1:(length(lines))) {
    if (key %in% (strsplit(lines[i], split = " "))[[1]]) {
      rowind = c(rowind, i)
    }
  }
  year_rows = (lines[rowind - 1])
  years = NULL
  for (i in year_rows) {
    years <- c(years, substr(strsplit(i, split = '"')[[1]][3], 2, 5))
  }
  rm(lines, key, year_rows, i, rowind)
  #wczytanie tabel
  table <- html_table(page, fill = TRUE)
  length(table)
  j = 2
  table[[j]][, 2]
  convert_to_date <- function(y, string) {
    string = as.character(string)
    x = strsplit(string, split = " ")[[1]]
    if (nchar(x[1]) == 1) {
      x[1] = paste("0", x[1], sep = "")
    }
    x[2] = switch(
      x[2],
      "stycznia" = "01",
      "lutego" = "02",
      "marca" = "03",
      "kwietnia" = "04",
      "maja" = "05",
      "czerwca" = "06",
      "lipca" = "07",
      "sierpnia" = "08",
      "września" = "09",
      "października" = "10",
      "listopada" = "11",
      "grudnia" = "12"
    )
    return((paste(y, '-', x[2], '-', x[1], sep = "")))
  }
  convert_to_date %<>% Vectorize
  ntd <- c()
  for (j in 2:length(table)) {
    for (k in as.data.frame(table[[j]])[,2]) {
      ntd =c(ntd,convert_to_date(years[j - 1], k))
    }
    
  }  
  rm(years,convert_to_date,j,k,table,page)
  return(ntd)
}
no_trade <- as.Date(get_no_trade_days())
terminal_date <- function(string, month) {
  month %<>% as.character()
  if (nchar(month) == 1) {
    month = paste("0", month, sep = "")
  }
  date = as.Date(paste("20", substr(string, 6, 7), "-", month, "-01", sep =
                         ""))
  while (weekdays(date) != "piątek") {
    date = date + 1
  }
  date = date + 14
  while(date %in% no_trade || (weekdays(date) == "sobota" || (weekdays(date) == "niedziela"))){
    date=date+1
  }
  return(as.character(date))
}
terminal_date %<>% Vectorize()
data %<>% mutate(Terminal_Date = terminal_date(Profil, Month_number))
rm(terminal_date)
time <- function(string) {
  string %<>% as.Date()
  return(as.numeric(string - Sys.Date()) / 365)
}
stoch_time<- function(string) {
  string %<>% as.Date()
  t=0
  date=as.Date(Sys.Date())
  while (date!=string){
    if(date %in% no_trade || (weekdays(date)) == "sobota" || (weekdays(date)) == "niedziela"){
      
    }
    else
    {t=t+1}
    date=date+1
  }
  return(t/252)
}
stoch_time %<>% Vectorize 
data %<>% mutate(Time = time(Terminal_Date))
data %<>% filter(Time>0) 
data %<>% mutate(Stoch_Time = stoch_time(Terminal_Date))
rm(time)


## wyczyszczenie data frame
row.names(data) = (data$Profil)
data %<>% select(-Profil,-MonthLetter,-Month_number)
## download S_0
url <- "https://strefainwestorow.pl/notowania"
page <- read_html(url)
indeksy <- as.data.frame(html_table(page, fill = TRUE)[[1]])
S0 = indeksy[indeksy[, 1] == "WIG20",]$`Aktualna wartość`
rm(url, page, indeksy)
## download rf
url <- "https://wibor.money.pl/"
page <- read_html(url)
wibor <- as.data.frame(html_table(page, fill = TRUE)[[1]])[, 1:2]
wibor %<>% filter(Termin != "WIBOR TN")
rm(page, url)
wibor_time <- function(name) {
  return(switch(
    name,
    "WIBOR ON" = 1 / 365,
    "WIBOR 1W" = 7 / 365,
    "WIBOR 2W" = 14 / 365,
    "WIBOR 1M" = 1 / 12,
    "WIBOR 3M" = 3 / 12,
    "WIBOR 6M" = 6 / 12,
    "WIBOR 1Y" = 1
  ))
}
wibor_time %<>% Vectorize()
wibor %<>% mutate(Time = wibor_time(Termin))
rm(wibor_time)
cont_rate <- function(r_napis, t) {
  r_simp = gsub(",", ".", r_napis) %>% as.numeric() %>% '/'(., 100)
  return((1 / t) * log(1 + r_simp * t))
}
cont_rate %<>% Vectorize()
wibor %<>% mutate(r = cont_rate(Wibor, Time))
rm(cont_rate)
wibor %<>% select(Time, r)
#plot(r~Time,wibor,ylim=c(0,0.1))
## iterpolacja r
r_interpol <- function(t) {
  t_wib = wibor$Time
  r_wib = wibor$r
  i = 1
  while (t > t_wib[i]) {
    i = i + 1
  }
  return(r_wib[i - 1] + (r_wib[i] - r_wib[i - 1]) / (t_wib[i] - t_wib[i -
                                                                        1]) * (t - t_wib[i - 1]))
}
r_interpol %<>% Vectorize()
# black scholes formula
BlackScholes <- function(S, K, r, T, sig, type,T_stoch) {
  if (type == 1) {
    d1 <- (log(S / K) + (r + sig ^ 2 / 2) * T_stoch) / (sig * sqrt(T_stoch))
    d2 <- d1 - sig * sqrt(T_stoch)
    
    value <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
    return(value)
  }
  
  if (type == -1) {
    d1 <- (log(S / K) + (r + sig ^ 2 / 2) * T_stoch) / (sig * sqrt(T_stoch))
    d2 <- d1 - sig * sqrt(T_stoch)
    
    value <-  (K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1))
    return(value)
  }
}
BlackScholes %<>% Vectorize
#imlied volatility
impvol <- function(Price, S, K, t, r, type,t_stoch) {
  upper = 1
  lower = 0
  no_bs = 20 #number of bin search
  while (BlackScholes(S, K, r, t, upper, type,t_stoch) < Price) {
    upper = upper * 2
    no_bs = no_bs + 1
  }
  for (i in 1:no_bs) {
    m = 0.5 * (upper + lower)
    if (BlackScholes(S, K, r, t, m, type,t_stoch) < Price) {
      lower = m
    }
    else{
      upper = m
    }
  }
  return(m)
}

impvol %<>% Vectorize()
data %<>% mutate(ImpVol = impvol(Kurs, S0, Strike, Time, r_interpol(Time), Call_or_Put,Stoch_Time))
data %<>% mutate(ImpPrice = BlackScholes(S0, Strike, r_interpol(Time), Time, ImpVol, Call_or_Put,Stoch_Time))

data <- data[(abs(data$Kurs - data$ImpPrice) < 0.01),]
data %>% ggplot(aes(Strike, ImpVol)) + geom_point() + facet_grid(~ Terminal_Date)
data %>% ggplot(aes(Strike, ImpVol, color = Terminal_Date)) + geom_point() +
  geom_line()
#identyfikacja odstajacych
#odstajace1 <-  data %>% filter(Terminal_Date == "2022-12-16" & ImpVol > 1.7) %>% rownames()
#rawdata %>% filter(Profil %in% odstajace1) %>%  View

#regresja liniowa
lm_data <- data %>% select(ImpVol, Strike, Time)
lm1 = lm(data = lm_data,
         ImpVol ~ Strike + I(Strike ^ 2) + Time + I(Time ^ 2) + I(Strike * Time))
summary(lm1)
vol_surface <- function(k, t) {
  b = coefficients(lm1)
  return(b[1] + k * b[2] + k ^ 2 * b[3] + b[4] * t + b[5] * t ^ 2 + b[6] *
           t * k)
}
##wykres
wykres_3d <-
  function(f,
           xlim = c(0.01, 5),
           ylim = c(0.01, 5),
           stepx = 0.01,
           stepy = 0.01,x_scatter,y_scatter,z_scatter)
  {
    x = seq(xlim[1], xlim[2], by = stepx)
    y = seq(ylim[1], ylim[2], by = stepy)
    x2 = rep(x = x, times = length(y))
    y2 = sort(rep(x = y, times = length(x)))
    z <- f(x2, y2)
    open3d()
    #create custom color for rgl plot
    myColorRamp <- function(colors, values) {
      v <- (values - min(values)) / diff(range(values))
      x <- colorRamp(colors)(v)
      rgb(x[, 1], x[, 2], x[, 3], maxColorValue = 255)
    }
    col <-
      myColorRamp(c("blue", "green", "yellow", "orange", "red"), z)
    
    plot3d(
      x = c(x_scatter, x2),
      y = c(y_scatter, y2),
      z = c(z_scatter, z),
      col = c(rep("#FF1493", length(z_scatter)), col),
      xlab = "K",
      ylab = "t",
      zlab = "impVol"
    )
  }
#wykres_3d(vol_surface,xlim=c(1000,2000),ylim=c(0,1),stepx=10,stepy = 0.1)
# open3d(plot3d(
#   x = data$Strike,
#   y = data$Time,
#   z = data$ImpVol,
#   xlab = "K",
#   ylab = "t"
# ))
wykres_3d(
  vol_surface,
  xlim = c(1000, 2500),
  ylim = c(0, 1),
  stepx = 1,
  stepy = 0.01,data$Strike,data$Time,data$ImpVol)
# 3 potegi
lm3 = lm(data = lm_data,
         ImpVol ~Strike  + I(Strike ^ 2) + Time + I(Time ^ 2)+I(Time ^ 3) + I(Strike * Time)+ I(Strike^2 * Time)+ I(Strike * Time^2))
summary(lm3)
vol_surface3 <- function(k, t) {
  b = coefficients(lm3)
  return(b[1]+b[2]*k+b[3]*k^2+b[4]*t+b[5]*t^2+b[6]*t^3+b[7]*k*t+b[8]*k^2*t+b[9]*k*t^2)
}
wykres_3d(
  vol_surface3,
  xlim = c(1000, 2500),
  ylim = c(0, 1),
  stepx = 1,
  stepy = 0.01,data$Strike,data$Time,data$ImpVol)
