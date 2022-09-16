library(covid19.analytics)
library(plotly)
library(lubridate)
library(ggplot2)
library(prophet)
library(dplyr)

options(scipen = 6)

covidData <- covid19.data()
## obtain time series data for "confirmed" cases ##
confirmed_cases <- covid19.data(case = "ts-confirmed")
# India Confirmed Cases

confirmed_cases_in  <-
  confirmed_cases %>% filter(Country.Region == "India")
print(confirmed_cases_in)


## Death Count / reads time series data for casualties ##
death_cases <- covid19.data(case = "ts-deaths")
# India Death Cases
death_cases_in <- death_cases %>% filter(Country.Region == "India")
print(death_cases_in)

## obtain time series data for "recovered" cases ##.
recovered_cases <- covid19.data(case = "ts-recovered")
# India Recovered Cases
recovered_cases_in <-
  recovered_cases %>% filter(Country.Region == "India")
print(recovered_cases)



#Summary Statistics

#GLOBAL
report.summary(Nentries = 5,
               graphical.output = T)

report.summary(Nentries = 5,
               graphical.output = T,
               geo.loc = "India")



#one way anova
one.way = aov(Confirmed ~ Deaths, data = covidData)
summary(one.way)


# Graphs and Visualization

total_ts <- covid19.data(case = "ts-ALL")
totals.plt(total_ts)
#totals per location  //Regression Analysis
tots.per.location(confirmed_cases, geo.loc = "India")

# growth rates
growth.rate(confirmed_cases, geo.loc = c('US', 'India'))
growth.rate(confirmed_cases, geo.loc = c('Brazil', 'India'))
#SIR modelling
SIR = generate.SIR.model(confirmed_cases, 'India', tot.population = 34478517)

#livemap
live.map(confirmed_cases)
#######################################################################################################

# Updation Part

# Pandemics Trend Prediction (time in day wise)
tsc <- covid19.data(case = 'ts-confirmed')
tsc <- tsc %>% filter(Country.Region == 'India')
tsc  <- data.frame(t(tsc))
tsc <- cbind(rownames(tsc), data.frame(tsc, row.names = NULL))
colnames(tsc) <- c('Date', 'Confirmed')
tsc <- tsc[-c(1:4), ]
tsc$Date <- ymd(tsc$Date)
str(tsc)
tsc$Confirmed <- as.numeric(tsc$Confirmed)

#Plot
qplot(Date, Confirmed, data = tsc, main = 'Covid19 confirmed cases in India')
ds <- tsc$Date
y <- tsc$Confirmed
df <- data.frame(ds, y)

# Forecasting
m <- prophet(df)

# Prediction
future <- make_future_dataframe(m, periods = 28)
forecast <- predict(m, future)

#Plot forecast
plot(m, forecast)
dyplot.prophet(m, forecast)

#Forecast components
prophet_plot_components(m, forecast)

#Model Performance

pred <- forecast$yhat[1:121]
actual <- m$history$y
plot(actual, pred)



#Single Trend for India

indiaData <-
  confirmed_cases[confirmed_cases$Country.Region == "India" , ]
single.trend(indiaData)

#Multiple Location


mtrends (confirmed_cases, geo.loc = c ("US", "India"))
#Interactive Locations trend daily cases
itrends (covid19.data("ts-confirmed") , geo.loc = "India")
itrends (covid19.data("ts-confirmed") , geo.loc = "ALL")

#TESTING
covidTest <- covid19.testing.data(tgt = "testing", disclaimer = TRUE)
#covidTest<-covidTest %>% filter(Country.Region == "India")
print(covidTest)

#VACCINATION
covidvaccine <-
  covid19.vaccination(tgt = "global",
                      data.fmt = "orig",
                      disclaimer = TRUE)
print(covidvaccine)

#GENOMIC
covidgenomic <- covid19.genomic.data(
  type = "genome",
  src = "livedata",
  graphics.ON = TRUE,
  accOnly = TRUE
)
print(covidgenomic)
