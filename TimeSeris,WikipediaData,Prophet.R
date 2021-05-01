#Forecasting time series data

#In this project we will be building and forecasting times series based on wikipedia data from which we will extract the
#number of view on a particular search.


#getting wikipedia trend data

#here we will search for number of views for Lionel Messi for the mentioned dates 
library(wikipediatrend)
cris_data<-wp_trend(page = "Lionel_Messi",
         from = "2018-01-01",
         to = "2020-12-31")

#plot
library(ggplot2)
qplot(date, views, data = cris_data)
summary(cris_data)
#check number of 0 present in the data
sum(cris_data$views==0) # sum of 0 is 120
sum(cris_data$views>100000) # sum of values greater than 1L view is 6 which can affect 
#the data so remove it
#cris_data$views[cris_data$views>100000]<- NA
sum(is.na(cris_data)) #6 NA values created

ds<- cris_data$date
y<- log(cris_data$views)
df<- data.frame(ds,y)
colSums(is.na(df)) # 6 NA values found

qplot(ds, y, data = df)

#forecasting with facebook prophet library
library(prophet)
m<- prophet(df)#Error in setup_dataframe(m, history, initialize_scales = TRUE) : 
#Found infinity in column y.
sum(df$y==-Inf)
df$y[df$y==-Inf]<-9.2
#run m again

#prediction
future<-make_future_dataframe(m, periods = 365)
tail(future)
forecast<-predict(m, future)
tail(forecast)

#plot Forecast
plot(m, forecast)
prophet_plot_components(m, forecast)
#To get more interactive plot
dyplot.prophet(m, forecast)

