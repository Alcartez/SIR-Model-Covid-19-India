library(jsonlite)
library(plotly)

#Getting the JSON data 
json_data <- fromJSON("https://api.covid19india.org/data.json")

#Converting to dataframes
model_data <- as.data.frame(json_data$cases_time_series)
model_data$date <- as.Date(model_data$date,format = '%d %B')

#Creating vectors for available data
totalconfirmed <-as.numeric(model_data$totalconfirmed)
totalrecovered <-as.numeric(model_data$totalrecovered)
totaldeceased <-as.numeric(model_data$totaldeceased)
dates <-model_data$date

len <- length(totalconfirmed)

#Creating vectors with S,I,R values
S <- 1352600000  - totalconfirmed
I <- totalconfirmed - totaldeceased - totalrecovered 
R <- totaldeceased + totalrecovered

#Creating Dataframe with SIR Vectors
SIR_df <- data.frame(S,I,R,dates)


#Calculating number of total-active cases
totalactive <- totalconfirmed - (totalrecovered + totaldeceased)

#Graph for the SIR Model
x <- dates
y1 <- S
y2 <- I
y3 <- R
fig <- plot_ly(x = ~x,y = ~y1, name = 'S',mode = 'lines', type = 'scatter')
fig <- fig %>% add_trace(y = ~y2, name = 'I',mode = 'lines' )
fig <- fig %>% add_trace(y = ~y3, name = 'R',mode = 'lines')
fig <- fig %>% layout(title = "R", xaxis = list(title = "Dates"), yaxis = list (title = "R"))
fig

#Creating a dataframe that stores the difference between every 2 rows
diff_df <- SIR_df[-1,] - SIR_df[-nrow(SIR_df),]

#Getting the mean of the differences to get the values of dS , dI , dR
dS <-mean(diff_df$S)
dI <- mean(diff_df$I)
dR <-mean(diff_df$R)

#Calculating the coefficients
last <- length(S)

beta = dS/(I[last]*S[last])
print(beta)
gamma = dR/(I[last])
print(gamma)

