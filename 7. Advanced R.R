rm(list = ls())
setwd("G:/Analytics/Edwisor/Edwisor/Advanced Predictive Analytics/R Code")

##Connect database
library(RODBC)

#Connect to local SQL db from local machine
channel = odbcDriverConnect("driver=SQL Server;server=(local);database=COE;uid=big;pwd=edwisor")

# extract data from database
data = sqlFetch(channel, "data_Movie")

#Extract data based on condition 
data_Q1 = sqlQuery(channel, 'select * from Emp where EmpId > 1')

##Let's connect to twitter
library(twitteR)
library(RCurl)
library(ROAuth)

#Connect R to twitter. Login to twitter account and creat app to get key and tokens
#https://apps.twitter.com/
api_key = "ihQlwgbHg3VkiQ89GOADr3Ime"
api_secret = "t7XSE1WcPIyXqVZ4H6qTXVz18h3IqljMn8Sb69M8wnltCGGnip"
access_token = "415259154-BRQUbCQQlMXy57yLmUnOJl0S9p4q0PhgiE4vNK83"
access_token_secret = "J9YgUJtjH1OxulznGLMGyVGaK4orj4SJ2QGjagyomNTUl"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret) #Press 0

#establish Connection
tweets = searchTwitter("education", since = "2016-06-01", until = "2016-07-31", n = 1500)
#tweets = searchTwitter('education', n = 1000, retryOnRateLimit = 1)

#Convert tweets list into data frame
tweets = twListToDF(tweets)

#Wrtite back tweets
write.csv(tweets, "tweets_Mar17-Mar30.csv", row.names = F)

#connect to facebook
library(Rfacebook)

#generic functions 
library("ggplot2")
library("scales")
library("corrplot")
library("psych")
library("gplots")
library("vcd")

german = read.csv("German.csv", header = T)

#Bar plot(categorical data)
#If you want count then stat="bin"
bar = function(data, xaxis, yaxis, xlabel, title){
      plot = ggplot(data, aes_string(x = xaxis, y = yaxis)) +
      geom_bar(stat = "identity", fill = "DarkGoldenRod") + theme_bw() +
      xlab(xlabel) + scale_y_continuous(breaks = pretty_breaks(n = 10)) +
      ggtitle(title) +  theme(text = element_text(size = 20))
  
  return(plot)
}

bar(german,"ResidenceDuration","Amount","Installement rate",
    "German: Installement Rate")

bar(german, "InstallmentRatePercentage", "Amount", "Percentage", "Bar plot")

#sort function
sort_fn = function(df, var){
          df = df[order(-df[[var]]),]
      return(df)
} 

german = sort_fn(german, "InstallmentRatePercentage")
german = sort_fn(german, "ResidenceDuration")

###loops
# Lets define vector
Player1 = c(16, 9, 13, 5, 2, 17, 14)

for (i in Player1) {
  print(i)
}

#use if condition in for loop
for(i in 1:nrow(german)){
  if(german$ResidenceDuration[i] == 4){
    german$newcol[i] = 1
  } else {german$newcol[i] = 0}
}

#ifelse condition
german$installement = ifelse(german$InstallmentRatePercentage == 4, 1, 0)

###Apply family
#apply function
apply(german[,-10], 2, mean)
apply(german[1:10, 1:5], 1, mean)

apply(german[,1:5], 2, sd)
apply(german[,1:5], 2, var)

german[2,4] = NA
df = data.frame(apply(german, 2, function(x){sum(is.na(x))}))

#create your own function
m = matrix(data = cbind(rnorm(30, 0), rnorm(30, 2), rnorm(30, 5)), nrow = 30,
           ncol = 3)
apply(m, 2, function(x) length(x[x<0]))

#lapply
lapply(german, mean)

#sapply
# Find the square of each number using sapply and lapply
temp = c(10, 20, 30, 40, 50)

sapply(temp, function(x){x * x}) #return vector
lapply(temp, function(x){x * x}) #return list

unlist(lapply(temp, function(x){x * x}))

#grouping of data /summarizing
library(sqldf)
df = sqldf("select InstallmentRatePercentage, sum(Amount) as Total from german group by
           InstallmentRatePercentage")

df = aggregate(german, by = list(german$InstallmentRatePercentage), FUN = mean,
               na.rm=TRUE)

library(plyr)
aggdat1 <- ddply(german, .(InstallmentRatePercentage), summarize,
                 Average = mean(Amount))


##trycatch
#reshape2
library(reshape2)
names(airquality) = tolower(names(airquality))

#melt data by a variable
aqm = melt(airquality, id = c("month", "day"), na.rm = TRUE)

#cast data
#df = data.frame(acast(aqm, day ~ month ~ variable))
df = data.frame(acast(aqm, month ~ variable, mean))
acast(aqm, month ~ variable, mean, margins = TRUE)
dcast(aqm, month ~ variable, mean, margins = c("month", "variable"))

##code performance
S1 = Sys.time()
S2 = Sys.time()

S2-S1


