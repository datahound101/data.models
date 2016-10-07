rm(list=ls())
setwd("G:/Analytics/Edwisor/Edwisor/Advanced Predictive Analytics/R Code/Code and Data")
german = read.csv("German.csv", header=T)

str(german)

#load libraries
library("ggplot2")
library("scales")
library("corrplot")
library("psych")
library("gplots")
library("vcd")

#Univariate 
#Bar plot(categorical data)
#If you want count then stat="bin"
ggplot(german, aes_string(x = german$InstallmentRatePercentage, y = german$Amount)) +
  geom_bar(stat="identity",fill= "DarkSlateBlue") + theme_bw() +
  xlab("Installement") + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("German Data") +  theme(text=element_text(size=15))

#Histogram 
ggplot(german, aes_string(x = german$Age)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Age") + ylab("Frequency") + ggtitle("German: Age") +
  theme(text=element_text(size=20))

# Box plot
german$InstallmentRatePercentage = as.factor(german$InstallmentRatePercentage)
ggplot(german, aes_string(x = german$InstallmentRatePercentage, y = german$Age, 
  fill = german$InstallmentRatePercentage)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 3) + 
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  guides(fill=FALSE) + theme_bw() + xlab("InstallmentRatePercentage") + 
  ggtitle("Outlier Analysis") +  
  theme(text=element_text(size=20))

# Multivariate 
# Scatter plot
# scatter function will first convert shape and colour attributes to factors 
german$InstallmentRatePercentage = as.factor(german$InstallmentRatePercentage)
german$NumberPeopleMaintenance = as.factor(german$NumberPeopleMaintenance)

ggplot(german, aes_string(x = german$Amount, y = german$Age)) + 
  geom_point(aes_string(colour = german$InstallmentRatePercentage),size=4) +
  theme_bw()+ ylab("Age") + xlab("Amount") + ggtitle("Main") + 
  theme(text=element_text(size=25)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_colour_discrete(name="ABC") + 
  scale_shape_discrete(name="CAD")


#correlation plots
#converting data into numeric   
data = german
data1 <- rep(1:nrow(data))
for(i in 1:ncol(data)){
    data1 <- data.frame(cbind(data1,as.numeric(data[,i])))
}
colnames(data1) <- c("abd", colnames(data)) #Changing colnames
data1 <- data1[,-1]
  
# Removing the variables whose variance is 0 (i.e. unique value in the variable)
german_sd <- data1[sapply(data1, function(x) length(levels(factor(x)))>1)]
german_sd <- german_sd[,1:10]
M <- cor(german_sd)
#cor.plot(M,numbers = TRUE, show.legend = TRUE, main = titleName)
plot = corrplot(M, title = "Correlation Plot", tl.cex=0.5, tl.col="black")
#plot = corrplot(M, order = "hclust", addrect = 2, 
#                title="Correlation", tl.cex=0.8, tl.col="black")
#cor.plot(M,numbers = TRUE, show.legend = TRUE, main = "asd")


