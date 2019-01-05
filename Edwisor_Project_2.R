# removing previously loaded objects
rm(list =ls())
getwd()

## Setting Working directory
setwd('E:/DS Courses/Edwisor/Project 2')
day = read.csv("day.csv")
data = day
head(data)
summary(data)
dim(data)
names(data)
is.null(data)
is.integer(data)
data = data.frame(data)
str(data)


library(dplyr)
#install.packages('corrplot')
library(corrplot)
library(ggplot2)
library(stats)

data$season = factor(format(data$season, format="%A"),
              levels = c("1", "2","3","4") , labels = c("Spring","Summer","Fall","Winter"))
table(data$season)
data$holiday = factor(format(data$holiday, format="%A"),
                           levels = c("0", "1") , labels = c("generic","Holiday"))
table(data$holiday)


data$weathersit = factor(format(data$weathersit, format="%A"),
                              levels = c("1", "2","3","4") , 
                              labels = c("Pleasant","Moderate","Bad","Extreme"))
table(data$weathersit)

data$yr = factor(format(data$yr, format="%A"),
                      levels = c("0", "1") , labels = c("2011","2012"))
table(data$yr)

data$actual_temp = data$temp*41
data$actual_feel_temp = data$atemp*50
data$actual_windspeed = data$windspeed*67
data$actual_humidity <- data$hum*100
data$mean_acttemp_feeltemp <- (data$actual_temp+data$actual_feel_temp)/2
str(data)

summary(data)



## EXPLORATORY ANALYSIS
# Setting the margins to fit the plot
#par(mar = rep(2, 4))
par(mar=c(1,1,1,1))

# Distributio of the target variable
h = hist(data$cnt, breaks = 25, ylab = 'Frequency of Rental', xlab = 'Total Bike Rental Count', main = 'Distribution of Total Bike Rental Count', col = 'blue')

xfit = seq(min(data$cnt),max(data$cnt), length = 50)
yfit = dnorm(xfit, mean =mean(data$cnt),sd=sd(data$cnt))
yfit = yfit*diff(h$mids[1:2])*length(data$cnt)
lines(xfit,yfit, col='red', lwd= 3)

#par(mfcol=c(2,2))

boxplot(data$cnt ~ data$season,
        data = data,
        main = "Total Bike Rentals Vs Season",
        xlab = "Season",
        ylab = "Total Bike Rentals",
        col = c("coral", "coral1", "coral2", "coral3")) 



boxplot(data$cnt ~ data$holiday,
        data = data,
        main = "Total Bike Rentals Vs Holiday/Working Day(generic)",
        xlab = "Holiday/Working Day(generic)",
        ylab = "Total Bike Rentals",
        col = c("pink", "pink1", "pink2", "pink3")) 

boxplot(data$cnt ~ data$weathersit,
        data = data,
        main = "Total Bike Rentals Vs Weather Situation",
        xlab = "Weather Situation",
        ylab = "Total Bike Rentals",
        col = c("purple", "purple1", "purple2", "purple3")) 


plot(data$dteday, data$cnt,type = "p",
     main = "Total Bike Rentals Vs DateDay",
     xlab = "Year",
     ylab = "Total Bike Rentals",
     col  = "orange",
     pch  = 19)

#par(mfrow=c(2,2))

plot(data$actual_temp, data$cnt ,type = 'h', col= 'yellow', xlab = 'Actual Temperature', ylab = 'Total Bike Rentals')

plot(data$actual_feel_temp, data$cnt ,type = 'h', col= 'yellow', xlab = 'Actual Feel Temperature', ylab = 'Total Bike Rentals')

plot(data$actual_windspeed, data$cnt ,type = 'h', col= 'yellow', xlab = 'Actual Windspeed', ylab = 'Total Bike Rentals')

plot(data$actual_humidity, data$cnt ,type = 'h', col= 'yellow', xlab = 'Actual Humidity', ylab = 'Total Bike Rentals')

###############   CORRELATION PLOT    ###############
Cor_actual_temp = cor(x = data$actual_temp, y = data$cnt)
Cor_actual_feel_temp = cor(x = data$actual_feel_temp, y =data$cnt)
data_cor = data %>% select (cnt,actual_temp,actual_feel_temp,mean_acttemp_feeltemp,actual_humidity,actual_windspeed)
data_cor = data.frame(data_cor)

colnames(data_cor)[1] = "Total Number of Bike Rentals"
colnames(data_cor)[2] = "Temperature"
colnames(data_cor)[3] = "Feel Temperature"
colnames(data_cor)[4] = "Mean Actual Temp Feel Temp"
colnames(data_cor)[5] = "Humidity"
colnames(data_cor)[6] = "Windspeed"

cor(data_cor)
corplot_data = cor(data_cor)
corplot_data
library(corrgram)
corrgram(corplot_data, order = F,
         upper.panel = panel.pie, text.panel = panel.txt, main = 'correlation Plot')

######## Scatter Plot between Bike Rentals and Actual Temperature  ########

ggplot_Temp_Rent = ggplot(data, aes(x=data$actual_temp,y=data$cnt))+geom_point(shape=1)+geom_smooth(method=lm)+ xlab("Actual Temp. in Celcius")+ylab("Bike Rentals")
ggplot_Temp_Rent+scale_y_continuous(breaks=c(0,1100,2345,3500,5000,6000,7000,8000))+labs(title="Total Bike Rentals Vs Actual Temperature | Intercept = 2345")

#*** OLS Regression ***
lm_test<- lm(data$cnt~data$actual_temp)
summary(lm_test)
plot(lm_test, col = "green")

lm_test1<- lm(sqrt(data$cnt)~data$actual_temp+data$actual_humidity+data$actual_windspeed)

lm_test1
summary(lm_test1)
plot(lm_test1, col = "red")

lm_test2<- lm(((data$cnt)^2)~data$actual_temp+data$actual_humidity+data$actual_windspeed)

lm_test2
summary(lm_test2)
plot(lm_test2, col = "gold")

lm_test3<- lm((log(data$cnt))~data$actual_temp+data$actual_humidity+data$actual_windspeed)

lm_test3
summary(lm_test3)
plot(lm_test3, col = "green")

lm_final<- lm(data$cnt~data$actual_temp+data$actual_humidity+data$actual_windspeed)

lm_final
summary(lm_final)
plot(lm_final,col = "blue", main = "Linear Regression: Bike Rentals, Temp, Windspeed and Humidity")

df$dteday

#########################################################################
df  = subset(data, select = -c(instant,temp,atemp,actual_feel_temp,actual_temp,casual,registered,hum,windspeed))

x = df$dteday
as.numeric(factor(substr(x,9,10)))
df$dteday = as.numeric(factor(substr(df$dteday,9,10)))
df$mnth = as.numeric(factor(df$mnth,
                             levels = c(1:12),
                             labels = c(1:12)))
df$season = as.numeric(factor(df$season,
                               levels = c(1:4),
                               labels = c(1:4)))
df$yr = as.numeric(factor(df$yr,
                           levels = c('2011','2012'),
                           labels = c(1,2)))
df$holiday = as.numeric(factor(df$holiday,
                                levels = c('generic','Holiday'),
                                labels = c(1,2)))
df$weekday = as.numeric(factor(df$weekday,
                                levels = c(0:6),
                                labels = c(1:7)))
df$workingday = as.numeric(factor(df$workingday,
                                   levels = c(0,1),
                                   labels = c(1:2)))
df$weathersit = as.numeric(factor(df$weathersit,
                                   levels = c('Pleasant','Moderate','Bad','Extreme'),
                                   labels = c(1:4)))

str(df)


library(randomForest)
library(caTools)
library(RRF)
library(inTrees)
library(caret)
#####
split = sample.split(df$cnt,SplitRatio = 0.8)
tr_d = subset(df, split == TRUE)
ts_d = subset(df, split == FALSE)

regressor = randomForest(cnt~., data = tr_d, mtry = 6, importance= TRUE, ntree = 100)


# Rules extraction
# Transform RF to Tree format
treel  = RF2List(regressor)

#Extract Rules
exe = extractRules(treel, tr_d[,-9])
#Visualise rules
exe[1:2]
readb = presentRules(exe,colnames(tr_d))
readb[1:2]

#Get rule Metrics
ruleMetric = getRuleMetric(exe,tr_d[-9],tr_d$cnt)
ruleMetric[1:2,]

pr_cnt = predict(regressor, ts_d[,-9])

pr_cnt

importance(regressor)
library(DMwR)
regr.eval(ts_d[,9], preds = pr_cnt, stats = c('mae','rmse','mape'))  

randomForest::importance(regressor)

reg_err  = resid(regressor)

par(mar=c(1,1,1,1))
plot(ts_d$cnt, reg_err,ylab="Bike count", xlab="Residuals",main="Old fitted Count") 
pr_cnt


##############################################
library('caret')
folds = createFolds(tr_d$cnt, k = 10)
cv = lapply(folds, function(x){
  tr_fold = tr_d[-x,]
  ts_fold = tr_d[x,]
  regressor = randomForest(cnt~., tr_fold, importance= TRUE, ntree = 100) 
  
  pre_loss = predict(regressor, ts_fold[-9])  
  
  error = mape(ts_fold[,9], pre_loss)
  return(error)
})
cv
error = mean(as.numeric(cv))
error
randomForest::importance(regressor)
##############################################

#install.packages('xgboost')
library(xgboost)
library(caTools)

dfx = df
str(dfx)


split = sample.split(dfx$cnt,SplitRatio = 0.8)
tr_d = subset(dfx, split == TRUE)
ts_d = subset(dfx, split == FALSE)

regxg= xgboost(data = as.matrix(tr_d[-9]), label = tr_d$cnt, nrounds = 10)

prex_cnt = predict(regxg, as.matrix(ts_d[-9])) 

regr.eval(ts_d[,9], preds = prex_cnt, stats = c('mae','rmse','mape'))  

Accuracy = 1-mape(ts_d[,9], prex_cnt)
Accuracy

##############################################
folds = createFolds(tr_d$cnt, k = 10)
cv = lapply(folds, function(x){
  tr_fold = tr_d[-x,]
  ts_fold = tr_d[x,]
  regressor= xgboost(data = as.matrix(tr_d[-9]), label = tr_d$cnt, nrounds = 5)
  
  prex_cnt = predict(regressor, as.matrix(ts_fold[-9])) 
  
  error = mape(ts_fold[,9], prex_cnt)
  return(error)
})

error = mean(as.numeric(cv))
error
## Error  = 3.6122
Accuracy = 1-error
Accuracy

