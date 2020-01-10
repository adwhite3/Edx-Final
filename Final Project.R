if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate",repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

url<-"https://github.com/adwhite3/Edx-Final/blob/master/weatherAUS.csv?raw=TRUE"
data<-read_csv(url)
#I will be making a classification tree to predict if there will or will not be precipitation in austrailia
#Data ranges from 2007-2017
#First I remove RISK_MM column because it is future information that will interfere with the prediction.
data$RISK_MM<-NULL
#Begin looking at data
summary(data)
head(data)
dim(data)
#I will check what percent of each column is NA
colSums(is.na(data))/nrow(data)*100
#Evaporation, Sunshine, and both Cloud columns have too many NA's to be useful predictors so I can remove them
drops<-c("Evaporation","Sunshine","Cloud9am","Cloud3pm")
data<-data[,!(names(data)%in%drops)]
rm(drops)
na.exclude(data)%>%dim()
data<-na.exclude(data)
#I will change RainTomorrow and RainToday from character class to factor so it is easier to sort them and use a decision tree on them
data$RainTomorrow<-as.factor(data$RainTomorrow)
data$RainToday<-as.factor(data$RainToday)
nrow(data)
#Over 100.000 observations is still plenty of data for predicting so I can remove all other NA's without impacting the results significantly

#Now I can split the data in test and training sets
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = data$RainTomorrow, times = 1, p = 0.1, list = FALSE)
train<- data[-test_index,]
temp <- data[test_index,]

validation <- temp %>% 
  semi_join(train, by = "RainTomorrow") %>%
  semi_join(train, by = "RainToday") %>%
  semi_join(train, by = "Humidity3pm")%>%
  semi_join(train, by = "Pressure3pm")%>%
  semi_join(train, by = "Date")

# Add rows removed from validation set back into data set

removed <- anti_join(temp, validation)
data <- rbind(data, removed)
rm(url, removed, temp, test_index)

#Time for Data Exploration

#Look at number of cities and date range of the data
range(train$Date)
length(unique(train$Location))
#There is data from 44 Australian cities from 2007-2017
sort(table(train$Location),decreasing =TRUE)
#Next I will figure out which data are the best predictors for Rain Tomorrow and remove the irrelevant columns
train%>%ggplot(aes(RainToday, fill= RainTomorrow))+
  geom_bar(position = "dodge")+ 
  scale_y_continuous(labels = function(x) paste0(round(x/nrow(train)*100,1),"%"),breaks = seq(0,nrow(train),nrow(train)*.10,))

#When it rains today, it is far more likely to rain tomorrow. In almost half of all occurences when it Rained Today, it Rained Tomorrow.
monthly_rain_tomorrow<-train%>%filter(month(Date),RainTomorrow=="Yes")%>%count(month(Date))
monthly_days<-count(train,month(train$Date))
pct_rain_tomorrow_monthly<-data_frame("Month"=factor(month.abb,levels = month.abb),"Percent_Rain_Tomorrow"=monthly_rain_tomorrow$n/monthly_days$n*100)
#Look at a histogram of the monthly average rainfall relative to the average rainfall which is represented by the horizontal line
pct_rain_tomorrow_monthly%>%ggplot(aes(Month, Percent_Rain_Tomorrow))+
  geom_col()+
  geom_hline(yintercept = mean(pct_rain_tomorrow_monthly$Percent_Rain_Tomorrow))+
  geom_text(aes(2,mean(pct_rain_tomorrow_monthly$Percent_Rain_Tomorrow), label= "Average", vjust=-.5))
#The Australian rainy season is from June to August

#Does the amount of rain today predict the likelihood of rain tomorrow?
#A few extreme days with lots of rain will make it difficult to visualize a density plot so by checking the standard deviation of days with rain (y) I can tell how to limit the x-axis
tail(sort(train$Rainfall),n=10)
y<-filter(train,train$Rainfall>=0.1)
n<-filter(train,train$Rainfall==0)
sd(y$Rainfall)

ggplot(train,aes(Rainfall))+
  geom_density()+
  scale_x_continuous(limits = c(0.1,15))
#The density plot shows that the majority of days with rain get under 13mm of rain
train %>%
  filter(Rainfall > 0.1) %>% 
  ggplot(aes(x = Rainfall, fill = RainTomorrow, color = RainTomorrow)) +
  geom_histogram(aes(y = ((..count..) / sum(..count..))), position = "identity", alpha = 0.5) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.02),
                     labels = scales::percent)+
  labs(x="Rainfall in mm", y="Percent of Rainy Days")
#After a certain amount of rainfall, it is more likely to rain tomorrow, so I will zoom in to figure out the cutoff
train %>%
  filter(Rainfall > 1 & Rainfall < 15) %>% 
  ggplot(aes(x = Rainfall, fill = RainTomorrow, color = RainTomorrow)) +
  geom_histogram(aes(y = ((..count..) / sum(..count..))), position = "identity", alpha = 0.5, binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 15, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.02),
                     labels = scales::percent)+
  labs(x="Rainfall in mm", y="Percent of Rainy Days")
#After 8mm of rain it is more likely to rain the next day than not rain
#Check if Pressure at 3pm is a good predictor
n%>%ggplot(aes(x=RainTomorrow, y=Pressure3pm, fill= RainTomorrow)) + 
  geom_violin()+
  geom_hline(yintercept = 1012)+
  geom_text(aes(.85,1012), label= "1012hpa", vjust=-.5)
#Below 1012hpa, it is more likely to rain tomorrow
#Time to check if Humidity at 3pm is a good predictor of rainfall
train%>%ggplot(aes(x=RainTomorrow, y=Humidity3pm, fill= RainTomorrow)) + 
  geom_violin()+
  geom_hline(yintercept = 67)+
  geom_text(aes(.65,67), label= "67%", vjust=-.5)

#Rain today, months June to August, more than 8mm of rainfall, pressure at 3pm below 1012hpa, and humidity at 3pm above 67% are the best predictors
#Now to create the prediction model with these predictors
predictors<- c("Date","Rainfall", "Humidity3pm","Pressure3pm", "RainToday","RainTomorrow")
train<-train[,(names(train)%in%predictors)]
Decision_Tree<-rpart(RainTomorrow~ RainToday + Date + Humidity3pm + Pressure3pm, data = train, method = 'class')

#I can visualize the tree to see where it makes the breaks
prp(Decision_Tree, type = 2, extra = "auto", branch = 1)
#The rpart function automatically chose the predictors Humidity and Rain Today

prediction_class_1<-predict(Decision_Tree, validation, type = "class")
confusionMatrix(prediction_class_1,validation$RainTomorrow,positive = "Yes")
#The decision tree model has an 83.83% accuracy which is pretty good. 
#However, the Sensitivity is 42.7% which is a bit low and the Balanced Accuracy is only 69.05%
#But by using Random Forrests I can improve the balanced accuracy and sensitivity
#First I create a randomForest with the selected predictors and look at how many trees I need
rforest<-randomForest(RainTomorrow~ RainToday + Date + Humidity3pm + Pressure3pm, data = train, ntree=200)
plot(rforest)
#The error stops changing significantly around 50 trees so I can limit the forest to 50 trees to save computing power
rforest<-randomForest(RainTomorrow~ RainToday + Date + Humidity3pm + Pressure3pm, data = train, ntree=50)
#Now I can test to see if the randomForest improves the prediction of Rain Tomorrow
prediction_class_2<-predict(rforest,validation, type = "class")
confusionMatrix(prediction_class_2,validation$RainTomorrow, positive = "Yes")
#With RandomForest, our new accuracy decreased slightly by 0.3% to 83.44%.
#However, the Sensitivity rose 4.7% to 47.47% and the Balanced Accuracy rose 1.52% to 70.57%

#Conclusion,
#The Random Forest method slightly improved the balanced accuracy of the decision tree by raising the sensitivity
#However the Specificity dropped 1.74% which explains why the balanced accuracy did not increase further. However with a specificity above 90% I think improving the low Sensitivity is worth the trade-off.
#one area of future research is altering the node size of the random forest to get a better accuracy, my computer just kept freezing everytime I tried so with more computing power, perhaps a high accuracy could be achieved.

