#Load the Dataset
data <- read.csv("C:/Users/dassa/OneDrive/Documents/R Project/Traffic_Accident_(1).csv",header =TRUE,check.names=TRUE,na.strings=c(""))
head(data,3)
dim(data)

#Summary of the Dataset
summary(data)

#Identifying the columns with missing values
colSums(is.na(data)==TRUE|data=='')

#The Number of Missing Values
library(ggplot2)
#gg_miss_var(data)

#The Precentage of missing values
#vis_miss(data)


#Impute Data
data$Accident_severity[is.na(data$Accident_severity)==TRUE] = round(mean(data$Accident_severity, na.rm = TRUE))
getmode <- function(a) {
  uniqage <- unique(a)
  uniqage[which.max(tabulate(match(a, uniqage)))]
}

data$Age_Categories[is.na(data$Age_Categories)==TRUE]= getmode(data$Age_Categories)
print( getmode(data$Age_Categories))

data$Educational_level[is.na(data$Educational_level)==TRUE]= getmode(data$Educational_level)
print( getmode(data$Educational_level))

data$Driving_experience[is.na(data$Driving_experience)==TRUE] = getmode(data$Driving_experience)
print(getmode(data$Driving_experience))

data$Lanes_or_Medians[is.na(data$Lanes_or_Medians)==TRUE] = getmode(data$Lanes_or_Medians)
print(getmode(data$Lanes_or_Medians))

data$Road_surface_type[is.na(data$Road_surface_type)==TRUE]= "Unknown"
data$Type_of_collision[is.na(data$Type_of_collision)==TRUE]= "Unknown"
data$Cause_of_accident[is.na(data$Cause_of_accident)==TRUE]= "Unknown"
data$Types_of_Junction [is.na(data$Types_of_Junction )==TRUE]= "Unknown"
data$Vehicle_movement[is.na(data$Vehicle_movement)==TRUE]= "Unknown"
data$Vehicle_driver_relation[is.na(data$Vehicle_driver_relation)==TRUE]= "Unknown"
data$Weather_conditions[is.na(data$Weather_conditions)==TRUE]= "Unknown"
data$Sex_of_driver[is.na(data$Sex_of_driver)==TRUE]= "Unknown"

#vis_miss(data)
colSums(is.na(data)==TRUE|data=='')


# Data Anaylysis
# Accident Severity and Type of Collision
ggplot(data) +
  geom_bar(aes(x = Accident_severity, fill = Type_of_collision)) +
  xlab("Severity of Accidents") + ylab("Number of Accidents")


#Causes of Accident
p_causes <- ggplot(data) +
  geom_bar(aes(x = Cause_of_accident), position = "dodge", width = 0.2) +
  xlab("Causes of Accidents") + ylab("Number of Accidents")
p_causes + coord_flip()

#Accident Severity and Gender
ggplot(data, aes(x=Accident_severity, fill=Sex_of_driver )) + 
  geom_bar(position = "dodge") + 
  labs(x="Severity of Accidents",y="Number of Accidents")

#Accident Severity and Age
ggplot(data, aes(x=Accident_severity, fill=Age_band_of_driver )) + 
  geom_bar(position = "dodge") + 
  labs(x="Severity of Accidents",y="Number of Accidents")

#Accident Severity and Experience
ggplot(data, aes(x=Accident_severity, fill=Driving_experience )) + 
  geom_bar(position = "dodge") + 
  labs(x="Severity of Accidents",y="Number of Accidents")

#Accident Severity and Junction
ggplot(data) +
  geom_bar(aes(x = Accident_severity, fill = Types_of_Junction)) +
  xlab("Severity of Accidents") + ylab("Number of Accidents")

#Weather Conditions
p_weather <- ggplot(data) +
  geom_bar(aes(x =Weather_conditions , fill = Weather_conditions), width = 0.5) +
  xlab("Weather Condistions") + ylab("Number of Accidents")
p_weather + coord_flip()

#Type of Roads
p_road <- ggplot(data) +
  geom_bar(aes(x = Road_surface_type, fill=Road_surface_type), position = "dodge", width = 0.5) +
  xlab("Type of Roads Surface") + ylab("Number of Accidents")
p_road + coord_flip()

#Accident Severity and Light Conditions
library(ggmosaic)
ggplot(data) + geom_mosaic(aes(x = product(Light_conditions, Accident_severity), fill = Light_conditions)) + 
  xlab("Severity of Accidents") + ylab("Light Conditions")


#Machine Learning
library(lattice)
library(caret)
data$Accident_severity <- as.factor(data$Accident_severity)
str(data)


#Random Forest
library(randomForest)
RF<-function(s, df, col) {
  trainIndex<-createDataPartition(col, p=s, list=F)
  data_train<-df[trainIndex,]
  data_test<-df[-trainIndex,]           
  model <- randomForest(Accident_severity~., data=data_train)
  
  # make predictions
  x_test <- data_test[,1:14]
  y_test <- data_test[,15]
  predictions <- predict(model, x_test)
  cm<-confusionMatrix(predictions, y_test)
  return(cm)
}

#Train & Test Split
split<-0.70  
result4<-RF(split, data, data$Accident_severity)
result4

#k-Fold
train_control <- trainControl(method="cv", number=5)
modelRF <- train(Accident_severity~., data=data, trControl=train_control, method="rf")
modelRF

#KNN
KNN<-function(s, df, col) {
  trainIndex<-createDataPartition(col, p=s, list=F)
  data_train<-df[trainIndex,]
  data_test<-df[-trainIndex,]           
  model <- train(Accident_severity~., data=data_train, method = "knn")
  
  # make predictions
  x_test <- data_test[,1:14]
  y_test <- data_test[,15]
  predictions <- predict(model, x_test)
  cm<-confusionMatrix(predictions, y_test)
  return(list(model, cm))
}

##Train & Test Split
split<-0.70  
result6<-KNN(split, data, data$Accident_severity)
result6

#K-Fold
train_control <- trainControl(method="cv", number=5)
modelknn <- train(Accident_severity~., data=data, trControl=train_control, method="knn")
modelknn

