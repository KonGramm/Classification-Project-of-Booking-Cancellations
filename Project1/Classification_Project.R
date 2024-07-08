#------------------------- Msc In Statistics AUEB ---------------------------
#
# Purpose: Classification Project in Course Statistical Machine Learning. 
# In this project, we address the challenge of predicting cancellations of hotel bookings 
# using a dataset that includes plenty of variables such as booking ID, number of guests, 
# stay duration, meal type, room type, market segment, lead time and other more.
# 
# In the code below first we did some exploratory analysis 
# and then we created  models/algorithms to classify whether a booking will be cancelled or not.
# 
# We applied and compared six different models/algorithms which are a Logistic Regression model, 
# Decision Tree, Random Forests, the Naïve Bayes classifier, the Linear Discriminant Analysis method 
# and finally Support Vector Machines.
#
# in order to run this code you have to download the classification project xlsx file and store it 
# in a folder in your PC. Then you have to assign to the variable named path the path of this file.
# 
# Author: Konstantinos Grammenos
# 
# Date: 17/04/2024
#
# Proffesor:D.Karlis



#Import all the necessary libraries for our code
library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)
library(glmnet)
library(caret)
library(verification)
library(knitr)
library(ROCR)
library(stats)
library(verification)
library(car)
library(corrgram)
library(nnet)
library(class)
library(sda)
library(tree)
library(MASS)
library(pgmm)
library(klaR)
library(scoring)
library(nnet)
library(class)
library(e1071)
library(randomForest)
library(caTools)
library(rpart)
library(rattle)
library(rpart.plot)

path <- "C:\\Users\\kosti\\OneDrive\\Desktop\\Statistical Machine Learning\\Project1\\classification project.xlsx"
data <- read_excel(path, sheet = "project2")

head(data)
summary(data)


#---------------------------- EXPLORATORY DATA ANALYSIS -----------------------------

#we do not have any NAs values
colSums(is.na(data))
classes <- sapply(data, class)
print(classes)



#transform the average price from characther to numeric
data$average.price <- as.numeric(data$average.price)

#we observed that some values of some categorical only appear very few times
#so we combined them, we did it because there was also a problem with the split
#some levels were in test set and they were not in the train set
data$type.of.meal[data$type.of.meal %in% c("Meal Plan 3", "Not Selected")] <- "Other"

data$market.segment.type[data$market.segment.type%in% c("Aviation", "Complementary","Corporate")] <- "Other"

data$room.type[data$room.type%in% c("Room_Type 3", "Room_Type 5","Room_Type 6","Room_Type 7")] <- "Other_Type"


#transform the columns which were of class characther to factors
characther_columns <- c('type.of.meal', 'room.type', 'market.segment.type')
data[characther_columns] <- lapply(data[characther_columns], factor)
data$type.of.meal
data$room.type
data$market.segment.type
data$car.parking.space = factor(data$car.parking.space, labels = c("no","yes"))
data$car.parking.space
data$repeated = factor(data$repeated, labels = c("no","yes"))
data$repeated
data$total.nights <- data$number.of.weekend.nights + data$number.of.week.nights
data$booking.status.encoded=ifelse(data$booking.status=="Not_Canceled",0,1)

#see the class  of each variable
classes <- sapply(data, class)
print(classes)

#make the data a dataframe
data <- as.data.frame(data)

#---------------BARPLOTS ------------------------------------------------------


par(mfrow=c(1,1))

library(patchwork)
library(gridExtra)
#Barplot of the room type
room_type_table <- as.data.frame(table(data$room.type))
colnames(room_type_table) <- c("room.type", "count")
room_type_table$room.type <- factor(room_type_table$room.type,
                                    levels = c("Other_Type", "Room_Type 1", "Room_Type 2", "Room_Type 4"),
                                    labels = c("Other", "Type 1", "Type 2", "Type 4"))

p1 <- ggplot(room_type_table, aes(x = room.type, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue3") +
  labs(title = "Type of room selected", x = "Room Type", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))

# Barplot of the market segment
market_segment_type_table <- as.data.frame(table(data$market.segment.type))
colnames(market_segment_type_table) <- c("market.segment.type", "count")

p2 <- ggplot(market_segment_type_table, aes(x = market.segment.type, y = count)) +
  geom_bar(stat = "identity", fill = "aquamarine4") +
  labs(title = "Type of market segment type", x = "Market Segment Type", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))

# Barplot for the type of meal
data$type.of.meal <- factor(data$type.of.meal,
                            levels = c("Meal Plan 1", "Meal Plan 2", "Other"),
                            labels = c("Plan 1", "Plan 2", "Other"))

p3 <- ggplot(data, aes(x = type.of.meal)) + 
  geom_bar(fill = "darkslategray3") + 
  labs(title = "Type of meal selected ", x = "Type of Meal", y = "Frequency") + 
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))

# Barplot for special requests
p4 <- ggplot(data, aes(x = special.requests)) + 
  geom_bar(fill = "darkmagenta", color = "black") + 
  labs(title = "Number of Special Requests", x = "Special Requests", y = "Frequency") + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))

# Arrange plots in a grid
grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

#barplot(table(data$market.segment.type),col="purple",main="Type of the market segment type selected",las=2,cex.names=0.7)


# Barplot of the Number of week nights
p5 <- ggplot(data, aes(x = number.of.week.nights)) + 
  geom_bar(fill = "coral") + 
  labs(title = "Number of week nights", x = "Week Nights", y = "Frequency") + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))+
  ylim(0, 800)

#Barplot of the Number of weekend nights
p6 <- ggplot(data, aes(x = number.of.weekend.nights)) + 
  geom_bar(fill = "darkmagenta") + 
  labs(title = "Number of weekend nights", x = "Weekend Nights", y = "Frequency") + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))+
  ylim(0, 1000)

#barplot(table(data$total.nights),col="lightgreen",main="Number of total nights of stay in the hotel",ylim=c(0,700))



p7 <- ggplot(data, aes(x = number.of.children)) + 
  geom_bar(fill = "aquamarine4", color = "black") +
  labs(title = "Number of Children", x = "Children", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))+
  ylim(0, 2000) 



p8 <- ggplot(data, aes(x = number.of.adults)) + 
  geom_bar(fill = "skyblue4", color = "black") + 
  labs(title = "Number of Adults", x = "Adults", y = "Frequency") + 
  theme_minimal()+  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14))+
  ylim(0, 1500)


grid.arrange(p5, p6, p7,p8, nrow = 2,ncol=2)

library(RColorBrewer)
colors <- c( "#C70039","skyblue4")
# Barplot for booking.status
booking_status_table <- table(data$booking.status)
booking_status_df <- as.data.frame(booking_status_table)
booking_status_df$Percentage <- (booking_status_df$Freq / sum(booking_status_df$Freq)) * 100


p9 <- ggplot(booking_status_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = colors) + 
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5, size = 5) +
  labs(title = "Booking status", x = "Category", y = "Count") +
  ylim(0, max(booking_status_df$Freq) * 1.1) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )



repeated_table <- table(data$repeated)
repeated_df <- as.data.frame(repeated_table)

repeated_df$Percentage <- (repeated_df$Freq / sum(repeated_df$Freq)) * 100


# barplot of if a booking is repeated or not 
p10 <- ggplot(repeated_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = colors) + 
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5, size = 5) +
  labs(title = "Repeated Booking", x = "Category", y = "Count") +
  ylim(0, max(repeated_df$Freq) * 1.1) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )


parking_table <- table(data$car.parking.space)
parking_df <- as.data.frame(parking_table)

parking_df$Percentage <- (parking_df$Freq / sum(parking_df$Freq)) * 100


p11 <- ggplot(parking_df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = colors) + 
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5, size = 5) +
  labs(title = "Car Parking Space", x = "Category", y = "Count") +
  ylim(0, max(parking_df$Freq) * 1.1) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )


grid.arrange(p9,p10,p11, nrow = 2,ncol=2)



#--------------some more plots----------------------------

p13 <- ggplot(data, aes(x = lead.time)) + 
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black") + 
  labs(title = "Histogram of Lead Time", x = "Lead Time", y = "Frequency") + 
  theme_minimal() +
  theme(
    text = element_text(size = 16), 
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )



#boxplot of the lead time
#lead time is the # of days between the booking date and the arrival date
p14 <- ggplot(data, aes(y = lead.time)) + 
  geom_boxplot(fill = "lightblue", color = "black") + 
  labs(title = "Boxplot of Lead Time", y = "Lead Time") + 
  theme_minimal() +
  theme(
    text = element_text(size = 16), 
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

p15 <- ggplot(data, aes(x = average.price)) + 
  geom_histogram(binwidth = 10, fill = "aquamarine4", color = "black") + 
  labs(title = "Histogram of Lead Time", x = "Average Booking Price", y = "Frequency") + 
  theme_minimal() +
  theme(
    text = element_text(size = 16), 
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )


#boxplot of the Average Booking Price
p16 <- ggplot(data, aes(y = average.price)) + 
  geom_boxplot(fill = "darkseagreen4", color = "black") + 
  labs(title = "Boxplot of Average Booking Price", y = "Average Booking Price") + 
  theme_minimal() +
  theme(
    text = element_text(size = 16), 
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

grid.arrange(p13,p14,p15,p16, nrow = 2,ncol=2)


par(mfrow=c(1,1))
# only the numeric columns
numeric_columns <- select_if(data, is.numeric)

#the correlation matrix
## Calculate pairwise correlations
correlation_matrix <- cor(numeric_columns)

#Create the correlation plot
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, tl.cex =0.9, cl.cex = 0.9, addCoef.col = "black",
         number.cex = 0.9)

#boxplot of average price by market segment type

p17 <- ggplot(data, aes(x = market.segment.type, y = average.price, fill = factor(booking.status.encoded))) +
  geom_boxplot() +
  labs(title = "Boxplot of Avg Price by Market Segment",
       x = "Market Segment Type",
       y = "Average Price",
       fill = "Booking Status") +
  theme_light() + theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18)
  )




p18 <- ggplot(data, aes(x = booking.status.encoded, y = lead.time, fill = factor(booking.status.encoded))) +
  geom_boxplot() +
  labs(title = "Lead Time and Cancellation",
       x = "Booking Status", 
       y = "Lead Time (days)", 
       fill = "Booking Status") +
  scale_fill_manual(values = c("0" = "turquoise", "1" = "purple")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )


data_summary <- data %>%
  group_by(special.requests, booking.status.encoded) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))

p19 <- ggplot(data_summary, aes(x = special.requests, y = freq, fill = factor(booking.status.encoded))) +
  geom_bar(stat = "identity", position = "fill") + 
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("0" = "skyblue3", "1" = "darkmagenta")) + 
  labs(x = "Number of Special Requests", y = "Percentage", fill = "Booking Status") +
  theme_minimal()+  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )


data_summary <- data %>%
  group_by(number.of.adults, booking.status.encoded) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))

p20<- ggplot(data_summary, aes(x = number.of.adults, y = freq, fill = factor(booking.status.encoded))) +
  geom_bar(stat = "identity", position = "fill") + 
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("0" = "antiquewhite3", "1" = "aquamarine3")) + 
  labs(x = "Number of Adults", y = "Percentage", fill = "Booking Status") +
  theme_minimal()+theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )

grid.arrange(p17,p18,p19,p20, nrow = 2,ncol=2)


#-------------------- CLASSIFICATION ALGORITHMS ---------------------------



#---------------splitting dataset into training and testing data--------------
#The data was Randomly splitted to training (80%) and testing (20%) datasets

#we throw away the booking id , the date and the new column the total nights that we created
data <- data[,-c(1,16, 17, 18)]
n=nrow(data)
#shuffled=data[sample(n),-c(16,17,18)]
#shuffled
#trainSet=shuffled[1:round(0.8 * n),]
#testSet = shuffled[(round(0.8 * n) + 1):n,]
#summary(trainSet)
#summary(testSet)

# 1600 rows
#dim(trainSet)
#400 rows
#dim(testSet)



#-------------------Split to train and test set 80-20---------------------------
num_train <- round(0.8 * n)  
ind <- sample(1:n, num_train, replace=FALSE)
trainSet <- data[ind,]
testSet <- data[-ind,]


#----------------------------LASSO ---------------------------------------------

x_train<- model.matrix(booking.status.encoded ~ lead.time + number.of.children + market.segment.type + repeated +
                             number.of.adults +type.of.meal+ number.of.weekend.nights+ number.of.week.nights + car.parking.space + P.C +P.not.C+
                             room.type + average.price + special.requests - 1, 
                           data = trainSet)  
y_train <- trainSet$booking.status.encoded

# cross-validation to find the optimal lambda 
lasso3 <- cv.glmnet(x_train, y_train, family = "binomial")

plot(lasso3)

best_lambda <- lasso3$lambda.min
best_lambda
lasso3$lambda.1se


#alpha=1 is for lasso
m1 <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = best_lambda)
summary(m1)

#the coefficients of the model
#if they have . this means that they were schrunk to zero
#for example number.of.children, market.segment.typeAviation, market.segment.typeComplementary,market.segment.typeCorporate      . 
#P.C and room.typeRoom_Type 3  they were schrunk to 0
coef(m1, s = best_lambda)



set.seed(123) 
ind <- sample(1:n, num_train, replace = FALSE)
trainSet <- data[ind,]
x_train <- model.matrix(booking.status.encoded ~ . - 1, data = trainSet)
lasso1 <- cv.glmnet(x_train, y_train, family = "binomial")
plot(lasso1)
best_lambda <- lasso1$lambda.min
lasso2 <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = best_lambda)
coef_lasso2 <- as.matrix(coef(lasso2,  best_lambda))
#if coefficient from lasso is zero then put a 1 otherwise put a 0
#so in the count we will have all the variables of the train data with 0 if their coeficient is not 0
# and with 1 if it is zero
count <- ifelse(coef_lasso2 != 0, 0, 1)

# repeat the lasso 100 times in order to see the coeficients of which variables shrunk to 0 the more times
for (i in 2:100) {  
  ind <- sample(1:n, num_train, replace = FALSE)
  trainSet <- data[ind,]
  x_train <- model.matrix(booking.status.encoded ~ . - 1, data = trainSet)  
  y_train <- trainSet$booking.status.encoded
  
  lasso1 <- cv.glmnet(x_train, y_train, family = "binomial")
  best_lambda <- lasso1$lambda.min
  m1_lasso <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = best_lambda)
  coef <- as.matrix(coef(m1_lasso, s = best_lambda))
  
  # update the coefficient each time again with the same logic
  coef_with_zero <- ifelse(coef != 0, 0, 1)
  count <- count + coef_with_zero
}

#here are the number of times each variable ' s coefficint shrank to 0
count
colnames(count)[colnames(count) == 's1'] <- 'count'
count


#--------------------LOGISTIC REGRESSION ---------------------------------------

#--------------------GLM MODEL ----------------------


#we took all the variables of the trainset, binomial because our response is 0 or 1
m1_glm <- glm(booking.status.encoded~ ., data = trainSet, family = binomial())
summary(m1_glm)




#-----------------FOR TRAIN SET ------------------------------

#relative frequencies of the booking status
table(trainSet$booking.status.encoded)

#predictions for the train data using the glm model
train_pred <- predict(m1_glm, newdata = trainSet, type = "response")
train_pred_class <- ifelse(train_pred > 0.5, 1, 0)
table(train_pred_class)
table(trainSet$booking.status.encoded)

#the confusion matrix
table <- table(Actual = trainSet$booking.status.encoded,Predicted = train_pred_class)
print(table)

confusionMatrix(table)

# ROC curve and AUC for Train and Test Data
pred_train <- prediction(train_pred, trainSet$booking.status.encoded)
perf_train <- performance(pred_train, "tpr", "fpr")
plot(perf_train, main = "", col = "blue4",cex.axis = 2, cex.lab = 1.5)
abline(0, 1, lty = 2, col = "gray")


#True Negative, False Negative,....
TN_train <- table[1,1]
FN_train <- table[2,1]
FP_train <- table[1,2]
TP_train <- table[2,2]

N_train <- sum(table[1,])
P_train <- sum(table[2,])

# Calculating Specificity and Sensitivity for train data
Specificity_train <- FP_train / N_train
Sensitivity_train <- TP_train / P_train
df_train <- data.frame(Specificity = Specificity_train, Sensitivity = Sensitivity_train)
print(kable(df_train))

#the accuracy of the train data
accuracy_train <- sum(diag(table)) / sum(table)
print(paste("Accuracy on training set: ", accuracy_train))



auc_train <- performance(pred_train, "auc")
auc_value_train <- auc_train@y.values[[1]]


auc_value_train<- round(auc_value_train, 2)

print(paste("AUC for train data is : ", auc_value_train))




#-----------------FOR TEST SET ------------------------------

#predictions for the test data using the glm model
test_pred <- predict(m1_glm, newdata = testSet, type = "response")
test_pred_class <- ifelse(test_pred > 0.5, 1, 0)  
table(test_pred_class)
table(testSet$booking.status.encoded)

#the confusion matrix with the TP,FN,TN,FP
# 0 | TN    FP, true negatives, false positives
# 1 |  FN    TP,false negatives,true positives
table1 <- table(Actual = testSet$booking.status.encoded,Predicted = test_pred_class)
print(table1)

confusionMatrix(table1)

TN <- table1[1,1]
FN <- table1[2,1]
FP <- table1[1,2]
TP <- table1[2,2]


N <- sum(table1[1,])
N
P <- sum(table1[2,])
P
Specificity <- FP/N
Specificity
Sensitivity <- TP/N
Sensitivity 
df <- data.frame(Specificity,Sensitivity)
kable(df)
pred <- prediction(test_pred, testSet$booking.status.encoded)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main = "", col = "red4",cex.axis = 2, cex.lab = 1.5)
abline(0, 1, lty = 2, col = "gray")
#the accuracy for the predictions for the test data
accuracy <- sum(diag(table1)) / sum(table1)
print(paste("Accuracy: ", accuracy))



auc <- performance(pred, "auc")
auc_value <- auc@y.values[[1]]

auc_value_test<- round(auc_value, 2)
print(paste("AUC: ", auc_value_test))



# ROC curve and AUC for Train and Test Data
# Set up the plotting area for side-by-side plots and increase font size
par(mfrow = c(1, 2), cex.axis = 1.8, cex.lab = 1.8, cex.main = 1.8)

# Plot for training data
plot(perf_train, main = "ROC Curve - Train Data", col = "blue4", lwd = 3, cex.axis = 2, cex.lab = 1.5)
abline(0, 1, lty = 2, col = "gray")

# Plot for testing data
plot(perf, main = "ROC Curve - Test Data", col = "red4", lwd = 3, cex.axis = 2, cex.lab = 1.5)
abline(0, 1, lty = 2, col = "gray")

# Reset the plotting area to default
par(mfrow = c(1, 1))
#----------------------------------- mean accuracies and auc-----------------------------------------

train_accuracies <- numeric(100)
test_accuracies <- numeric(100)
test_auc <- numeric(100)
#fit the glm model 100 times
for(i in 1:100) {
  ind <- sample(1:n, num_train, replace=FALSE)
  trainSet <- data[ind,]
  testSet <- data[-ind,]
  m1_glm <- glm(booking.status.encoded~ ., data = trainSet, family = binomial())

  train_pred <- predict(m1_glm, newdata = trainSet, type = "response")
  train_pred_class <- ifelse(train_pred > 0.5, 1, 0)
  
  table <- table(Predicted = train_pred_class, Actual = trainSet$booking.status.encoded)
  train_accuracies[i] <- sum(diag(table)) / sum(table)
  
  
  test_pred <- predict(m1_glm, newdata = testSet, type = "response")
  test_pred_class <- ifelse(test_pred > 0.5, 1, 0)  
  table1 <- table(Actual = testSet$booking.status.encoded,Predicted = test_pred_class)
  test_accuracies[i] <- sum(diag(table1)) / sum(table1)
  pred <- prediction(test_pred, testSet$booking.status.encoded)
  
  auc <- performance(pred, "auc")
  auc_value <- auc@y.values[[1]]
  
  test_auc[i]<- auc_value
  
}

summary(train_accuracies)
mean_train_accuracy <- mean(train_accuracies)

summary(test_accuracies)
mean_test_accuracy <- mean(test_accuracies)

print(paste("Mean Accuracy on train data over 100 iterations is: ", mean_train_accuracy))
print(paste("Mean Accuracy on test data over 100 iterations is:", mean_test_accuracy))

summary(test_auc)
mean(test_auc)

#------------------------------------------ RANDOM FOREST ----------------------------------------------------------

str(trainSet)



#in order to find the best combination of number of trees and number of variables
oob<-NULL
for (ntree in c(100,200,300,400,500)) {
  for (mtry in c(3,4,5)) {
    m2<- randomForest(as.factor(booking.status.encoded) ~ .,data = trainSet,ntree=ntree,mtry=mtry)
    oob<-c(oob,m2$err.rate[ntree,1])
  }}

results<- matrix(oob,byrow=TRUE,5,3)
colnames(results)<- c(3,4,5)
rownames(results) <- c(100,200,300,400,500)
results

rf_accuracy <- NULL
oob.error <- NULL
for (i in 1:30) {
  ind <- sample(1:n, round(0.8 * n), replace = FALSE)
  trainSet <- data[ind, ]
  testSet <- data[-ind, ]
  #mtry is a parameter m that specifies the number of variables (or features) to randomly sample 
  m2 <- randomForest(as.factor(booking.status.encoded) ~ . ,data = trainSet, ntree = 500, mtry = 3, importance = T)

  pred_test <- predict(m2, testSet)
  test_table_rf <- table(pred_test, testSet$booking.status.encoded)

  rf_accuracy<- c(rf_accuracy,sum(diag(test_table_rf)) / sum(test_table_rf))
}
mean_accuracy <- mean(rf_accuracy)
summary(rf_accuracy)

oob.error <- 1-rf_accuracy
summary(oob.error)
mean(oob.error)


round(importance(m2), 2)
print(m2)
m2$confusion #confusion matrix sta train data
varImpPlot(m2, main = "", cex = 1.2, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2)

plot(m2, main = "",cex.axis = 1.5, cex.lab = 1.6, lwd = 4)


pred_test <- predict(m2, testSet)
test_table_rf <- table(Actual = testSet$booking.status.encoded,Predicted = pred_test)
print("Confusion Matrix for Test Data:")
confusionMatrix(test_table_rf )

m2$predicted
m2$err.rate
m2$votes




#------------------------------------ NAIVE BAYES ----------------------------

ind <- sample(1:n, round(0.8 * n), replace = FALSE)
trainSet <- data[ind, ]
testSet <- data[-ind, ]
m3 = naiveBayes(booking.status.encoded ~ .,data = trainSet)

m3

#Predictions for Naive Bayes for test set
pred_nb = predict(m3,as.data.frame(testSet))
pred_prob_nb = predict(m3, as.data.frame(testSet), type = "raw")

table <- table(testSet$booking.status.encoded,pred_nb, dnn=c("Actual","Predicted"))

accuracy <- sum(diag(table)) / sum(table)

#Predictions for Naive Bayes for train set
pred_nb = predict(m3,as.data.frame(trainSet))
pred_prob_nb = predict(m3, as.data.frame(trainSet), type = "raw")

table <- table(trainSet$booking.status.encoded,pred_nb, dnn=c("Actual","Predicted"))

accuracy <- sum(diag(table)) / sum(table)


#------------------------------Variable Selection for Naive Bayes -----------------------------------

ind <- sample(1:n, round(0.8 * n), replace = FALSE)
trainSet <- data[ind, ]
testSet <- data[-ind, ]
accuracies <- vector("numeric", length = ncol(trainSet) - 1)
names(accuracies) <- names(trainSet)[-ncol(trainSet)]
i=1
final_accuracies=NULL
final_accuracies[i] <- accuracy
#in the begining I have the accuracy of the full model as the initial accuracy
max_accuracy <- accuracy
End <- FALSE
removing_variables <- NULL

variables <- names(trainSet)[-ncol(trainSet)]

while(End==FALSE) {
  i=i+1
  for (v in variables) {
    #fit the model without the variable v
    formula <- as.formula(paste("booking.status.encoded ~ . -", v))
    
    m3 <- naiveBayes(formula, data = trainSet)
    #Predictions for Naive Bayes
    pred_train = predict(m3,as.data.frame(trainSet))
    pred_prob_nb = predict(m3, as.data.frame(trainSet), type = "raw")
    
    table <- table(trainSet$booking.status.encoded,pred_train, dnn=c("Actual","Predicted"))
    #find the accuracy of the fitted model without the v parameter
    accuracy <- sum(diag(table)) / sum(table)
    #check if the accuracy of the fitted model is larger than this of the previous model
    #in the first loop the accuracy is the one of the full model
    #but after is the accuracvy of the fitted model after I have remove the neccesary variable
    accuracies[v] <- accuracy
  }
  max_accuracy <- max(accuracies)
  final_accuracies[i]<- max_accuracy
  if (final_accuracies[i]-final_accuracies[i-1]>=0.001){
    #store the variable in which the max is in the removing variables
    removed_var <- names(which.max(accuracies))
    removed_variables <- c(removing_variables,removed_var)
    #also remove it from trainset and from the variables vector
    trainSet <- trainSet[, -which(names(trainSet) == removed_var)]
    variables <- variables[-which(variables == removed_var)]
    
    print(paste("We removed the variable :", removed_var, "and we found a new accuracy of:", max_accuracy))
  }else{
    End=TRUE
  }
    
}



naive_accuracies <- NULL
for (i in 1:100) {
  ind <- sample(1:n, round(0.8 * n), replace = FALSE)
  trainSet <- data[ind, ]
  testSet <- data[-ind, ]

  m3 <- naiveBayes(booking.status.encoded ~ lead.time + number.of.children + market.segment.type + repeated +
                     number.of.adults +type.of.meal+ number.of.weekend.nights + car.parking.space +
                     room.type + average.price + special.requests,
                   data = trainSet)

  pred_nb <- predict(m3, as.data.frame(testSet))
  table <- table( Actual = testSet$booking.status.encoded,Predicted = pred_nb)

  accuracy <- sum(diag(table)) / sum(table)
  naive_accuracies[i] <- accuracy
}


#  mean accuracy
mean_naive_accuracy <- mean(naive_accuracies)
print(paste("Mean Test Accuracy over 100 iterations is:", mean_naive_accuracy))

summary(naive_accuracies)

#-------------------------------- LDA------------------------------------

#### arbitrary train-test split 80-20
ind <- sample(1:n, round(0.8 * n), replace = FALSE)
trainSet <- data[ind, ]
testSet <- data[-ind, ]

m1<-lda(booking.status.encoded ~., data=trainSet)
m2<-predict(m1, newdata=testSet)
t<- table(testSet[,15],m2$class)
sum(diag(t))/sum(t)

par(mfrow=c(1,1))
#plot(m2$x,col=data[,15], cex=1.5)
plot(m2$x, col = m2$class, cex = 1, pch = 19,
     xlab = "Index", ylab = "Value",
     cex.axis = 1.5, cex.lab = 1.5)

plot(m2$x,col=data[,15])
plot(m1)




## repeat 100 times
B<-100
lda_accuracy<-NULL
for ( i in 1:B) {
  ind <- sample(1:n, round(0.8 * n), replace = FALSE)
  trainSet <- data[ind, ]
  testSet <- data[-ind, ]
  
  m1<-lda(booking.status.encoded ~., data=trainSet)
  m2<-predict(m1, newdata=testSet)
  t<- table(testSet[,15],m2$class)
  lda_accuracy<-c(lda_accuracy,sum(diag(t))/sum(t))
}

summary(lda_accuracy)
hist(lda_accuracy, col = 'skyblue4', border = 'black', 
     main = '',
     xlab = 'Accuracy', ylab = 'Frequency', ylim = c(0, 30),
     cex.axis = 1.5, cex.lab = 1.8, cex.main = 2)
#-------------------------DECISION TREE----------------------

par(mfrow=c(1,1))
tree_accuracy <- NULL
for ( i in 1:B){
#default splitting method for classification (gini)
  ind <- sample(1:n, round(0.8 * n), replace = FALSE)
  trainSet <- data[ind, ]
  testSet <- data[-ind, ]
  fit1 <- tree(as.factor(booking.status.encoded) ~ ., trainSet)
  
  pred <- predict(fit1, newdata=testSet, type='class')
  fit1$frame
  fit1$where
  tree_table <- table(testSet$booking.status.encoded, pred)
  tree_accuracy <- c(tree_accuracy,sum(diag(tree_table)) / sum(tree_table))
}

summary(tree_accuracy)
hist(tree_accuracy,col='darkmagenta', border='black', main='Histogram of Accuracies from Decision Tree Algorithm',
     xlab='Accuracy', ylab='Frequency')
plot(fit1)
summary(fit1)
text(fit1,cex = 1.5)

cbind(data$booking.status.encoded,predict(fit1,type="class"), resid(fit1))


sum(sapply(data$booking.status.encoded,function(x)(x-mean(data$booking.status.encoded))^2))
sum(sapply(resid(fit1),function(x)(x-mean(resid(fit1)))^2))

fit <- rpart(as.factor(booking.status.encoded) ~ ., trainSet, method = "class")
# Plot the decision tree using rattle
fancyRpartPlot(fit, main = "")

rpart.plot(fit, type = 3, extra = 104, under = TRUE, fallen.leaves = TRUE,
           main = "", 
           cex = 1.3, box.palette = c("red", "green"), shadow.col = "gray", 
           nn = TRUE)




predict(fit1,type='vector')
predict(fit1,type='class')
predict(fit1,type='tree')

#------------------------------- SVM ----------------------------------------------------

ind <- sample(1:n, round(0.8 * n), replace = FALSE)
trainSet <- data[ind, ]
testSet <- data[-ind, ]




#----------------different choices of gamma and cost ------------------------
score<-NULL

for (cost in c(0.5,1,1.5) ) {
  for (gamma in c(0.7,1,1.2,1.5,2)) {
      
    svm_model <- svm(as.factor(booking.status.encoded) ~ ., data=trainSet,gamma=gamma, cost=cost)
    pred<- predict(svm_model,testSet)
    score <- c(score, sum(diag(table(pred, testSet$booking.status.encoded))) / nrow(testSet))
  }
}
results<- matrix(score,byrow=TRUE,3,5)
colnames(results)<- c(0.7,1,1.2,1.5,2)
rownames(results) <- c(0.5,1,1.5)
results


#--------------Variable Selection for SVM ----------------------------------


svm_model <- svm(as.factor(booking.status.encoded) ~ ., data=trainSet,gamma=0.7, cost=1)
pred<- predict(svm_model,trainSet)
table <- table(trainSet$booking.status.encoded,pred, dnn=c("Actual","Predicted"))

accuracy <- sum(diag(table)) / sum(table)

                 
svm_accuracies <- vector("numeric", length = ncol(trainSet) - 1)
names(svm_accuracies) <- names(trainSet)[-ncol(trainSet)]
i=1
final_svm_accuracies=NULL
final_svm_accuracies[i] <- accuracy
#in the begining I have the accuracy of the full model as the initial accuracy
max_accuracy <- accuracy
End <- FALSE
removing_variables <- NULL

variables <- names(trainSet)[-ncol(trainSet)]

while(End==FALSE) {
  i=i+1
  for (v in variables) {
   #fit the model without the variable v
   formula <- as.formula(paste("as.factor(booking.status.encoded) ~ . -", v))
  
   svm_m1 <- svm(formula, data = trainSet,gamma=0.7, cost=1)
   #Predictions for SVM
   pred_train = predict(svm_m1,trainSet)
   
   table <- table(trainSet$booking.status.encoded,pred_train, dnn=c("Actual","Predicted"))
   #find the accuracy of the fitted model without the v parameter
   accuracy <- sum(diag(table)) / sum(table)
   #check if the accuracy of the fitted model is larger than this of the previous model
   #in the first loop the accuracy is the one of the full model
   #but after is the accuracvy of the fitted model after I have remove the neccesary variable
   svm_accuracies[v] <- accuracy
  }
  max_accuracy <- max(svm_accuracies)
  final_svm_accuracies[i]<- max_accuracy
  if (final_svm_accuracies[i]-final_svm_accuracies[i-1]>=0.001){
     #store the variable in which the max is in the removing variables
     removed_var <- names(which.max(svm_accuracies))
     removed_variables <- c(removing_variables,removed_var)
     #also remove it from trainset and from the variables vector
     trainSet <- trainSet[, -which(names(trainSet) == removed_var)]
     variables <- variables[-which(variables == removed_var)]
     print(paste("We removed the variable :", removed_var, "and we found a new accuracy of:", max_accuracy))
  }else{
     End=TRUE
   }
 
}



#we saw that only 1 variable is removed the mumber.of.week.nights but the accuracy either fitting
#the full model or fitting the model without this variable is almos the same 

svm_accuracy <- NULL
for ( i in 1:B){
  ind <- sample(1:n, round(0.8 * n), replace = FALSE)
  trainSet <- data[ind, ]
  testSet <- data[-ind, ]
  svm_model <- svm(as.factor(booking.status.encoded) ~ ., data=trainSet,gamma=0.7, cost=1)
  pred<- predict(svm_model,testSet)
  svm_table <- table(testSet$booking.status.encoded, pred)
  svm_accuracy <- c(svm_accuracy,sum(diag(svm_table)) / sum(svm_table))
}


summary(svm_accuracy)
hist(svm_accuracy,col='aquamarine4', border='black', main='',
     xlab='Accuracy', ylab='Frequency',cex.axis = 1.5, cex.lab = 1.6, cex.main = 2)





