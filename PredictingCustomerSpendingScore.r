# KNN model
library(tidyverse)#loading tidyverse
library(class)
library(dummies)
library(e1071)#loading tidyverse
library(corrplot)
 
Spendingscore1 <- read_csv("C:/Users/INDIA02/Documents/University_documents/545/Project/ProjectDataClean.csv",col_types="lilfiil")# reading csv
 
print(Spendingscore1) #displaying tibble
head(Spendingscore1, 20) #displaying 20 rows
str(Spendingscore1) #structure of tibble
summary(Spendingscore1) #summary of tibble
 
#Separate the tibble into two. One with just the label and one with the other
#variables. Note: after this step, you should have two tibbles:
SpendingscoreDF <- data.frame(Spendingscore1)
Spendingscore <- as_tibble(dummy.data.frame(data =SpendingscoreDF, names="Profession"))
view(Spendingscore)
 
# Displays a correlation matrix rounded to 2 decimal places
round(cor(Spendingscore), 2)
 
# Display a correlation plot  with the 'number' method and only
# displaying the bottom left section
corrplot(cor(Spendingscore), method = "number",type = "lower")
#this is the class we are looking to predict - y variable
SpendingscoreLabels <- Spendingscore %>% select(SpendingScore)
 
#removing Sedan Size from x variables
Spendingscore <- Spendingscore %>% select(-SpendingScore)
#seed variable is set to ensure we get the same result every time we run a
#sampling process
set.seed(100)
SampleSpendingscore<- sample(nrow(Spendingscore),
round(nrow(Spendingscore) * 0.75), replace = FALSE)
 
#Creating training and testing data
SpendingscoreTraining<- Spendingscore[SampleSpendingscore, ]
SpendingscoreTrainingLabels<-SpendingscoreLabels[SampleSpendingscore, ]
 
SpendingscoreTesting<- Spendingscore[-SampleSpendingscore, ]
SpendingscoreTestingLabels<- SpendingscoreLabels[-SampleSpendingscore, ]
 
#generate the K-nn neighbor model
#We are using 71 as it is the nearest odd number to the square root of records
#in the training dataset.
SpendingscorePrediction <- knn(train =SpendingscoreTraining,
test = SpendingscoreTesting,cl= SpendingscoreTrainingLabels$SpendingScore,k =71)
 
#print the predicted data
print(SpendingscorePrediction)
print(summary(SpendingscorePrediction))
 
#creating confusion matrix
SpendingscoreConfusionMatrix <- table (SpendingscoreTestingLabels$SpendingScore,SpendingscorePrediction)
print(SpendingscoreConfusionMatrix)
 
#false positive rate
SpendingscoreConfusionMatrix[1,2] /(SpendingscoreConfusionMatrix[1,2]+
SpendingscoreConfusionMatrix[1,1] )
 
#false negative
SpendingscoreConfusionMatrix[2,1] /(SpendingscoreConfusionMatrix[2,1]+
SpendingscoreConfusionMatrix[2,2] )
 
#accuracy
predictiveAccuracy <- sum(diag(SpendingscoreConfusionMatrix)) / nrow(SpendingscoreTesting)
print(predictiveAccuracy)
 
#Create a matrix of k-values with their predictive accuracy
kValueMatrix <- matrix(data= NA,nrow=0,ncol=2)
colnames(kValueMatrix) <- c("k value","Predictive accuracy")
 
for (kValue in 1:nrow(SpendingscoreTraining)){
  if(kValue %% 2 !=0){
	#generate the model
    SpendingscorePrediction <- knn(train =SpendingscoreTraining,
test=SpendingscoreTesting,cl=SpendingscoreTrainingLabels$SpendingScore,k = kValue)
	#generate confusion matrix
SpendingscoreConfusionMatrix <- table (SpendingscoreTestingLabels$SpendingScore,SpendingscorePrediction)
	#calculate the predictive accuracy
	predictiveAccuracy <- sum(diag(SpendingscoreConfusionMatrix)) /
      nrow(SpendingscoreTesting)
	
	# add a new row to KValueMatrix
	kValueMatrix <- rbind(kValueMatrix, c(kValue,predictiveAccuracy))
	kValue= kValue+2
  }
}
print(kValueMatrix)
 
#generate neural network model
 
#install.packages("tidyverse")
#install.packages("neuralnet")
library(tidyverse)#loading tidyverse
library(neuralnet)#loading neuralnet
library(dummies)
 
Spendingscore <- read_csv("C:/Users/INDIA02/Documents/University_documents/545/Project/ProjectDataClean.csv",col_types="lilfiil")# reading csv
print(Spendingscore) #displaying tibble
head(Spendingscore, 20) #displaying 20 rows
str(Spendingscore) #structure of tibble
summary(Spendingscore) #summary of tibble
SpendingscoreDF <- data.frame(Spendingscore)
Spendingscore <- as_tibble(dummy.data.frame(data =SpendingscoreDF, names="Profession"))
view(Spendingscore)
 
#Scaling the variables from 0 to 1
Spendingscore <- Spendingscore %>%
mutate(AgeScaled = (Age - min(Age))/(max(Age)-min(Age)))
 
Spendingscore <- Spendingscore %>%
mutate(WorkExperienceScaled = (WorkExperience - min(WorkExperience))/
(max(WorkExperience)-min(WorkExperience)))
 
Spendingscore <- Spendingscore %>%
mutate(FamilySizeScaled = (FamilySize - min(FamilySize))/
(max(FamilySize)-min(FamilySize)))
 
Spendingscore <- Spendingscore %>% select(-Age)
Spendingscore <- Spendingscore %>% select(-WorkExperience)
Spendingscore <- Spendingscore %>% select(-FamilySize)
 
#seed variable is set to ensure we get the same result every time we run a
#sampling process
set.seed(100)
SampleSpendingscore<- sample(nrow(Spendingscore),
round(nrow(Spendingscore) * 0.75),replace = FALSE)
#Creating training and testing data
SpendingscoreTraining<- Spendingscore[SampleSpendingscore, ]
SpendingscoreTesting<- Spendingscore[-SampleSpendingscore, ]
 
#Generate the neural network model to predict
SpendingscoreNeuralNet <- neuralnet(formula = SpendingScore ~ .,
data = SpendingscoreTraining,hidden =3,act.fct= "logistic",
linear.output =FALSE)
 
#Display the neural network numeric results
print(SpendingscoreNeuralNet$result.matrix)
#Visualize the neural network
plot(SpendingscoreNeuralNet)
#generate probabilities
SpendingscoreProbability <- compute(SpendingscoreNeuralNet,
SpendingscoreTesting)
#Display the probabilities from the testing dataset on the console
print(SpendingscoreProbability$net.result)
 
#Convert probability predictions into 0/1 predictions
SpendingscorePrediction <-ifelse(SpendingscoreProbability$net.result> 0.5,1,0) 
#Display the 0/1 predictions on the console
print(SpendingscorePrediction)

#Evaluate the model by forming a confusion matrix
SpendingscoreConfusionMatrix<-  table(SpendingscoreTesting$SpendingScore,
SpendingscorePrediction )
 
#Display the confusion matrix on the console
print(SpendingscoreConfusionMatrix)
#Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(SpendingscoreConfusionMatrix))/
nrow(SpendingscoreTesting)
#Display the predictive accuracy on the console
print(predictiveAccuracy)

# Decision tree model on the vehicle customer dataset
# use decision tree to predict customer's spending score(1 for high, 0 for low)
# so that we can advertise different models to the corresponding customers.

library(tidyverse)
library(rpart)
library(rpart.plot)

# set working directory to the project folder
setwd("/Users/jianbo/Documents/UA/MIS Master/2021 Fall/MIS545/Project")

# read in training and testing data
customerData <- read_csv(file = "ProjectDataClean.csv",
                 col_types = "lilfiil",
                 col_names = TRUE)
customerData <- customerData %>%
        mutate(SpendingScore = ifelse(SpendingScore == TRUE, "High", "Low"))

set.seed(100)
sampleSet <- sample(nrow(customerData), 
                    nrow(customerData) * 0.75,
                    replace = FALSE)

customerTraining <- customerData[sampleSet,]
customerTesting <- customerData[-sampleSet,]

# show summaries of the data
summary(customerTraining)
summary(customerTesting)

# run three interesting queries on the datasets
# 1/3 show mean age of different profession groups
print(customerTraining %>%
        group_by(Profession) %>%
        summarize(mean(Age)))

# 2/3 show work experience status for different gender groups
# (True for male, False for female; True for graduated, False for not graduated)
print(customerTraining %>%
        group_by(Male) %>%
        count(Graduated))

# 3/3 count group high spending score numbers
print(customerTraining %>%
        group_by(Profession) %>%
        filter(SpendingScore == "High")%>%
        count())

# build the first tree model using cp 0.01
tree1 <- rpart(data = customerTraining,
              formula = SpendingScore ~.,
              cp = 0.01,
              method = "class")
rpart.plot(tree1,main = "Spending Score Classification 1: \n 
           cp = 0.01  accuracy = 0.806",cex.main = 1.5)

tree2 <- rpart(data = customerTraining,
               formula = SpendingScore ~.,
               cp = 0.007,
               method = "class")

rpart.plot(tree2,main = "Spending Score Classification 2: \n 
           cp = 0.007  accuracy = 0.819",cex.main = 1.6)

tree3 <- rpart(data = customerTraining,
                        formula = SpendingScore ~.,
                        cp = 0.002,
                        method = "class")

rpart.plot(tree3,main = "Spending Score Classification 3: \n 
           cp = 0.002  accuracy = 0.815",cex.main = 1.6)

tree4 <- rpart(data = customerTraining,
               formula = SpendingScore ~.,
               cp = 0.0015,
               method = "class")

rpart.plot(tree4,main = "Spending Score Classification 4: \n 
           cp = 0.0015  accuracy = 0.815",cex.main = 1.6)


tree5 <- rpart(data = customerTraining,
               formula = SpendingScore ~.,
               cp = 0.001,
               method = "class")
vit
rpart.plot(tree5,main = "Spending Score Classification 5: \n 
           cp = 0.001  accuracy = 0.819",cex.main = 1.6)

# prediction using tree1 model and show accuracy
prediction1 <- predict(tree1,customerTesting,type = "class")
print(prediction1)
tree1ConfusionMatrix <- table(customerTesting$SpendingScore,
                                 prediction1)
print(tree1ConfusionMatrix)
accuracy1 <- sum(diag(tree1ConfusionMatrix))/nrow(customerTesting)
print(accuracy1)

# prediction using tree2 model and show accuracy
prediction2 <- predict(tree2,customerTesting,type = "class")
print(prediction2)
tree2ConfusionMatrix <- table(customerTesting$SpendingScore,
                              prediction2)
print(tree2ConfusionMatrix)
accuracy2 <- sum(diag(tree2ConfusionMatrix))/nrow(customerTesting)
print(accuracy2)

# prediction using tree3 model and show accuracy
prediction3 <- predict(tree3,customerTesting,type = "class")
print(prediction3)
tree3ConfusionMatrix <- table(customerTesting$SpendingScore,
                              prediction3)
print(tree3ConfusionMatrix)
accuracy3 <- sum(diag(tree3ConfusionMatrix))/nrow(customerTesting)
print(accuracy3)

# prediction using tree4 model and show accuracy
prediction4 <- predict(tree4,customerTesting,type = "class")
print(prediction4)
tree4ConfusionMatrix <- table(customerTesting$SpendingScore,
                              prediction4)
print(tree4ConfusionMatrix)
accuracy4 <- sum(diag(tree4ConfusionMatrix))/nrow(customerTesting)
print(accuracy4)

# prediction using tree5 model and show accuracy
prediction5 <- predict(tree5,customerTesting,type = "class")
print(prediction5)
tree5ConfusionMatrix <- table(customerTesting$SpendingScore,
                              prediction5)
print(tree5ConfusionMatrix)
accuracy5 <- sum(diag(tree5ConfusionMatrix))/nrow(customerTesting)
print(accuracy5)

#Installing Various packages as necessary in the generation of #the regression model
install.packages("tidyverse")
install.packages("dummies")
install.packages("corrplot")
install.packages("olsrr")
install.packages("smotefamily")

# Loading the tidyverse, corrplot, olsrr libraries
library(tidyverse)
library(dummies)
library(scales)
library("corrplot")
library("olsrr")

#Setting up the working directory
setwd("C:/Users/UAL-Laptop/Desktop/MIS545/Project")

# creating and reading the ProjectDataClean csv into a tibble Project
Project <- read_csv("ProjectDataClean.csv",
                        col_types = "lilfiil",
                        col_names = TRUE)
#Displaying Project in the console 
print (Project)

#Displaying the structure of Project on the console
str(Project)



#Displaying the summary of Project in the console
summary(Project)

#Creating a displayAllHistograms() function that will take in a tibble demonstration parameter that will display a histogram for all numeric features
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() +geom_histogram(mapping = aes(x=value, fill=key),
                             color = "black") +
    facet_wrap (~key, scales = "free") +
    theme_minimal ()
}

#Calling the displayAllHistograms() function, passing in Project as an argument

displayAllHistograms(Project)

# Dummy coding the Profession variable using dummy.data.frame() 
Project1 <- data.frame(Project)

Project2 <- as_tibble(dummy.data.frame(data = Project1, names = "Profession"))

#Display the correlation matrix of Project2 rounded to two decimal places                                        
round(cor(Project2), 2)

#Displaying a correlation plot using the "number" method and limit output to the bottom left
corrplot(cor(Project2),
         method = "number",
         type = "lower")

cor(Project1[, c('Male', 'Age', 'Graduated','WorkExperience','SpendingScore')])
Project2 <- select(Project1, -Profession)

#Randomly splitting the dataset into Project2Training (75% of records) and Project2Testing (25% of records) using 100 as the random seed

set.seed(100)

sampleSet <- sample(nrow(Project1),
                    round(nrow(Project1) * 0.75),
                    replace = FALSE)
#Putting the records from the 75% sample into Project2Training 
Project2Training <- Project1[sampleSet, ]

#Putting the records from the 25% sample into Project2Testing
Project2Testing <- Project1[-sampleSet, ]






# Generating the logistic regression model (using SpendingScore as the binary dependent variable) and saving it in an object called SpendingScoreModel

SpendingScoreModel <- glm(data = Project2Training,
                        family = binomial,
                        formula = SpendingScore ~.)

#Displaying the logistic regression model results using the summary() function 
summary(SpendingScoreModel)

#Using the model to predict outcomes in the testing dataset
SpendingScorePrediction <-  predict(SpendingScoreModel,
               Project2Testing,type = "response")

#Displaying the prediction model
print(SpendingScorePrediction)

# Treating anything below or equal to 0.5 as TRUE, anything above 0.5 as FALSE

SpendingScorePrediction <- ifelse(SpendingScorePrediction >= 0.5,TRUE,FALSE)
print(SpendingScorePrediction)

#Generating a confusion matrix of predictions

SpendingScoreConfusionMatrix <-
  table(Project2Testing$SpendingScore,
        SpendingScorePrediction)
#Displaying Confusion Matrix
print(SpendingScoreConfusionMatrix)

#Calculating false positive rate
SpendingScoreConfusionMatrix[1,2] /
  (SpendingScoreConfusionMatrix[1,2] +
     SpendingScoreConfusionMatrix[1,1])

#Calculating the false negative rate
SpendingScoreConfusionMatrix[2,1] /
  (SpendingScoreConfusionMatrix[2,1] +
     SpendingScoreConfusionMatrix[2,2])

#Calculating the model prediction accuracy
sum(diag(SpendingScoreConfusionMatrix)) / nrow(Project2Testing)

# Used to install tidyvere and e1071 packages. Commented out after first use
# install.packages("tidyverse")
# install.packages("e1071")

# Loads in the tidyverse and e1071 libraries
library(tidyverse)
library(e1071)
library(dummies)

# Sets the working directory to the Lab08 folder
setwd("~/Desktop/MIS 545/Project")

# Reads ProjectDataClean.csv into a tibble called SpendingScore
SpendingScore <- read_csv(file = "ProjectDataClean.csv", 
                         col_types = "lilfiil",
                         col_names = TRUE)

# Displays dwellingType in the SpendingScore
print(SpendingScore)

# Displays the structure of SpendingScore in the console
str(SpendingScore)

# Display the summary of SpendingScore in the console
summary(SpendingScore)

# dummy
SpendingscoreDF <- data.frame(SpendingScore)
SpendingScore <- as_tibble(dummy.data.frame(data =SpendingscoreDF, names="Profession"))


view(SpendingScore)

# Creates a function called DisplayAllHistograms that takes in a tibble paramter
# that will display a histogram for all numeric features in a tibble
displayAllHistograms <- function(SpendingScore) {
  SpendingScore %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value,fill=key),
                              color = "black") +
    facet_wrap (~ key, scales = "free") +
    theme_minimal ()
}

# Call the displayAllHistograms() function, passing in SpendingScore as an 
# argument
displayAllHistograms(SpendingScore)

# Displays a correlation matrix of SpendingScore rounded to 2 decimal places
round(cor(SpendingScore), 2)
  
# Display a correlation plot of SpendingScore with the 'number' method and only
# displaying the bottom left section
corrplot(cor(SpendingScore),
         method = "number",
         type = "lower")


# Sets our random seed to 100 so our result stays the same
set.seed(100)

# Creates a vector of 75% randomly sampled rows from the original dataset.
sampleSet <- sample(nrow(SpendingScore),
                    round(nrow(SpendingScore) * 0.75),
                    replace = FALSE)

# Puts the records from the 75% sample into SpendingScoreTraining
SpendingScoreTraining <- SpendingScore[sampleSet, ]

# Puts all other records, 25%, into SpendingScoreTesting
SpendingScoreTesting <- SpendingScore[-sampleSet, ]

# Generate the Naive Bayes model to predict SpendingScore based on the other 
# variables in the dataset
spendingModel <- naiveBayes(formula = SpendingScore ~ .,
                            data = SpendingScoreTraining,
                            laplace = 1)

# Build probabilities for each record in the testing dataset and store them in
# SpendingScoreProbability
SpendingScoreProbability <- predict(spendingModel,
                                    SpendingScoreTesting,
                                    type = "raw")

# Display SpendingScoreProbability on the console
print(SpendingScoreProbability)

# Predict classes for each record in the testing dataset and store them in 
# SpendingScorePrediction
SpendingScorePrediction <- predict(spendingModel,
                                   SpendingScoreTesting,
                                   type = "class")

# Display SpendingScorePrediction on the console
print(SpendingScorePrediction)

# Evaluate the model by forming a confusion matrix
SpendingScoreConfusionMatrix <- table(SpendingScoreTesting$SpendingScore,
                                      SpendingScorePrediction)

# Display the confusion matrix on the console
print(SpendingScoreConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(SpendingScoreConfusionMatrix)) / 
  nrow(SpendingScoreTesting)

# Display the predictive accuracy on the console
print(predictiveAccuracy)

#Calculating false positive rate
SpendingScoreConfusionMatrix[1,2] /
  (SpendingScoreConfusionMatrix[1,2] +
     SpendingScoreConfusionMatrix[1,1])

#Calculating the false negative rate
SpendingScoreConfusionMatrix[2,1] /
  (SpendingScoreConfusionMatrix[2,1] +
     SpendingScoreConfusionMatrix[2,2])

