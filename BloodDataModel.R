###Blood Donation Dataset Classification Problem###

#Importing dataset and libraries
blooddata <- read.csv("~/Desktop/Blood Data Compitition/train.csv")
testset <- read.csv("~/Desktop/Blood Data Compitition/testset.csv")
View(blooddata)
attach(blooddata)
library("Metrics", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")




#Data Exploration

#To explore, I am going to look at some simple summary statistics and also get some histograms to 
#see the distribution of the attributes for each observation (donor) 
summary(blooddata)
VolumeHist <- ggplot(data=blooddata, aes(blooddata$Total.Volume.Donated..c.c..)) + geom_histogram(binwidth = 1000)
LastDonationHist <- ggplot(data=blooddata, aes(blooddata$Months.since.Last.Donation)) + geom_histogram(binwidth = 10)
NumDonationsHist <- ggplot(data=blooddata, aes(blooddata$Number.of.Donations)) + geom_histogram(binwidth = 10)
SinceFirstDonationHist <- ggplot(data=blooddata, aes(blooddata$Months.since.First.Donation)) + geom_histogram(binwidth = 10)

#The Volume Histogram shows that most people donated less than 4000cc of blood
print(VolumeHist)

#The Months Since Last Donation attribute shows that most people in this dataset donated within the last 20 months
print(LastDonationHist)

#Most people have donated less than 20 or so times
print(NumDonationsHist)

#This plot is much more distributed than the previous. Many people have donated for the first time, but a large spike in 
#the middle of the histogram shows that many people are now donating again after roughly 2-2.5 years of being out of the game
#after that, the general downward trend continues. This may suggest that many people generally donate roughly every 2/2.5 years
print(SinceFirstDonationHist)


#Check for collinearity
cor(blooddata)

#After looking at the correlation matrix between all the potential factors, I found that Total Volume
#and Number of Donatations is perfectly correlated, so I will drop Total.Volume.Donated..c.c.. and keep Number of DOnations




#Running Model
initialmodel <- glm(Made.Donation.in.March.2007 ~ Number.of.Donations + Months.since.First.Donation + Months.since.Last.Donation, data = blooddata, family = "binomial")




#Diagnostics

#Factors seem to be significant, based on the confidence intervals and p-values
summary(initialmodel)
confint(initialmodel)
#plot(initialmodel)


#Getting Predicted Probabilities
testset$predictions <- predict(initialmodel, newdata=testset, type = "response")


#Making Submission File
BloodDonationSubmissionFormat <- read.csv("~/Downloads/BloodDonationSubmissionFormat.csv")
submission <- data.frame(BloodDonationSubmissionFormat$X)
colnames(submission)[1] <- 'X'
submission$Made.Donation.in.March.2007 <- testset$predictions

#Viewing
View(testset)
View(submission)

#write submission df to csv
write.csv(submission, file="submissionfile", quote=FALSE, row.names = FALSE)
#Simple post-proccessing on file done afterwards on column titles

