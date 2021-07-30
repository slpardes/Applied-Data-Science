#Sammy Pardes
#IST687
#Final Project

######load the data
survey <- read.csv("c:/Users/samantha.pardes/Desktop/graduate/IST687/final-project/SatisfactionSurvey2_2_2_2.csv")

######cleanse the data
head(survey)
str(survey)

any(is.na(survey)) #there are NAs in the data
sum(is.na(survey)) #there are 7,821 rows with NAs
colSums(is.na(survey)) #the only columns missing data are Arrival.Delay.in.Minutes, Flight.time.in.minutes, and Departure.Delay.in.Minutes

#let's assume there were 0 minute delays where left blank
survey$Arrival.Delay.in.Minutes[is.na(survey$Arrival.Delay.in.Minutes)] <- 0
any(is.na(survey$Arrival.Delay.in.Minutes))

survey$Departure.Delay.in.Minutes[is.na(survey$Departure.Delay.in.Minutes)] <- 0
any(is.na(survey$Departure.Delay.in.Minutes))

#let's replace NA flight with 0 minutes
library(sqldf)

sqldf('SELECT "Flight.time.in.minutes", "Flight.cancelled" FROM survey WHERE "Flight.cancelled"=="No" AND "Flight.time.in.minutes"==0')
#no rows where flight time is 0 and flight is not cancelled

NoFlightTime <- 0
survey$Flight.time.in.minutes[is.na(survey$Flight.time.in.minutes)] <- NoFlightTime
survey$Flight.time.in.minutes

#ensure state names are lowercase
survey$Destination.State <- tolower(survey$Destination.State)
survey$Origin.State <- tolower(survey$Origin.State)

#remove "." from column names to use SQL
names(survey)[names(survey) == "Origin.State"] <- "OriginState"
names(survey)[names(survey) == "Orgin.City"] <- "OriginCity" #fixes typo in col name
names(survey)[names(survey) == "Price.Sensitivity"] <- "PriceSensitivity"
names(survey)[names(survey) == "Year.of.First.Flight"] <- "YearOfFirstFlight"
names(survey)[names(survey) == "No.of.Flights.p.a."] <- "NoOfFlights"
names(survey)[names(survey) == "X..of.Flight.with.other.Airlines"] <- "NumFlightsWithOtherAirlines"
names(survey)[names(survey) == "Type.of.Travel"] <- "TypeOfTravel"
names(survey)[names(survey) == "No..of.other.Loyalty.Cards"] <- "NumLoyaltyCards"
names(survey)[names(survey) == "Shopping.Amount.at.Airport"] <- "ShoppingAmtAtAirport"
names(survey)[names(survey) == "Eating.and.Drinking.at.Airport"] <- "EatDrinkAtAirport"
names(survey)[names(survey) == "Day.of.Month"] <- "DayOfMonth"
names(survey)[names(survey) == "Flight.date"] <- "FlightDate"
names(survey)[names(survey) == "Airline.Code"] <- "AirlineCode"
names(survey)[names(survey) == "Airline.Name"] <- "AirlineName"
names(survey)[names(survey) == "Scheduled.Departure.Hour"] <- "SchedDepartHr"
names(survey)[names(survey) == "Departure.Delay.in.Minutes"] <- "DepartDelayMin"
names(survey)[names(survey) == "Flight.cancelled"] <- "FlightCancelled"
names(survey)[names(survey) == "Flight.time.in.minutes"] <- "FlightTimeMin"
names(survey)[names(survey) == "Flight.Distance"] <- "FlightDist"
names(survey)[names(survey) == "Arrival.Delay.greater.5.Mins"] <- "ArrivalDelayOver5Min"
names(survey)[names(survey) == "Destination.City"] <- "DestinationCity"
names(survey)[names(survey) == "Destination.State "] <- "DestinationState"
names(survey)[names(survey) == "Arrival.Delay.in.Minutes"] <- "ArrDelayMin"
names(survey)[names(survey) == "Destination.State"] <- "DestState"
names(survey)[names(survey) == "Airline.Status"] <- "Status"

head(survey)
dim(survey)

#use Cool&Young Airlines Inc. responses only
any(is.na(survey$AirlineCode))

surveyCool <- sqldf('SELECT * FROM survey WHERE AirlineCode=="VX"')
head(surveyCool)
dim(surveyCool)
#1,288 responses for Cool&Young Airlines Inc. 

######data is cleansed!!

#How do we compare?
ratingByAirline <- sqldf('SELECT AVG(Satisfaction) as Satisfaction, AirlineName, AirlineCOde from survey GROUP BY AirlineCode')
ratingByAirline <- ratingByAirline[order(-ratingByAirline$Satisfaction),]
ratingByAirline
#Cool&Young Airlines Inc. ranks second among it's competitors for most satisfied clients
  
######business question 1: which cities/states have the largest impact on satisfaction? Do these cities have higher/lower delays?
#find cities w/ lowest satisfaction scores
ratingByCity <- sqldf('SELECT AVG(surveyCool.Satisfaction)AS AvgScore, OriginCity, OriginState FROM surveyCool GROUP BY OriginCity')
dim(ratingByCity) #18 cities represented

ratingByCity <- ratingByCity[order(ratingByCity$AvgScore),] #order from lowest to highest satisfaction
ratingByCity[1:5,] 
#the 5 lowest ranking cities are:
#1) San Jose, CA 2.) Orlando, FL 3.) Chicago, IL 4.) Austin, TX 5.) San Diego, CA

#find max delay times
delaysByCity <- sqldf('SELECT AVG(surveyCool.DepartDelayMin)AS AvgDelay, OriginCity, OriginState FROM surveyCool GROUP BY OriginCity')
dim(delaysByCity)

delaysByCity <- delaysByCity[order(-delaysByCity$AvgDelay),]
delaysByCity[1:5,]
#the cities w/ the most departure delays are:
#1) Chicago, IL 2.) Palm Springs, CA 3.) San Diego, CA 4.) Newark, NJ 5.) Washington, DC

#compare low ranking cities to delayed cities
data.frame(ratingByCity$OriginCity[1:5],delaysByCity$OriginCity[1:5])
#Chicago and San Diego appear in the 5 lowest rated and most departure delays

#let's try arrival delays
arrDelaysByCity <- sqldf('SELECT AVG(surveyCool.ArrDelayMin)AS AvgArrDelay, OriginCity, OriginState FROM surveyCool GROUP BY OriginCity')
dim(arrDelaysByCity)

arrDelaysByCity <- arrDelaysByCity[order(-arrDelaysByCity$AvgArrDelay),]
arrDelaysByCity[1:5,]
#the cities w/ the most arrivial delays are:
#1.) Chicago, IL 2.) Palm Springs, CA 3.) San Diego, CA 4.) Washington, DC 5.) Newark, NJ
#same cities as most departure delays

#avg rating vs. avg dept. delays
RatingVsDelay <- sqldf('SELECT AVG(DepartDelayMin)AS AvgDelay, AVG(Satisfaction)AS AvgRating, OriginCity, OriginState AS state FROM surveyCool GROUP BY OriginCity')
head(RatingVsDelay)

#plot findings
library(ggplot2)
library(ggmap)
myStates <- data.frame(tolower(state.name), state.region)
colnames(myStates) <- c('state', 'region')

RatingVsDelay <- merge(RatingVsDelay, myStates, by="state") #leaves out U.S. Pacific Trust Territories

ggRatingDelay <- ggplot(RatingVsDelay, aes(x=AvgDelay, y=AvgRating)) + geom_point()
ggRatingDelay <- ggRatingDelay + xlab("Average Delay in Minutes") + ylab("Average Satisfaction Score")
ggRatingDelay <- ggRatingDelay + aes(color=region, size=4) + ggtitle("Average Satisfaction vs. Departure Delay")
ggRatingDelay <- ggRatingDelay + geom_text(aes(label=OriginCity),hjust=0.5, vjust=-1.3, size=3, color="black")
ggRatingDelay

#cities in West region
RatingVsDelay[(RatingVsDelay$region=="West"),]

#cancellations
surveyCool$cancelledNum <- as.numeric(surveyCool$FlightCancelled)

RatingVsCancel <- sqldf('SELECT AVG(cancelledNum)AS AvgCancelled, AVG(Satisfaction)AS AvgRating, OriginCity, OriginState AS state FROM surveyCool GROUP BY OriginCity')
RatingVsCancel <- RatingVsCancel[order(-RatingVsCancel$AvgCancelled),]
head(RatingVsCancel)

######business question 2: how are airlines performing across different types of customers?
#status
ratingByStatus <- sqldf('SELECT AVG(Satisfaction)AS AvgRating, Status FROM surveyCool GROUP BY Status')
ratingByStatus
#most to least satisfied by status: platinum silver, gold, blue
#big difference between blue and gold

ratingbyStatusPlot <- ggplot(ratingByStatus, aes(x=Status, y=AvgRating)) + geom_histogram(stat="identity")
ratingbyStatusPlot <- ratingbyStatusPlot + aes(fill=Status) + xlab("Status") + ylab("Satisfaction") + ggtitle("Satisfaction vs. Status")
ratingbyStatusPlot

#class
ratingByClass <- sqldf('SELECT AVG(Satisfaction)AS AvgRating, Class FROM surveyCool GROUP BY Class')
ratingByClass
#Business class most satisfied, then Eco Plus, then Eco

ratingbyClassPlot <- ggplot(ratingByClass, aes(x=Class, y=AvgRating)) + geom_histogram(stat="identity")
ratingbyClassPlot <- ratingbyClassPlot + aes(fill=Class) + xlab("Class") + ylab("Satisfaction") + ggtitle("Satisfaction vs. Class")
ratingbyClassPlot

#age
ratingByAge <- sqldf('SELECT AVG(Satisfaction)AS AvgRating, Age FROM surveyCool GROUP BY Age')
ratingByAge <- ratingByAge[order(-ratingByAge$AvgRating),]
ratingByAge[1:10,] 
#most satisfied ages: 43, 35, 40, 52, 51

ratingByAge <- ratingByAge[order(ratingByAge$AvgRating),]
ratingByAge[1:10,] 
#least satisfied ages: 70, 85, 71, 80, 69

#gender
ratingByGender <- sqldf('SELECT AVG(Satisfaction)AS AvgRating, Gender FROM surveyCool GROUP BY Gender')
ratingByGender
#males are on average, more satisfied than females

#gender + age plot
agesGenders <- sqldf('SELECT AVG(Satisfaction) AS AvgRating, Gender, FLOOR((Age)/5)*5 AS AgeGroup FROM surveyCool GROUP BY AgeGroup, Gender')
agesGenders

ageGenderPlot <- ggplot(agesGenders, aes(AgeGroup, AvgRating, fill=Gender))
ageGenderPlot <- ageGenderPlot + geom_bar(stat = "identity", position = 'dodge')
ageGenderPlot <- ageGenderPlot + xlab("Age Group") + ylab("Rating") + ggtitle("Average Raing by Age Group")
ageGenderPlot

#type of travel
ratingByTravel <- sqldf('SELECT AVG(Satisfaction)AS AvgRating, TypeOfTravel FROM surveyCool GROUP BY TypeOfTravel')
ratingByTravel
#business travelers most satisfied, then mileage travelers, personal travelers least satisfied

ratingbyTravelPlot <- ggplot(ratingByTravel, aes(x=TypeOfTravel, y=AvgRating)) + geom_histogram(stat="identity")
ratingbyTravelPlot <- ratingbyTravelPlot + aes(fill=TypeOfTravel) + xlab("Type of Travel") + ylab("Satisfaction") + ggtitle("Satisfaction vs. Type of Travel")
ratingbyTravelPlot <- ratingbyTravelPlot + theme(axis.text.x = element_text(angle=15))
ratingbyTravelPlot

#multiple plots
library(gridExtra)
ratingPlots <- grid.arrange(ratingbyClassPlot, ratingbyStatusPlot, ratingbyTravelPlot, nrow = 1)
ratingPlots

######business question 3: who are the most loyal customers within each airline? Does loyalty align with satisfaction? Who are the least loyal?

#define loyalty: NumFlightsWithOtherAirlines/NoOfFlights (low % is more loyal)

#can't divide by 0. must exclude passengers w/ 0 # of flights
sqldf('SELECT COUNT(*) FROM surveyCool WHERE NoOfFlights ==0') #54 participants have taken 0 flights
sqldf('SELECT COUNT(*) FROM surveyCool WHERE NoOfFlights !=0') #1234 participants have taken at least 1 flight

surveyCoolWFlights <- sqldf('SELECT * FROM surveyCool WHERE NoOfFlights !=0')
dim(surveyCoolWFlights)
dim(surveyCool)

#create loyal % column, 0% would be most loyal
head(sqldf('SELECT NumFlightsWithOtherAirlines, NoOfFlights, loyalty FROM surveyCoolWFlights'))

surveyCoolWFlights$loyalty <- (surveyCoolWFlights$NumFlightsWithOtherAirlines/surveyCoolWFlights$NoOfFlights)
head(surveyCoolWFlights$loyalty)

#gender
mostLoyalGender <- sqldf('SELECT AVG(loyalty) AS AvgLoyalty, Gender FROM surveyCoolWFlights GROUP BY Gender')
mostLoyalGender
#males are more loyal than females

#class
mostLoyalClass <- sqldf('SELECT AVG(loyalty) AS AvgLoyalty, Class FROM surveyCoolWFlights GROUP BY Class')
mostLoyalClass
#most loyal is Business, then Eco Plus, then Eco

#status
mostLoyalStatus <- sqldf('SELECT AVG(loyalty) AS AvgLoyalty, Status FROM surveyCoolWFlights GROUP BY Status')
mostLoyalStatus
#most loyal is Blue, then Silver, then Gold. Least loyal is platinum
#opposite from rating by Status. Blue is most loyal but least satisfied. Platinum is most satisfied, least loyal.

#travel type
mostLoyalTravel <- sqldf('SELECT AVG(loyalty) AS AvgLoyalty, Class FROM surveyCoolWFlights GROUP BY TypeOfTravel')
mostLoyalTravel
#most loyal is Personal, then Mileage, then Business
#also the opposite from rating by Type of Travel. Business travelers are most satisfied, least loyal.
#Personal travelers are least satisfied but most loyal

######business question 4: what variables have the strongest impact on Satisfaction score?

#linear models
str(surveyCool$Satisfaction) #need all variables to be the same type (numeric)

surveyCool$travelNum <- as.numeric(surveyCool$TypeOfTravel)
surveyCool$classNum <- as.numeric(surveyCool$Class)
surveyCool$statusNum <- as.numeric(surveyCool$Status)
surveyCool$satisfactionNum <- as.numeric(surveyCool$Satisfaction)
surveyCool$genderNum <- as.numeric(surveyCool$Gender)

#shopping
shoppingRating <- sqldf('SELECT Avg(Satisfaction) as avgSatisfaction, ShoppingAmtAtAirport FROM surveyCool WHERE ShoppingAmtAtAirport > 0 GROUP BY ShoppingAmtAtAirport')

shoppingLm <- lm(formula=avgSatisfaction ~ ShoppingAmtAtAirport, data=shoppingRating)
summary(shoppingLm)
#very low R^2 value, not a good predictor

#eat drink
eatingRating <- sqldf('SELECT Avg(Satisfaction) as avgSatisfaction, EatDrinkAtAirport FROM surveyCool GROUP BY EatDrinkAtAirport')
eatingRating

eatingLm <- lm(formula=avgSatisfaction ~ EatDrinkAtAirport, data=eatingRating)
summary(eatingLm)
#very low R^2 value, not a good predictor

#multiple variable models
#travel type, status
surveyCoolLm <- lm(formula=satisfactionNum ~ travelNum + statusNum, data=surveyCool)
summary(surveyCoolLm)
#R^2 value ~36%, p-value very low

#travel type, status, class, gender, age
surveyCool2 <- lm(formula=satisfactionNum ~ travelNum + statusNum + classNum + genderNum + Age, data=surveyCool)
summary(surveyCool2)
#R^2 value ~37%, p-value very low

#travel type, status, class, gender, age, shopping, eating
surveyCoolLm3 <- lm(formula=satisfactionNum ~ travelNum + statusNum + classNum + genderNum + Age + ShoppingAmtAtAirport + EatDrinkAtAirport, data=surveyCool)
summary(surveyCoolLm3)
#R^2 value ~37%, p-value very low

#KSVM and SVM Models
#creat training and testing data sets
surveyRows <- nrow(surveyCool)
surveyCutPoint <- floor((surveyRows*2)/3)
surveyRandom <- sample(1:surveyRows)

surveyTrain <- surveyCool[surveyRandom[1:surveyCutPoint],] #create training data set w/ first 2/3 of data
surveyTest <- surveyCool[surveyRandom[(surveyCutPoint+1):surveyRows],] #create test data set w/ remaining 1/3

dim(surveyTrain) #check # of rows
dim(surveyTest)

#ksvm model
library(kernlab)
surveyKsvm <- ksvm(surveyTrain$satisfactionNum ~., data=surveyTrain, kernel="rbfdot", kpar="automatic", prob.model=TRUE, cross=10, C=10) #create model training data
surveyKsvm

surveyPredict <- predict(surveyKsvm, surveyTest, type = "votes")

surveyCompare <- data.frame(surveyTest[,1], surveyPredict[,1])
colnames(surveyCompare) <- c("TestData", "Prediction")
head(surveyCompare) #compare predicted score to actual satisisfaction

#calculate RSME
library(Metrics)

rmse(surveyTest$satisfactionNum, surveyPredict)
#Root Mean Squared Error for KSVM = 2.132004

#SVM
library(e1071)
library(caret)
surveySvm <- svm(surveyTrain$satisfactionNum ~., data=surveyTrain) #create model
surveyPredictSvm <- predict(surveySvm, surveyTest, type = "votes") #predict values

rmse(surveyTest$satisfactionNum, surveyPredictSvm)
#Root Mean Squared Error for SVM = 2.132854