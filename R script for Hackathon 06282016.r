# Titanic project at Predictive Analysis Meet Up (6/28/2016)
library('data.table') # data.table
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library('rpart')

## Load the data

# Import the training set: train
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)
  
# Import the testing set: test
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url)
full  <- as.data.table(bind_rows(train, test)) # bind training & test data


rm(test,train)

## Create new variables

# Family size
full$Fsize <- full$SibSp + full$Parch + 1 # Create a family size variable including the passenger 

# Discritize Family size
full$FsizeD[full$Fsize == 1] <- 'singleton' # Discretize family size
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# Surname of family
full$Surname <- sapply(full$Name,  
                      function(x) strsplit(x, split = '[,.]')[[1]][1]) # Grab surname from passenger name

# Surname + family size
full$Family <- paste(full$Surname, full$Fsize, sep='_') # Create a family variable by combining Surname and Family size. "paste" concatenates strings.
					  
# Title from Name
full$Title <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
full$Title <- sub(' ', '', full$Title) 

full$Title[full$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
				
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'
				
# Deck variable using cabin
full$Deck<-substring(full$Cabin,1,1)
full$Deck[is.na(full$Deck)]="noinfo"
full$Deck[full$Deck==""] = "noinfo"
full$Deck=as.factor(full$Deck)

# Zero fare: probably the crew
full$Zerofare <-0
full$Zerofare[full$Fare==0] <-1

# Count of each ticket number
nticket <-as.data.table(table(full$Ticket)) 
#nticket2 <-nticket[rep(seq(1,nrow(nticket)),nticket$N)] #Duplicates ticket numbers by the frequency of each ticket number
names(nticket)[names(nticket)=="V1"] <-"Ticket"
full <- merge(full,nticket,by="Ticket")
names(full)[names(full)=="N"] <-"Nticket"

# Survival probability by ticket
small <- full[is.na(full$Survived)==0] #1. drop rows with Survived = NA 
surp <- aggregate(small$Survived,list(small$Ticket),mean) # 2. compute survival probability
names(surp)[names(surp)=="Group.1"] <-"Ticket"	# 3. rename the group var
names(surp)[names(surp)=="x"] <-"SurpTicket"	# 3. rename the mean var
full <- merge(full,surp,by="Ticket",all=TRUE) #4. and merge with full

# Survival probability by family
surp <- aggregate(small$Survived,list(small$Family),mean) # 2. compute survival probability
names(surp)[names(surp)=="Group.1"] <-"Family"	# 3. rename the group var
names(surp)[names(surp)=="x"] <-"SurpFamily"	# 3. rename the mean var
full <- merge(full,surp,by="Family",all=TRUE) #4. and merge with full

full$Surp <- full$SurpTicket 
full$Surp[full$Nticket<full$Fsize] <-full$SurpFamily[full$Nticket<full$Fsize] # When family size is larger than Ticket group size, use family
full$SurpTicket[full$Nticket==1 & full$Fsize==1] <- NA #5. replace survival prob for singleton with NA

full$SurpTicket[full$SurpTicket<1 & full$SurpTicket>0.5] <-0.75 # 6. Discritize SurpTicket and make a factor variable
full$SurpTicket[full$SurpTicket<0.5 & full$SurpTicket>0] <-0.25 # 6. Discritize SurpTicket and make a factor variable
full$SurpTicket <-as.character(full$SurpTicket)
full$SurpTicket[is.na(full$SurpTicket)==1] <-"singleton"

rm(nticket,small,surp) #remove the variables
 
## Missing variable

# Sort data
full <- full[order(full$PassengerId),]
# Fare: median of each passenger class
colSums(is.na(full)) #check the number of missing obs for each variable
which(is.na(full$Fare))

temp <-full$Pclass[full$PassengerId==1044]
medianfare <- median(full$Fare[full$Pclass==temp], na.rm = TRUE)
full$Fare[full$PassengerId==1044] <- medianfare

# Embarked: Check if people with the same surname (after creating surname+family size variable)
sum(full$Embarked=="")
full$Embarked[missing=1]

unique(full$Embarked)
full$Embarked[full$Embarked==""]
count=1:length(full$Embarked)
count[full$Embarked==""]

# missing row : 62, 830

full$Embarked[62] = "S"
full$Embarked[830] ="S"


full$Embarked <-as.factor(full$Embarked)
full$Title<-as.factor(full$Title)
full$Pclass<-as.factor(full$Pclass)
full$FsizeD<-as.factor(full$FsizeD)
full$Zerofare<-as.factor(full$Zerofare)
full$SurpTicket <- as.factor(full$SurpTicket)


# Age: predict using decision tree

full$Singletonman <- 0 # who are on board alone
full$Singletonman[full$FsizeD=="singleton" & full$Sex=="male"] <-1
full$Singletonwoman <- 0
full$Singletonwoman[full$FsizeD=="singleton" & full$Sex=="female"] <-1

#1. Identify the age of the siblings if there is
#2. Identify the age of the parents if there is
full$Mother<-NA
full$Mother[full$Parch>0 & full$Title=="Mrs"] <-1 #Mark mothers
full$Mother[full$Parch==0] <- 0 
full$Mother[full$Title=="Miss"] <- 0 
full$Mother[full$Sex=="male"] <- 0 

full$Child<-NA
full$Child[full$Parch>0 & full$Parch<=2 & full$Age<=16] <- 1 #Mark children
full$Child[full$Parch>0 & full$Title=="Master"] <- 1 #Mark children
full$Child[full$Parch>0 & full$Title=="Miss"] <- 1 #Mark children
full$Child[full$Parch==0] <- 0 
full$Child[full$Title=="Mrs"] <- 0 

# Age gap between mother and child=NA
full$MotherAge <-NA
full$MotherAge[full$Mother==1] <-full$Age[full$Mother==1]
small <- full[full$Mother==1]
motherage <- 

full$MotherAge <- aggregate(full$MotherAge,list(full$Ticket),mean)

  
#By ticket, among Parch, compute the age gap
full$Parent <-0
full$NoParent <-0
full$NoParent[full$Parch==0] <-1 #those who do not have any parents or children on board
full$HasChild <-0 


full$HasChild[]
full$Mother[full$Parch>0 & full$Title=="Mrs" & full$HasChild==1 & full$Sex=="female"] 

#Compute maximum age of a child per ticket
small <- full[is.na(full$Age)==0] #1. drop rows with Survived = NA 
surp <- aggregate(small$Survived,list(small$Ticket),mean) # 2. compute survival probability

full$MaxAgeChild 
full$MaxAgeChild[full$Mother==0 & full$Father==0] <- NA
full$NoParent[full$Parch>0 & ] <-

full$Parent[full$Title=="Mrs"]
full$Children<-0
full$HasSpouse<-0
full$Siblings<-0



full$Parent[full$Title=="Mrs" & full$Parch>0] # married or single

predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FsizeD,
data = full[!is.na(full$Age),], method = "anova")
full$Age[is.na(full$Age)] <- predict(predicted_age, full[is.na(full$Age),])


train <- full[1:891,]
test <- full[892:1309,]


my_forest1 <- randomForest(as.factor(Survived)~Pclass + Sex + Age+FsizeD+Fare+Embarked+Title+Deck,data=train,importance=TRUE,ntree=1000)
my_forest2 <- randomForest(as.factor(Survived)~Pclass + Sex + Age+FsizeD+Fare+Embarked+Title+Deck,data=train,importance=TRUE,ntree=1000)
my_forest3 <- randomForest(as.factor(Survived)~Pclass + Sex + Age+FsizeD+Fare+Zerofare+Embarked+Title+SurpTicket+Deck,data=train,importance=TRUE,ntree=1000)
my_forest3 <- randomForest(as.factor(Survived)~Pclass + Sex + Age+FsizeD+Fare+Zerofare+Embarked+Title+Deck,data=train,importance=TRUE,ntree=1000)


my_prediction <- predict(my_forest1,test)
my_solution <- data.frame(PassengerId=test$PassengerId,Survived=my_prediction)

write.csv(my_prediction,file="D:/Dropbox/Kaggle/Titanic/my_prediction.csv",row.names=TRUE)