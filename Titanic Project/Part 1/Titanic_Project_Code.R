getwd()
setwd("C:/Users/juanj/Downloads/Universidad/1er Curso/1er Cuatri/Introduction to Data Science/Assignament1")
load("titanic_train.Rdata")
head(titanic.train)
library(ggplot2)
data = titanic.train
titanic.train = NULL

# Before starting we need to see the structure of the data frame and extract a 
# summary of all its variables to try to understand them in an easier way.
str(titanic.train)

# We start with the numeric variables, which we can see that are: Age, SibSp,
# Parch, Ticket and Fare.
summary(titanic.train$Age)
summary(titanic.train$SibSp)
summary(titanic.train$Parch)
summary(titanic.train$Ticket)
summary(titanic.train$Fare)
# We can conclude that the Ticket variable has no interesting outcomes


aux = which(data$Cabin != "")
data$Room = rep("No Cabin",length(data$Cabin))
data$Room[aux] = "Cabin" 
data$Room = factor(data$Room)
prop.table(table(data$Room, data$Survived),1)
  
ggplot(data = data) +
  aes(x=Pclass, fill = Survived) +
  geom_bar()+
  facet_grid(Room~.)

ggplot(data = data) +
  aes(x=Pclass, fill = Survived) +
  geom_bar(alpha = 0.5, colour = "black")+
  facet_grid(Room~.)


#How does the fare variate between 

ggplot(data = data) +
  aes(x=Pclass, fill = Survived) +
  geom_bar(alpha = 0.5, colour = "black")+
  facet_grid(Room~.)

fare_intervals = cut(data$Age, breaks = 20)
auxRoom = which(data$Room == "Cabin")
auxNoRoom = which(data$Room == "No Cabin")
ggplot(data = data[data$Room == 1]) +
  geom_histogram(aes(x = Fare[auxRoom], y = ..density..),
                 alpha = 0.6,
                 fill = "#FC766AFF", color = "white") +
  geom_histogram(data = data[data$Room == 2],
                 aes(x = Fare[auxNoRoom], y =- ..density..),
                 alpha = 0.6,
                 fill = "#5B84B1FF", color = "white") +
  xlab("") + theme(text = element_text(size = 6)) 



# The relationship between the survival rate and the age
data$DiscretizedAge = cut(data$Age, c(0,10,20,30,40,50,60,70,80,100))
data$DiscretizedAge

ggplot(data=data) +
  aes(y=DiscretizedAge) +
  geom_bar(aes(fill=Survived),position = "fill") +
  scale_fill_manual(labels = c("NO","YES"), values=c("red","blue")) +
  scale_x_continuous(labels=percent) +
  xlab("PERCENTAGES") +
  ylab("AGES INTERVALS") +
  labs(title="PROPORTION OF SURVIVAL IN EACH AGE INTERVAL")




# Violin-boxplot - Relationship between the age, class and survival
# How does the age and the class of a person affected to its chances of surviving?

# First we need to change the labels for the Pclass variable to make it more
# understandable in the plot
data$Pclass = factor(data$Pclass, labels = c("1st Class", "2nd Class", "3rd Class"))
ggplot(data = data) +
  aes(x=Survived, y=Age, fill=Survived, alpha = 0.6)+
  scale_fill_manual(labels= c("Not Survived", "Survived"), values = c("firebrick","limegreen")) +
  geom_violin()+
  geom_boxplot(width=0.5, color="black", alpha=0.2) +
  geom_jitter(color = "blue", alpha = 0.1) +
  facet_grid(.~Pclass)+
  theme(legend.position = "none") +
  xlab("SURVIVAL") +
  ylab("AGE") +
  labs(title = "RELATIONSHIP BETWEEN AGE, CLASS AND SURVIVAL")


# Is having a cabin related to the class of the person and the embarkation place?

# First we need to create a now variable that differentiates between the people
# with cabin and the ones without
aux = which(data$Cabin != "")
data$Room = rep("No Cabin",length(data$Cabin))
data$Room[aux] = "Cabin" 
data$Room = factor(data$Room)

data$Embarked = factor(data$Embarked, labels = c("Cherbourg", "Queenstown", "Southampton"))

ggplot(data) +
  aes(x = Embarked, fill = Pclass, alpha = 0.9) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("deepskyblue4","darkgoldenrod", "brown")) +
  geom_text(stat="count", aes(label=stat(count)), vjust=-0.5) +
  facet_grid(Room~.) +
  xlab("Port of embarkation") +
  ylab("Number of passengers") +
  scale_alpha(guide = 'none') +
  labs(title = "Relationship between embarkation, class and cabin")


# How does travelling alone affects each interval of age in their chances of surviving?

aux = data$Parch == 0 & data$SibSp == 0
sum(aux)
travels_alone = rep("Not Alone",length(aux))
travels_alone[aux] = "Alone"
data$travels_alone = travels_alone

children = which(data$Age < 18)
adults = which(data$Age >= 18 & data$Age < 60)
seniors = which(data$Age >= 60)
stages = rep("children", length(data$Age))
stages[adults] = "adult"
stages[seniors] = "senior"
data$stages = stages
data$stages = factor(data$stages, levels = c("children", "adult", "senior"))


ggplot(data=data) +
  aes(y = stages) +
  geom_bar(aes(fill = Survived),position = "fill") +
  facet_grid(travels_alone~.) +
  scale_fill_manual(labels= c("Not Survived", "Survived"), values = c("firebrick4","forestgreen")) +
  scale_x_continuous(labels=percent) +
  xlab("Percentages") +
  ylab("Ages Stages") +
  labs(title="Proportion of survival being accompanied in each stage")


