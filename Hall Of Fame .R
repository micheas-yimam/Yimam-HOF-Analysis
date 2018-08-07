# Used R for all the analysis: http://www.sthda.com/english/wiki/two-way-anova-test-in-r#import-your-data-into-r

library(readr)
Hall_of_Fame_Data_R_Data_HOF <- read_csv("Hall of Fame Data - Sheet1.csv")

#Don't need the below code just to view my dataset. 
View(Hall_of_Fame_Data_R_Data_HOF)

install.packages("dplyr")

str(Hall_of_Fame_Data_R_Data_HOF)


## Changing the char values in to factors 
Hall_of_Fame_Data_R_Data_HOF$`Year Group` <- as.factor(Hall_of_Fame_Data_R_Data_HOF$`Year Group`)
Hall_of_Fame_Data_R_Data_HOF$`Race` <- as.factor(Hall_of_Fame_Data_R_Data_HOF$`Race`)


# check if the type changed. 
head(Hall_of_Fame_Data_R_Data_HOF)
table(Hall_of_Fame_Data_R_Data_HOF$`Year Group`, Hall_of_Fame_Data_R_Data_HOF$`Race` )

# needed
install.packages("ggpubr")






## War first then voter percentagte
WarBP <- boxplot(WAR ~ `Race` * `Year Group`, data=Hall_of_Fame_Data_R_Data_HOF, frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), ylab="WAR")

VoteBP <- boxplot(`Vote Percentage` ~ `Race` * `Year Group`, data=Hall_of_Fame_Data_R_Data_HOF, frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), ylab="%")

VoteInd <- boxplot(`Induction Hall Of Fame` ~ `Race` * `Year Group`, data=Hall_of_Fame_Data_R_Data_HOF, frame = FALSE, 
                  col = c("#00AFBB", "#E7B800"), ylab="%")


WarIntplot<-interaction.plot(x.factor = rev(Hall_of_Fame_Data_R_Data_HOF$`Year Group`), trace.factor = Hall_of_Fame_Data_R_Data_HOF$`Race`, 
                 response = Hall_of_Fame_Data_R_Data_HOF$WAR, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Year Grouping", ylab="War Average",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))

VoteIntplot<-interaction.plot(x.factor = rev(Hall_of_Fame_Data_R_Data_HOF$`Year Group`), trace.factor = Hall_of_Fame_Data_R_Data_HOF$`Race`, 
                              response = Hall_of_Fame_Data_R_Data_HOF$`Vote Percentage`, fun = mean, 
                              type = "b", legend = TRUE, 
                              xlab = "Year Grouping", ylab="Vote Percentage Average",
                              pch=c(1,19), col = c("#00AFBB", "#E7B800"))
# One Way Anova War 
WarAnova<-res.aov2 <- aov(WAR ~ `Year Group` + `Race`, data=Hall_of_Fame_Data_R_Data_HOF)
summary(res.aov2)

# One Way Anova Vote Percentage 
VoteAnova <- res.aov2 <- aov(`Vote Percentage` ~ `Year Group` + `Race`, data=Hall_of_Fame_Data_R_Data_HOF)
summary(res.aov2)

# two way Anova War 
IndAnova<-res.aov2 <- aov(WAR ~ `Year Group` + `Race`, data=Hall_of_Fame_Data_R_Data_HOF)
IndAnova2 <- aov(WAR ~ `Year Group` + `Race` + `Year Group`:`Race`, data=Hall_of_Fame_Data_R_Data_HOF)
summary(IndAnova2)

# two way Anova Vote Percentage
VoteAnova<-res.aov2 <- aov(WAR ~ `Year Group` + `Race`, data=Hall_of_Fame_Data_R_Data_HOF)
VoteAnova2 <- aov(`Vote Percentage` ~ `Year Group` + `Race` + `Year Group`:`Race`, data=Hall_of_Fame_Data_R_Data_HOF)
summary(VoteAnova2)

# two way Anova Vote Percentage
IndAnova<-res.aov2 <- aov(`Induction Hall Of Fame` ~ `Year Group` + `Race`, data=Hall_of_Fame_Data_R_Data_HOF)
IndAnova2 <- aov(`Induction Hall Of Fame` ~ `Year Group` + `Race` + `Year Group`:`Race`, data=Hall_of_Fame_Data_R_Data_HOF)
summary(IndAnova2)

# Computing Summary Statistics 
#IND
library("dplyr")
SummaryIND<-group_by(Hall_of_Fame_Data_R_Data_HOF, Hall_of_Fame_Data_R_Data_HOF$`Year Group`, Hall_of_Fame_Data_R_Data_HOF$`Race`) %>%
  summarise(
    count = n(),
    mean = mean(Hall_of_Fame_Data_R_Data_HOF$`Induction Hall Of Fame`, na.rm = TRUE),
    sd = sd(Hall_of_Fame_Data_R_Data_HOF$`Induction Hall Of Fame`, na.rm = TRUE)
  )


#WAR
library("dplyr")
SummaryWAR<-group_by(Hall_of_Fame_Data_R_Data_HOF, Hall_of_Fame_Data_R_Data_HOF$`Year Group`, Hall_of_Fame_Data_R_Data_HOF$`Race`) %>%
  summarise(
    count = n(),
    mean = mean(Hall_of_Fame_Data_R_Data_HOF$WAR, na.rm = TRUE),
    sd = sd(Hall_of_Fame_Data_R_Data_HOF$WAR, na.rm = TRUE)
  )


#Vote Percentage
SummaryVote<-group_by(Hall_of_Fame_Data_R_Data_HOF, Hall_of_Fame_Data_R_Data_HOF$`Year Group`, Hall_of_Fame_Data_R_Data_HOF$`Race`) %>%
  summarise(
    count = n(),
    mean = mean(Hall_of_Fame_Data_R_Data_HOF$`Vote Percentage`, na.rm = TRUE),
    sd = sd(Hall_of_Fame_Data_R_Data_HOF$`Vote Percentage`, na.rm = TRUE)
  )

#Check the homogeneity of variance assumption
plot(WarAnova2, 1)

## This is all refferring to WAR
#Check the normality assumpttion NOT NORMAL
plot(IndAnova2, 2)
IndAnova2Residulas <- residuals(object = IndAnova2)
#Normally is violated because The conclusion above, is supported by the Shapiro-Wilk test on the ANOVA residuals
#(W = 0.98, p = 0.5) which finds no indication that normality is violated.
shapiro.test(x = IndAnova2Residulas )

plot(VoteAnova2, 2)
VoteAnova2Residulas <- residuals(object = VoteAnova2)
#Normally is violated because The conclusion above, is supported by the Shapiro-Wilk test on the ANOVA residuals
#(W = 0.98, p = 0.5) which finds no indication that normality is violated.
shapiro.test(x = VoteAnova2Residulas )


plot(IndAnova2, 2)
IndAnova2Residulas <- residuals(object = IndAnova2)
#Normally is violated because The conclusion above, is supported by the Shapiro-Wilk test on the ANOVA residuals
#(W = 0.98, p = 0.5) which finds no indication that normality is violated.
shapiro.test(x = IndAnova2Residulas )

# Group 1 simulation, pull 32 names 

# Group 2 simulation pull 149 names

# Group 3 simulation pull 227 names

setwd("~/Donwloads")
library(readr)
Graph <- read_csv("Hall of Fame Data - Graph.csv")
install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")
hist(Graph$`Group 1 Histogram`)
install.packages("ggplot2")
library(ggplot2)
qplot(Graph$`Group 1 Histogram`, geom="histogram")
qplot(Graph$`Group 2 Histogram`, geom="histogram")
qplot(Graph$`Group 3 Histogram`, geom="histogram")
ggplot(df, aes(x=weight)) + geom_histogram()
# Add mean line
p+ geom_vline(aes(xintercept=mean(weight)),
              color="blue", linetype="dashed", size=1)
ggplot(df, aes(x=Graph$`Group 1 Histogram`)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

Group1Histo<-ggplot2.histogram(data=Graph$`Group 1 Histogram`)+
  geom_histogram(aes(fill = ..count..))+
  scale_fill_gradient("Count", low = "green", high = "red")
Group2Histo<-ggplot2.histogram(data=Graph$`Group 2 Histogram`)+
  geom_histogram(aes(fill = ..count..))+
  scale_fill_gradient("Count", low = "green", high = "red")
Group3Histo<-ggplot2.histogram(data=Graph$`Group 3 Histogram`)+
  geom_histogram(aes(fill = ..count..))+
  scale_fill_gradient("Count", low = "green", high = "red")

CountBP <- boxplot(Count ~ `Group` * `Color`, data=Graph, frame = FALSE, 
                 col = c("#00AFBB", "#E7B800"), ylab="Number of Players",xlab="Group#.POC")
WarBP <-boxplot(`Average WAR` ~ `Group` * `Color`, data=Graph, frame = FALSE, 
                col = c("#00AFBB", "#E7B800"), ylab="WAR",xlab="Group#.POC")
VoteBP <-boxplot(`Average Vote percentage` ~ `Group` * `Color`, data=Graph, frame = FALSE, 
                col = c("#00AFBB", "#E7B800"), ylab="Vote %",xlab="Group#.POC")

CountIntplot<-interaction.plot(x.factor = rev(Graph$`Group`), trace.factor = Graph$`Color`, 
                             response = Graph$Count, fun = mean, 
                             type = "b", legend = TRUE, 
                             xlab = "Year Grouping", ylab="Number of Players",
                             pch=c(1,2000), col = c("#00AFBB", "#E7B800"))
