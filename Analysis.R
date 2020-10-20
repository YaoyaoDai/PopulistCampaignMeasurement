### Load the polling data
rm(list=ls(all=TRUE))

## Initial polls Candidate-Year 
PollsCY <- read.csv("Polls.csv", fileEncoding="UTF-8-BOM")
library(plyr)
PollsCY <- rename(PollsCY, c("Margin_initial"="Margin", "Win_initial"="Win"))
PollsCY$Margin <- PollsCY$Margin/2

#PollsCY$Candidate_Year <- paste(PollsCY$Candidate, PollsCY$Year, sep = " ")

## Extrapolated polls Candidate-Year-Month
PollsCYM <- read.csv("Polls_mod.csv", fileEncoding="UTF-8-BOM")
library(reshape2)
PollsCYM <- melt(PollsCYM,
                 measure.vars=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November"),
                 variable.name="Month", value.name="Poll")
PollsCYM$Year_Month <- paste(PollsCYM$Year, PollsCYM$Month, sep = " ")
PollsCYMmax <- aggregate(PollsCYM$Poll, by = list(PollsCYM$Year, PollsCYM$Month), max)
PollsCYMmax$Year_Month <- paste(PollsCYMmax$Group.1, PollsCYMmax$Group.2, sep = " ")
PollsCYMmax <- PollsCYMmax[3:4]
colnames(PollsCYMmax)[colnames(PollsCYMmax)=="x"] <- "Max"
maxN <- function(x, N=2){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=len-N+1)[len-N+1]
}
PollsCYMmin <- aggregate(PollsCYM$Poll, by = list(PollsCYM$Year, PollsCYM$Month), maxN) # Second max (excl third candidates)
PollsCYMmin$Year_Month <- paste(PollsCYMmin$Group.1, PollsCYMmin$Group.2, sep = " ")
PollsCYMmin <- PollsCYMmin[3:4]
colnames(PollsCYMmin)[colnames(PollsCYMmin)=="x"] <- "Min"
PollsCYM <- merge(PollsCYM, PollsCYMmax, by = "Year_Month")
PollsCYM <- merge(PollsCYM, PollsCYMmin, by = "Year_Month")
PollsCYM$Win <- ifelse(PollsCYM$Poll == PollsCYM$Max, 1, 0)
PollsCYM$Margin <- ifelse(PollsCYM$Win == 1, PollsCYM$Max - PollsCYM$Min, PollsCYM$Poll - PollsCYM$Max)
PollsCYM$Margin <- PollsCYM$Margin/2
#PollsCYM$Candidate_Year_Month <- paste(PollsCYM$Candidate, PollsCYM$Year, PollsCYM$Month, sep = " ")
PollsCYM <- PollsCYM[ , !names(PollsCYM) %in% c("Year_Month")]
rm(PollsCYMmax)
rm(PollsCYMmin)
rm(maxN)
### Load and merge the speech data

## Candidate-Year data

SpeechesCY <- read.csv("CandidateMerged_Level.csv", fileEncoding="UTF-8-BOM")
SpeechesCY$Populist <- ifelse(SpeechesCY$Pop_prop == 0, 0, 1)
## Merge the data on the Candidate-Year level
dataCY <- merge(PollsCY, SpeechesCY, by = c("Candidate", "Year"))
dataCY <- dataCY[dataCY$Third_can == 0,]
dataCY$Party <- factor(dataCY$Party)
dataCY$Candidate_Year <- paste(dataCY$Candidate, dataCY$Year, sep = " ")

### Candidate-Year-Month data

SpeechesCYM <- read.csv("CandidateYearMonth_Level_Pop.csv", fileEncoding="UTF-8-BOM")
SpeechesCYM$Populist <- ifelse(SpeechesCYM$Pop_prop == 0, 0, 1)

dataCYM <- merge(PollsCYM, SpeechesCYM, by = c("Candidate", "Year", "Month"))
dataCYM <- dataCYM[dataCYM$Third_can == 0,]
dataCYM$Party <- factor(dataCYM$Party)
dataCYM$Candidate_Year <- paste(dataCYM$Candidate, dataCYM$Year, sep = " ")
dataCYM$Candidate_Year_Month <- paste(dataCYM$Candidate, dataCYM$Year, dataCYM$Month, sep = " ")

## Speech-level and Subspeech-level data

Speeches <- read.csv("Speech_Level_Pop_MajorCand.csv", fileEncoding="UTF-8-BOM")
SubSpeeches <- read.csv("Subspeech_Level_Pop_MajorCand.csv", fileEncoding="UTF-8-BOM")
SubSpeeches_all <- read.csv("AllCandidate_SubSpeech_Pop.csv", fileEncoding="UTF-8-BOM")

library(plyr)
#Speeches$Month <- revalue(Speeches$Month, c("10.0"="October", "11.0"="November", "7.0"="July", "8.0"="August", "9.0"="September"))
Speeches$Month[Speeches$Month==""] <- NA
Speeches$Month <- factor(Speeches$Month)
#hist(Speeches$Pop_prop)
Speeches$Populist <- ifelse(Speeches$Pop_prop == 0, 0, 1)
#mean(Speeches$Populist)
#Speeches$Candidate_Year <- paste(Speeches$Candidate, Speeches$Year, sep = " ")
#Speeches$Candidate_Year_Month <- paste(Speeches$Candidate, Speeches$Year, Speeches$Month, sep = " ")
SubSpeeches$Month[SubSpeeches$Month==""] <- NA
SubSpeeches$Month <- factor(SubSpeeches$Month)

dataSpeechCY <- merge(PollsCY, Speeches, by = c("Candidate", "Year"))
dataSpeechCY$Candidate_Year <- paste(dataSpeechCY$Candidate, dataSpeechCY$Year, sep = " ")
dataSpeechCY <- dataSpeechCY[dataSpeechCY$Third_can == 0,]
dataSpeechCY$Party <- factor(dataSpeechCY$Party)
dataSpeechCY$Speech_len_01 <- (dataSpeechCY$Speech_len - min(dataSpeechCY$Speech_len, na.rm = TRUE)) / 
  (max(dataSpeechCY$Speech_len, na.rm = TRUE) - min(dataSpeechCY$Speech_len, na.rm = TRUE))

dataSpeechCYM <- merge(PollsCYM, Speeches, by = c("Candidate", "Year", "Month"))
dataSpeechCYM$Candidate_Year <- paste(dataSpeechCYM$Candidate, dataSpeechCYM$Year, sep = " ")
dataSpeechCYM$Candidate_Year_Month <- paste(dataSpeechCYM$Candidate, dataSpeechCYM$Year, dataSpeechCYM$Month, sep = " ")

dataSubSpeechCYM <- merge(PollsCYM, SubSpeeches, by = c("Candidate", "Year", "Month"))
dataSubSpeechCYM$Candidate_Year <- paste(dataSubSpeechCYM$Candidate, dataSubSpeechCYM$Year, sep = " ")
dataSubSpeechCYM$Year_Candidate <- paste(dataSubSpeechCYM$Year, dataSubSpeechCYM$Candidate, sep = " ")
dataSubSpeechCYM$Candidate_Year_Month <- paste(dataSubSpeechCYM$Candidate, dataSubSpeechCYM$Year, dataSubSpeechCYM$Month, sep = " ")

# Force excluding third candidates 
dataSpeechCYM <- dataSpeechCYM[dataSpeechCYM$Third_can == 0,]
dataSpeechCYM$Party <- factor(dataSpeechCYM$Party)
dataSpeechCYM$Speech_len_01 <- (dataSpeechCYM$Speech_len - min(dataSpeechCYM$Speech_len, na.rm = TRUE)) / 
  (max(dataSpeechCYM$Speech_len, na.rm = TRUE) - min(dataSpeechCYM$Speech_len, na.rm = TRUE))

dataSubSpeechCYM <- dataSubSpeechCYM[dataSubSpeechCYM$Third_can == 0,]
dataSubSpeechCYM$Party <- factor(dataSubSpeechCYM$Party)

### List of the datasets

#IV (polling) varies only by Candidate-Year or Candidate-Year-Month [Margin/Win]
#DV (populism) varies by Speech [Pop_prop/Populist]

# dataSpeechCYM     Speech-level Populism by Candidate-Year-Month Polling [MAIN]
# dataCYM           Candidate-Year-Month Populism by Candidate-Year-Month Polling
# dataSpeechCY      Speech-level Populism by Candidate-Year Polling
# dataCY            Candidate-Year Populism by Candidate-Year Polling

# dataSubSpeechCYM     Subspeech-level Populism by Candidate-Year-Month Polling for ggplot visualizations

# Start at 1952 

dataCY1952 <- dataCY[dataCY$Year >= 1950,]
dataCYM1952 <- dataCYM[dataCYM$Year >= 1950,]
dataSpeechCYM1952 <- dataSpeechCYM[dataSpeechCYM$Year >= 1950,]
dataSpeechCY1952 <- dataSpeechCY[dataSpeechCY$Year >= 1950,]
dataSubSpeechCYM1952 <- dataSubSpeechCYM[dataSubSpeechCYM$Year >= 1950,]

dataSpeechCYM31952 <- dataSpeechCYM[dataSpeechCYM1952$Month == "September" | dataSpeechCYM1952$Month == "October" |  dataSpeechCYM1952$Month == "November",]

dataSubSpeechCYM3 <- dataSubSpeechCYM[dataSubSpeechCYM$Month == "September" | dataSubSpeechCYM$Month == "October" |  dataSubSpeechCYM$Month == "November",]
dataSubSpeechCYM31952 <- dataSubSpeechCYM3[dataSubSpeechCYM3$Year >= 1950,]


### Descriptives

#prop.table(table(dataSpeechCYM$Type))
#tapply(dataSpeechCYM$Type, dataSpeechCYM$Year, table)
#prop.table(table(dataSpeechCYM$Chanel))

### Descriptive statistics (Table A1)
library(stargazer)
dataSpeechCYM1952$Republican <- ifelse(dataSpeechCYM1952$Party == "R", 1, 0)

stargazer(dataSpeechCYM1952[c("Pop_prop", "Win", "Margin", "Incumbent", "Republican", "Speech_len_01")],
          covariate.labels=c("Populism Rhetoric (Average Share)", "Electoral Advantage (Binary)", "Electoral Advantage (Percent)", "Party Incubency", "Party Membership (Republican)",
                             "Speech Length (Standardized)"), column.sep.width = "-5pt",
          nobs = FALSE, digits=2, label = "tab:summary",
          title="Summary Statistics", iqr = TRUE) #Full Speech-level Data (n = 3,435). The mean speech length is 2,167 words.

## Reproduce some of the results from Bonikowski2015
library(ggplot2)

# Variation across all, including primary candidate, exluding candidates with 
# fewer than 5 speeches
#SpeechesAll <- read.csv("Speech_Level_Pop_All.csv", fileEncoding="UTF-8-BOM")

#SpeechesAll <- read.csv("AllCandidate_SubSpeech_Pop.csv", fileEncoding="UTF-8-BOM")

#ggplot(SpeechesAll, aes(x = reorder(CandiYear, Pop_class), y = Pop_class, fill = Party)) + geom_bar(stat = "summary", fun.y = "mean") + 
#  scale_fill_manual(values = c("D" = "blue", "R" = "red")) + theme_minimal() + coord_flip() +
#  labs(title="", x ="", y = "Populist rhetoric, average share")

# Variation across candidate and campaign

## Figure 2: Average Share of Populist Rhetoric across Years
BG <- read.csv("BG_Populism.csv", fileEncoding="UTF-8-BOM") # Bonikovski year level measure 
library(dplyr)
yearave <- summarise_at(group_by(dataSubSpeechCYM1952,Year),vars(Pop_class),list(mean=mean))
yearave <- yearave[which(yearave$Year<=1996),]
  
a <- (max(yearave$mean)-min(yearave$mean[which(yearave$mean>0)]))/(max(BG$Populism)-min(BG$Populism)) 
b <- max(yearave$mean) - a * max(BG$Populism)

BG$PopScaled <- a * BG$Populism + b

yearplot <- ggplot(dataSubSpeechCYM1952, aes(x = Year, y = Pop_class)) + geom_bar(stat = "summary", fun.y = "mean") + 
  scale_x_continuous("Year", labels = as.character(dataSubSpeechCYM1952$Year), breaks = dataSubSpeechCYM1952$Year) +
  labs(title="", x ="", y = "Populist rhetoric, average share")  #+ theme(legend.position="none") 

yearplot + geom_point(data = BG, aes(x = Year, y = PopScaled, color='grey') ) + theme_minimal(base_size = 14) + 
  scale_color_identity(name = "", guide = "legend", labels = c('Bonikowski and Gidron (2015)')) + 
  theme(legend.position = "top") + theme(axis.text.x = element_text(angle = 45))

ggsave("images/YearLevel_prct.png")


yearplot <- ggplot(dataSubSpeechCYM31952, aes(x = Year, y = Pop_class)) + geom_bar(stat = "summary", fun.y = "mean") + 
  scale_x_continuous("Year", labels = as.character(dataSubSpeechCYM31952$Year), breaks = dataSubSpeechCYM31952$Year) +
  labs(title="", x ="", y = "Populist rhetoric, average share")  #+ theme(legend.position="none") 

yearplot + geom_point(data = BG, aes(x = Year, y = Populism*0.16, color='grey') ) + theme_minimal(base_size = 14) + 
  scale_color_identity(name = "", guide = "legend", labels = c('Bonikowski and Gidron (2015)')) + 
  theme(legend.position = "top") + theme(axis.text.x = element_text(angle = 45))


## Figure 3: verage Share of Populist Rhetoric by Party across Years

ggplot(dataSubSpeechCYM1952, aes(x = Year, y = Pop_class, fill = Party)) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") + 
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_x_continuous("Year", labels = as.character(dataSubSpeechCYM1952$Year), breaks = dataSubSpeechCYM1952$Year) +
  labs(title="", x ="", y = "Populist rhetoric, average share\n") + 
  scale_fill_manual(name = "", labels = c("Democratic", "Republican"), values = c("black", "grey")) +
  theme(legend.position = "top")

## Figure A1: Average Share of Populist Rhetoric across Campaigns

# Variation across years by party

ggplot(dataSubSpeechCYM1952, aes(x = Year_Candidate, y = Pop_class, fill = Party)) + geom_bar(stat = "summary", fun.y = "mean") + 
  scale_fill_manual(values = c("D" = "black", "R" = "grey")) + theme_minimal(base_size = 14) + coord_flip() + theme(legend.position="none") +
  labs(title="", x ="", y = "Populist rhetoric, average share") +
  scale_fill_manual(name = "", labels = c("Democratic", "Republican"), values = c("black", "grey")) +
  theme(legend.position = "top")

ggsave("images/candidate_prct.png")

ggplot(SubSpeeches_all, aes(x = YearCandi, y = Pop_class, fill = Party)) + geom_bar(stat = "summary", fun.y = "mean") + 
  scale_fill_manual(values = c("D" = "black", "R" = "grey")) + theme_minimal(base_size = 14) + coord_flip() + theme(legend.position="none") +
  labs(title="", x ="", y = "Populist rhetoric, average share") +
  scale_fill_manual(name = "", labels = c("Democratic", "Republican"), values = c("black", "grey")) +
  theme(legend.position = "top")

ggplot(dataSubSpeechCYM31952, aes(x = Year_Candidate, y = Pop_class, fill = Party)) + geom_bar(stat = "summary", fun.y = "mean") + 
  scale_fill_manual(values = c("D" = "black", "R" = "grey")) + theme_minimal(base_size = 14) + coord_flip() + theme(legend.position="none") +
  labs(title="", x ="", y = "Populist rhetoric, average share") +
  scale_fill_manual(name = "", labels = c("Democratic", "Republican"), values = c("black", "grey")) +
  theme(legend.position = "top")

ggsave("images/candidate_3prct.png")


#ggplot(dataSubSpeechCYM1952, aes(x = reorder(Candidate, Pop_class), y = Pop_class, fill = Party)) + geom_bar(stat = "summary", fun.y = "mean") + 
#  scale_fill_manual(values = c("D" = "blue", "R" = "red")) + theme_minimal() + coord_flip() +
#  labs(title="", x ="", y = "Populist rhetoric, average share")

#ggplot(dataSubSpeechCYM1952, aes(x = reorder(Candidate_Year, Pop_class), y = Pop_class, fill = Party)) + geom_bar(stat = "summary", fun.y = "mean") + 
#  scale_fill_manual(values = c("D" = "blue", "R" = "red")) + theme_minimal(base_size = 14) + coord_flip() +
#  labs(title="", x ="", y = "Populist rhetoric, average share\n") + 
#  theme(legend.position="top") + scale_fill_manual(name = "Party", labels = c("Democratic", "Republican"), values = c("blue", "red")) 

  
# Variation across months
ggplot(dataSubSpeechCYM, aes(x = Month, y = Pop_class)) + geom_bar(stat = "summary", fun.y = "mean") + theme_minimal() 

#dataSpeechCYM$Pop_prop[dataSpeechCYM$Candidate == "Dwight Eisenhower"]

## Decomposing/categorizing variation by time, candidate, speech

summary(lm(Pop_prop ~ as.factor(Candidate), data = dataSpeechCYM))
summary(lm(Pop_prop ~ as.factor(Year), data = dataSpeechCYM))
summary(lm(Pop_prop ~ as.factor(Month), data = dataSpeechCYM))
summary(lm(Pop_prop ~ as.factor(Candidate_Year), data = dataSpeechCYM))
summary(lm(Pop_prop ~ as.factor(Candidate_Year_Month), data = dataSpeechCYM))

summary(lm(Pop_prop ~ as.factor(Candidate), data = dataCY))
summary(lm(Pop_prop ~ as.factor(Year), data = dataCY))

summary(lm(Pop_prop ~ as.factor(Candidate), data = dataCYM))
summary(lm(Pop_prop ~ as.factor(Year), data = dataCYM))
summary(lm(Pop_prop ~ as.factor(Month), data = dataCYM))
summary(lm(Pop_prop ~ as.factor(Candidate) + as.factor(Year), data = dataCYM))
summary(lm(Pop_prop ~ as.factor(Candidate) + as.factor(Year) + as.factor(Month), data = dataCYM))

summary(lm(Pop_prop ~ as.factor(Candidate), data = dataSpeechCY)) # less than 25%
summary(lm(Pop_prop ~ as.factor(Year), data = dataSpeechCY)) # less than 20%
summary(lm(Pop_prop ~ as.factor(Candidate) + as.factor(Year), data = dataSpeechCY)) # less than 30%

### Speech-level Analysis
library(gplots)
library(stargazer)

## Figure 4: Electoral Advantage and Populist Rhetoric in U.S. Presidential Speeches

plotmeans(Pop_prop ~ Win, data = dataSpeechCYM31952, ylim = c(0, 0.03),p = 0.95, barcol = "black", barwidth = 1, connect=F,
          xlab="Electoral advantage", ylab="Populist rhetoric, average share", main="", n.label = F)
plotmeans(Pop_prop ~ Win, data=dataSpeechCYM31952, p = 0.84, barcol = "black", barwidth = 2, connect=F,
          xlab="", ylab="", main="", n.label = F, xaxt = "n", add = TRUE)


#options(scipen=999)
#summary(lm(Pop_class ~ Win, data = dataSubSpeechCYM))
#summary(lm(Pop_prop ~ Win, data = dataSpeechCYM))
#summary(lm(Pop_class ~ Win + Incumbent + Party + factor(Month) + factor(Year) + factor(Candidate), data = dataSubSpeechCYM))
#summary(lm(Pop_prop ~ Win + Incumbent + Party + Speech_len + Type + factor(Month) + factor(Year) + factor(Candidate), data = dataSpeechCYM))


W0a <- lm(Pop_prop ~ Win, data = dataSpeechCYM1952)
W1a <- lm(Pop_prop ~ Win + Incumbent + Party, data = dataSpeechCYM1952)
W2a <- lm(Pop_prop ~ Win + Incumbent + Party + Speech_len_01 + factor(Month), data = dataSpeechCYM1952)
W3a <- lm(Pop_prop ~ Win + Incumbent + Party + Speech_len_01 + factor(Month) + factor(Year) + factor(Candidate), data = dataSpeechCYM1952)

dataSpeechCYM1952T <- dataSpeechCYM1952[dataSpeechCYM1952$Candidate != "Donald J. Trump",]
dataSpeechCYM1952Y <- dataSpeechCYM1952[dataSpeechCYM1952$Year < 2016,]

W4a <- lm(Pop_prop ~ Win + Incumbent + Party + Speech_len_01 + factor(Month) + factor(Year) + factor(Candidate), data = dataSpeechCYM1952T)


# Table 1: Populist Rhetoric as a Function of Electoral Advantage

stargazer(W0a, W1a, W2a, W3a, W4a,  
          title="Populist Rhetoric as a Function of Electoral Advantage", dep.var.caption = "", dep.var.labels="",
          covariate.labels=c("Electoral Advantage", "Party Incumbency", "Partisanship (GOP)", 
                             "Speech Length"),
          align=TRUE, no.space=TRUE, column.sep.width = "-15pt",
          label="tab:dataSpeechCYM",
          omit = c("Year", "Month", "Candidate"),
          omit.stat=c("rsq", "ser", "f", "adj.rsq"), notes = "", notes.append = F,  notes.label = "",
          star.char = c("*", "**", "***"), star.cutoffs = c(0.05, 0.01, 0.001),
          add.lines = list(c("Month FE", "No", "No", "Yes", "Yes", "Yes"),
                           c("Year FE", "No", "No", "No", "Yes", "Yes"),
                           c("Candidate FE", "No", "No", "No", "Yes", "Yes"),
                           c("Excluding D. Trump", "No", "No", "No", "No", "Yes")))

### ROBUSTNESS CHECKS

## Table A2: Populist Rhetoric as a Function of Electoral Advantage (Sep.-Nov. Only)

dataSpeechCYM3 <- dataSpeechCYM[dataSpeechCYM$Month == "September" | dataSpeechCYM$Month == "October" |  dataSpeechCYM$Month == "November",]
dataSpeechCYM31952 <- dataSpeechCYM3[dataSpeechCYM3$Year >= 1950,]

W0d <- lm(Pop_prop ~ Win, data = dataSpeechCYM31952)
W1d <- lm(Pop_prop ~ Win + Incumbent + Party, data = dataSpeechCYM31952)
W2d <- lm(Pop_prop ~ Win + Incumbent + Party + Speech_len_01 + factor(Month), data = dataSpeechCYM31952)
W3d <- lm(Pop_prop ~ Win + Incumbent + Party + Speech_len_01 + factor(Month) + factor(Year) + factor(Candidate), data = dataSpeechCYM31952)

dataSpeechCYM31952T <- dataSpeechCYM31952[dataSpeechCYM31952$Candidate != "Donald J. Trump",]

W4d <- lm(Pop_prop ~ Win + Incumbent + Party + Speech_len_01 + factor(Month) + factor(Year) + factor(Candidate), data = dataSpeechCYM31952T)


stargazer(W0d, W1d, W2d,
          title="Populist Rhetoric as a Function of Electoral Advantage (September, October, November Only)", dep.var.caption = "", dep.var.labels="",
          covariate.labels=c("Electoral Advantage", "Party Incumbency", "Partisanship (GOP)", 
                             "Speech Length"),
          label="tab:dataSpeechCYM3",
          align=TRUE, no.space=TRUE, column.sep.width = "-5pt", 
          omit = c("Year", "Month", "Candidate"),
          omit.stat=c("rsq", "ser", "f", "adj.rsq"), notes = "", notes.append = F,  notes.label = "",
          star.char = c("*", "**", "***"), star.cutoffs = c(0.05, 0.01, 0.001),
          add.lines = list(c("Month FE", "No", "No", "Yes", "Yes")))

stargazer(W0d, W1d, W2d, W3d, W4d, 
          title="Populist Rhetoric as a Function of Electoral Advantage (September, October, November Only)", dep.var.caption = "", dep.var.labels="",
          covariate.labels=c("Electoral Advantage", "Party Incumbency", "Partisanship (GOP)", 
                             "Speech Length"),
          label="tab:dataSpeechCYM3",
          align=TRUE, no.space=TRUE, column.sep.width = "-15pt",
          omit = c("Year", "Month", "Candidate"),
          omit.stat=c("rsq", "ser", "f", "adj.rsq"), notes = "", notes.append = F,  notes.label = "",
          star.char = c("*", "**", "***"), star.cutoffs = c(0.05, 0.01, 0.001),
          add.lines = list(c("Month FE", "No", "No", "Yes", "Yes", "Yes"),
                           c("Year FE", "No", "No", "No", "Yes", "Yes"),
                           c("Candidate FE", "No", "No", "No", "Yes", "Yes"),
                           c("Excluding D. Trump", "No", "No", "No", "No", "Yes")))

## Table A: Populist Rhetoric as a Function of Electoral Advantage (Initial Polls Only)

W0b <- lm(Pop_prop ~ Win, data = dataSpeechCY1952)
W1b <- lm(Pop_prop ~ Win + Incumbent + Party, data = dataSpeechCY1952)
W2b <- lm(Pop_prop ~ Win + Incumbent + Party + Speech_len_01, data = dataSpeechCY1952)
W3b <- lm(Pop_prop ~ Win + Incumbent + Party + Speech_len_01 + factor(Year), data = dataSpeechCY1952)

stargazer(W0b, W1b, W2b, W3b,  
          title="Populist Rhetoric as a Function of Electoral Advantage (Initial Polls Only)", dep.var.caption = "", dep.var.labels="",
          covariate.labels=c("Electoral Advantage", "Party Incumbency", "Partisanship (GOP)", "Speech Length"),
          align=TRUE, no.space=TRUE, column.sep.width = "-5pt",
          label="tab:dataSpeechCY",
          omit.stat=c("rsq", "ser", "f", "adj.rsq"), notes = "", notes.append = F,  notes.label = "",
          star.char = c("*", "**", "***"), star.cutoffs = c(0.05, 0.01, 0.001),
          add.lines = list(c("Year FE", "No", "No", "No", "Yes"),
          c("Candidate FE", "No", "No", "No", "Yes")))
          
W0c <- lm(Pop_prop ~ Win, data = dataCYM1952)
W1c <- lm(Pop_prop ~ Win + factor(Year) + factor(Month), data = dataCYM1952)
#W1c <- lm(Pop_prop ~ Win + Incumbent + Party, data = dataCYM1952)
#W2c <- lm(Pop_prop ~ Win + Incumbent + Party + factor(Year), data = dataCYM1952)

stargazer(W0c, W1c,
          title="Populist Rhetoric as a Function of Electoral Advantage (Candidate-Month)", dep.var.caption = "", dep.var.labels="",
          covariate.labels=c("Electoral Advantage"),
          label="tab:dataCYM",
          align=TRUE, no.space=TRUE, column.sep.width = "-5pt", 
          omit = c("Year", "Month"),
          omit.stat=c("rsq", "ser", "f", "adj.rsq"), notes = "", notes.append = F,  notes.label = "",
          star.char = c("+", "*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          add.lines = list(c("Month FE", "No", "Yes"), c("Year FE", "No", "Yes")))


## Table A3: Populist Rhetoric as a Function of Electoral Advantage (Candidate-Month)
#summary(lm(Pop_prop ~ Win + Incumbent + Party + factor(Year), data = dataCYM))


#stargazer(W0c, W1c, W2c, 
#          title="Populist Rhetoric as a Function of Electoral Advantage (Candidate-Month)", dep.var.caption = "", dep.var.labels="",
#          covariate.labels=c("Electoral Advantage", "Party Incumbency", "Partisanship (GOP)"),
#          label="tab:dataCYM",
#          align=TRUE, no.space=TRUE, column.sep.width = "-5pt", 
#          omit = c("Year", "Month", "Candidate"),
#          omit.stat=c("rsq", "ser", "f", "adj.rsq"), notes = "", notes.append = F,  notes.label = "",
#          star.char = c("+", "*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
#          add.lines = list(c("Year FE", "No", "No", "Yes")))


## Table A: Populist Rhetoric as a Function of Electoral Advantage (Continuous)

W0a <- lm(Pop_prop ~ Margin, data = dataSpeechCYM1952)
W1a <- lm(Pop_prop ~ Margin + Incumbent + Party, data = dataSpeechCYM1952)
W2a <- lm(Pop_prop ~ Margin + Incumbent + Party + Speech_len_01 + factor(Month), data = dataSpeechCYM1952)
W3a <- lm(Pop_prop ~ Margin + Incumbent + Party + Speech_len_01 + factor(Month) + factor(Year) + factor(Candidate), data = dataSpeechCYM1952)

stargazer(W0a, W1a, W2a, W3a,  
          title="Populist Rhetoric as a Function of Electoral Advantage (Continuous)", dep.var.caption = "", dep.var.labels="",
          covariate.labels=c("Electoral Advantage (Percent)", "Party Incumbency", "Partisanship (GOP)", 
                             "Speech Length"),
          align=TRUE, no.space=TRUE, column.sep.width = "-5pt", 
          label="tab:Margin",
          omit = c("Year", "Month", "Candidate"),
          omit.stat=c("rsq", "ser", "f", "adj.rsq"), notes = "", notes.append = F,  notes.label = "",
          star.char = c("+", "*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          add.lines = list(c("Month FE", "No", "No", "Yes", "Yes"),
                           c("Year FE", "No", "No", "No", "Yes"),
                           c("Candidate FE", "No", "No", "No", "Yes")))
