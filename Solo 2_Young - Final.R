#Solo 2 Assignment Code
#MSDS 450, Winter 2019

#Load Libraries
require(useful)
require(Hmisc)
library(HSAUR)
library(MVA)
library(HSAUR2)
library(fpc)
library(mclust)
library(lattice)
library(car)
library(proxy)
library(VIM) #Missingness Map
library(mice)
library(plyr) 
library(likert) #Visualize Likert Scale Data
require(ggplot2)
library(reshape)
library(dummies) #For functions
library(bayesm) #Hierarchical Bayes Multinomial Logit Regression
library(knitr)
library(scales)

# Multiple plot function

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

###### Load Data #####
setwd("~/R/MSDS 450/Solo 2")

# Load the dataset
load("stc-cbc-respondents-v3.RData") #Load respondent data
ls() #Load dataframe
str(resp.data.v3)
head(resp.data.v3)
#resp.data.v3

#Load dataframes and csv provided
resp.data.v3 <- resp.data.v3
resp.data.eda <- resp.data.v3 #For EDA
taskV3 <- taskV3 <- read.csv("stc-dc-task-cbc -v3.csv", sep="\t")
str(taskV3) #Levels indicated by 0,1,2,3 according to prob description; 108 rows, 7 columns
head(taskV3)
scenarios.data <- read.csv("extra-scenarios-v3.csv") #Descriptions of the two additional
ex_scen <- read.csv("extra-scenarios.csv")

#choice scenarios that you'll analyze after estimating your MNL model

# Load functions
load("efCode.RData") 
ls() #Load dataframe
#Includes a couple of R functions that you'll use to code your attributes and levels 
#as the predictor variables for MNL model.

str(efcode.att.f)
str(efcode.attmat.f)
str(resp.data.v3)
apply(resp.data.v3[4:39], 2, function(x){tabulate(na.omit(x))}) #summarizes info

###################################### Exploratory Data Analysis #####################################

#convert to Factors
resp.data.eda$D1 <- as.factor(resp.data.eda$D1)
resp.data.eda$D2 <- as.factor(resp.data.eda$D2)
resp.data.eda$D3 <- as.factor(resp.data.eda$D3)

resp.data.eda$D5 <- as.factor(resp.data.eda$D5)
resp.data.eda$D6 <- as.factor(resp.data.eda$D6)

#resp.data.eda$STT2r1 <- as.factor(resp.data.eda$STT2r1)
#resp.data.eda$STT2r2 <- as.factor(resp.data.eda$STT2r4)
#resp.data.eda$STT2r3 <- as.factor(resp.data.eda$STT2r4)
#resp.data.eda$STT2r4 <- as.factor(resp.data.eda$STT2r4)

#resp.data.eda$D4r1 <- as.factor(resp.data.eda$D4r1)
#resp.data.eda$D4r2 <- as.factor(resp.data.eda$D4r2)
#resp.data.eda$D4r3 <- as.factor(resp.data.eda$D4r3)
#resp.data.eda$D4r4 <- as.factor(resp.data.eda$D4r4)
#resp.data.eda$D4r5 <- as.factor(resp.data.eda$D4r5)
#resp.data.eda$D4r6 <- as.factor(resp.data.eda$D4r6)

#Descriptive Statistics
str(resp.data.eda) 
head(resp.data.eda)
tail(resp.data.eda)
summary(resp.data.eda) 
dim(resp.data.eda) #424  55
describe(resp.data.eda)

#Check for Missingness
sum(is.na(resp.data.eda)) #1238
sapply(resp.data.eda, function(x) sum(is.na(x)))
aggr_plot <- aggr(resp.data.eda, col=c('#9ecae1','#de2d26'), numbers=TRUE,prop=FALSE, sortVars=TRUE, labels=names(resp.data.eda), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))
#D4r1 to D4r6; vList3

#Check missing data percentage
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(resp.data.eda,2,pMiss)
#D4r1 to D4r6 (147 Each); vList3 (356)

#Conduct Imputation
#Impute D4r1 to D4r6 with 0 and vList3 with 3 which equates to "other" 
resp.data.eda$D4r1[is.na(resp.data.eda$D4r1)] <- 0
resp.data.eda$D4r2[is.na(resp.data.eda$D4r2)] <- 0
resp.data.eda$D4r3[is.na(resp.data.eda$D4r3)] <- 0
resp.data.eda$D4r4[is.na(resp.data.eda$D4r4)] <- 0
resp.data.eda$D4r5[is.na(resp.data.eda$D4r5)] <- 0
resp.data.eda$D4r6[is.na(resp.data.eda$D4r6)] <- 0 #No children in household

#Impute vList3 with newly created level of 3 which equates to "other" 
resp.data.eda$vList3[is.na(resp.data.eda$vList3)] <- 3

#Check N/A values have been removed
summary(resp.data.eda)
sapply(resp.data.eda, function(x) sum(is.na(x)))
sum(is.na(resp.data.eda))
aggr_plot <- aggr(resp.data.eda, col=c('#9ecae1','#de2d26'), numbers=TRUE,prop=FALSE, sortVars=TRUE, labels=names(resp.data.eda), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))

#Buyer Interest
#STT2: How interested are you in purchasing or doing each of the following in the next 12 months?
#Values: 1-10

#Histogram & Density of [STT2r1] Purchasing a new tablet
a1<-ggplot(resp.data.eda, aes(x=STT2r1)) + 
  geom_histogram(aes(y=..count..),      
                 binwidth=.5,
                 colour="black", fill="#3182bd") + 
  geom_vline(aes(xintercept=mean(STT2r1, na.rm=T)),   
             color="red", linetype="dashed", size=1) +
  annotate("text", x = 7, y = 60, label = "Mean=6.3",color="black")+
  ggtitle("[STT2r1] New Tablet") +
  theme(plot.title = element_text(hjust = 0.5))
a1

#Histogram of [STT2r2] Purchasing a new smart phone
a2<-ggplot(resp.data.eda, aes(x=STT2r2)) + 
  geom_histogram(aes(y=..count..),      
                 binwidth=.5,
                 colour="black", fill="#3182bd") + 
  geom_vline(aes(xintercept=mean(STT2r2, na.rm=T)),   
             color="red", linetype="dashed", size=1) +
  annotate("text", x = 6.8, y = 60, label = "Mean=6.1",color="black")+
  ggtitle("[STT2r2] New Smart Phone") +
  theme(plot.title = element_text(hjust = 0.5))
a2

#Histogram of [STT2r3] Using cloud storage for storing your personal digital content
a3<-ggplot(resp.data.eda, aes(x=STT2r3)) + 
  geom_histogram(aes(y=..count..),      
                 binwidth=.5,
                 colour="black", fill="#3182bd") + 
  geom_vline(aes(xintercept=mean(STT2r3, na.rm=T)),   
             color="red", linetype="dashed", size=1) +
  annotate("text", x = 6.8, y = 60, label = "Mean=6.1",color="black")+
  ggtitle("[STT2r3] Cloud Storage") +
  theme(plot.title = element_text(hjust = 0.5))
a3

#Histogram of [STT2r4] Taking an online course to improve relevant skills
a4<-ggplot(resp.data.eda, aes(x=STT2r4)) + 
  geom_histogram(aes(y=..count..),      
                 binwidth=.5,
                 colour="black", fill="#3182bd") + 
  geom_vline(aes(xintercept=mean(STT2r4, na.rm=T)),   
             color="red", linetype="dashed", size=1) +
  annotate("text", x = 5.7, y = 60, label = "Mean=4.9",color="black")+
  ggtitle("[STT2r4] Taking Online Course") +
  theme(plot.title = element_text(hjust = 0.5))
a4

multiplot(a1, a2, a3, a4, cols=2)

#Respondent Demographics

#vList3. STC Ownership?
vList3c<-ggplot(resp.data.eda) +
  geom_bar( aes(vList3),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("vList3. STC Ownership" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())+ 
  annotate("text", x = 1, y = 20, label = "Owns",color="black") +
  annotate("text", x = 2, y = 40, label = "Has Owned",color="white")+
  annotate("text", x = 3, y = 40, label = "Never",color="white")
vList3c #Count

vList3<-ggplot(resp.data.eda, aes(x = as.factor(vList3))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),colour="#2b8cbe",fill="#2b8cbe") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs( title="vList3. STC Ownership", y = "", x = "")+
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank())+
  annotate("text", x = 1, y = 0.015, label = "Owns",color="white") +
  annotate("text", x = 2, y = 0.1, label = "Has Owned",color="white")+
  annotate("text", x = 3, y = 0.1, label = "Never",color="white")
vList3 #Percentage

#D1. What is your gender?
D1c<-ggplot(resp.data.eda) +
  geom_bar( aes(D1),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("D1. Gender" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())+ 
  annotate("text", x = 1, y = 100, label = "Male",color="white") +
  annotate("text", x = 2, y = 100, label = "Female",color="white")
D1c #Count


D1<-ggplot(resp.data.eda, aes(x = as.factor(D1))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),colour="#2b8cbe",fill="#2b8cbe") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs( title="D1. Gender", y = "", x = "")+
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank())+
  annotate("text", x = 1, y = 0.3, label = "Male",color="white") +
  annotate("text", x = 2, y = 0.3, label = "Female",color="white")
D1 #Percentage

#D2. What is your age range? 
D2c<-ggplot(resp.data.eda) +
  geom_bar(aes(D2),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("D2. Age Range" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())+
  annotate("text", x = 1, y = 5, label = "Under 18",color="black") +
  annotate("text", x = 2, y = 20, label = "18-24",color="white")+
  annotate("text", x = 3, y = 20, label = "25-34",color="white")+
  annotate("text", x = 4, y = 20, label = "35-44",color="white")+
  annotate("text", x = 5, y = 20, label = "45-54",color="white")+
  annotate("text", x = 6, y = 20, label = "55-64",color="white")+
  annotate("text", x = 7, y = 20, label = "65+",color="white")
D2c #Count

D2<-ggplot(resp.data.eda, aes(x = as.factor(D2))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),colour="#2b8cbe",fill="#2b8cbe") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs( title="D2. Age Range", y = "", x = "")+
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank())+
  annotate("text", x = 1, y = 0.02, label = "Under 18",color="black") +
  annotate("text", x = 2, y = 0.05, label = "18-24",color="white")+
  annotate("text", x = 3, y = 0.05, label = "25-34",color="white")+
  annotate("text", x = 4, y = 0.05, label = "35-44",color="white")+
  annotate("text", x = 5, y = 0.05, label = "45-54",color="white")+
  annotate("text", x = 6, y = 0.05, label = "55-64",color="white")+
  annotate("text", x = 7, y = 0.05, label = "65+",color="white")
D2 #Percentage


#D3. Are you a parent? 
D3c<-ggplot(resp.data.eda) +
  geom_bar(aes(D3),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("D3. Parent (Y/N)" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())+ 
  annotate("text", x = 1, y = 100, label = "Yes",color="white") +
  annotate("text", x = 2, y = 100, label = "No",color="white")
D3c #Count

D3<-ggplot(resp.data.eda, aes(x = as.factor(D3))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),colour="#2b8cbe",fill="#2b8cbe") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs( title="D3. Parent (Y/N)", y = "", x = "")+
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank())+
  annotate("text", x = 1, y = 0.3, label = "Yes",color="white") +
  annotate("text", x = 2, y = 0.3, label = "No",color="white")
D3 #Percentage

#D5. Total annual household income from all sources and before taxes for 2010? 
D5c<-ggplot(resp.data.eda) +
  geom_bar(aes(D5),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("D5. Total Annual Household Income ($)" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())+ 
  annotate("text", x = 1, y = 10, label = "<25k",color="white") +
  annotate("text", x = 2, y = 10, label = "25k-<35k",color="white")+
  annotate("text", x = 3, y = 10, label = "35k-<50k",color="white")+
  annotate("text", x = 4, y = 10, label = "50k-<75k",color="white")+
  annotate("text", x = 5, y = 10, label = "75k-<100k",color="white")+
  annotate("text", x = 6, y = 10, label = "100k-<200k+",color="white")+
  annotate("text", x = 7, y = 10, label = "200k+",color="white")+
  annotate("text", x = 8, y = 10, label = "Prefer NTA",color="white")
D5c #Count

D5<-ggplot(resp.data.eda, aes(x = as.factor(D5))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),colour="#2b8cbe",fill="#2b8cbe") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs( title="D5. Total Annual Household Income ($)", y = "", x = "")+
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank())+
  annotate("text", x = 1, y = 0.02, label = "<25k",color="white") +
  annotate("text", x = 2, y = 0.02, label = "25k-<35k",color="white")+
  annotate("text", x = 3, y = 0.02, label = "35k-<50k",color="white")+
  annotate("text", x = 4, y = 0.02, label = "50k-<75k",color="white")+
  annotate("text", x = 5, y = 0.02, label = "75k-<100k",color="white")+
  annotate("text", x = 6, y = 0.02, label = "100k-<200k+",color="white")+
  annotate("text", x = 7, y = 0.02, label = "200k+",color="white")+
  annotate("text", x = 8, y = 0.02, label = "Prefer NTA",color="white")
D5 #Percentage

#D6. In which state do you live? 
D6c<-ggplot(resp.data.eda) +
  geom_bar(aes(D6),colour="#2b8cbe",fill="#2b8cbe") +
  ggtitle("D6. State" ) +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank())+ 
  annotate("text", x = 5, y = 39, label = "CA",color="black") +
  annotate("text", x = 10, y = 31, label = "FL",color="black")+
  annotate("text", x = 32, y = 24, label = "NY",color="black")+
  annotate("text", x = 38, y = 29, label = "PA",color="black")+
  annotate("text", x = 43, y = 25, label = "TX",color="black")
D6c

D6<-ggplot(resp.data.eda, aes(x = as.factor(D6))) +
  geom_bar(aes(y = (..count..)/sum(..count..)),colour="#2b8cbe",fill="#2b8cbe") +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25, size=3) +
  scale_y_continuous(labels = percent) +
  labs( title="D6. State", y = "", x = "")+
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5),axis.title.x=element_blank(), 
        axis.text.y=element_blank(),axis.ticks=element_blank())+
  annotate("text", x = 5, y = 0.045, label = "CA",color="white",fontface=2, size=3.5) +
  annotate("text", x = 10, y = 0.045, label = "FL",color="white",fontface=2, size=3.5)+
  annotate("text", x = 32, y = 0.045, label = "NY",color="white",fontface=2, size=3.5)+
  annotate("text", x = 38, y = 0.045, label = "PA",color="white",fontface=2, size=3.5)+
  annotate("text", x = 43, y = 0.045, label = "TX",color="white",fontface=2, size=3.5)
D6 #Percentage

#Mutiplots
multiplot(vList3, D1, D2, D3,D5, D6,cols=2)

#D4: What are the age(s) of the children living in your household?
#Values: 0-1
#0=Unchecked
#1=Checked

D4_MyMultResp_labels<-data.frame(resp.data.eda[48:53])

names(D4_MyMultResp_labels) <- make.names(names(D4_MyMultResp_labels)) 
colnames(D4_MyMultResp_labels)[1] <- "1+, <2"       
colnames(D4_MyMultResp_labels)[2] <- "1+, 3-5"   
colnames(D4_MyMultResp_labels)[3] <- "1+, 6-12"   
colnames(D4_MyMultResp_labels)[4] <- "1+, 13-18"   
colnames(D4_MyMultResp_labels)[5] <- "1+, >18"   
colnames(D4_MyMultResp_labels)[6] <- "None in household"   

str(D4_MyMultResp_labels)

D4_MyMultResp<-data.frame(Freq = colSums(D4_MyMultResp_labels[1:6]),
                          Pct.of.all.Resp = (colSums(D4_MyMultResp_labels[1:6])/sum(D4_MyMultResp_labels[1:6]))*100,
                          Pct.Check.All.That.Apply = (colSums(D4_MyMultResp_labels[1:6])/nrow(D4_MyMultResp_labels[1:6]))*100)
D4_MyMultResp


barplot(D4_MyMultResp[[3]]
        ,names.arg=row.names(D4_MyMultResp)
        ,main = "D4: What are the age(s) of the children living in your household?"
        ,xlab = "Percent Check All That Apply"
        ,ylab = "Children (1 or more, age range)"
        ,col = "#2b8cbe"
        ,horiz = TRUE
        ,cex.names = 0.8)

###################################### Ch. 1 Data Preparation #####################################

##### Create X Matrix or Design Matrix #####

#Effects Coding
xvec=c(0,1,2,3) 
efcode.att.f(xvec) #Check 

#Provide Matrix for Choice Design
task.mat <- as.matrix(taskV3[, c("screen", "RAM", "processor", "price", "brand")])
task.mat 
dim(task.mat) #108 rows: 3 rows for each of 36 choice sets by 5 columns
head(task.mat)

#Produces Effects Coded Version of task.mat
X.mat=efcode.attmat.f(task.mat) 
X.mat
dim(X.mat) #108 by 11
head(X.mat)

#Add interaction between brand and price to X.mat
#scale(taskV3$price,scale=FALSE)
pricevec=taskV3$price-mean(taskV3$price) #Get the vector of prices from taskV3 and center it on its mean by taking the element-wise difference of two numeric vectors.
head(pricevec)
str(pricevec)

#Extract the last 3 columns from X.mat that represent brand. 
X.brands=X.mat[,9:11]
X.brands
dim(X.brands)
str(X.brands)

#Multiply each column in X.brands by pricevec.
X.BrandByPrice = X.brands*pricevec
X.BrandByPrice
dim(X.BrandByPrice)
str(X.BrandByPrice)

#Combine X.mat and X.BrandsByPrice to get the X matrix we'll use for choice modeling
X.matrix=cbind(X.mat,X.BrandByPrice)
X.matrix
dim(X.matrix) #108 by 14
str(X.matrix)
head(X.matrix)

#Check to ensure it's a postive #
det(t(X.matrix)%*%X.matrix) 

#Create data frame for Y Vector or Y Response Data
ydata=resp.data.v3[,4:39]
str(ydata)
head(ydata)
summary(ydata) #No missing data within the responses
names(ydata) #Check to see if you have all 36 response variables
ydata=na.omit(ydata) #Make sure you have no missing data

ydatadf <- ydata
head(ydatadf)

ydata=as.matrix(ydata) #Convert ydata to matrix
dim(ydata) #424 by 36

#Create indicator variable for STC product ownership (vList3)
zowner <- 1 * ( ! is.na(resp.data.v3$vList3) ) #Equal to 1 if a respondent has ever owned an STC product, other 0.

#Create the list of data lists for HB MNL models - rhierMnlDP()
#Contains data for each respondent and is a list with two elements (X.matrix and ydata)
lgtdata = NULL # a starter placeholder for your list 
for (i in 1:424) { 
    lgtdata[[i]]=list(y=ydata[i,],X=X.matrix) 
 } 

length(lgtdata) #424
str(lgtdata)

################################# Ch. 2 Modeling - Fitting HB MNL Model ################################

require(bayesm)

lgtdata=lgtdata #x and y data

#Specify 100k and kth = 5
mcmctest=list(R=100000,keep=5) #R = 100k iterations; kth = 5 to adjust for autocorrelation

#Create the "Data" list rhierMnlDP() expects
Data1=list(p=3,lgtdata=lgtdata)   # p is choice set size

#Output for testrun1
set.seed(123)
testrun1=rhierMnlDP(Data=Data1,Mcmc=mcmctest)
names(testrun1) #"betadraw" is an array that has the draws (i.e. samples from marginal posterior distributions) for the regression coefficients
dim(testrun1$betadraw) #424 rows, 14 columns in X.matrix, 20000 blocks or samples produced by the (thinned) iterations of the algorithm

#Extract betadraw out of testrun1
betadraw1=testrun1$betadraw #betadraw1
betadraw1
dim(betadraw1) #424 people, 14 betas, x simulated values

#Respondent 1
#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[1,1,]),betadraw1[1,1,]) #1st: Person 1, 2nd: Beta 1 (e.g., dummy X1 or screen 7), 3rd: Iteration # after thinning
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[1,2,]),betadraw1[1,2,]) #Screen10
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[1,3,]),betadraw1[1,3,]) #RAM16
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[1,4,]),betadraw1[1,4,]) #RAM32
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[1,5,]),betadraw1[1,5,]) #Proc2
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[1,6,]),betadraw1[1,6,]) #Proc2.5
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[1,7,]),betadraw1[1,7,]) #Price299
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[1,8,]),betadraw1[1,8,]) #Price399
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[1,9,]),betadraw1[1,9,]) #Somesong
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[1,10,]),betadraw1[1,10,]) #Pear
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[1,11,]),betadraw1[1,11,]) #Gaggle
abline(v=10000, col="red", lwd=3, lty=2)

#Respondent 2
#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[2,1,]),betadraw1[2,1,]) #1st: Person 1, 2nd: Beta 1 (e.g., dummy X1 or screen 7), 3rd: Iteration # after thinning
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[2,2,]),betadraw1[2,2,]) #Screen10
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[2,3,]),betadraw1[2,3,]) #RAM16
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[2,4,]),betadraw1[2,4,]) #RAM32
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[2,5,]),betadraw1[2,5,]) #Proc2
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[2,6,]),betadraw1[2,6,]) #Proc2.5
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[2,7,]),betadraw1[2,7,]) #Price299
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[2,8,]),betadraw1[2,8,]) #Price399
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[2,9,]),betadraw1[2,9,]) #Somesong
abline(v=10000, col="red", lwd=3, lty=2)

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[2,10,]),betadraw1[2,11,]) #Pear

#Plot betadraw1 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw1[2,11,]),betadraw1[2,11,]) #Gaggle
abline(v=10000, col="red", lwd=3, lty=2)

#Respondent 1 Continued
#Density smooth histogram of converged points, post burning period
plot(density(betadraw1[1,1,10001:20000],width=2))
abline(v=0) ## vertical line
abline(v=mn) ## vertical line

#Compute the probability that person 1 beta 1 > 0
# Assumes Normal distribution of beta 1
mn <- mean(betadraw1[1,1,10001:20000])
sd <- sd(betadraw1[1,1,10001:20000])
mn
sd

prob <- pnorm(0,mean=mn, sd=sd, lower.tail = FALSE)
prob

#Empirical probability based on sample values
p1b1 <- betadraw1[1,1,10001:20000]
quantile(p1b1, probs = seq(0, 1, by= 0.1))

####Summary####
#Mean for Person 1 and Beta 1 (7 inch)
summary(betadraw1[1,1,10001:20000]) #Positive mean denotes like
exp(0.7929) #log(odds ratio)=7929; odds ratio = exp(0.7929)=2.209796

#Mean for Person 1 and Beta 2 (10 inch)
summary(betadraw1[1,2,10001:20000]) #Negative mean denotes dislike
exp(-0.38501) #log(odds ratio)=-0.38501; odds ratio = exp(-0.38501)=0.6804438

#Means of the 14 coefficients and the means across all respondents 
betameansoverall<-apply(betadraw1[,,17500:20000],c(2),mean) 
betameansoverall

#Percentiles of the 14 coefficients or betas
perc <- apply(betadraw1[,,10001:20000],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))
perc #50% = median; 90% confidence obtained by 5% and 95%

#Extras
#Matrix of coefficient means by each respondent 
apply(betadraw1[,,10001:20000],c(1,2),mean) #424 respondents, 14 mean coefficients. 

#Compare Beta 1 vs. Beta 2 for Person 1
summary((betadraw1[1,1,10001:20000]-betadraw1[1,2,10001:20000]))
plot(density(betadraw1[1,1,10001:20000]-betadraw1[1,2,10001:20000],width=2))

########## Ch. 3 Modeling - Fitting a HB MNL model with Prior STC Ownership as a Covariate ############

#Center z covariate, "demean" zoner and make result a 1 column matrix
zownertest=matrix(scale(zowner,scale=FALSE),ncol=1) 

#Create Data2 list from Data1 to Include zowner as a covariate
Data2=list(p=3,lgtdata=lgtdata,Z=zownertest) 

#Output for testrun2
set.seed(123)
testrun2=rhierMnlDP(Data=Data2,Mcmc=mcmctest)
names(testrun2) #Check to see that Deltadraw is included
dim(testrun2$Deltadraw) #Deltadraw is a matrix with # rows = saved iterations, #number of columns = number of regression predictors in the X.matrix (14):

#Extract betadraw out of testrun2
betadraw2=testrun2$betadraw #betadraw1
betadraw2
dim(betadraw2) #424 people, 14 betas, x simulated values

#Plot betadraw2 & Decide Burning Period (Convergance/Stablization)
plot(1:length(betadraw2[1,1,]),betadraw2[1,1,]) #1st: Person 1, 2nd: Beta 1 (e.g., dummy X1 or screen 7), 3rd: Iteration # after thinning
abline(v=10000, col="red", lwd=3, lty=2)

#Density smooth histogram of converged points, post burning period
plot(density(betadraw2[1,1,10001:20000],width=2))
abline(v=0) ## vertical line
abline(v=mn) ## vertical line

# Assumes Normal distribution of beta 1
mn <- mean(betadraw2[1,1,10001:20000])
sd <- sd(betadraw2[1,1,10001:20000])
mn
sd

prob <- pnorm(0,mean=mn, sd=sd, lower.tail = FALSE)
prob

#Empirical probability based on sample values
p1b1 <- betadraw2[1,1,10001:20000]
quantile(p1b1, probs = seq(0, 1, by= 0.1))

### Summary ###
#Means of the 14 coefficients and the means across all respondents for Deltadraw
#Samples from the posterior distributions of the regression coefficients of the 14 betas on  (mean-centered) zownertest.
apply(testrun2$Deltadraw[10001:20000,],2,mean) #Postive = Like; #Negative = Dislike

#Percentiles of the 14 coefficients or betas
#Summary of the posterior distributions of correlations b/w choice models regression coefficients and zownertest variable
apply(testrun2$Deltadraw[10001:20000,],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))

#Means of the 14 coefficients and the means across all respondents for Betadraw
betameansoverall2<-apply(betadraw2[,,10001:20000],c(2),mean) 
betameansoverall2

#Percentiles of the 14 coefficients or betas
perc2 <- apply(betadraw2[,,10001:20000],2,quantile,probs=c(0.05,0.10,0.25,0.5 ,0.75,0.90,0.95))
perc2 #50% = median; 90% confidence obtained by 5% and 95%

betadraw2=testrun2$betadraw
dim(betadraw2)

########## Ch. 4 Make customer choice prediction using the individual respondent's model  ############
######################## & goodness of fit & validation ########################################

########Beta Draw 1########

#Means of the 14 coefficients and the means across all respondents from Betadraw 1
betameans <- apply(betadraw1[,,10001:20000],c(1,2),mean)
str(betameans)
dim(betameans) #424 by 14
dim(t(betameans))
dim(X.matrix)

#Get the product of our X.matrix and each subjects vector of mean betas
xbeta=X.matrix%*%t(betameans)
dim(xbeta) #108 by 424

#Reorganize xbeta data
xbeta2=matrix(xbeta,ncol=3,byrow=TRUE) #subjects in rows with choice set alternatives across columns
dim(xbeta2) #15264 by 3

#Exponentiate xbeta2
expxbeta2=exp(xbeta2)
dim(expxbeta2) #15264 by 3

#Obtain Predicted Choice Probabilities 
rsumvec=rowSums(expxbeta2) #divide each row by its sum
pchoicemat=expxbeta2/rsumvec
pchoicemat
head(pchoicemat)
dim(pchoicemat)

#Provide prediction of the customer choice
custchoice <- max.col(pchoicemat)
custchoice
head(custchoice)
str(custchoice)

#Assess Model Fit

#Confusion Matrix
ydatavec <- as.vector(t(ydata))
str(ydatavec)
head(ydatavec)
cm<-table(custchoice,ydatavec)
cm
accuracy<-sum(diag(cm))/sum(cm)
accuracy #0.8839099

#ROC Curve and AUC
require("pROC")
roctest <- roc(ydatavec, custchoice, plot=TRUE)    
auc(roctest)                       
roctestMC <- multiclass.roc(ydatavec, custchoice, plot=TRUE)
auc(roctestMC) #0.9078

#-2log(likelihood) test applies to nested models only
logliketest <- testrun2$loglike 
mean(logliketest) #Lower the better
hist(logliketest) #-5789.52

#Prediction for 36 choice sets Using Individual Respondent's model
m <- matrix(custchoice, nrow =36,  ncol = 424)
m2 <- t(m)
apply(m2, 2, function(x){tabulate(na.omit(x))})

#Predictions based on overall beta means, for 36 choice sets
#We can predict the original 36 choice sets using the pooled model. 
#The code below, provides the probabilities as well as the frequencies for the 36 choice sets #####

#Means of the 14 coefficients and the means across all respondents 
betavec=matrix(betameansoverall,ncol=1,byrow=TRUE)

#Get the product of our X.matrix and each subjects vector of mean betas
xbeta=X.matrix%*%(betavec)
dim(xbeta)

#Reorganize xbeta2 data
xbeta2=matrix(xbeta,ncol=3,byrow=TRUE)
dim(xbeta2)

#Exponentiate expxbeta2
expxbeta2=exp(xbeta2)

#Obtain Predicted Choice Probabilities 
rsumvec=rowSums(expxbeta2)

#Provide prediction of the customer choice probabilites
pchoicemat=expxbeta2/rsumvec
pchoicemat

#Provide prediction of the customer choice frequencies 
pchoicemat2 <- round(pchoicemat*424,digits=0)
pchoicemat2 

########Beta Draw 2########

#Means of the 14 coefficients and the means across all respondents from Betadraw 1
betameans <- apply(betadraw2[,,10001:20000],c(1,2),mean)
str(betameans)
dim(betameans) #424 by 14

#Get the product of our X.matrix and each subjects vector of mean betas
xbeta=X.matrix%*%t(betameans)
dim(xbeta) #108 by 424

#Reorganize xbeta data
xbeta2=matrix(xbeta,ncol=3,byrow=TRUE) #subjects in rows with choice set alternatives across columns
dim(xbeta2) #15264 by 3

#Exponentiate xbeta2
expxbeta2=exp(xbeta2)
dim(expxbeta2) #15264 by 3

#Obtain Predicted Choice Probabilities 
rsumvec=rowSums(expxbeta2) #divide each row by its sum
pchoicemat=expxbeta2/rsumvec
pchoicemat
head(pchoicemat)
dim(pchoicemat)

#Provide prediction of the customer choice
custchoice <- max.col(pchoicemat)
custchoice
head(custchoice)
str(custchoice)

#Assess Model Fit

#Confusion Matrix
ydatavec <- as.vector(t(ydata))
str(ydatavec)
cm<-table(custchoice,ydatavec)
cm
accuracy<-sum(diag(cm))/sum(cm)
accuracy #0.8844995

#ROC Curve and AUC
require("pROC")
roctest <- roc(ydatavec, custchoice, plot=TRUE)    
auc(roctest)                       
roctestMC <- multiclass.roc(ydatavec, custchoice, plot=TRUE)
auc(roctestMC) #0.9081

#-2log(likelihood) test applies to nested models only
logliketest <- testrun2$loglike 
mean(logliketest) #Lower the better
hist(logliketest)

#Prediction for 36 choice sets Using Individual Respondent's model
m <- matrix(custchoice, nrow =36,  ncol = 424)
m2 <- t(m)
apply(m2, 2, function(x){tabulate(na.omit(x))})

#Predictions based on overall beta means, for 36 choice sets
#We can predict the original 36 choice sets using the pooled model. 
#The code below, provides the probabilities as well as the frequencies for the 36 choice sets #####

#Means of the 14 coefficients and the means across all respondents 
betavec=matrix(betameansoverall,ncol=1,byrow=TRUE)

#Get the product of our X.matrix and each subjects vector of mean betas
xbeta=X.matrix%*%(betavec)
dim(xbeta)

#Reorganize xbeta2 data
xbeta2=matrix(xbeta,ncol=3,byrow=TRUE)
dim(xbeta2)

#Exponentiate expxbeta2
expxbeta2=exp(xbeta2)

#Obtain Predicted Choice Probabilities 
rsumvec=rowSums(expxbeta2)

#Provide prediction of the customer choice probabilites
pchoicemat=expxbeta2/rsumvec
pchoicemat

#Provide prediction of the customer choice frequencies 
pchoicemat2 <- round(pchoicemat*424,digits=0)
pchoicemat2 

########## Ch. 5 Predicting extra scenarios, as well as the 36 choice sets  ############
################### using betas from all the pooled respondents ########################

###### Predicting Extra Scenarios #####
ex_scen <- read.csv("extra-scenarios.csv")
Xextra.matrix <- as.matrix(ex_scen[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9",
                                      "V10","V11","V12","V13","V14")])
dim(Xextra.matrix)

###### Predict extra scenarios using the overall model###### 
#Means of the 14 coefficients and the means across all respondents 
betavec=matrix(betameansoverall,ncol=1,byrow=TRUE)
dim(betavec)
betavec

#Get the product of our X.matrix and each subjects vector of mean betas
xextrabeta=Xextra.matrix%*%(betavec)
dim(xextrabeta)

#Reorganize xbetaextra2 data
xbetaextra2=matrix(xextrabeta,ncol=3,byrow=TRUE)
dim(xbetaextra2)

#Exponentiate expxbetaextra2
expxbetaextra2=exp(xbetaextra2)

#Obtain Predicted Choice Probabilities 
rsumvec=rowSums(expxbetaextra2)

#Provide prediction of the customer choice
pchoicemat=expxbetaextra2/rsumvec
pchoicemat
dim(pchoicemat)

###### Predict extra scenarios based on individual models ###### 
xextrabetaind=Xextra.matrix%*%(t(betameans))
xbetaextra2ind=matrix(xextrabetaind,ncol=3,byrow=TRUE)

#xextrabetaind=Xextra.matrix%*%(t(betameansindividual))
#dim(xextrabetaind)
#xbetaextra2ind=rbind(matrix(xextrabetaind[1:3,],ncol=3,byrow=TRUE),
#                     matrix(xextrabetaind[4:6,],ncol=3,byrow=TRUE))
dim(xbetaextra2ind)

##Exponentiate expxbetaextra2ind
expxbetaextra2ind=exp(xbetaextra2ind)

#Obtain Predicted Choice Probabilities 
rsumvecind=rowSums(expxbetaextra2ind)

#Provide prediction of the customer choice
pchoicematind=expxbetaextra2ind/rsumvecind
dim(pchoicematind)
head(pchoicematind)

#Highest probability identifier
custchoiceind <- max.col(pchoicematind)
head(custchoiceind)
str(custchoiceind)

#Use individual respondents models to predict the extra scenario & then using 'voting' kind of techniques
#make the final prediction
#Each individual model is separately predicting each additional scenario. 
extra1 <- custchoiceind[1:424]
extra2 <- custchoiceind[425:848]
table(extra1)
table(extra2)

##### Accuracy based on confusion matrix for each of the 424 respondents using individual models #####
############ for all the 36 choice sets - actual response vs predicted response ###############

#Compute accuracy based on confusion matrix for each of the 424 respondents using individual models
resp_accuracy <- NULL
for (i in 1:424) {
  start <- i*36-35
  end <- i*36
  d <- table(factor(custchoice[start:end],levels = 1:3),
             factor(ydatavec[start:end], levels = 1:3))
  resp_accuracy[i] <- sum(diag(d))/sum(d)
} 

#Plot Accuracy
plot(resp_accuracy, main = "Model Accuracy by Respondent")

#Create dataframe of accuracy for each of 424 respondents using individual models
respdf <- data.frame(resp_accuracy)
head(respdf)
str(respdf)

#Add respondent number to dataframe
head(ydatadf)
rn <- rownames(ydatadf)
rndf <- as.data.frame(rn)
resp_all <- cbind(rndf,respdf)
head(resp_all)
str(resp_all)

#Plot histogram of accuracy with frequency
hist(resp_all$resp_accuracy)

#Identify accuracy lower than 0.6
outlier <- subset(resp_all, resp_accuracy < 0.6)
outlier[order(outlier$resp_accuracy),]



