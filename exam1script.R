# getting data (data in csv format)
# make sure "readstata13" package is libraried
# install.packages("readstata13")
# library(readstata13)
# set working directory to the folder containing the file
rm(list = ls())
#Create useful functions repo
#add vartoNA() to useful functions
#add mysumstat() to useful functions
#source useful functions
# source("~/GitHub/useful-functions/unsortedFunctions.Rmd")
# source("~/GitHub/useful-functions/statisticalFunctions.Rmd")

#install packages and library

myinstall<-function(mypackages=NULL){
        neededpackages<-as.character(mypackages[which(!is.element(el=mypackages,set=installed.packages()))])
        if(length(neededpackages>0)){
                install.packages(neededpackages)
        }else{
                warning("All packages already installed")
        }
        lapply(mypackages,function(x) library(x,character.only = T))
}

myinstall("stats")
setwd("./SOAR 1111/")

#make sure data file is included in folder
dir()
# read data into r as a dataframe
exam2data<-read.csv("E2FALL17.csv")

names(exam2data)[which(names(exam2data)=="section")]<-"soar"
names(exam2data)
#paste the contents of n1:n3 as numeric 
# exam2data$n1<-as.numeric(paste(exam2data$n1,exam2data$n2,exam2data$n3,sep=""))

# Drop extra column at end, pi info, and extra columns post pasting
# exam2data<-exam2data[,-c(2:4, 9:10, length(exam2data))] #check these each time

# change the name of the variable, "n1", to "labsection"
names(exam2data)[which(names(exam2data)=="Score")]<-"n.correct"

#create indices identifying students in my sections
# unique(exam2data$soar)
#add assignSec()- should automatically recognize sections and assign to an appropriately named vector
#apply assignSec() here
sec81<-which(exam2data[,"soar"]== 81)
# sec80<-which(exam2data[,"soar"]== 80)
# sec78<-which(exam2data[,"soar"]== 78)
sec76<-which(exam2data[,"soar"]== 76)
sec75<-which(exam2data[,"soar"]== 75)
# sec74<-which(exam2data[,"soar"]== 74)
# sec73<-which(exam2data[,"soar"]== 73)
sec72<-which(exam2data[,"soar"]== 72)
# sec71<-which(exam2data[,"soar"]== 71)
# sec9<-which(exam2data[,"soar"]== 9)
# sec7<-which(exam2data[,"soar"]== 7)
# sec4<-which(exam2data[,"soar"]== 4)
# sec3<-which(exam2data[,"soar"]== 3)
# sec2<-which(exam2data[,"soar"]== 2)
sectest<-which(is.na(exam2data[,"soar"]))

#create index to identify high achievers (score>75th percentile)
highAch<-which(exam2data$n.correct>(median(exam2data$n.correct,na.rm = T)+IQR(exam2data$n.correct,na.rm = T)/2))

#high achievers
names(exam2data)<-tolower(names(exam2data))
all.highach<-exam2data[highAch,"tuid"]
mysec.highach<-exam2data[intersect(highAch,sec81),"tuid"]
# sec73.highach<-exam2data[intersect(highAch,sec73),"id"]
# sec74.highach<-exam2data[intersect(highAch,sec74),"id"]
# sec75.highach<-exam2data[intersect(highAch,sec75),"id"]
# sec82.highach<-exam2data[intersect(highAch,sec82),"id"]

#add vector that groups students as mine or not
mine<-rep("not mine",nrow(exam2data));mine[sec81]<-"mine"
exam2data<-cbind(exam2data,mine)

# convert soar vector to factor class
exam2data$soar<-as.factor(exam2data$soar)

# get summary stats for class
mysumstat<-function(x){
        mysumresults<-list("mean"=mean(x,na.rm=T),
                           "sd"=sd(x,na.rm=T),
                           "median"=median(x,na.rm=T),
                           "IQR"=IQR(x,na.rm=T),
                           "n"= if(is.null(dim(x))){
                                   length(x)
                           }
                           else{
                                   nrow(x)
                                   
                           }
        )
        return(mysumresults)
}

#summary stats for major groupings
classdata<-as.data.frame(cbind("full.class"=mysumstat(exam2data[,"n.correct"]), 
                               "mysections"=mysumstat(exam2data[sec81,"n.correct"])
                               # "sec.73"=mysumstat(exam2data[sec73,"n.correct"]),
                               # "sec.74"=mysumstat(exam2data[sec74,"n.correct"]),
                               # "sec.75"=mysumstat(exam2data[sec75,"n.correct"]),
                               # "sec.82"=mysumstat(exam2data[sec82,"n.correct"])
                               ))

#summary stats for high achievers
highAchData<-as.data.frame(cbind("full.class"=mysumstat(exam2data[highAch,"n.correct"]),
                                 "mysections"= mysumstat(exam2data[intersect(highAch,sec81),"n.correct"])
                                 # "sec.73" = mysumstat(exam2data[intersect(highAch,sec73),"n.correct"]),
                                 # "sec.74" = mysumstat(exam2data[intersect(highAch,sec74),"n.correct"]),
                                 # "sec.75" = mysumstat(exam2data[intersect(highAch,sec75),"n.correct"]),
                                 # "sec.82" = mysumstat(exam2data[intersect(highAch,sec82),"n.correct"])
                                 ))


#creates subset of data with limited info for high Achievers then orders those data by soar section
highAchievers<-as.data.frame(exam2data[highAch,c("n.correct","soar","tuid")]) 
highAchievers<-highAchievers[order(highAchievers[,"soar"]),]

#boxplots of scores

"full"<-boxplot(n.correct~soar, exam2data, xlab= "SOAR Section", ylab= "Number Correct");abline(h=17)
"Mine vs. Not" <- boxplot(n.correct~mine, exam2data,xlab ="Mine vs. Not Mine", ylab = "Number Correct"); abline(h=17)

##inferential stats
#high achiever distribution

#are my scores different from class scores
scores<-kruskal.test(formula=n.correct~mine,data=exam2data)
print(scores)

#do any of the soar sections do better than others
sec.scores<-kruskal.test(formula=n.correct~soar,data=exam2data)
print(sec.scores)

#do I have a disproportionate amount of high achievers
highdata<-exam2data[which(!is.na(exam2data[highAch,])),]
chisq.test(highdata[,"mine"]) #fix this

## finding the trouble items (top 10 most frequently incorrect items)

#section81
Top10sec.81<-c()
for(i in which(names(exam2data)=="item1"):which(names(exam2data)=="item27")){
        Top10sec.81<-c(Top10sec.81,length(which(exam2data[sec81,i]!=exam2data[1,i])))
}
names(Top10sec.81)<-names(exam2data[,which(names(exam2data)=="item1"):which(names(exam2data)=="item27")])
Top10sec.81<-sort(Top10sec.81,decreasing = T)[1:10]

print(Top10sec.81)

# #section74
# Top10sec.74<-c()
# for(i in 6:32){
#         Top10sec.74<-c(Top10sec.74,length(which(exam2data[sec74,i]!=exam2data[1,i])))
# }
# names(Top10sec.74)<-names(exam2data[,6:32])
# Top10sec.74<-sort(Top10sec.74,decreasing = T)[1:10]
# 
# #section75
# Top10sec.75<-c()
# for(i in 6:32){
#         Top10sec.75<-c(Top10sec.75,length(which(exam2data[sec75,i]!=exam2data[1,i])))
# }
# names(Top10sec.75)<-names(exam2data[,6:32])
# Top10sec.75<-sort(Top10sec.75,decreasing = T)[1:10]
# 
# #section82
# Top10sec.82<-c()
# for(i in 6:32){
#         Top10sec.82<-c(Top10sec.82,length(which(exam2data[sec82,i]!=exam2data[1,i])))
# }
# names(Top10sec.82)<-names(exam2data[,6:32])
# Top10sec.82<-sort(Top10sec.82,decreasing = T)[1:10]
# 
# allTopTens<-list("sec73"=Top10sec.73,"sec74"=Top10sec.74,"sec75"=Top10sec.75,"sec82"=Top10sec.82)