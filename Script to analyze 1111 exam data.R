# getting data (data in csv format)

rm(list=ls())
#install.packages('vars')
require(stats);require(vars)
#setwd("C:\\Users\\Christopher\\Google Drive\\TU 2018\\SOAR\\Exams")

# read data into r as a dataframe
df<-read.csv("Spr19Exam 1_cleaned.csv")
head(df)

##create indices identifying students in my soars
sec73<-which(df[,"soar"]== 73)
sec74<-which(df[,"soar"]== 74)
sec75<-which(df[,"soar"]== 75)
# sec80<-which(df[,"soar"]== 80)
# sec81<-which(df[,"soar"]== 81)
# sec82<-which(df[,"soar"]== 82)
allsoars<-c(sec73,sec74,sec75)
# allsoars<-c(sec80,sec81,sec82)

#create index to identify high achievers (ncorrect>75th percentile)
highAch<-which(df$ncorrect>(median(df$ncorrect,na.rm = T)+IQR(df$ncorrect,na.rm = T)/2))

#high achievers
all.highach<-df[highAch,"tuid"]
mysec.highach<-df[intersect(highAch,allsoars),"tuid"]
sec73.highach<-df[intersect(highAch,sec73),"tuid"]
sec74.highach<-df[intersect(highAch,sec74),"tuid"]
sec75.highach<-df[intersect(highAch,sec75),"tuid"]
# sec80.highach<-df[intersect(highAch,sec80),"tuid"]
# sec81.highach<-df[intersect(highAch,sec81),"tuid"]
# sec82.highach<-df[intersect(highAch,sec82),"tuid"]

# convert soar vector to factor class
df$soar<-as.factor(df$soar)

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
classdata<-as.data.frame(cbind("full.class"=mysumstat(df[,"ncorrect"]), 
                               "mysoars"=mysumstat(df[allsoars,"ncorrect"]),
                               "sec.73"=mysumstat(df[sec73,"ncorrect"]),
                               "sec.74"=mysumstat(df[sec74,"ncorrect"]),
                               "sec.75"=mysumstat(df[sec75,"ncorrect"])))

#summary stats for high achievers
highAchData<-as.data.frame(cbind("full.class"=mysumstat(df[highAch,"ncorrect"]),
                                 "mysoars"= mysumstat(df[intersect(highAch,allsoars),"ncorrect"]),
                                 "sec.73" = mysumstat(df[intersect(highAch,sec73),"ncorrect"]),
                                 "sec.74" = mysumstat(df[intersect(highAch,sec74),"ncorrect"]),
                                 "sec.75" = mysumstat(df[intersect(highAch,sec75),"ncorrect"])))
# classdata<-as.data.frame(cbind("full.class"=mysumstat(df[,"ncorrect"]), 
#                                "mysoars"=mysumstat(df[allsoars,"ncorrect"]),
#                                "sec.80"=mysumstat(df[sec80,"ncorrect"]),
#                                "sec.81"=mysumstat(df[sec81,"ncorrect"]),
#                                "sec.82"=mysumstat(df[sec82,"ncorrect"])))
# 
# #summary stats for high achievers
# highAchData<-as.data.frame(cbind("full.class"=mysumstat(df[highAch,"ncorrect"]),
#                                  "mysoars"= mysumstat(df[intersect(highAch,allsoars),"ncorrect"]),
#                                  "sec.80" = mysumstat(df[intersect(highAch,sec80),"ncorrect"]),
#                                  "sec.81" = mysumstat(df[intersect(highAch,sec81),"ncorrect"]),
#                                  "sec.82" = mysumstat(df[intersect(highAch,sec82),"ncorrect"])))


#creates subset of data with limited info for high Achievers then orders those data by soar soar
highAchievers<-as.data.frame(df[highAch,c("ncorrect","soar","tuid")]) 
highAchievers<-highAchievers[order(highAchievers[,"soar"]),]

#boxplots of ncorrects

"full"<-boxplot(ncorrect~soar, df, xlab= "SOAR Section", ylab= "Number Correct");abline(h=median(df$ncorrect))
"Mine vs. Not" <- boxplot(ncorrect~soarType, df[which(df$soarType!='key'),],xlab ="Mine vs. Not Mine", ylab = "Number Correct"); abline(h=median(df$ncorrect))

##inferential stats
#high achiever distribution

#are my ncorrects different from class ncorrects
ncorrects<-kruskal.test(formula=ncorrect~soarType,data=df)
print(ncorrects)

#do any of the soar soars do better than others
sec.ncorrects<-kruskal.test(formula=ncorrect~soar,data=df)
print(sec.ncorrects)

#do I have a disproportionate amount of high achievers
highdata<-df[which(!is.na(df[highAch,])),]
# chisq.test(highdata[highdata$soarType!='key',"soarType"])

## finding the trouble items (top 10 most frequently incorrect items)
itemNames=grep("item_",names(df),value = T)
#soar80
# nWrong<-c()
# for(name in itemNames){
#     nWrong<-c(nWrong,length(which(df[sec80,name]!=df[1,name])))
# }
# Top10sec.80<-data.frame("itemNames"=itemNames,"nWrong"=nWrong)
# Top10sec.80<-data.frame("itemNames"=itemNames,"nWrong"=nWrong)[order(Top10sec.80$nWrong,decreasing = T),]
# Top10sec.80
# 
# #soar81
# nWrong<-c()
# for(name in itemNames){
#     nWrong<-c(nWrong,length(which(df[sec81,name]!=df[1,name])))
# }
# Top10sec.81<-data.frame("itemNames"=itemNames,"nWrong"=nWrong)
# Top10sec.81<-data.frame("itemNames"=itemNames,"nWrong"=nWrong)[order(Top10sec.81$nWrong,decreasing = T),]
# Top10sec.81
# 
# #soar82
# nWrong<-c()
# for(name in itemNames){
#     nWrong<-c(nWrong,length(which(df[sec82,name]!=df[1,name])))
# }
# Top10sec.82<-data.frame("itemNames"=itemNames,"nWrong"=nWrong)
# Top10sec.82<-data.frame("itemNames"=itemNames,"nWrong"=nWrong)[order(Top10sec.82$nWrong,decreasing = T),]
# Top10sec.82
# 
# allTopTens<-list("sec80"=Top10sec.80,"sec81"=Top10sec.81,"sec82"=Top10sec.82)
# allTopTens
# 
# write.csv(allTopTens,"Exam3_topTens.csv")
