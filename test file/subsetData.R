getwd()
setwd('~/Udacity/Project 4 Explore and Summarize Data/test file/')
statesInfo=read.csv('StateData.csv')
statesSubset<-subset(statesInfo,state.region==1)
statesSubsetBrakets<-statesInfo[statesInfo$state.region==1,]
