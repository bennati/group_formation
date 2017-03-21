# Place this script in the subdirectory containing the simulation's results.
# The script expects a subdir called "results" that contains the csv files
source("../../analyze_tools.R")

read.gz.files<-function(dir){
  files=list.files(dir)
  files=sapply(files,function(x) strsplit(x,".",fixed = TRUE)) #split name and extension
  files=Filter(function(x) x[length(x)]=="gz",files) #remove all non-csv files
  files=Filter(function(x) grepl("stats",x[[1]][1]),files) #remove all non-csv files
  files=lapply(files,FUN=function(x) paste(unlist(x),collapse = "."))
  files=sapply(unlist(files),function(x)paste(dir,x,sep="/"))
  #read files in parallels
  if(length(files)==0){
    cat("No files found, aborting.")
    quit("no",status=1)
  }
  my_data_tot<-mclapply(files,FUN=function(x){read.table(gzfile(x),sep=",",header=T)},mc.cores=1) # length(files))
  my_data_tot
}

read.csv.files<-function(dir){
  files=list.files(dir)
  files=sapply(files,function(x) strsplit(x,".",fixed = TRUE)) #split name and extension
  files=Filter(function(x) x[length(x)]=="csv",files) #remove all non-csv files
  files=Filter(function(x) grepl("stats",x[[1]][1]),files) #remove all non-csv files
  files=lapply(files,FUN=function(x) paste(unlist(x),collapse = "."))
  files=sapply(unlist(files),function(x)paste(dir,x,sep="/"))
  #read files in parallels
  if(length(files)==0){
    cat("No files found, aborting.")
    quit("no",status=1)
  }
  my_data_tot<-mclapply(files,FUN=fread,mc.cores=1) # length(files))
  my_data_tot
}

library(data.table)
library(parallel)
library(plyr) #count
root = "."
dir=paste(root,"/results",sep = "")
my_data_tot=data.table()
#find csv files
my_data_tot=read.gz.files(dir)
counts=lapply(my_data_tot,FUN=function(tbl){count(tbl,c('seed','timeStep'))})
counts=lapply(counts,FUN=function(tbl){tbl[tbl$seed!="-1",]})
my_data_tot=rbindlist(my_data_tot)
my_data_tot=my_data_tot[,c(1,7:ncol(my_data_tot)),with=FALSE] #drop some columns
my_data_tot=my_data_tot[my_data_tot$seed!="-1",] # remove lines about food sources
## print avg behavior of agents
cols=c("foodH","agentN","agentW","agentE","agentS")
actions=c("north","west","east","south","eat","")
n=length(cols)
f<-function(x,...){summarySE(measurevar=x,...)}
stats=lapply(cols,FUN=f,data=my_data_tot,groupvars=c("seed","timeStep"),na.rm=TRUE)
res=data.table(timeStep=c(0),seed=c(0))
for(i in stats){
  res=merge(res,i,by=c("timeStep","seed"),all=TRUE)
}
write.csv(res,file.path(root,"statistics.csv"),row.names = FALSE)
for(seed_lvl in levels(as.factor(my_data_tot$seed))) {
  tmp=res[res$seed==seed_lvl]
  colors = rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
  x=tmp$timeStep
  old.par=par
  png(paste(root,"/results_",seed_lvl,".png",sep = ""),width=20,height = 20,units="cm",res = 300)
  plot(x,xlim=c(min(x),max(x)),
       ##main=paste("Avg population performance, seed",seed_lvl),
       type="n",ylab="Action",xlab="Iterations",mgp=c(2.5,1,0),ylim=c(-0.5,4.5),yaxt="n",cex.lab=2,cex.axis=1.5)
  axis(2, at=0:6,labels=FALSE)
  abline(h=0,col = "gray60")
  abline(h=1,col = "gray60",lty=3)
  abline(h=2,col = "gray60",lty=3)
  abline(h=3,col = "gray60",lty=3)
  abline(h=4,col = "gray60",lty=3)
  abline(h=5,col = "gray60",lty=3)
  text(y=0:6+0.2, x=par()$usr[1]-0.05*(par()$usr[2]-par()$usr[1])
       ,labels=actions, srt=90, adj=1, xpd=TRUE)
  legend("bottomright",cols,fill=colors,horiz=TRUE)
  for(i in 1:n) {
    lines(x,eval(parse(text=paste("tmp$mean.",cols[i],sep=""))),col=colors[i],lwd=4)
    lines(x,eval(parse(text=paste("tmp$mean.",cols[i],"-tmp$ci.",cols[i],sep=""))),col=colors[i],lwd=1,lty=3)
    lines(x,eval(parse(text=paste("tmp$mean.",cols[i],"+tmp$ci.",cols[i],sep=""))),col=colors[i],lwd=1,lty=3)
  }
  dev.off()
  par=old.par
}
my_data_aggr=rbindlist(counts)
my_data_aggr=summarySE("freq",data=my_data_aggr,groupvars=c("seed","timeStep"),na.rm=TRUE)
#setnames(my_data_aggr,c("seed","timeStep","mean","sd"))
my_data_aggr[is.na(my_data_aggr)]<-0
write.csv(my_data_aggr,"population_evolution.csv")
my_data_aggr=my_data_aggr[my_data_aggr$timeStep%%1000==0,]
#plot evolution of groups in population
n=length(levels(as.factor(my_data_aggr$seed)))
colors = c('Blue','Red') #rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
#plot pdf
pdf(paste(root,"population_evolution.pdf",sep = "/"))
x=as.numeric(levels(as.factor(my_data_aggr$timeStep)))
y=as.numeric(levels(as.factor(my_data_aggr$mean.freq)))
yerr=as.numeric(levels(as.factor(my_data_aggr$sd.freq)))
plot(x,xlim=c(min(x),max(x)),ylim=c(min(y)-max(yerr)-200,max(y)+max(yerr)),main="Groups sizes",type="n",ylab="Size",xlab="Time",cex.lab=2,cex.axis=1.5)
legend("bottomleft",c("Random","Sociable"),col=colors,lty=c(1,1),lwd=c(4,4),horiz=TRUE)
for(seed_lvl in levels(as.factor(my_data_aggr$seed))) {
  x=my_data_aggr[my_data_aggr$seed==seed_lvl,]$timeStep
  y=my_data_aggr[my_data_aggr$seed==seed_lvl,]$mean.freq
  yerr=my_data_aggr[my_data_aggr$seed==seed_lvl,]$sd.freq
  lines(x,y,col=colors[as.numeric(seed_lvl)+1],lwd=4)
  lines(x,y+yerr,col=colors[as.numeric(seed_lvl)+1],lwd=2,lty=3)
  lines(x,y-yerr,col=colors[as.numeric(seed_lvl)+1],lwd=2,lty=3)
  }
dev.off()
#plot png
png(paste(root,"population_evolution.png",sep = "/"),width=20,height = 20,units="cm",res = 300)
x=as.numeric(levels(as.factor(my_data_aggr$timeStep)))
y=as.numeric(levels(as.factor(my_data_aggr$mean.freq)))
yerr=as.numeric(levels(as.factor(my_data_aggr$sd.freq)))
plot(x,xlim=c(min(x),max(x)),ylim=c(min(y)-max(yerr)-200,max(y)+max(yerr)),main="Groups sizes",type="n",ylab="Size",xlab="Time",cex.lab=2,cex.axis=1.5)
legend("bottomleft",c("Random","Sociable"),col=colors,lty=c(1,1),lwd=c(4,4),horiz=TRUE)
for(seed_lvl in levels(as.factor(my_data_aggr$seed))) {
  x=my_data_aggr[my_data_aggr$seed==seed_lvl,]$timeStep
  y=my_data_aggr[my_data_aggr$seed==seed_lvl,]$mean.freq
  yerr=my_data_aggr[my_data_aggr$seed==seed_lvl,]$sd.freq
  lines(x,y,col=colors[as.numeric(seed_lvl)+1],lwd=4)
  lines(x,y+yerr,col=colors[as.numeric(seed_lvl)+1],lwd=2,lty=3)
  lines(x,y-yerr,col=colors[as.numeric(seed_lvl)+1],lwd=2,lty=3)
}
dev.off()
rm(x,y,colors,n)
rm(my_data_tot,seed_lvl,root,my_data_aggr)
