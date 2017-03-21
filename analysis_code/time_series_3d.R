source("../../analyze_tools.R")
library(parallel)
library(data.table)
root = "."
dir=paste(root,"/results",sep = "")
my_data_tot=data.table()
filename="./time_series_data.csv"
read.gz.files<-function(dir){
  files=list.files(dir)
  files=sapply(files,function(x) strsplit(x,".",fixed = TRUE)) #split name and extension
  files=Filter(function(x) x[length(x)]=="gz",files) #remove all non-csv files
  files=Filter(function(x) grepl("stats",x[[1]][1]),files) #remove all non-csv files
  files=lapply(files,FUN=function(x) paste(unlist(x),collapse = "."))
  files=sapply(unlist(files),function(x)paste(dir,x,sep="/"))
  if(length(files)==0){
    cat("No files found, aborting.")
    quit("no",status=1)
  }
  #read files in parallels
  my_data_tot<-mclapply(files,FUN=function(file){
    ret=read.table(gzfile(file),sep=",",header=T)
    # remove food entries
    ret=ret[ret$agentID!=-1 & ret$timeStep < 50000,]
    ### aggregate data in bins of 1000 timesteps
    ret$bins<-sapply(ret$timeStep,function(x){x %/% 1000}) #reminder
    ret},mc.cores=1) # length(files))
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
  my_data_tot<-mclapply(files,FUN=function(file){
    ret=fread(file)
    # remove food entries
    ret=ret[ret$agentID!=-1 & ret$timeStep < 50000,]
    ### aggregate data in bins of 1000 timesteps
    ret$bins<-sapply(ret$timeStep,function(x){x %/% 1000}) #reminder
    ret
  },mc.cores=1) # length(files))
  my_data_tot
}

if(!file.exists(file.path(root,filename))){
  #find csv files
  my_data_tot=read.gz.files(dir)
  my_data_tot=rbindlist(my_data_tot)
  my_data_tot$value<-1 # reuse this column as variable for reshape
  #my_data_tot=data.frame(my_data_tot,value=1)
  write.csv(my_data_tot,file.path(root,filename))
} else {
  my_data_tot=fread(file.path(root,filename),colClasses=rep("numeric",14),drop=1)
  #my_data_tot=read.table(gzfile(file.path(root,"time_series_data.csv.gz")),sep=",",header=T)
  #my_data_tot=as.data.table(my_data_tot[,-1])
}

library(reshape2)
count.occurrences<-function(var,data) {
  ## count how many times each value of the column 'var' occurs in a bin
  res=as.data.table(data[,c("timeStep","bins","agentID","value",var),with=FALSE])
  res=as.data.table(dcast(res,paste("timeStep+bins+agentID~",var,sep=""),value.var="value",fun.aggregate = sum)) # reshape (new columns count occurrence of each value in factor)
  ## fill in empty columns
  cols=c("0","1","2","3","4") # possible actions, column names
  for(action in cols){
    if(! action %in% names(res)){
      res[,action]<-0
    }
  }
  # normalize over the number of actions
  row.sums<-rowSums(res[,cols,with=FALSE]) # compute sums
  res[,cols]<-res[,lapply(.SD,function(x){x/row.sums}),.SDcols=cols] # normalize
  # compute stats
  f<-function(x,...){summarySE(measurevar=x,...)}
  tmp=lapply(cols,FUN=f,data=res,groupvars="bins",na.rm=TRUE)
  res=data.table(bins=c(0))
  for(i in tmp){
    res=merge(res,i,by=c("bins"),all=TRUE)
  }
  res
}

### count agents that are eating where there is no food
temp=my_data_tot[my_data_tot$action==4 & my_data_tot$foodH ==0 & my_data_tot$timeStep<20000, ]
if(nrow(temp)>0){
  temp$bins<-sapply(temp$timeStep,function(x){x %/% 100}) #reminder
  temp=aggregate(value~bins,data=temp,FUN=length)
  write.csv(temp,"./pointless_eating.csv")
  pdf("./pointless_eating.pdf",width=10,height=5)
  par(oma=c(0,0,3,0),mar=c(5,5,1,1))
  print(plot(temp$bins,temp$value,type="l",xaxt="n",xlab="",ylab="",cex.axis=2,lwd=2,mgp=c(1,0,0)),las=3)
  axis(1,at=c(50,100,150,200),labels=c("5k","10k","15k","20k"),cex.axis=2)
  mtext("Inefficiency of strategy",outer=TRUE,cex=2)
  title(ylab="Frequency",cex.lab=2,line=3)
  title(xlab="Time",cex.lab=2,line=3)
  dev.off()
} else {
  print("Agents eat efficiently")
}

### action codes
# 0: North
# 1: West
# 2: East
# 3: South
# 4: Eat
###

#data.wide<-mclapply(c("foodH","agentN","agentW","agentE","agentS"),FUN=count.occurrences,my_data_tot,mc.cores=1) # length(files))

for(action in c("foodH","agentN","agentW","agentE","agentS")) {
  wide=count.occurrences(action,my_data_tot)
  write.csv(wide,file.path(root,paste("",action,".count.csv",sep="")),row.names = FALSE)
  for(s in c(0,1)){
    wide=count.occurrences(action,my_data_tot2[my_data_tot2$seed==s,])
    write.csv(wide,file.path(root,paste("",action,".count_",s,".csv",sep="")),row.names = FALSE)
  }
}

library(Hmisc)
for(s in c("","_0","_1")){
  eat.wide=read.csv(file.path(root,paste("foodH.count",s,".csv",sep="")))
  north.wide=read.csv(file.path(root,paste("agentN.count",s,".csv",sep="")))
  south.wide=read.csv(file.path(root,paste("agentS.count",s,".csv",sep="")))
  west.wide=read.csv(file.path(root,paste("agentW.count",s,".csv",sep="")))
  east.wide=read.csv(file.path(root,paste("agentE.count",s,".csv",sep="")))
  z=c(0,eat.wide$bins+1)
  
  ### 2D visualization
  x=eat.wide$bins
  pdf(file.path(root,paste("action_frequency",s,".pdf",sep="")),width=10,height=10)
  par(mfrow=c(5,1),oma = c(4, 0, 3, 0),mar=c(1,8,1,1))
  mapply(function(n,data,action) {
    y=eval(parse(text=paste(data,"$mean.",n,sep="")))
    ci=eval(parse(text=paste(data,"$ci.",n,sep="")))
    print(plot(x,type="n",xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,1),cex.lab=2,mgp=c(1,0,0)))
    lines(x,y,cex.lab=2,lwd=2,mgp=c(1,0,0))
    mask=seq(1,length(x),5)
    errbar(x[mask],y[mask],yplus=y[mask]+ci[mask],yminus=y[mask]-ci[mask],lwd=2,add=T)
    axis(2,at=c(0,1),labels=FALSE,cex.axis=2,srt=90,adj=1,xpd=TRUE)
    text(y=c(0,1), x=par()$usr[1],
         labels=c("0%","100%"), srt=0, adj=1, xpd=TRUE,cex=1.5)
    mtext(action,side=2,line=3,cex=1.5)
  },c(4,0,1,2,3)
  ,list("eat.wide","north.wide","west.wide","east.wide","south.wide")
  ,c("Eat","North","West","East","South"))
  axis(1,at=c(9,19,29,39,49),labels=c("10k","20k","30k","40k","50k"),cex.axis=2)
  #mtext("Evolution of gregarious behavior",outer=TRUE,cex=2)
  dev.off()
}
