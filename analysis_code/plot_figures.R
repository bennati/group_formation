source("./analyze_tools.R")
library(Hmisc)

#custom
produce.graph<-function(plot.dir,name,cond,plot_y,stats,offset,ylabel="Average fitness") {
  temp=select.rows(stats,plot_y,cond)

  seeds=seeds=c("0","1")#levels(as.factor(temp_mean$seed))
  n=length(seeds)
  colors = c("blue","red")#rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 0.5)
  x=temp[,plot_y]
  y_max=max(max(temp[,"mean.energy"]),max(temp[,"ci.energy"]))+offset
  y_min=min(min(temp[,"mean.energy"]),min(temp[,"ci.energy"]))
  old.par=par
  pdf(file.path(plot.dir,paste(name,".pdf",sep = "")),width=10,height = 7.5)
  par(xpd = T, mar = par()$mar + c(0,0,0,0)) # space at the bottom
  print(plot(x,xlim=c(min(x),max(x)),ylim=c(0,y_max),#main="Avg population performance",
             mgp=c(2.5,1,0),
             type="n",xlab=params[name==plot_y,desc],ylab=ylabel,cex.lab=2,cex.axis=1.5))
  ltypes=c(1,2)
  legend("top",c("Random","Gregarious"),col=colors,lty=ltypes,lwd=c(5,5,5,5),cex=1.5,horiz=TRUE,inset=c(0,-0.12))
  for(s in 1:n) {
    mu=temp[temp$seed==seeds[s],"mean.energy"]
    ci=temp[temp$seed==seeds[s],"ci.energy"]
    lines(temp[temp$seed==seeds[s],plot_y],mu,col=colors[s],lty=ltypes[s],lwd=5)
    # change errbar color
    par(fg = colors[s]) 
    errbar(temp[temp$seed==seeds[s],plot_y],mu,yplus=mu+ci,yminus=mu-ci,col=colors[s],lty=ltypes[s],lwd=3,add=T)
  }
  dev.off()
  par=old.par
}

produce.graph.comparison<-function(plot.dir,stats,stats_s,cond,name,plot_y,offset=0,offset_s=0) {
  dataset=select.rows(stats,plot_y,cond)
  dataset2=select.rows(stats_s,plot_y,cond)
  seeds=levels(as.factor(dataset$seed))
  n=length(seeds)
  colors = c("blue","red","red")#rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 0.5)
  ltypes=c(1,2,4)
  x=dataset[,plot_y]
  y_max=max(max(dataset[,"mean.energy"]),max(dataset[,"ci.energy"]))+offset
  y_min=min(min(dataset[,"mean.energy"]),min(dataset[,"ci.energy"]))
  y_max_s=max(max(dataset2[,"mean.energy"]),max(dataset2[,"ci.energy"]))+offset_s
  y_min_s=min(min(dataset2[,"mean.energy"]),min(dataset2[,"ci.energy"]))
  y_min=min(y_min,y_min_s)
  y_max=max(y_max,y_max_s)
  rm(y_max_s,y_min_s)
  old.par=par
  png(file.path(plot.dir,paste(name,".png",sep = "")),width=20,height = 16,units="cm",res = 300)
  par(xpd = T, mar = par()$mar + c(0,0,0,0)) # space at the bottom
  print(plot(x,xlim=c(min(x),max(x)),ylim=c(y_min,y_max),#main="Avg population performance",
       yaxt="n",mgp=c(2.5,1,0),
       type="n",xlab=params[name==plot_y,desc],ylab="Average fitness",cex.lab=1.5,cex.axis=1.5))
  legend("top",c("Random","Gregarious, s","Gregarious, a"),col=colors,lty=ltypes,lwd=c(3,3,3),cex=1.5,horiz=TRUE,inset=c(0,-0.15))
  for(s in 1:n) {
   mu=dataset[dataset$seed==seeds[s],"mean.energy"]
    ci=dataset[dataset$seed==seeds[s],"ci.energy"]
    lines(dataset[dataset$seed==seeds[s],plot_y],mu,col=colors[s],lty=ltypes[s],lwd=5)
    # change errbar color
    par(fg = colors[s]) 
    errbar(dataset[dataset$seed==seeds[s],plot_y],mu,yplus=mu+ci,yminus=mu-ci,col=colors[s],lty=ltypes[s],lwd=3,add=T)
  }
  ## print second line
  mu=dataset2[dataset2$seed=="1","mean.energy"]
  ci=dataset2[dataset2$seed=="1","ci.energy"]
  lines(dataset2[dataset2$seed=="1",plot_y],mu,col=colors[3],lty=ltypes[3],lwd=5)
  # change errbar color
  par(fg = colors[3]) 
  errbar(dataset2[dataset2$seed=="1",plot_y],mu,yplus=mu+ci,yminus=mu-ci,col=colors[3],lty=ltypes[3],lwd=3,add=T)
  dev.off()
  ## plot pdf
  par=old.par
  pdf(file.path(plot.dir,paste(name,".pdf",sep = "")),width=10,height = 7.5)
  par(xpd = T, mar = par()$mar + c(0,0,0,0)) # space at the bottom
  print(plot(x,xlim=c(min(x),max(x)),ylim=c(y_min,y_max),#main="Avg population performance",
             yaxt="n",mgp=c(2.5,1,0),
             type="n",xlab=params[name==plot_y,desc],ylab="Average fitness",cex.lab=1.5,cex.axis=1.5))
  legend("top",c("Random","Gregarious, s","Gregarious, a"),col=colors,lty=ltypes,lwd=c(3,3,3),cex=1.5,horiz=TRUE,inset=c(0,-0.12))
  for(s in 1:n) {
    mu=dataset[dataset$seed==seeds[s],"mean.energy"]
    ci=dataset[dataset$seed==seeds[s],"ci.energy"]
    lines(dataset[dataset$seed==seeds[s],plot_y],mu,col=colors[s],lty=ltypes[s],lwd=5)
    # change errbar color
    par(fg = colors[s]) 
    errbar(dataset[dataset$seed==seeds[s],plot_y],mu,yplus=mu+ci,yminus=mu-ci,col=colors[s],lty=ltypes[s],lwd=3,add=T)
  }
  ## print second line
  mu=dataset2[dataset2$seed=="1","mean.energy"]
  ci=dataset2[dataset2$seed=="1","ci.energy"]
  lines(dataset2[dataset2$seed=="1",plot_y],mu,col=colors[3],lty=ltypes[3],lwd=5)
  # change errbar color
  par(fg = colors[3]) 
  errbar(dataset2[dataset2$seed=="1",plot_y],mu,yplus=mu+ci,yminus=mu-ci,col=colors[3],lty=ltypes[3],lwd=3,add=T)
  dev.off()
  par=old.par
}

library(data.table)
library(parallel)
library(stringr) #string_cat
library(lattice)

root = "../"
rep_dir="results"

#wireframe plot orientation
face_y=data.table(screen="list(z = 90,x=-75)",
                  rot_x=-75,
                  rot_y=0)
face_x=data.table(screen="list(z = 180,x=-75)",
                  rot_x=0,
                  rot_y=-75)
face_default=data.table(screen="list(z = 45,x=-60)",
                        rot_x=35,
                        rot_y=-35)

params=data.table(name=c("NF","A","FR","N","SR")
                  ,type=c(int,int,int,int,flt)
                  ,desc=c("Number of food cells","Max Age","Field Of View radius","Number of agents","SAs ratio")
)
###############
# Figure 1, no static SA=0.1 outperform random for few food sources
type_dir="figure_nostatic"
stats=compute.stats(file.path(root,rep_dir,type_dir))
plot.dir=file.path(root,rep_dir,type_dir,"plots")
dir.create(plot.dir)

type_dir_s="figure_static"
stats_s=compute.stats(file.path(root,rep_dir,type_dir_s))
plot.dir.s=file.path(root,rep_dir,type_dir_s,"plots")
dir.create(plot.dir.s)

nfs=c(5,10,20,50,100)
frs=c(1,2,4,6,9)
ns=c(20,30,50,70,100)

for(nf in nfs){
  for(fr in frs){
    plot_y="N"
    cond=data.frame("NF"=nf,"FR"=fr,"SR"=0.1,"A"=1000)
    produce.graph(plot.dir,paste("nostatic_N","_NF",nf,"_FR",fr,sep=""),cond,plot_y,stats,0,ylabel="Average fitness")
  }
}

###############
# Figure 2, no static SA performance decreases with increasing FOV

for(nf in nfs){
  for(n in ns){
    plot_y="FR"
    cond=data.frame("NF"=nf,"N"=n,"SR"=0.1,"A"=1000)
    # cond=data.frame("NF"=5,"F"=200,"FK"=0.0,"N"=20,"SR"=0.1,"AR"=0.0)
    produce.graph(plot.dir,paste("nostatic_FOV","_NF",nf,"_N",n,sep=""),cond,plot_y,stats,0,ylabel="Average fitness")
  }
}


###############
# Figure 3, no static. SA performance decreases with increasing NF


for(fr in frs){
  for(n in ns){
    plot_y="NF"
    cond=data.frame("FR"=fr,"N"=n,"SR"=0.1,"A"=1000)
    produce.graph(plot.dir,paste("nostatic_NF","_FR",fr,"_N",n,sep=""),cond,plot_y,stats,0,ylabel="Average fitness")
  }
}

###############
# Figure 1, static SA=0.1 outperform random for few food sources

sr=0.5
for(nf in nfs){
  for(fr in frs){
    plot_y="N"
    cond=data.frame("NF"=nf,"FR"=fr,"SR"=sr,"A"=1000)
    produce.graph(plot.dir.s,paste("static_N","_NF",nf,"_FR",fr,"_SR",sr,sep=""),cond,plot_y,stats_s,0,ylabel="Average fitness")
  }
}


###############
# Figure 2, static. SA performance decreases with increasing FOV


for(nf in nfs){
  for(n in ns){
    plot_y="FR"
    cond=data.frame("NF"=nf,"N"=n,"SR"=sr,"A"=1000)
    produce.graph(plot.dir.s,paste("static_FOV","_NF",nf,"_N",n,"_SR",sr,sep=""),cond,plot_y,stats_s,0,ylabel="Average fitness")
  }
}


###############
# Figure 3, static. SA performance decreases with increasing NF


for(fr in frs){
  for(n in ns){
    plot_y="NF"
    cond=data.frame("FR"=fr,"N"=n,"SR"=sr,"A"=1000)
    produce.graph(plot.dir.s,paste("static_NF","_FR",fr,"_N",n,"_SR",sr,sep=""),cond,plot_y,stats_s,0,ylabel="Average fitness")
  }
}


### Comparisons ###
plot.dir=file.path(root,rep_dir,"plots")
dir.create(plot.dir)

## N
for(fr in frs){
  for(nf in nfs){
    plot_y="N"
    cond=data.frame("NF"=nf,"FR"=fr,"SR"=0.1,"A"=1000)
    name=paste("N_Comparison","_FR",fr,"_NF",nf,sep="")
    produce.graph.comparison(plot.dir,stats,stats_s,cond,name,plot_y)
}}


## FR
for(n in ns){
  for(nf in nfs){
    plot_y="FR"
    cond=data.frame("NF"=nf,"N"=n,"SR"=0.1,"A"=1000)
    name=paste("FR_Comparison","_N",n,"_NF",nf,sep="")
    produce.graph.comparison(plot.dir,stats,stats_s,cond,name,plot_y)
  }}

## NF
for(n in ns){
  for(fr in frs){
    plot_y="NF"
    cond=data.frame("FR"=fr,"N"=n,"SR"=0.1,"A"=1000)
    name=paste("NF_Comparison","_N",n,"_FR",fr,sep="")
    produce.graph.comparison(plot.dir,stats,stats_s,cond,name,plot_y)
  }}
rm(temp_mean,temp_sd,temp_mean_s,temp_sd_s,plot_vars,plot_x,plot_y,plot_z,params,var,cond)


params=data.table(name=c("NF","A","FR","N","SR")
                  ,type=c(int,int,int,int,flt)
                  ,desc=c("Number of food cells","Max age","Field Of View radius","Number of agents","SAs ratio")
)
plot_x="NF"
plot_y="FR"
plot_z="freq"
plot_face=face_y

### Comparison ###
plot.evo.comparison<-function(dataset,dataset_s,plot_y) {
  # delete some points in dataset
  dataset=dataset[dataset$timeStep %% 10 ==0,]
  dataset_s=dataset_s[dataset_s$timeStep %% 10 ==0,]
  seeds=levels(as.factor(dataset$seed))
  n=length(seeds)
  colors = c("blue","red","blue","red")#rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 0.5)
  x=dataset[,plot_y]
  y_max=max(dataset[,"freq"])
  y_min=min(dataset[,"freq"])
  y_max_s=max(dataset_s[,"freq"])
  y_min_s=min(dataset_s[,"freq"])
  y_min=min(y_min,y_min_s)-500
  y_max=max(y_max,y_max_s)
  rm(y_max_s,y_min_s)
  #png
  png(paste(name,".png",sep = ""),width=20,height = 16,units="cm",res = 300)
  print(plot(x,xlim=c(min(x),max(x)),ylim=c(y_min,y_max),#main="Avg population performance",
             #sub=paste(names(cond),sapply(cond,as.character),sep=":",collapse=" "),
             yaxt="n",mgp=c(2.5,1,0),
             type="n",xlab="Time",ylab="Average size",cex.lab=1.5,cex.axis=1.5))
  ltypes=c(1,3)
  for(s in 1:n) {
    lines(dataset[dataset$seed==seeds[s],plot_y],dataset[dataset$seed==seeds[s],"freq"],col=colors[s],lwd=5,lty=ltypes[s])
  }
  ## print second line
  for(s in 1:n) {
    lines(dataset_s[dataset_s$seed==seeds[s],plot_y],dataset_s[dataset_s$seed==seeds[s],"freq"],col=colors[s+n],lwd=5,lty=ltypes[s]+1)
  }
  legend("bottomleft",c("Random","Gregarious","Random (+)","Gregarious (+)"),col=colors,lty=c(ltypes,ltypes+1),lwd=c(3,3,3,3),horiz=TRUE,cex=1.3)
  dev.off()
  #pdf
  pdf(paste(name,".pdf",sep = ""),width=10,height = 7.5)
  print(plot(x,xlim=c(min(x),max(x)),ylim=c(y_min,y_max),#main="Avg population performance",
             #sub=paste(names(cond),sapply(cond,as.character),sep=":",collapse=" "),
             yaxt="n",mgp=c(2.5,1,0),
             type="n",xlab="Time",ylab="Average size",cex.lab=1.5,cex.axis=1.5))
  for(s in 1:n) {
    lines(dataset[dataset$seed==seeds[s],plot_y],dataset[dataset$seed==seeds[s],"freq"],col=colors[s],lwd=5,lty=ltypes[s])
  }
  ## print second line
  for(s in 1:n) {
    lines(dataset_s[dataset_s$seed==seeds[s],plot_y],dataset_s[dataset_s$seed==seeds[s],"freq"],col=colors[s+n],lwd=5,lty=ltypes[s]+1)
  }
  legend("bottomleft",c("Random","Gregarious","Random (+)","Gregarious (+)"),col=colors,lty=c(ltypes,ltypes+1),lwd=c(3,3,3,3),horiz=TRUE,cex=1.3)
  dev.off()
}

plot_y="timeStep"
name=paste(root,rep_dir,"evo_comparison",sep = "/")
dataset=read.csv(file.path(root,rep_dir,"figure_evolution_nostatic","measurement_NF50_A1000_FR1_N50_SR0.1","population_evolution.csv"))
dataset_s=read.csv(file.path(root,rep_dir,"figure_evolution_static","measurement_NF50_A1000_FR1_N50_SR0.1","population_evolution.csv"))
plot.evo.comparison(dataset,dataset_s,plot_y)

## effect of parameters
params=data.table(name=c("NF","A","FR","N","SR")
                  ,type=c(int,int,int,int,flt)
                  ,desc=c("Number of food cells","Max age","Field Of View radius","Number of agents","SAs ratio")
)
type_dir="figure_evolution_nostatic"
dir=file.path(root,rep_dir,type_dir)

# get all directories
files=list.dirs(dir)
files=sapply(files,function(x) strsplit(x,"/",fixed = TRUE)) #split dirs
files=Filter(function(x) x[length(x)]!=rep_dir,files) #remove root
files=Filter(function(x) x[length(x)]!=type_dir,files) #remove root
files=Filter(function(x) x[length(x)]!="results",files) #remove all subdirs
pattern=paste("measurement",paste("_",params$name,params$type,sep="",collapse=""),sep="")
# create table with all combinations of parameters
levs=create.empty.table(columns=c(params$name),codep=FALSE)
for(file in files){
  #extract properties from file name
  name=unlist(file)
  param_list=as.numeric(str_match(name[length(name)],pattern)[,-1])
  levs=rbind(levs,param_list)
  rm(name,param_list)
}
setnames(levs,old=names(levs),new=params$name)
# compute effect of every parameter
dataset=create.empty.table(columns=c("seed","timeStep","freq",params$name),codep=FALSE)
# read all files with that parameter value
for(d in 1:nrow(levs)){
  param_list=levs[d,params$name]
  file=paste("measurement",paste("_",params$name,param_list,sep="",collapse=""),sep="")
  report=as.data.table(read.csv(paste(dir,file,"population_evolution.csv",sep="/")))
  report$X<-NULL
  report=report[report$timeStep %% 100 ==0,] #reduce data
  #add parameter description
  param=param_list
  param=param[rep(row.names(param),nrow(report)),]

  dataset=rbind(dataset,cbind(report,param))
  rm(param_list,param,report,file)
}
## compute average for each value of parameter
dataset_mean=create.empty.table(columns=c("seed","timeStep","freq",params$name),codep=FALSE)
dataset_sd=dataset_mean
for(p in params$name) {
  # for all values of this parameter
  for(l in levels(as.factor(levs[,p]))) {
    temp=as.data.frame(dataset)
    temp=temp[temp[,p]==l,]
    temp=as.data.table(temp)
    stats<-mclapply(c("mean","sd"),function(f) {
      temp[,lapply(.SD,eval(parse(text=f))),by=c("timeStep","seed")]},mc.cores=1)
    mean=as.data.table(stats[[1]]);
    sd=as.data.table(stats[[2]]);
    rm(temp,stats)

    mean[,params$name[params$name != p]:=NA] #remove values in other columns
    sd[,params$name[params$name != p]:=NA] #remove values in other columns
    dataset_mean=rbind(dataset_mean,mean)
    dataset_sd=rbind(dataset_sd,sd)
    rm(mean,sd)
}}

## plot effect of each param
library(rgl)
r3dDefaults$windowRect <- c(0,50, 800, 800) #make window bigger
old.par<-r3dDefaults$userMatrix
r3dDefaults$userMatrix<-matrix(c(0.7539899,-0.1587339, 0.6374187,0.0000000,
                                 -0.01133741,0.96707529,0.25423795,0.00000000,
                                 -0.6567879,-0.1989195,0.7273654,0.0000000,
                                 0,0,0,1),c(4,4))
library(reshape2)
plot_face=face_default
plot_y="timeStep"
plot_z="freq"
for(p in params$name) {
  plot_x=p
  temp1=as.data.frame(dataset_mean)[!is.na(dataset_mean[,p,with=FALSE]),]
  for(s in levels(as.factor(temp1$seed))) {
    temp=temp1[temp1$seed==s,]
    temp[,p]<-as.factor(temp[,p])
    name=paste("avg_param_",p,"_seed_",s,".pdf",sep="")
    temp.mat=subset(temp,select=c("timeStep","freq",p))
    temp.mat=dcast(temp.mat,paste("timeStep~",p,sep=""),value.var = "freq")
    temp.mat$timeStep<-NULL
    temp.mat=as.matrix(temp.mat)
    if(nlevels(as.factor(temp[,p]))>1) { #print in 3D
      ##plot
      pdf(file.path(dir,name))
      if(p=="NF"){
        temp1d=temp[temp[p]==5 & temp["timeStep"]<20000,plot_z]
        print(plot(seq(0,19900,100),temp1d,ylim=c(0,max(temp1d)),main="Average Gregarious group size",cex.main=1.5,
                   mgp=c(2.5,1,0),type="l",lwd=3
                   ,xlab="Timestep",ylab="Average size",cex.lab=2,cex.axis=1.5))
      }
      print(wireframe(eval(parse(text=paste(plot_z,"~",plot_x,"*",plot_y,sep=""))),data=temp
                      #,group=seed, col.groups=colors
                      ,scales = list(arrows=FALSE, col="black",font=10)
                      ,drape = TRUE,colorkey = TRUE
                      ,xlab=list(p,rot=plot_face$rot_x),ylab=list("timeStep",rot=plot_face$rot_y),zlab=list("Freq",rot=90)
                      ,par.settings = list(axis.line = list(col = "transparent"))
                      ,screen = eval(parse(text=plot_face$screen))))
      dev.off()
      ## plot interactive opengl graph
      ylim<-range(temp.mat)
      ylen <- ylim[2] - ylim[1] + 1
      scale=ceiling(ylen/1000)
      y<-temp.mat/scale
      ylim<-range(y)
      ylen <- ylim[2] - ylim[1] + 1
      x<-(1000/nrow(y))*(1:nrow(y))
      z<-(1000/ncol(y))*(1:ncol(y))
      colorlut <- terrain.colors(ylen,alpha=0) # height color lookup table
      col <- colorlut[ y-ylim[1]+1 ] # assign colors to heights for each point
      open3d()
      rgl.surface(x, z, y, color=col, alpha=0.75)
      title3d(paste("Effect of parameter",p), '', 'timeStep', 'Avg Performance', p, col='black')
      axis3d('z',at=z,labels = levels(as.factor(temp[,p])),col="black")
      x.interval=seq(0,length(x),by=length(x)/4)
      x.axis=c(0,x[x.interval])
      x.max=20000
      x.labs=seq(0,x.max,by=x.max/4)
      axis3d('x',at=x.axis,labels = x.labs,col="black")
      axis3d('y',col="black")
      rgl.snapshot(paste(dir,"/avg_param_",p,"_seed_",s,"_3d.png",sep=""),fmt="png")
      rm(x.max,x.interval,x.axis,x.labs)
    } else { #print in 2D
      xs=temp[,plot_y]
      ys=temp[,plot_z]
      pdf(file.path(dir,name))
      print(plot(xs,ys,xlim=c(min(xs),max(xs)),ylim=c(min(ys),max(ys)),xlab=params[name==plot_y,desc],ylab="Freq",cex.lab=2,cex.axis=1.5))
      dev.off()
      rm(xs,ys)
    }
  }
  rm(temp,temp1,name,plot_x)
}
