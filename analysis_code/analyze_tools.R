int="([0-9]+)"
neg_int="([0-9\\-]+)"
flt="([0-9\\.]+)"

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  ## taken from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  datac <- rename(datac, c("N" = paste("N",measurevar,sep=".")))
  datac <- rename(datac, c("mean" = paste("mean",measurevar,sep=".")))
  datac <- rename(datac, c("sd" = paste("sd",measurevar,sep=".")))
  datac <- rename(datac, c("se" = paste("se",measurevar,sep=".")))
  datac <- rename(datac, c("ci" = paste("ci",measurevar,sep=".")))
  
  return(datac)
}

partial <- function(f, ...) {
  l <- list(...)
  function(...) {
    do.call(f, c(l, list(...)))
  }
}

getstr<-function(...) {eval(parse(text = paste(...,sep="")))}

create.empty.table <- function(rows = c(),columns, to.num,codep=TRUE) {

  ## to.num identifies the names of columns to be converted to numeric. By default all columns but "code" are converted. Use c(FALSE) to specify no columns
  if(codep && ! "code" %in% columns) {
    col.names=c("code",columns)
  } else {
    col.names=columns
  }
  out=data.frame(matrix(vector(),
                        length(rows), # num of rows
                        length(col.names), #num of cols
                        dimnames=list(rows, col.names) #names
  ), stringsAsFactors=F)
  setnames(out,old = names(out),new=col.names) #rename the columns
  if(!is.null(rows))
    out$code<-rows #fill the rows
  if(missing(to.num)) { #convert all columns to numeric
    for(c in 2:length(col.names)) {
      out[,c]<-as.numeric(out[,c])
      #out[,c]<-lapply(out[,c],as.numeric)
    }
  } else {
    for(c in to.num) {
      out[,c]<-as.numeric(out[,c])
      #out[,c]<-lapply(out[,c],as.numeric)
    }
  }
  out
}

#returns a subset of all rows in dataframe that meet conditions, conditions must be a data frame with colnames matching those of dataframe
matching.rows<-function(dataframe,conditions){
  dataframe[apply(dataframe,1,FUN = function(row){ #for every row of dataframe
    all(sapply(names(conditions),FUN = function(n){ #for all names in condition
      as.data.frame(t(row))[,n]==as.data.frame(conditions)[,n] #are the conditions matched? (check column by column)
    }))}),]
}

seed.to.str<-function(s){
  arg=as.character(s)
  switch(arg,"0" = "random","1" = "social","2" = "antisocial")
}

get.plot.vars<-function(my_data_mean,params,plot_x,plot_y) {
  #change these to modify the plot
  #list of lists containing the parameter levels
  plot_vars=sapply(params$name,FUN = function(x){levels(as.factor(my_data_mean[,x]))})
  #remove x and y axis from the list
  plot_vars=plot_vars[names(plot_vars)!=plot_x & names(plot_vars)!=plot_y]
  #compute all combinations
  plot_vars=expand.grid(plot_vars)
}

compute.stats<-function(dir) {
  #find csv files
  files=list.dirs(dir)
  files=sapply(files,function(x) strsplit(x,"/",fixed = TRUE)) #split dirs
  files=Filter(function(x) x[length(x)]!=rep_dir,files) #remove root
  files=Filter(function(x) x[length(x)]!=type_dir,files) #remove root
  files=Filter(function(x) x[length(x)]!="results",files) #remove all subdirs
  files=Filter(function(x) grepl("measurement",tail(x,n=1)),files) #remove all non-csv files

  ##construct aggregate table
  my_data_mean=create.empty.table(columns=c("agent.id","energy","seed",params$name))
  my_data_sd=my_data_mean
  pattern=paste("measurement",paste("_",params$name,params$type,sep="",collapse=""),sep="")
  f<-function(i){
    #extract properties from file name
    .name=unlist(i)
    report=as.data.table(read.csv(paste(paste(.name,collapse = "/"),"report.csv",sep="/")))
    stats=summarySE(measurevar="energy",data=report,groupvars=c("seed"),na.rm=TRUE)
    # add columns with parameter values
    param_list=as.numeric(str_match(.name[length(.name)],pattern)[,-1])
    param=t(data.frame(param_list))
    param=param[rep(row.names(param),nrow(stats)),]
    param=as.data.table(param)
    if(ncol(param)==1) { #convert it to row
      param=as.data.table(t(param))
    }
    setnames(param,old=names(param),new=params$name)
    
    cbind(stats,param)
    }
  stats=lapply(files,FUN=f)
  rbindlist(stats)
  # list("mean" = my_data_mean, "sd"= my_data_sd)
}

compute.stats.evolution<-function(dir) {
  #find csv files
  files=list.dirs(dir)
  files=sapply(files,function(x) strsplit(x,"/",fixed = TRUE)) #split dirs
  files=Filter(function(x) x[length(x)]!=rep_dir,files) #remove root
  files=Filter(function(x) x[length(x)]!=type_dir,files) #remove root
  files=Filter(function(x) x[length(x)]!="results",files) #remove all subdirs
  
  ##construct aggregate table
  end_values=create.empty.table(columns=c("seed","timeStep","freq",params$name))
  end_values$code<-NULL
  pattern=paste("measurement",paste("_",params$name,params$type,sep="",collapse=""),sep="")
  for(file in files){
    #extract properties from file name
    name=unlist(file)
    param_list=as.numeric(str_match(name[length(name)],pattern)[,-1])
    report=as.data.table(read.csv(paste(paste(unlist(file),collapse = "/"),"population_evolution.csv",sep="/")))
    report=report[report$timeStep==max(as.numeric(levels(as.factor(report$timeStep)))),]
    report=report[,-1,with=FALSE]
    if(nrow(report)==1) {
      temp=report
      if(temp$seed==0) {
        temp$seed<-1
      } else if(temp$seed==1) {
        temp$seed<-0
      }
      temp$freq<-0
      report=rbind(report,temp)
      rm(temp)
    }
    
    param=t(data.frame(param_list))
    param=param[rep(row.names(param),nrow(report)),]
    param=as.data.table(param)
    setnames(param,old=names(param),new=params$name)
    
    end_values=rbind(end_values,cbind(report,param))
    rm(name,param_list,param,report)
  }
  rm(file,files)
  end_values
}

select.rows<-function(dataset,plot_y,cond) {
  ret=matching.rows(dataset,cond)
  ret=as.data.table(ret[with(ret,order(eval(parse(text=plot_y)),seed)),])
  as.data.frame(ret[!apply(ret,1,function(x) all(is.na(x)))])
}

produce.graph<-function(name,cond,plot_y,my_data_mean,my_data_sd,offset) {
  temp_mean=select.rows(my_data_mean,plot_y,cond)
  temp_sd=select.rows(my_data_sd,plot_y,cond)

  seeds=levels(as.factor(temp_mean$seed))
  n=length(seeds)
  colors = rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 0.5)
  x=temp_mean[,plot_y]
  y_max=max(max(temp_mean[,"energy"]),max(temp_sd[,"energy"]))+offset
  y_min=min(min(temp_mean[,"energy"]),min(temp_sd[,"energy"]))
  pdf(name)
  print(plot(x,xlim=c(min(x),max(x)),ylim=c(y_min,y_max),#main="Avg population performance",
             type="n",xlab=params[name==plot_y,desc],ylab="Average performance",cex.lab=2,cex.axis=1.5))
  for(s in 1:n) {
    lines(temp_mean[temp_mean$seed==seeds[s],plot_y],temp_mean[temp_mean$seed==seeds[s],"energy"],col=colors[s],lwd=4)
    lines(temp_mean[temp_mean$seed==seeds[s],plot_y],temp_mean[temp_mean$seed==seeds[s],"energy"]+temp_sd[temp_sd$seed==seeds[s],"energy"],col=colors[s],lty=3,lwd=2)
    lines(temp_mean[temp_mean$seed==seeds[s],plot_y],temp_mean[temp_mean$seed==seeds[s],"energy"]-temp_sd[temp_sd$seed==seeds[s],"energy"],col=colors[s],lty=3,lwd=2)
  }
  rm(seeds,temp_mean,temp_sd,n,colors,x,y_max,y_min)
  dev.off()
}

produce.3d.graph <- function(name,my.data,cond,plot_x,plot_y,plot_z,plot_face) {
    temp=matching.rows(my.data,cond)

    ##add lines to temp with missing values (sum of ratios > 1), empty lines are not accepted
    xs=as.numeric(levels(as.factor(temp[,plot_x])))
    ys=as.numeric(levels(as.factor(temp[,plot_y])))
    seeds=as.numeric(levels(as.factor(temp$seed)))
    test=expand.grid(xs,ys,seeds)
    setnames(test,old=names(test),new=c(plot_x,plot_y,"seed"))
    conds=cond
    conds=conds[rep(row.names(conds),nrow(test)),]
    test=cbind(test,conds)
    temp=merge(temp,test,all.y = T)

    ##plot
    n=length(levels(as.factor(temp$seed)))
    colors = rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 0.5)
    pdf(name)
    print(wireframe(eval(parse(text=paste(plot_z,"~",plot_x,"*",plot_y,sep=""))),data=temp,group=seed, col.groups=colors,scales = list(arrows=FALSE, col="black",font=10)
                    #,main=paste("Effect of food scarcity on strategy,",f)
                    ,sub=paste(names(cond),sapply(cond,as.character),sep=":",collapse=" ")
                    ,xlab=list(params[name==plot_x,desc],rot=plot_face$rot_x),ylab=list(params[name==plot_y,desc],rot=plot_face$rot_y),zlab=list("Final avg energy",rot=90)
                    ,key=list(text=list(sapply(levels(as.factor(temp$seed)),seed.to.str)),
                              lines=list(lty=rep(1,n),col=colors))
                    ,par.settings = list(axis.line = list(col = "transparent"))
                   ,screen = eval(parse(text=plot_face$screen))))
    rm(temp,test,conds,n,colors,xs,ys,seeds)
    dev.off()
}
