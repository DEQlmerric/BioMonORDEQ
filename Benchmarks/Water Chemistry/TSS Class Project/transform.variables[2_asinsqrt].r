# a new and improved version of the 
transform.view2<-function(ds){

#################################################################################
# histogram functions that are only used within this function
# the reason i am using functions within functions is in case one of the
# plots throws and error. By using the try() function "around" the plotting function
# i can catch the error and ignore it so that it does not crash the code
instructions<-"Use this function to loop through all your numerical variables and record the best transformation.\nTo use this best you should arrange your console window and the graphic window so that you can see them both.\nUse the following list of responses to navigate through your dataset, hit <enter> after each response.\n1. Use 1-6 to record the transformation, 6 records that you want to drop this variable.\n2. Hit <enter> alone to move forward without recording anything.\n3. Use b to move back one variable.\n4. s or show will display what you have recorded so far.\n5. i or instructions will show this list again.\n6. x or exit will exit the function and pass back what you have done so far.\n"

hist.normal<-function(variable,variable.name){
  keep<-!(variable==-Inf | is.na(variable))
  variable<-variable[keep]
  vmin<-round(min(variable),3)
  vmax<-round(max(variable),3)
  n<-sum(keep)
  hist(variable,xlab=variable.name,sub=paste("N =",n,"; ",vmin,"-",vmax),col="light blue",main=NA,ylab=NA,col.sub="red")
  return(NA)
}
hist.log.plus1<-function(variable,variable.name){
  variable<-log10(variable+1)
  keep<-!(variable==-Inf | is.na(variable))
  variable<-variable[keep]
  n<-sum(keep)
  hist(variable,xlab=paste("Log10(",variable.name,"+1)",sep=""),sub=paste("N =",n),col="light blue",main=NA,ylab=NA,col.sub="red")
  return(NA)
}
hist.arcsine.sqrt<-function(variable,variable.name){
  variable<-variable[!is.na(variable)]
  if (any((variable/100)>1)){
  return("not going to happen")
  }else{
  variable<-asin(sqrt(variable/100))
  keep<-!(variable==-Inf | is.na(variable))
  variable<-variable[keep]
  n<-sum(keep)
  hist(variable,xlab=paste("asin(sqrt(",variable.name,"))",sep=""),sub=paste("N =",n),col="light blue",main=NA,ylab=NA,col.sub="red")
  return(NA)
  }
}
hist.sqrt<-function(variable,variable.name){
variable<-variable[!is.na(variable)]
  if (any(variable<0)){
  return("not going to happen")
  }else{
  variable<-sqrt(variable)
  keep<-!(variable==-Inf | is.na(variable))
  variable<-variable[keep]
  n<-sum(keep)
  hist(variable,xlab=paste("sqrt(",variable.name,")",sep=""),sub=paste("N =",n),col="light blue",main=NA,ylab=NA,col.sub="red")
  return(NA)
  }
}
hist.log10<-function(variable,variable.name){
  variable<-log10(variable)
  keep<-!(variable==-Inf | is.na(variable))
  variable<-variable[keep]
  n<-sum(keep)
  hist(variable,xlab=paste("Log10(",variable.name,")",sep=""),sub=paste("N =",n),col="light blue",main=NA,ylab=NA,col.sub="red")
  return(NA)
}

#qq plot functions that are only to be used within this function
boxplot.normal<-function(variable,variable.name){
  missing.values<-sum(is.na(variable))
  zero.values<-sum(variable==0)
  variable<-variable[!is.na(variable)]
  qqnorm(variable,xlab=NA,col="light blue",main=NA,ylab=NA,col.sub="red")
  qqline(variable)
  return(NA)
}
boxplot.log.plus1<-function(variable,variable.name){
variable<-variable[!is.na(variable)]
  qqnorm(log10(variable+1),xlab=NA,col="light blue",main=NA,ylab=NA,col.sub="red")
  qqline(log10(variable+1))
  return(NA)
}
boxplot.arcsine.sqrt<-function(variable,variable.name){
  variable<-variable[!is.na(variable)]
  if (any(variable>1)){
  return("not going to happen")
  }else{
  qqnorm(asin(sqrt(variable)),xlab=NA,col="light blue",main=NA,ylab=NA,col.sub="red")
  qqline(asin(sqrt(variable)))
  return(NA)
  }
}
boxplot.sqrt<-function(variable,variable.name){
variable<-variable[!is.na(variable)]
  if (any(variable<0)){
  return("not going to happen")
  }else{
 qqnorm(sqrt(variable),xlab=NA,col="light blue",main=NA,ylab=NA,col.sub="red")
 qqline(sqrt(variable))
 return(NA)
 }
}
boxplot.log10<-function(variable,variable.name){
  variable<-variable[!is.na(variable)]
  if (any(variable==0)){
  return("not going to happen")
  }else{
  qqnorm(log10(variable),xlab=NA,col="light blue",main=NA,ylab=NA,col.sub="red")
  qqline(log10(variable))
 return(NA)
 }
}

#the error plot
plot.error<-function(error=NA){
 plot(1:100,1:100,type="n",bty="n",fg='white',col.axis="white",xlab=NA)
 box()
 text(50,50,"Not Available",col="brown")
}
###############################################################################
# below is the rest of the code for this function
 par(mfrow=c(2,5),mar=c(5, 3, 1, 1))
 number.of.columns<-ncol(ds)
 transform.log<-data.frame(variable=NA,transformation=NA)
 cat(instructions)
 readline("Hit <enter> to start recording.")
 i<-1
 while (i <= number.of.columns){
  if (i<1){i<-1}
  
  # display the variable
  variable.name<-colnames(ds)[i]
  variable.data<-ds[,i]
  transform.log[i,1]<-variable.name
  
  if (is.numeric(variable.data)){
    if (!is.na(try(hist.normal(variable.data,variable.name),silent=T))){plot.error()}
    if (!is.na(try(hist.log.plus1(variable.data,variable.name),silent=T))){plot.error()}
    if (!is.na(try(hist.log10(variable.data,variable.name),silent=T))){plot.error()}
    if (!is.na(try(hist.arcsine.sqrt(variable.data,variable.name),silent=T))){plot.error()}
    if (!is.na(try(hist.sqrt(variable.data,variable.name),silent=T))){plot.error()}
    
    if (!is.na(try(boxplot.normal(variable.data,variable.name),silent=T))){plot.error()}
    if (!is.na(try(boxplot.log.plus1(variable.data,variable.name),silent=T))){plot.error()}
    if (!is.na(try(boxplot.log10(variable.data,variable.name),silent=T))){plot.error()}
    if (!is.na(try(boxplot.arcsine.sqrt(variable.data,variable.name),silent=T))){plot.error()}
    if (!is.na(try(boxplot.sqrt(variable.data,variable.name),silent=T))){plot.error()}

    # check for response from user
    keystroke<-readline("Choose a graph or use f to go forward and b to go back, x to exit: ")
    if (keystroke %in% 1:6){
      transform.log[i,2]<-keystroke
      i<-i+1
    }else{
      if (keystroke %in% c("f","F","forward","forward","next","Next","")){i<-i+1}
      if (keystroke %in% c("b","B","Back","back","last","Last")){i<-i-1}
      if (keystroke %in% c("reset")){par(mfrow=c(2,5))}
      if (keystroke %in% c("par")){par(mfrow=c(3,5))}
      if (keystroke %in% c("x","exit")){i<-number.of.columns+1}
      if (keystroke %in% c("i","instructions")){cat(instructions)}
      if (keystroke %in% c("s","show")){print(transform.log[!is.na(transform.log$transformation),])}
    }
  }else{i<-i+1}
 }
#  transform.log$transformation<-as.character(transform.log$transformation)
#  transform.log[transform.log$transformation=="1","transformation"]<-"Original"
#  transform.log[transform.log$transformation=="2","transformation"]<-"log10"
#  transform.log[transform.log$transformation=="3","transformation"]<-"log10+1"
#  transform.log[transform.log$transformation=="4","transformation"]<-"asin(sqrt)"
#  transform.log[transform.log$transformation=="5","transformation"]<-"sqrt"
#  transform.log[transform.log$transformation=="6","transformation"]<-"Drop"#
# transform.log$transformation<-as.factor(transform.log$transformation)
# levels(transform.log$transformation)<c("","","","")
 return(transform.log)
}



var.table<-function(dataset,std=F){
 #m<-(data=NA,nrow=ncol(dataset),ncol=10)
   m<-data.frame(Variable=NA,Type=NA,n=NA,Missing=NA,Unique=NA,Mean=NA,Median=NA,Variance=NA,Stdev=NA,Min=NA,Max=NA,Range=NA)
 #rownames(m)<-colnames(dataset)
 #colnames(m)<-c('Variable','n','Mean','Median','Variance','Stdev','Min','Max','Range','Missing')
   
 for (i in 1:ncol(dataset)){
 
  v<-dataset[,i]
  m[i,'Variable']<-colnames(dataset)[i]
  m[i,'Unique']<-length(unique(v))
  m[i,'Missing']<-sum(is.na(v)) 
  m[i,'n']<-length(v)
  if (is.numeric(v)){
  
  v<-v[!is.na(v)]
  if (std){v<-(v-mean(v))/sd(v)}
  m[i,'Type']<-"Numeric"
  m[i,'Mean']<-mean(v)
  m[i,'Median']<-median(v)
  m[i,'Variance']<-var(v)
  m[i,'Stdev']<-sd(v)
  m[i,'Min']<-min(v)
  m[i,'Max']<-max(v)
  m[i,'Range']<-max(v)-min(v) 
  
  }else{
   m[i,'Type']<-"String"
  }
 }

 return(m)
}

transform.set<-function(env,transform.view.object){

  rst<-transform.view.object
  var.leave<-as.vector(rst[rst$transformation==1 & !is.na(rst$transformation),"variable"])
  var.log10plus1<-as.vector(rst[rst$transformation==2 & !is.na(rst$transformation),"variable"])
  var.log10<-as.vector(rst[rst$transformation==3 & !is.na(rst$transformation),"variable"])
  var.arc.sqrt<-as.vector(rst[rst$transformation==4 & !is.na(rst$transformation),"variable"])
  var.sqrt<-as.vector(rst[rst$transformation==5 & !is.na(rst$transformation),"variable"])
  var.drop<-as.vector(rst[rst$transformation==6 & !is.na(rst$transformation),"variable"])
  
  if (length(var.sqrt)>0) env[,var.sqrt]<-sqrt(env[,var.sqrt])
  if (length(var.arc.sqrt)>0) env[,var.arc.sqrt]<-asin(sqrt(env[,var.arc.sqrt]))
  if (length(var.log10)>0) env[,var.log10]<-log10(env[,var.log10])
  if (length(var.log10plus1)>0) env[,var.log10plus1]<-log10(env[,var.log10plus1]+1)
  if (length(var.drop)>0) env<-env[,!colnames(env) %in% var.drop]
      
  if (length(var.sqrt)>0) colnames(env)[colnames(env) %in% var.sqrt]<-paste("sqrt(",var.sqrt,")",sep="")
  if (length(var.arc.sqrt)>0) colnames(env)[colnames(env) %in% var.arc.sqrt]<-paste("arcsine(sqrt(",var.arc.sqrt,"))",sep="")
  if (length(var.log10plus1)>0) colnames(env)[colnames(env) %in% var.log10plus1]<-paste("Log10(",var.log10plus1,"+1)",sep="")
  if (length(var.log10)>0) colnames(env)[colnames(env) %in% var.log10]<-paste("Log10(",var.log10,")",sep="")

  return(env)
}