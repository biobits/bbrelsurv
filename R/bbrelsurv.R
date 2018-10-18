

##########################################################################################################################################################################################################
##Funktion für die Darstellung des relativen 1-5-Jahresüberlebn nach Gruppe
##########################################################################################################################################################################################################
#' R Function to plot the relative 1-5 years survival by a given group
#'
#' accepts a list of vectors of identical length and returns one vector with the first non-NA value
#'
#' @param x dataframe with survival data
#' @param gruppe the factor to be grouped by
#' @param ylab label of y axis
#' @param xlab label of x axis
#' @param title title of plot
#' @param jahrstart year to start the intervall
#' @param jahrende year of end of intervall
#'
#' @import periodR
#'
#' @return a base plot
#'
#' @author Stefan Bartels, \email{email@biobits.eu}
#'
#' @examples
#' relsurvplot<-bbRelSurvPlot(x,gruppe=NULL,ylab=NULL,xlab=NULL,titel=NULL,jahrstart=NULL,jahrende=NULL)
#'
#'@export
bbRelSurvPlot<- function(x,gruppe=NULL,ylab=NULL,xlab=NULL,titel=NULL,jahrstart=NULL,jahrende=NULL)
{


  j_start<-1997
  j_end<-2012
  if (is.null(jahrstart)==FALSE){j_start<-jahrstart}
  if (is.null(jahrende)==FALSE){j_end<-jahrende}

  #probs.male<-read.csv2(paste(c(GetRDirPath(),"/Data/PeriodensterbetafelD_M.csv"),collapse=""),header = TRUE, sep = ";",dec=",", quote="\"")
  #probs.female<-read.csv2(paste(c("C:/DATA/svn/r-skripte/Data/PeriodensterbetafelD_W.csv"),collapse=""),header = TRUE, sep = ";",dec=",", quote="\"")
  data("probsmale")
  names.male<-gsub("X","",names(probs.male))
  names(probs.male)<-names.male
 # probs.female<-read.csv2(paste(c(GetRDirPath(),"/Data/PeriodensterbetafelD_W.csv"),collapse=""),header = TRUE, sep = ";",dec=",", quote="\"")
  data("probsfemale")
  names.female<-gsub("X","",names(probs.female))
  names(probs.female)<-names.female
  survdata<-new.df(c("group","diagyear","fu1","fu2","fu3","fu4","fu5","obs"))
  for (gr in levels(x[,gruppe]))
  {

    subdata<-bbhelper::drop.levels(subset(x,x[,gruppe]==gr))
    data.year=levels(as.factor(subdata$dy))
    period.result <- period(subdata, 5,
                            probs.male, probs.female, j_start, j_end,
                            method="hakulinen")
    period.result
    rel<-period.result["rel.surv"]
    #fürs debugging
    # print(obs=obs$observations[1])
    #debugging ende
    obs<-period.result["observations"]
    rbind(survdata,data.frame(group=gr,diagyear=gr,fu1=rel$rel.surv[1],fu2=rel$rel.surv[2],
                              fu3=rel$rel.surv[3],fu4=rel$rel.surv[4],fu5=rel$rel.surv[5],obs=obs$observations[1]))->survdata
  }

  survdata[3:7][survdata[3:7]==0]<-NA
  survdata[3:7][survdata[3:7]>100]<-100
  survdata
  obs.dat<-t(survdata[c(8)])
  yhights<-max(obs.dat)*2


  cust.col<-c("gray75","darkblue","darkred","orange","darkgreen","black")


  op<-par(mar=c(5, 4, 4, 12) + 0.1,xpd=TRUE)
  mp<-barplot(obs.dat,yaxt="n",ylim=c(0,yhights),names.arg=survdata[[1]],col=cust.col[1])
  text(mp, obs.dat, labels = obs.dat, pos = 3,cex=0.75)

  axis(4)

  mtext("no. of observations", side=4, line=2, cex=0.8,las=3)
  op<-par(new=TRUE)
  #plot.default(survdata$group, survdata$fu1,ylim=c(0,100),col=2,type="n")
  plot.default(survdata$group, survdata$fu1,ylim=c(0,100),col="white",xaxt="n",xlab=xlab,ylab=ylab,main=titel,cex.main=0.8)
  lines(survdata$group, survdata$fu1,type="l",col=cust.col[2])
  lines(survdata$group, survdata$fu2,type="l",col=cust.col[3])
  lines(survdata$group, survdata$fu3,type="l",col=cust.col[4])
  lines(survdata$group, survdata$fu4,type="l",col=cust.col[5])
  lines(survdata$group, survdata$fu5,type="l",col=cust.col[6])
  tmp.u <- par('usr')
  #leg.pos<-list(x=tmp.u[2]+2, y=tmp.u[4], xjust=0, yjust=0,outer=TRUE)
  leg.pos<-list(x=tmp.u[2]*1.15, y=tmp.u[4], outer=TRUE)
  legend(leg.pos, c(paste("Observations\n( n =",sum(obs.dat),")"),"1 year","2 years","3 years", "4 years", "5 years"), col = cust.col,
         lty = c(1),lwd=c(10,1,1,1,1,1),bty="n",cex=0.8,merge = TRUE)
  par(op)

}

##########################################################################################################################################################################################################
##Funktion fuer die Darstellung des relativen 1-5-Jahres?berlebn als Kohortenanalyse (Kohorte = Gruppen der Diagnosehjahre)
##########################################################################################################################################################################################################
#' Function to generate a Plot of 1-5 yera relative survival for different cohortes
#'
#'
#' @param x survival data
#' @param ylab label of y axis
#' @param xlab label of x axis
#' @param title  title of plot
#' @param jahrstart year bto start the intervall
#' @param jahrende year to en the intervall
#' @param jahrintervall stepsize of the intervall to be displayed default is one year
#' @param method  to calculate relative survival default ='edererII'
#'
#' @return a plot
#'
#' @import periodR
#'
#' @author Stefan Bartels, \email{email@biobits.eu}
#'
#' @examples
#' relsurvcoplot<-bbRelSurvCohortPlot(x,ylab=NULL,xlab=NULL,titel=NULL,jahrstart=NULL,jahrende=NULL,jahrintervall=1,method='edererII', lang='de')
#'
#'@export
bbRelSurvCohortPlot<- function(x,ylab=NULL,xlab=NULL,titel=NULL,jahrstart=NULL,jahrende=NULL,jahrintervall=1,method='edererII', lang='de')
{


  j_start<-2003
  j_end<-2012
  if (is.null(jahrstart)==FALSE){j_start<-jahrstart}
  if (is.null(jahrende)==FALSE){j_end<-jahrende}



  probs.male<-read.csv2(paste(c(GetDataPath(),"/PeriodensterbetafelD_M.csv"),collapse=""),header = TRUE, sep = ";",dec=",", quote="\"")
  names.male<-gsub("X","",names(probs.male))
  names(probs.male)<-names.male
  probs.female<-read.csv2(paste(c(GetDataPath(),"/PeriodensterbetafelD_W.csv"),collapse=""),header = TRUE, sep = ";",dec=",", quote="\"")
  names.female<-gsub("X","",names(probs.female))
  names(probs.female)<-names.female
  survdata<-new.df(c("group","diagyear","fu1","fu2","fu3","fu4","fu5","obs"))
  jseq<-seq(j_start,j_end)
  counter=0
  for (j in 1:floor(length(jseq)/(jahrintervall+1)))
  {
    if(counter==0){counter=counter+1}
    lower_j<-jseq[counter]
    upper_j<-jseq[counter+jahrintervall]
    subdata<-bbhelper::drop.levels(subset(x,dy>=lower_j & dy<=upper_j))
    data.year=levels(as.factor(subdata$dy))
    period.result <- period(subdata, 5,
                            probs.male, probs.female, j_start, j_end,
                            method=method)
    period.result
    rel<-period.result["rel.surv"]

    obs<-period.result["observations"]
    rbind(survdata,data.frame(group=if(jahrintervall==0){lower_j}else{paste(lower_j,"-",upper_j)},
                              diagyear=paste(lower_j,"-",upper_j),
                              fu1=if(lower_j>=j_end){NA}else{rel$rel.surv[1]},
                              fu2=if(lower_j+1>=j_end){NA}else{rel$rel.surv[2]},#rel$rel.surv[2],
                              fu3=if(lower_j+2>=j_end){NA}else{rel$rel.surv[3]},
                              fu4=if(lower_j+3>=j_end){NA}else{rel$rel.surv[4]},
                              fu5=if(lower_j+4>=j_end){NA}else{rel$rel.surv[5]},
                              obs=obs$observations[1]))->survdata
    counter=counter+jahrintervall+1
    print(period.result)
  }

  survdata[3:7][survdata[3:7]==0]<-NA
  #survdata[3:7][survdata[3:7]>100]<-100
  print(survdata)
  obs.dat<-t(survdata[c(8)])
  yhights<-max(obs.dat)*2


  cust.col<-c("gray75","darkblue","darkred","orange","darkgreen","black")


  op<-par(mar=c(5, 4, 4, 12) + 0.1,xpd=TRUE)
  mp<-barplot(obs.dat,yaxt="n",ylim=c(0,yhights),names.arg=survdata[[1]],col=cust.col[1],cex.names=0.75)
  text(mp, obs.dat, labels = obs.dat, pos = 3,cex=0.75)

  axis(4)

  mtext("no. of observations", side=4, line=2, cex=0.8,las=3)
  op<-par(new=TRUE)
  #plot.default(survdata$group, survdata$fu1,ylim=c(0,100),col=2,type="n")
  plot.default(survdata$group, survdata$fu1,ylim=c(0,100),col="white",xaxt="n",xlab=xlab,ylab=ylab,main=titel,cex.main=0.8)
  lines(survdata$group, survdata$fu1,type="l",col=cust.col[2])
  lines(survdata$group, survdata$fu2,type="l",col=cust.col[3])
  lines(survdata$group, survdata$fu3,type="l",col=cust.col[4])
  lines(survdata$group, survdata$fu4,type="l",col=cust.col[5])
  lines(survdata$group, survdata$fu5,type="l",col=cust.col[6])
  tmp.u <- par('usr')
  #leg.pos<-list(x=tmp.u[2]+2, y=tmp.u[4], xjust=0, yjust=0,outer=TRUE)
  leg.pos<-list(x=tmp.u[2]*1.15, y=tmp.u[4], outer=TRUE)
  switch(lang,
         en=legend(leg.pos, c(paste("Observations\n( n =",sum(obs.dat),")"),"1 year","2 years","3 years", "4 years", "5 years"), col = cust.col,lty = c(1),lwd=c(10,1,1,1,1,1),bty="n",cex=0.8,merge = TRUE)
         ,de=legend(leg.pos, c(paste("Observationen\n( n =",sum(obs.dat),")"),"1 Jahr","2 Jahre","3 Jahre", "4 Jahre", "5 Jahre"), col = cust.col,lty = c(1),lwd=c(10,1,1,1,1,1),bty="n",cex=0.8,merge = TRUE)
  )
  par(op)

}

