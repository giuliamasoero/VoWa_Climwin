######################################
# Code used to analyse the datasets  #      
######################################
devtools::install_github("LiamDBailey/climwin")
library(doParallel)
library(foreach)
CPUS<-5
registerDoParallel(makeCluster(CPUS)) # Register cluster
   
foreach(v = 1:CPUS) %dopar% {
  library(climwin)
  library(lubridate)
  library(MuMIn)
  library(erer)
  library(ggplot2)
  setwd("D:/Users/MartijnP/OneDrive/Data/HowTo/simulations/2016/finalsimdata") 

  # model parameters
  filename<-"OUTPUT_100_5RAND_SIGNAL_dataset1_K10"
  SAVER<-25
  FURTHEST<-100
  SKIMDURATION<-10 # we explored the usefulness of excluding short windows, duration <10, unless they were more recent than SKIMCUTTOFF, but this was not included in final version of the paper 
  SKIMCUTTOFF<-10 
  replicates<-1000
  repeated<-5
 
  # treatments 
  samplesizes<-c(10,20,30,40,47)  # number of years with data, max 47
  dataset<-rep(1,5)  # 1: R2=0.8, 2: R2=0.40, 3: R2=0.20
  Kfold<-rep(0,5)  # 0=no k-fold cross-validation, 10= 10-fold cross-validation
  RANDOMIZED<-rep(FALSE,5)  # if true, then datasets contained no climate signal 
  SKIM<-rep(FALSE,5) # if TRUE then short distant windows were excluded, in final version of the paper we only explored SKIM==FALSE

  periodTrue<-seq(8,72,by=1)
  tracker<-tracker2<-matrix(ncol = 1, nrow = repeated) 
  z<-length(Biol$Laydate)   # length of max dataset
  
  namelist<-c("NUMBER", "DATASET","RANDOM","KFOLD","SKIMMED","SAMPLESIZE","FURTH","R2", "SLOPE","SLOPE_SE","SIMILAR","DAICc","DDAICcMean","DDAICcMedian","DDAICcMin", "RANDDAICc","DURATION","WEIGHTDISTR","OPEN","CLOSE","COR","PVALUE","MIDPOINT","DISTRIBUTION", "DDAICcVAR", "WVAR", "Wmean", "Wmin", "Wmedian")
  OUTPUTALL  <-as.data.frame(matrix(ncol = length(namelist), nrow = replicates) )
  colnames(OUTPUTALL)<-namelist

  Clim<-read.table(file="D:\\Users\\MartijnP\\OneDrive\\Data\\HowTo\\simulations\\2016\\ChaffinchClim.txt", sep="\t", header=T)  
  if(dataset[v]==1) {Biol<-read.table(file="D:\\Users\\MartijnP\\OneDrive\\Data\\HowTo\\simulations\\2016\\finalsimdata\\SimChaffinchBio_R2_0.8_sigma0.txt", sep="\t", header=T)}
  if(dataset[v]==2) {Biol<-read.table(file="D:\\Users\\MartijnP\\OneDrive\\Data\\HowTo\\simulations\\2016\\finalsimdata\\SimChaffinchBio_R2_0.4_sigma2.txt", sep="\t", header=T)}
  if(dataset[v]==3) {Biol<-read.table(file="D:\\Users\\MartijnP\\OneDrive\\Data\\HowTo\\simulations\\2016\\finalsimdata\\SimChaffinchBio_R2_0.2_sigma4.txt", sep="\t", header=T)}

  # run model for XvarTrue and expected values
  result <- climatewin(xvar = list(Clim$Temp), cdate = Clim$Date, bdate = Biol$Date,
                       baseline = lm(Biol$Laydate ~ 1),
                       range=c(100, 0),
                       stat = "mean", func = "lin",
                       type="absolute", refday=c(20,5),
                       cinterval = "day")  
  Biol$XVarTrue<-result[[1]]$BestModelData$climate
  slopeTrue<-summary(result[[1]]$BestModel)$coef[2]
  slopeSETrue<-summary(result[[1]]$BestModel)$coef[4]
  tTrue<-summary(result[[1]]$BestModel)$coef[6]
  R2True<-summary(result[[1]]$BestModel)$r.squared
  ifelse(RANDOMIZED[v]==TRUE, BiolSub <- Biol[,-(4:1003),drop=FALSE], BiolSub <- Biol[,-(1004:2003),drop=FALSE])
  if(v==1) {BiolSub <- BiolSub[,-(2004:5003),drop=FALSE]}  # sample size is 10
  if(v==2) {                                               # sample size is 20
    BiolSub <- BiolSub[,-(3004:5003),drop=FALSE]
    BiolSub <- BiolSub[,-(1004:2003),drop=FALSE]
    }   
  if(v==3) {                                               # sample size is 30
    BiolSub <- BiolSub[,-(4004:5003),drop=FALSE]
    BiolSub <- BiolSub[,-(1004:3003),drop=FALSE]
  }      
  if(v==4) {BiolSub <- BiolSub[,-(1004:4003),drop=FALSE]}  # sample size is 40
  if(v==5) {BiolSub <- BiolSub[,-(2004:5003),drop=FALSE]   # sample size is 47
            BiolSub[,(1004:2003)]<-1
  }  
  for (i in 1:replicates) {
     datacounter<-i+3
     ifelse(v<5, DataUsed<- BiolSub[-(which(BiolSub[,i+1003]==0)),,drop=FALSE], DataUsed<- BiolSub)
     DataUsed<-DataUsed[c(1,2,3,datacounter,2004)]
     if(SKIM[v]==FALSE) { 
       cutoff<-duration<-0
     } else {
       cutoff<-SKIMCUTTOFF
       duration<-SKIMDURATION 
      }
      result <- climatewin(xvar = list(Clim$Temp), cdate = Clim$Date, bdate = DataUsed$Date,
                          baseline = lm(DataUsed[,4] ~ 1),
                          range=c(FURTHEST, 0),
                          stat = "mean", func = "lin",
                          type="absolute", refday=c(20,5),
                          cinterval = "day", 
                          exclude = c(cutoff, duration), 
                          k=Kfold[v])
     randresult<-randwin(xvar = list(Clim$Temp), cdate = Clim$Date, bdate = DataUsed$Date,
                              baseline = lm(DataUsed[,4] ~ 1),
                              range=c(FURTHEST, 0),
                              stat = "mean", func = "lin",
                              type="absolute", refday=c(20,5),
                              cinterval = "day", 
                              exclude = c(cutoff, duration), 
                              k=Kfold[v], repeats=repeated) 
       result[[1]]$Dataset$DistOpen<-result[[1]]$Dataset$WindowOpen-result[[1]]$Dataset$WindowOpen[1]
       result[[1]]$Dataset$DistClose<-result[[1]]$Dataset$WindowClose-result[[1]]$Dataset$WindowClose[1]
       result[[1]]$Dataset$cw <- as.numeric(cumsum(result[[1]]$Dataset$ModWeight) <= 0.95)
       setcw  <- subset(result[[1]]$Dataset , result[[1]]$Dataset$cw == 1)
       OUTPUTALL$NUMBER[i]<-i
       OUTPUTALL$DISTRIBUTION[i]<-mean(sqrt(setcw$DistOpen^2+setcw$DistClose^2))/mean(sqrt(result[[1]]$Dataset$DistOpen^2+result[[1]]$Dataset$DistClose^2))
       OUTPUTALL$CLOSE[i]<-result[[1]]$Dataset$WindowClose[1]
       OUTPUTALL$OPEN[i]<-result[[1]]$Dataset$WindowOpen[1]
       OUTPUTALL$DAICc[i]<-result[[1]]$Dataset$deltaAICc[1]   
       OUTPUTALL$SLOPE[i]<-summary(result[[1]]$BestModel)$coef[2]
       OUTPUTALL$SLOPE_SE[i]<-summary(result[[1]]$BestModel)$coef[4]
       OUTPUTALL$DURATION[i]<-OUTPUTALL$OPEN[i]-OUTPUTALL$CLOSE[i]+1
       OUTPUTALL$MIDPOINT[i]<-mean(OUTPUTALL$OPEN[i],OUTPUTALL$CLOSE[i])
       period<-seq(OUTPUTALL$CLOSE[i],OUTPUTALL$OPEN[i],by=1)
       OUTPUTALL$SIMILAR[i]<-length(intersect(period,periodTrue))/(length(intersect(period,periodTrue))+length(setdiff(period,periodTrue))+length(setdiff(periodTrue,period)))
       OUTPUTALL$R2[i]<-summary(result[[1]]$BestModel)$r.squared
       OUTPUTALL$WEIGHTDISTR[i] <- mean(as.numeric(cumsum(result[[1]]$Dataset$ModWeight) <= 0.95))
       OUTPUTALL$COR[i]<-cor(DataUsed$XVarTrue,result[[1]]$BestModelData$climate)
       OUTPUTALL$PVALUE[i]<-anova(result[[1]]$BestModel)$'Pr(>F)'[1]      
       OUTPUTALL$DATASET[i]<-  dataset[v]
       OUTPUTALL$RANDOM[i]<-  RANDOMIZED[v]
       OUTPUTALL$KFOLD[i]<-  Kfold[v]
       OUTPUTALL$SKIMMED[i]<-SKIM[v]
       OUTPUTALL$SAMPLESIZE[i]<-samplesizes[v]
       OUTPUTALL$FURTH[i]<-FURTHEST
       tracker<-tracker2<-tracker*NA
       for(m in 1:repeated) {
         repset<-subset(randresult[[1]],randresult[[1]]$Repeat==m)
         tracker[m]<-min(repset$deltaAICc)
         tracker2[m]<-mean(as.numeric(cumsum(repset$ModWeight) <= 0.95))
       }
       OUTPUTALL$DDAICcMean[i]<-OUTPUTALL$DAICc[i]-mean(tracker)
       OUTPUTALL$DDAICcMedian[i]<-OUTPUTALL$DAICc[i]-median(tracker)
       OUTPUTALL$DDAICcMin[i]<-  OUTPUTALL$DAICc[i]-min(tracker)
       OUTPUTALL$RANDDAICc[i]<-mean(tracker)
       OUTPUTALL$DDAICcVAR[i]<-var(tracker)
       OUTPUTALL$WVAR[i]<-var(tracker2)
       OUTPUTALL$Wmean[i]<-OUTPUTALL$WEIGHTDISTR[i]-mean(tracker2)
       OUTPUTALL$Wmedian[i]<-OUTPUTALL$WEIGHTDISTR[i]-median(tracker2)
       OUTPUTALL$Wmin[i]<-OUTPUTALL$WEIGHTDISTR[i]-min(tracker2)
       #OUTPUTALL$rmse[i]<-summary(result[[1]]$BestModel)$sigma
     if (i%%SAVER==0 ) { write.table(OUTPUTALL, file=paste(filename, v), row.names = FALSE) }
  }
}
stopImplicitCluster()
 
   
 
######################################
# Code used to generate the datasets #      
######################################
SKIP<-TRUE
if(SKIP==FALSE) {
  #https://bioinf.nioo.knaw.nl/rstudio/
  install.packages("lubridate", dependencies=TRUE)
  install.packages("MuMIn", dependencies=TRUE)
  install.packages("erer", dependencies=TRUE)
  install.packages("foreach", dependencies=TRUE)
  install.packages("doParallel", dependencies=TRUE)
  install.packages("stringi", dependencies=TRUE)
  install.packages("magrittr", dependencies=TRUE)
  install.packages("colorspace", dependencies=TRUE)
  install.packages("ggplot2", dependencies=TRUE)
  
  devtools::install_github("LiamDBailey/climwin")
  ###############################################
  # generate the datasets to draw from!          #
  ###############################################
  library(climwin)
  library(lubridate)
  library(MuMIn)
  library(erer)
  library(ggplot2)
  
  # use Chaffinch data to generate new dataset with similar time window and slope, but smaller R2 and sample size N!
  Biol<-read.table(file="C:\\Users\\u4620427\\SkyDrive\\Data\\HowTo\\simulations\\2016\\ChaffinchBio.txt", sep="\t", header=T)  
  Clim<-read.table(file="C:\\Users\\u4620427\\SkyDrive\\Data\\HowTo\\simulations\\2016\\ChaffinchClim.txt", sep="\t", header=T)  
  NewBiolSize<-1000 
  replicates<-1000
  set.seed(126)  #123
  Nmax<-length(Biol$Year)
  
  v<-3 1: R2=0.8, 2: R2=0.40, 3: R2=0.20 
  
  sigma_scalar<-c(0.62,2.20,3.70)  # these values were derived by trial and error
  R2Aim<-c(0.80,0.40,0.20) # expected value for R2 that we would like to have in simulated data
  CloseAim<-8  # expected value for close that we would like to have in all simulated data
  OpenAim<-72 # expected value for open that we would like to have in all simulated data
  
  Result <- climatewin(xvar = list(Clim$Temp), cdate = Clim$Date, bdate = Biol$Date,
                       baseline = lm(Biol$Laydate ~ 1),
                       range=c(100, 0),
                       stat = "mean", func = "lin",
                       type="absolute", refday=c(20,5),
                       cinterval = "day")  
  interceptTrue<-summary(Result[[1]]$BestModel)$coef[1]
  slopeTrue<-summary(Result[[1]]$BestModel)$coef[2]
  sigmaTrue<-summary(Result[[1]]$BestModel)$sigma  # error sd 
  
  # generate a large dataset to draw from
  for (i in 1:NewBiolSize) {  ifelse(i==1, NewBiol<-Biol, NewBiol<-rbind (NewBiol,Biol)) }
  # simulate the Laydate with the desired R2 and close and open
  counter<-0
  repeat{
    counter<-counter+1 
    NewBiol$Laydate_new<-NewBiol$Laydate+rnorm(length(NewBiol$Laydate),0,sd=2.487*sigma_scalar[v])
    NewResult <- climatewin(xvar = list(Clim$Temp), cdate = Clim$Date, bdate = NewBiol$Date,
                            baseline = lm(NewBiol$Laydate_new ~ 1),
                            range=c(100, 0),
                            stat = "mean", func = "lin",
                            type="absolute", refday=c(20,5),
                            cinterval = "day")   
    R2True<-summary(NewResult[[1]]$BestModel)$r.squared
    slopeTrue<-summary(NewResult[[1]]$BestModel)$coef[2]
    CloseTrue<-NewResult[[1]]$Dataset$WindowClose[1]  # original value 8
    OpenTrue<-NewResult[[1]]$Dataset$WindowOpen[1]    # original value 72
    cat(R2True,, "\n")
    cat(slopeTrue, "\n")
    cat(OpenTrue, "\n")
    cat(CloseTrue, "\n")
    if(round(R2True, digits=2)==R2Aim[v] & CloseTrue==CloseAim & OpenTrue==OpenAim  ) { break }
  }
  print(c(R2True, slopeTrue, OpenTrue, CloseTrue, counter))
  
  #save the dataset in a more convenient format!
  # first the simulated laydates matrix
  SimBiol<-matrix(data = NA, nrow = Nmax, ncol = replicates, byrow = FALSE, dimnames = NULL) 
  RandBiol<-matrix(data = NA, nrow = Nmax, ncol = replicates, byrow = FALSE, dimnames = NULL) 
  # then a matrix stating which years to use in each sampling for lower sample size simualtions (1=select)
  Draw40<-Draw30<-Draw20<-Draw10<-matrix(data = 0, nrow = Nmax, ncol = replicates, byrow = FALSE, dimnames = NULL) 
  
  
  NewBiol$Laydate_rand<-sample(NewBiol$Laydate_new)
  records<-seq(1,47,1)
  years<-seq(1966,2012,1)
  for(i in 1:replicates) {
    a10<-sample(records,10)
    a20<-sample(records,20)
    a30<-sample(records,30)
    a40<-sample(records,40)
    Draw10[a10,i]<-1
    Draw20[a20,i]<-1
    Draw30[a30,i]<-1
    Draw40[a40,i]<-1
    for(j in 1:length(records)) {
      SimBiol[j,i]<-NewBiol$Laydate_new[sample(which(NewBiol$Year==years[j]),1)]
      RandBiol[j,i]<-NewBiol$Laydate_rand[sample(which(NewBiol$Year==years[j]),1)]
    }
  }
  SimulBiol<-cbind(Biol,SimBiol, RandBiol, Draw10, Draw20, Draw30, Draw40)  
  
  if(v==1) { 
    write.table(SimulBiol, file="C:\\Users\\u4620427\\SkyDrive\\Data\\HowTo\\simulations\\2016\\SimChaffinchBio_R2_0.8_sigma0.txt", sep="\t", row.names=T) 
  }
  
  if(v==2) { 
    write.table(SimulBiol, file="C:\\Users\\u4620427\\SkyDrive\\Data\\HowTo\\simulations\\2016\\SimChaffinchBio_R2_0.4_sigma2.txt", sep="\t", row.names=T) 
  }
  if(v==3) { 
    write.table(SimulBiol, file="C:\\Users\\u4620427\\SkyDrive\\Data\\HowTo\\simulations\\2016\\SimChaffinchBio_R2_0.2_sigma4.txt", sep="\t", row.names=T) 
  }
  print(c(R2True, slopeTrue, OpenTrue, CloseTrue, counter))
}   
