###Assignment predictive analytics (Group##)
rm(list=ls())

##INSTRUCTIONS:
#1. GO TO LINE 375 to fill in your filenames and dates
#2. AT THE END OF THE FILE THERE IS A SUMMARY OF ALL OUR FUNCTIONS 

##SUMMARY OF FUNCTIONS: 

#MODEL BUILDING
# makes models based on test,val and train datasets 
# returns a list containing:
#  predictions, a data.table with cust id and the probabilty per algorithm
#  a list of the AUCs 

#MODEL DEPLOYMENT 
# lets you test our results with your own training data (that you read from line 387)
# the deployment has the following steps: 
#   1. creates basetable for our data set 
#   2. split up the data in val test train 
#   3. runs models with these data sets 
#   4. creates basetable for your testdata
#   5. run models again with your test data 

#BASETABLEMAKER 
# makes a basetable
# we use this function twice in deployment: once for our data and once for your test data 
# takes 8 arguments:
#  4 tables (races,bills, ...)
#  4 dates (start_ind, start_dep,...)
# returns a basetable (data.table format)

#SPLIT 
# splits data in a TRAIN VAL TEST and TRAINbig set 
# takes in a basetable (made by basetablemaker)
# this function is used only once: for our own data 
# returns a list of data tables (TEST,VAL,...)

#DESCRIPTIVE
# makes plots and other descriptives for data exploration phase 
# does clustering (unsupervised learning)


basetableMaker <- function(races, bills, customers, membercards, start_ind, end_ind, start_dep, end_dep) {
  
  ###########################################################
  # 1.1 START DATA PREPARATION PHASE
  ###########################################################
  
  customers$SUBSCRIPTION_DATE <- as.Date(customers$SUBSCRIPTION_DATE,format=f)
  membercards$CREATIONDATE <- as.Date(membercards$CREATIONDATE,format=f)
  bills$SALESTIME <- as.Date(bills$SALESTIME,format=f)
  bills$PRODUCTPRICE <- as.numeric(bills$PRODUCTPRICE)
  races$DATETIME <- as.Date(races$DATETIME,format=f)
  races$DATETIME <- as.Date(races$DATETIME,format=f)
  customers$BIRTHDAY <- as.Date(customers$BIRTHDAY, format=f)
  
  # get unique customers and members 
  sortedcustomers <- customers[order(SUBSCRIPTION_DATE, decreasing=T)]
  sortedmembercards <- membercards[order(CREATIONDATE, decreasing=T)]
  unCustomers <- sortedcustomers[!duplicated(sortedcustomers$CUSTID),]
  unMembercards <- sortedmembercards[!duplicated(sortedmembercards$CUSTID),]
  
  # Get active racers
  # racers that did race in indep period (2013 - 2014)
  activeRacers <- unique(races[DATETIME <= end_ind & DATETIME >= start_ind, CUSTID]) 
  
  # Filter out inactive racers 
  races <- races[races$CUSTID %in% activeRacers,]
  
  # churn definition
  # people who raced in indep and dep => did not churn 
  # people who did not race in dep period => churned 
  
  # did race happen in indep period
  races$indep <- ifelse((races$DATETIME >= start_dep & races$DATETIME <= end_dep),1,0 )
  table(races$indep)
  
  #aggregate: take sum per customer
  churners <- races[,list(sum(indep)),by='CUSTID']
  colnames(churners)[2] <- "churn"
  
  #if sum = 0 then churn is true, else false
  churners$churn <- as.factor(ifelse(churners$churn==0,1,0))
  
  # create basetable from races
  # remove information outside of independent period 
  racesIndep <- races[DATETIME <= end_ind & DATETIME >= start_ind]
  #frequency: count instances
  racesIndep$recency <- Sys.Date() - racesIndep$DATETIME
  
  frequency <- racesIndep[,list(frequency=length(V1)), by = 'CUSTID']
  recency <- racesIndep[,list(recency=min(recency)), by = 'CUSTID']
  basetableRaces <- merge(frequency, recency, by="CUSTID", all.x=T)
  basetableRaces$recency <- as.integer(basetableRaces$recency)
  
  # create aggregate bills table 
  
  # create monetary sum per customer 
  bills_before_endind <- bills[bills$SALESTIME <= end_ind]
  billsaggregatedsum <- bills_before_endind[,list(TOTAL_SPENDINGS=sum(PRODUCTPRICE)), by = 'CUSTID']
  
  #create mean monetary value per visit 
  billsaggregatedmean <- bills_before_endind[,list(sum(PRODUCTPRICE)), by = list(CUSTID,SALESTIME)]
  billsaggregatedmean <- billsaggregatedmean[,list(AVERAGE_SPENDING=mean(V1)), by = 'CUSTID']
  
  # merge bill metrics 
  basetableBills <- merge(billsaggregatedmean,billsaggregatedsum, by = 'CUSTID')
  
  #Creating the basetable
  basetableCustomers <- merge(unCustomers,unMembercards,by="CUSTID",all.x=T)
  
  # merge races with basetable 
  basetable <- merge(basetableRaces, basetableCustomers, by="CUSTID", all.x=T)
  basetable <- merge(basetable, basetableBills, by="CUSTID", all.x=T)
  basetable <- merge(basetable, churners, by='CUSTID')
  
  # add extra variables of interest to basetable 
  basetable$age <- as.numeric(format(Sys.Date(), '%Y')) - as.numeric(format(basetable$BIRTHDAY ,'%Y'))
  
  #set unwanted variables to NULL 
  basetable$V1.x = basetable$V1.y = basetable$FIRSTNAME <- NULL
  basetable$FBUSERID = basetable$CITY= basetable$BIRTHDAY = basetable$ZIPCODE<- NULL
  basetable$STATEID  <- NULL
  basetable$COUNTRYNAME = basetable$SUBSCRIPTION_DATE = basetable$CREATIONDATE =basetable$ACTV <- NULL
  basetable$ACTIVATED = basetable$MOBILEFAIL = basetable$PARTY3TH <- NULL
  basetable$MAILFAIL <- NULL
  basetable$AGREE_MAIL <- NULL
  
  #handle missing values
  basetable$AVERAGE_SPENDING <- as.vector(sapply(basetable$AVERAGE_SPENDING, function(x) ifelse(is.na(x), 0, x)))
  basetable$TOTAL_SPENDINGS <- as.vector(sapply(basetable$TOTAL_SPENDINGS, function(x) ifelse(is.na(x), 0, x)))
  
  if(require(imputeMissings)==FALSE)  install.packages(imputeMissings); require(imputeMissings)
  basetable <- impute(basetable)
  
  #gender variable has problematic third value 
  genderAnalysis <- table(as.factor(basetable$GENDER))
  mostCommonGender <- as.integer(names(genderAnalysis[genderAnalysis == max(genderAnalysis)]))
  basetable$GENDER <- as.vector(sapply(basetable$GENDER, function(x) ifelse(x == 2, mostCommonGender, x)))
  
  return(basetable)
  ###########################################################
  # 1.1:END DATA PREPARATION PHASE
  ###########################################################
}

split <- function(basetable) {
  ###########################################################
  # 1.3:SPLIT UP IN VAL/TEST/TRAIN
  ###########################################################
  # Set up modeling data-sets:
  #Before starting the analyses, clean up by removing all objects except the Basetable
  #rm(list=setdiff(ls(),"basetable")) #setdiff returns which elements of the first argument are not in the second argument
  #ls()
  
  #create idicators
  #randomize order of indicators
  allind <- sample(x=1:nrow(basetable),size=nrow(basetable))
  
  #split the observations in three parts (TRAIN, VAL, TEST)
  trainind <- allind[1:round(length(allind)*0.6)]
  valind <- allind[(round(length(allind)*0.6)+1):round(length(allind)*0.8)]
  testind <- allind[(round(length(allind)*0.8)+1):length(allind)]
  
  basetableTRAIN <- basetable[trainind,]
  basetableVAL <- basetable[valind,]
  basetableTEST <- basetable[testind,]
  
  #Create a separate response variable
  yTRAIN <<- basetableTRAIN$churn
  basetableTRAIN$churn <- NULL
  basetableTRAIN$CUSTID <- NULL
 
  yVAL <<- basetableVAL$churn
  basetableVAL$churn <- NULL
  basetableVAL$CUSTID <- NULL
  
  yTEST <<- basetableTEST$churn
  basetableTEST$churn <- NULL
  custids <- basetableTEST$CUSTID
  basetableTEST$CUSTID <- NULL
  table(yTRAIN);table(yVAL);table(yTEST)
  
  #if no tuning is required then use TRAIN + VAL as a big training set
  basetableTRAINbig <- rbind(basetableTRAIN,basetableVAL)
  yTRAINbig <- factor(c(as.integer(as.character(yTRAIN)),as.integer(as.character(yVAL))))

  return(list(VAL= basetableVAL, TEST= basetableTEST, TRAIN= basetableTRAIN, TRAINbig= basetableTRAINbig,yVAL=yVAL, yTEST=yTEST, yTRAIN=yTRAIN,yTRAINbig=yTRAINbig, custids=custids))
}

buildModel <- function(basetableVAL, basetableTEST, basetableTRAIN, basetableTRAINbig, yVAL, yTEST, yTRAIN, yTRAINbig, custids) {
  
  ############################
  # 1.MODELBUILDING
  ############################
  
  
  ###########################################################################################
  ### BOOSTING
  ###########################################################################################
  if (require('AUC') == FALSE) install.packages('AUC'); require('AUC')
  ###########################################################################################
  ### RANDOM FORREST
  ###########################################################################################
  
  if (require('randomForest') == FALSE) install.packages('randomForest'); require('randomForest')
  
  #randomForest model
  rF <- randomForest(x=basetableTRAIN,y=yTRAIN, xtest=basetableVAL, ytest=yVAL, ntree=200, importance=TRUE, keep.forest = T)
  predrF <- predict(rF,basetableTEST, type = 'prob')[,2]
  randforestAUC <- auc(roc(predrF, yTEST))
  
  #plot the learning curve 
  plot(rF)
  #red line: class error 0
  #green line: class error 1
  # black line: OOB error
  #we see that class 1 has lower error than class O. This is because there are much more 1s to learn from.
  
  #variable importances
  varImpPlot(rF)
  importance(rF)
  
  #Partial Dependence Plots
  
  if (require('interpretR') == FALSE) install.packages('interpretR'); require('interpretR')
  
  #Tune the number of trees
  
  ##########################################################################################
  ### Hybrid Ensembles
  ##########################################################################################
  
  if (require('hybridEnsemble') == FALSE) install.packages('hybridEnsemble'); require('hybridEnsemble')
  
  hE <-hybridEnsemble(x= basetableTRAIN,
                      y= yTRAIN,
                      combine="GenSA",
                      RF.ntree=50,
                      AB.iter=50,
                      NN.size=5,
                      NN.decay=0,
                      SV.gamma = 2^-15,
                      SV.cost = 2^-5,
                      SV.degree=2,
                      SV.kernel='radial',
                      verbose = TRUE)
  
  predhE <- predict(hE,basetableTEST)
  hEAUC1 <- AUC::auc(roc(predhE$predGENSA, yTEST))
  hEAUC2 <- AUC::auc(roc(predhE$predMEAN, yTEST))
  
  plot(roc(predictions = predhE$predGENSA, labels = yTEST))
  plot(roc(predictions = predhE$predMEAN, labels = yTEST), add = TRUE, col = 2)
  
  importance(x=hE, xdata=basetableTEST, ydata=yTEST)
  
  #AUC
  predictions <- data.table(CUSTID=custids,hybridensemble=predhE, randomforest=predrF)
  return(list(predictions = predictions, list(randfor=randforestAUC, hybridens=hEAUC2)))
}

#OUR DATES
f <- "%Y-%m-%d"
#Set timewindow
start_ind <- as.Date('2013-01-01', f)
end_ind <- as.Date('2014-01-01', f)
start_dep <- as.Date('2014-01-02', f)
end_dep <- as.Date('2015-01-02', f)

#READ IN OUR DATA 
#Reading in the data
if(require(data.table)==FALSE)  install.packages(data.table); require(data.table)
if(require('bit64')==FALSE) install.packages('bit64'); require('bit64')
races <- fread('races.csv')
membercards <- fread('c/membercards.csv')
customers <- fread('c/customers.csv')
bills <- fread('c/bills.csv')

#############################
#THE FOLLOWING 8 VARIABLES SHOUlD BE FILLED IN 
############################
#YOUR DATES
# for example: '2013-17-01'
startTEST_ind <- as.Date('2013-01-01', f)
endTEST_ind <- as.Date('2014-01-01', f)
startTEST_dep <- as.Date('2014-01-02', f)
endTEST_dep <- as.Date('2015-01-02', f)

#YOUR DATA (TESTsample)
racesTEST <- fread('races.csv')
billsTEST <- fread('bills.csv')
memberscardsTEST <- fread('membercards.csv')
customersTEST <- fread('customers.csv')


modelDeployment <- function() {
  #make basetable from our data (kartingcenter 3)
  basetable <<- basetableMaker(races, bills, customers, membercards,start_ind,end_ind,start_dep , end_dep)
  #split up data in test/val/train
  sp <- split(basetable)
  #build our models (adaboost, randforest,...)
  model <- buildModel(sp$VAL, sp$TEST, sp$TRAIN, sp$TRAINbig,  sp$yVAL, sp$yTEST,sp$yTRAIN, sp$yTRAINbig, sp$custids)
  
  #make TEST basetable from your new dataset 
  newTEST <<- basetableMaker(racesTEST,billsTEST,customersTEST, memberscardsTEST,startTEST_ind,endTEST_ind,startTEST_dep , endTEST_dep)
  #make your yTEST 
  newYTEST <- newTEST$churn
  newCustids <- newTEST$CUSTID
  newTEST$CUSTID <- NULL
  newTEST$churn <- NULL
  #build the models again, but now with your data as testset
  newModel <<- buildModel(sp$VAL, newTEST, sp$TRAIN, sp$TRAINbig,  sp$yVAL, newYTEST,sp$yTRAIN, sp$yTRAINbig, newCustids)
  #the model objects contain a data table with individual predictions for the customers + the AUC's of our algorithms
  return(list(ourmodel=model, yourmodel=newModel))
  }

#CODE GETS INVOKEND HERE
OUTPUT <- modelDeployment()