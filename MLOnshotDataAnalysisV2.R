### Machine Learning in One-shot device 
#setwd("G:/My Drive/Research/MLInOneShot")
setwd("C:/Users/Hon Yiu So/My Drive (hso@oakland.edu)/Research/MLInOneShot")
### data  modification 

### should be based on each vehicle level merged with case number 

### data manual: 
### data manupulation  
data.PERSON.list<-data.VEHICLE.list<-data.VEVENT.list<-list(  )
for( i in 1:5){
data.PERSON.list[[i]] <- read.csv(paste("URCResearchFunding/DataSource/CRSS/DataExtracted/", 2015+i,"/PERSON.CSV",sep="" ))
data.VEHICLE.list[[i]]<- read.csv(paste("URCResearchFunding/DataSource/CRSS/DataExtracted/", 2015+i,"/VEHICLE.CSV",sep=""))
data.VEVENT.list[[i]] <- read.csv(paste("URCResearchFunding/DataSource/CRSS/DataExtracted/", 2015+i,"/VEVENT.CSV",sep="" ))
}


####  CASENUM, VEH_NO, and PER_NO are the unique identifiers for each record.

# head(data.PERSON.list[[1]]$CASENUM) 
# head(data.PERSON.list[[1]]$VEH_NO)
# 
# head(data.VEHICLE.list[[1]]$CASENUM) 
# head(data.VEHICLE.list[[1]]$VEH_NO)
# 
# head(data.VEVENT.list[[1]]$CASENUM) 
# head(data.VEVENT.list[[1]]$VEH_NO)




###### FARS  as census It cannot match with  CRSS
# FARS.PERSON.list<-FARS.VEHICLE.list<-list()
# FARS.PERSON.list[[1]] <-  readr::read_csv(unzip(paste(
#   "G:/My Drive/Research/MLInOneShot/URCResearchFunding/DataSource/FARS/FARS",
#   2016,"NationalCSV.zip",sep=""), "Person.CSV" )  )


## Target variables 



## 2017-2020
# 01 Deployed-Front 
# 02 Deployed-Side (door, seatback) 
# 03 Deployed-Curtain (roof)
# 07 Deployed-Other (knee, airbelt, etc.) 
# 08 Deployed-Combination 
# 09 Deployment-Unknown Location
# 20 Not Deployed    
# 98 Not Reported # 99 Reported as Deployment Unknown (missing data)

# 2016 https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/812449


# head(data.VEHICLE.list[[1]]$TOWED) 


#table(data.PERSON.list[[1]]$AIR_BAG)
# 0 No Apparent Injury (O)
# 1 Possible Injury (C) 
# 2 Suspected Minor Injury (B)
# 3 Suspected Serious Injury (A) # 4 Fatal Injury (K)   (target)
# 5 Injured, Severity Unknown  # 6 Died Prior to Crash* # 9 Unknown/Not Reported (missing)
#head(data.PERSON.list[[1]]$INJ_SEV) 


#V36 - Vehicle Removal
# 2 Towed Due to Disabling Damage
# 3 Towed But Not Due to Disabling Damage
# 7 Towed, Unknown Reason
# 5 Not Towed
# (missing) 8 Not Reported # 9 Reported as Unknown  
#head(data.VEHICLE.list[[1]]$TOWED) 

## Areas of Impact (this vehicle)
# 00 Non-Collision
# 01-12 Clock Points  ( 6 is rear, 45 left, 12 is front )
# 13 Top
# 14 Undercarriage
# 61 Left
# 62 Left-Front Side
# 63 Left-Back Side
# 81 Right
# 82 Right-Front Side
# 83 Right-Back Side
# 18 Cargo/Vehicle Parts Set-In-Motion
# 19 Other Objects Set-In-Motion
# 98 Not Reported
# 99 Unknown
## 62 82  9-12 1-3 ( 81, 61 treated as missing) , rest may not trigger airbag
#table(data.VEVENT.list[[1]]$AOI1)


## FILTER   FOCUS ON OCCUPANT < 10

#table(data.VEHICLE.list[[1]]$BODY_TYP) 





#### create  air bag success and failure 
### serious injury by case and vehicle 
library( dplyr)
library(survey)  ## define the survey design at the same time 
## Target variables 


# 2016 https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/812449
i<-1 
#missing.vec <-  which( data.PERSON.list[[i]]$INJ_SEV %in% c(5,6))
#data.PERSON.list[[i]]$INJ_SEV[missing.vec]<- NA

data.Merge.list <- svy.dgn<-list()

### survey weight: Weight
### STRATA PSUSTRAT; cLUSTER PSU_VAR; 
### VARMETHOD=JK;
### PROC SURVEYFREQ DATA=IMPUTED.ACCIDENT VARMETHOD=JK; 
# STRATA PSUSTRAT; 
# CLUSTER PSU_VAR; 
# TABLES MAXSEV_IM; 
# WEIGHT WEIGHT; 
# RUN


### car make origins 
library(readxl)
NCSAMake <- read_excel("NCSAMake.xlsx")[,1:4]
names(NCSAMake)<- c( "Codes",      "Attributes" , "MAKE_Country" ,   "MAKE_Origin"  )

for( i in 1:5){
Test<- data.PERSON.list[[i]]%>% group_by( CASENUM,VEH_NO) %>%
      summarise( sev.inj = sum(INJ_SEV %in% c(3,4)), 
                 air_bag_deploy = sum( 1- AIR_BAG %in%  c(0,97,98,99)),
             air_bag_not_deploy = sum( AIR_BAG ==0),
                  air_bag_NA    = sum( AIR_BAG %in%  c(97,98,99)),
                  DR.Age        = sum( AGE* (PER_TYP==1)   ) ,          
                 .groups = "keep")

Test$DR.Age[which(Test$DR.Age==0 |Test$DR.Age>=119)]<-NA
##Vehicle event 
VEVENT <- data.VEVENT.list[[i]]%>% group_by( CASENUM,VEH_NO) %>%
  summarise( AOI.VEVENT = max (!(AOI1 %in% c(0, 6,13,14,63,83,18,19,98,99))),   .groups = "keep") 

data.Merge.list[[i]]<-  (data.VEHICLE.list[[i]]) %>% 
                        left_join( Test, by =c("CASENUM"="CASENUM","VEH_NO"="VEH_NO"))%>%
                        left_join(VEVENT,by =c("CASENUM"="CASENUM","VEH_NO"="VEH_NO")) 

## print and see if there is any duplicate
print( table( duplicated( data.Merge.list[[i]][,c("CASENUM","VEH_NO")] ))) 
### 



### conditions that Airbag should deploy
data.Merge.list[[i]]$AirBag_Should<- with(data.Merge.list[[i]], ( sev.inj |AOI.VEVENT ) | 
                                          TOWED  == 2 )
## the deploy indicator reconstructed 
data.Merge.list[[i]]$AirBag_Deploy <- with(data.Merge.list[[i]], air_bag_deploy>0)
data.Merge.list[[i]]$AirBag_Deploy[which( data.Merge.list[[i]]$air_bag_NA!=0)]<-NA
                                            
## definition of  Air bag success 
#data.Merge.list[[i]]$AirBag_Failure <- with(data.Merge.list[[i]], ( sev.inj &  AirBag_Should) )
data.Merge.list[[i]]$AirBag_Success <- with(data.Merge.list[[i]], 
                          ( AirBag_Deploy &  AirBag_Should)|( !AirBag_Deploy &  !AirBag_Should) )




data.Merge.list[[i]]$Car_Age_Accident <-  2015 +i - (data.Merge.list[[i]]$MOD_YEAR)+1
data.Merge.list[[i]]$Car_Age_Accident[which(data.Merge.list[[i]]$Car_Age_Accident<0)]<-NA

data.Merge.list[[i]]  <- data.Merge.list[[i]] %>% left_join(  NCSAMake[,-2], by =c("MAKE"="Codes") )

# data.Merge.list[[i]]$Car_Make_Country  <-  
# data.Merge.list[[i]]$Car_Make_Orig     <- 
}

#### Covariates
# Accident REGION
# 1 Northeast (PA, NJ, NY, NH, VT, RI, MA, ME, CT)
# 2 Midwest (OH, IN, IL, MI, WI, MN, ND, SD, NE, IA, MO, KS)
# 3 South (MD, DE, DC, WV, VA, KY, TN, NC, SC, GA, FL, AL, MS, LA, AR, OK, TX)
# 4 West (MT, ID, WA, OR, CA, NV, NM, AZ, UT, CO, WY, AK, HI)
# URBANICITY  1 Urban  2 Rural


### proportion of failure 
Fail.Prop.Hist<- array(NA, c(5,22))

for( i in 1:5){
  data.Merge.list[[i]]$AirBag_Success1 <- data.Merge.list[[i]]$AirBag_Success
  data.Merge.list[[i]]$AirBag_Success1[ which(data.Merge.list[[i]]$AirBag_Success==0 & data.Merge.list[[i]]$AirBag_Failure==0)]<-NA

  Fail.Prop <- data.Merge.list[[i]] %>%    group_by(Car_Age_Accident) %>%
    dplyr::summarize(Fail.Prop = 1-mean(AirBag_Success, na.rm=TRUE))  
  Fail.Prop.Hist[i, ]<-unlist(Fail.Prop[1:22,2])
  if (i==1) {plot ( x=  2015+i -0:21, 
                    y= 1-unlist(Fail.Prop[1:22,2]),
                    xlab = "Manufacturing Year", 
                    ylab = "Success Rate", 
                    xlim= c(1995,2020),
                    ylim = c(0.8,1), 
                    type="l", 
                    col=0)
  }else{ lines( x=  2015+i -0:21, 
                y= 1-unlist(Fail.Prop[1:22,2]),
                col=1,lty=i-1)
    
  }
 # print(  t(as.matrix (Fail.Prop)) ) 
} 
title("Air Bag Success Rate by Manufacturing Years")
legend("bottomright", legend = c(2017:2020), col=1,lty =1:4, ncol = 2, title="Accident Year")
print( Fail.Prop.Hist)

## considering weights. 
Fail.Svy.Hist<- array(NA, c(5,22))

for( i in 1:5){
# data.Merge.list[[i]]$AirBag_Success1 <- data.Merge.list[[i]]$AirBag_Success
# data.Merge.list[[i]]$AirBag_Success1[ which(data.Merge.list[[i]]$AirBag_Success==0 & data.Merge.list[[i]]$AirBag_Failure==0)]<-NA
svy.dgn[[i]] <- svydesign(ids= ~ PSU+CASENUM, strata =  ~STRATUM, weights= ~WEIGHT , nest = TRUE,  data= data.Merge.list[[i]]  )
Fail.Svy.Hist[i,]<-svyby( formula= ~ AirBag_Success, by= ~ Car_Age_Accident, design= svy.dgn[[i]],  FUN=svymean, na.rm=TRUE)[1:22,2]
if (i==1) {plot ( x=  2015+i -0:21, 
                 y= svyby( formula= ~ AirBag_Success, by= ~ Car_Age_Accident, design= svy.dgn[[i]],  FUN=svymean, na.rm=TRUE)[1:22,2],
                 type="l", xlim= c(1995, 2020), ylim= c(0,0.15))
}else{ lines( x=  2015+i -0:21, 
              y= svyby( formula= ~ AirBag_Success, by= ~ Car_Age_Accident, design= svy.dgn[[i]],  FUN=svymean, na.rm=TRUE)[1:22,2],
              col=i)
}
#print(  svyby( formula= ~ AirBag_Success, by= ~ MAKE_Origin,  design= svy.dgn[[i]],  FUN=svymean, na.rm=TRUE) ) 
} 
print(Fail.Svy.Hist)

#################################################################################################
### modeling variables###########################################################################
#################################################################################################



table(data.Merge.list[[i]]$Car_Age_Accident)
table(data.Merge.list[[i]]$AirBag_Success)
table(data.Merge.list[[i]]$MAKE_Origin)
table(data.Merge.list[[i]]$REGION)
table(data.Merge.list[[i]]$URBANICITY)
table(data.Merge.list[[i]]$BODY_TYP)
table(data.Merge.list[[i]]$DR.Age)


source("EM.R")
source("ML1ShotEstimationFunction2a.R")
source("ML1ShotEvalMissFunc.R")
library(dplyr)


Data.real<-list()

for( i in 2:5){

data.extrac1 <- data.frame( 
  AirBag_Success1  = data.Merge.list[[i]]$AirBag_Success,
  MAKE_Origin      = as.factor(data.Merge.list[[i]]$MAKE_Origin),
  REGION           = as.factor(data.Merge.list[[i]]$REGION),
  URBAN_CITY       = data.Merge.list[[i]]$URBANICITY,
  BODY_TYP1        = data.Merge.list[[i]]$BODY_TYP,
  Car_Age_Accident = data.Merge.list[[i]]$Car_Age_Accident,
  Driver.Age       = data.Merge.list[[i]]$DR.Age
)

data.extrac1$BODY_TYP <-1
data.extrac1$BODY_TYP[ which(data.extrac1$BODY_TYP1%in% c(2,4,8))] <- 2
data.extrac1$BODY_TYP[ which(data.extrac1$BODY_TYP1%in% c(14:16,19))] <- 3

data.extrac1$BODY_TYP1<-NULL
data.extrac1$AirBag_Success1 <- as.character(as.numeric(data.extrac1$AirBag_Success1))

Data.real[[i]]<- data.extrac1
}

##Bootstrap Index 1000 times  #####################################################################
##  each column  represent 1 Bootstrap sample

set.seed(1)
bootsml <- bootmed <- boothig <-  list() 
for(i in 1:5){
  n.temp <- nrow(data.Merge.list[[i]])
  bootsml[[i]] <- replicate(1000,sample( n.temp , size =1000)) 
  bootmed[[i]] <- replicate(1000,sample( n.temp , size =2000)) 
  boothig[[i]] <- replicate(1000,sample( n.temp , size =4000))  
}

save( Data.real,  bootsml,  bootmed,  boothig, file = "CRSSData.RData")

##################The rest is estimation part #################################################################
Results.real<-list()

for( i in 2:5){
  set.seed(i)
### limit sample size, reduce biases due to mispecification
data.extrac<- dplyr::sample_n(data.extrac1, 5000 )
variate.miss.ind.b<- apply( data.extrac, 1, function(x){length(which(is.na(x)))>0} )
## grouped duplicates
#df <- data.extrac %>% group_by_all() %>% summarise(COUNT = n())
dis.mat <- cluster::daisy( data.extrac[,2:7], metric ="gower")
 #dis.mat <- vegan::vegdist( data.extrac[,2:7], method="gower",na.rm=T)

Results.real[[i]]<- list(
  k.proto   = Est.func.k.proto(data.extrac),
  dbscan    = Est.func.dbscan(data.extrac),
  dbscan2   = Est.func.dbscan2(data.extrac, dis.mat= dis.mat ),
  hclust    = Est.func.hclust(data.extrac, dis.mat= dis.mat, k.h = 5),
  Est.func1 = Est.func (data.extrac, Miss.ind =variate.miss.ind.b)
)
}

######################################################################################## 




#### a slow task function with various completion time 
task.func <-  function(a){
  k<- runif(1,min = 0, max = 1)
  Sys.sleep( k)
  a+k
}



####  Task list stores the inputs of the task.func 
task.list <- list()
for( i in 1:12){ task.list[[i]]<-i}












