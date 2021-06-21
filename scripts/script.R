#####################################################################
## Analysis script for the paper:                                   ##
##"Expecting to teach a novel golf putting task did not enhance     ##  
## retention performance: A replication experiment"                 ##
## -- McKay, Hussien, Carter, Yantha, and Ste-Marie                 ##
##                                                                  ##
## Created by Brad McKay (bradmckay8 [at] gmail [dot] com)          ##
######################################################################

# Check if required packages for analyses are installed. If it is, it
# will be loaded. If any are not, the missing package(s) will be
# installed and then loaded.

# CRAN packages
pkgs = c("data.table", "readr", "Routliers", "rstatix", "dplyr", "tidyr", "effects", "metafor", "TOSTER")
pkgs_check <- lapply(
  pkgs,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Run to manually verify all required packages are loaded
# Look in console output
(.packages())

#--------------------------

####### DATA WRANGLE ########

##Calculate pre-test radial error and bivariate variable error for each participant
# Import IMI data
IMIdat <- read_csv("data/IMI data/IMIdat.csv")
# Import raw pre-test data files for each participant
list_of_files <- list.files(path = "data/Session1", recursive = TRUE,pattern = "\\.txt$", full.names = TRUE)
# Read all the files and create a FileName column to store the filenames
DT <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),use.names = TRUE, idcol = "FileName", fill = TRUE)
#Delete extraneous columns
pretest = subset(DT, select = -c(V8,V7,RE))
# rename y and x error columns
names(pretest)[names(pretest)=="S/L"] <- "y"
names(pretest)[names(pretest)=="L/R"] <- "x"
#Replace erroneous y “90” values with “132”
pretest$y[pretest$y==90] <- 132
## replace missing y axis values with the participants maximum observed y value
# find max values for each participant
maxes <- aggregate(y~sID, pretest, max, na.rm = TRUE)
# find rows with missing data
nas <- which(is.na(pretest$y))
# match maximum values with missing rows
pretest$y[nas] <- maxes$y[match(pretest$sID[nas], maxes$sID)]
## replace missing x axis values with maximum width of the mat and the sign of the mean constant error for each participant
# calculate mean constant error in x axis
means <- aggregate(x~sID, pretest, mean, na.rm = TRUE)
# divide mean constant error by its absolute value to get a value of 1 and the sign of the constant error
signs <- transform(means, sign = x / abs(x))
# multiply the maximum width of the mat by the sign the mean constant error
constants <- transform(signs, constant = 33.02 * sign)
#find rows with missing x-axis data
nasx <- which(is.na(pretest$x))
# match maximum x values with correct sign with missing rows
pretest$x[nasx] <- constants$constant[match(pretest$sID[nasx], constants$sID)]

#Calculate radial error
# Create new column with radial error calculated from the coordinates
pretest$RAD <- sqrt((pretest$y^2)+(pretest$x^2))
# Create new object containing mean pre-test radial error for each participant
preRE <- aggregate(pretest$RAD~pretest$FileName+pretest$Grp+pretest$sID, pretest, mean)
#Calculate bivariate variable error
# Create new column with each participant’s x-axis centroid
 pretest$centroidx <- ave(pretest$x, pretest$sID)
# Create new column with each participant’s y-axis centroid
pretest$centroidy <- ave(pretest$y, pretest$sID)
# Create new column with square of deviations from x-axis centroid for each trial
pretest$veX <- (pretest$x-pretest$centroidx)^2
# Create new column with square of deviations from y-axis centroid for each trial
pretest$veY <- (pretest$y-pretest$centroidy)^2
# Create new column with sum of squared deviations
pretest$veSQR <- (pretest$veX+pretest$veY)
# Create new column with mean pre-test bivariate variable error for each participant
pretest$BVE <- ave(pretest$veSQR, pretest$sID)^.5
# Create new object containing mean pre-test bivariate variable error for each participant 
preBVE <- aggregate(pretest$BVE~pretest$FileName+pretest$Grp+pretest$sID,pretest, mean)

## Calculate acquisition radial error and bivariate variable error
# Import raw pre-test data files for each participant  
list_of_files <- list.files(path = "data/Session2", recursive = TRUE,pattern = "\\.txt$", full.names = TRUE)
# Read all the files and create a FileName column to store the filenames
DT <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),use.names = TRUE, idcol = "FileName", fill = TRUE)
#Delete extraneous columns
df = subset(DT, select = -c(V8,V7,RE))
#Choose the acquisition blocks by selecting the first 50 trials for each participant
acqdf <- df[, .SD[1:50], by = sID]
#Select practice trials, the trials remaining in df after selecting out the acquisition putts
x <- dplyr::setdiff(df, acqdf)
# Count practice trials
practice_trials <- tally(group_by(x, sID, ))
#Create a column labeling acquisition blocks 1 -5
acqdf$block <- rep(1:5, each=10, length.out=3800)
#Remove rows with missing data
acqdat <- na.omit(acqdf)
#Replace erroneous S/L “90” values with “132”
acqdat$`S/L`[acqdat$`S/L`==90] <- 132

#Calculate radial error
# Create new column with radial error calculated from the coordinates
acqdat$RAD <- sqrt((acqdat$`S/L`^2)+(acqdat$`L/R`^2))
#Aggregate radial error data into block means
acqREblk <- aggregate(acqdat$RAD, by = list(sID=acqdat$sID, block=acqdat$block, Grp=acqdat$Grp),mean)

#Calculate bivariate variable error
# Create new column with each participant’s x-axis centroid
acqdat$centroidx <- ave(acqdat$`L/R`, by = acqdat$block, acqdat$FileName)
# Create new column with each participant’s y-axis centroid
acqdat$centroidy <- ave(acqdat$`S/L`, by =acqdat$block, acqdat$FileName)
# Create new column with square of deviations from x-axis centroid for each trial
acqdat$veX <- (acqdat$`L/R`-acqdat$centroidx)^2
# Create new column with square of deviations from y-axis centroid for each trial
acqdat$veY <- (acqdat$`S/L`-acqdat$centroidy)^2
# Create new column with sum of squared deviations
acqdat$veSQR <- (acqdat$veX+acqdat$veY)
# Aggregate squared deviations into block means
acqBVEblk <- aggregate(acqdat$veSQR, by = list(sID=acqdat$sID, block=acqdat$block, Grp=acqdat$Grp),mean)
#Create new column with bivariate variable error
 acqBVEblk$BVE <- sqrt(acqBVEblk$x)

## Calculate retention radial error and bivariate variable error
# Import raw retention data files for each participant  
list_of_files <- list.files(path = "data/Session3", recursive = TRUE,pattern = "\\.txt$", full.names = TRUE)
# Read all the files and create a FileName column to store the filenames
DT <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),use.names = TRUE, idcol = "FileName", fill = TRUE)
#Delete extraneous columns
redat = subset(DT, select = -c(V8,V7,RE))
# rename y and x error columns
names(redat)[names(redat)=="S/L"] <- "y"
names(redat)[names(redat)=="L/R"] <- "x"
#Replace erroneous y “90” values with “132”
redat$y[redat$y==90] <- 132
## replace missing y axis values with the participants maximum observed y value
# find max values for each participant
maxes <- aggregate(y~sID, redat, max, na.rm = TRUE)
# find rows with missing data
nas <- which(is.na(redat$y))
# match maximum values with missing rows
redat$y[nas] <- maxes$y[match(redat$sID[nas], maxes$sID)]

## replace missing x axis values with maximum width of the mat and the sign of the mean constant error for each participant
# calculate mean constant error in x axis
means <- aggregate(x~sID, redat, mean, na.rm = TRUE)
# divide mean constant error by its absolute value to get a value of 1 and the sign of the constant error
signs <- transform(means, sign = x / abs(x))
# multiply the maximum width of the mat by the sign the mean constant error
constants <- transform(signs, constant = 33.02 * sign)
#find rows with missing x-axis data
nasx <- which(is.na(redat$x))
# match maximum x values with correct sign with missing rows
redat$x[nasx] <- constants$constant[match(redat$sID[nasx], constants$sID)]


# Create new column with radial error calculated from the coordinates
redat$RAD <- sqrt((redat$y^2)+(redat$x^2))
# Create new object containing mean retention radial error for each participant
retRE <- aggregate(redat$RAD~redat$FileName+redat$Grp+redat$sID, redat, mean)
#Calculate bivariate variable error
# Create new column with each participant’s x-axis centroid
 redat$centroidx <- ave(redat$x, redat$sID)
# Create new column with each participant’s y-axis centroid
redat$centroidy <- ave(redat$y, redat$sID)
# Create new column with square of deviations from x-axis centroid for each trial
redat$veX <- (redat$x-redat$centroidx)^2
# Create new column with square of deviations from y-axis centroid for each trial
redat$veY <- (redat$y-redat$centroidy)^2
# Create new column with sum of squared deviations
redat$veSQR <- (redat$veX+redat$veY)
# Create new column with mean retention bivariate variable error for each participant
redat$BVE <- ave(redat$veSQR, redat$sID)^.5
# Create new object containing mean retention bivariate variable error for each participant 
retBVE <- aggregate(redat$BVE~redat$FileName+redat$Grp+redat$sID,redat, mean)

##Calculate transfer radial error and bivariate variable error for each participant
# Import raw transfer data files for each participant  
list_of_files <- list.files(path = "data/Session4", recursive = TRUE,pattern = "\\.txt$", full.names = TRUE)
# Read all the files and create a FileName column to store the filenames
DT <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),use.names = TRUE, idcol = "FileName", fill = TRUE)
#Delete extraneous columns
tradat = subset(DT, select = -c(V8,V7,RE))
# rename y and x error columns
names(tradat)[names(tradat)=="S/L"] <- "y"
names(tradat)[names(tradat)=="L/R"] <- "x"
#Replace erroneous y “90” values with “132”
tradat$y[tradat$y==90] <- 132
## replace missing y axis values with the participants maximum observed y value
# find max values for each participant
maxes <- aggregate(y~sID, tradat, max, na.rm = TRUE)
# find rows with missing data
nas <- which(is.na(tradat$y))
# match maximum values with missing rows
tradat$y[nas] <- maxes$y[match(tradat$sID[nas], maxes$sID)]
## replace missing x axis values with maximum width of the mat and the sign of the mean constant error for each participant
# calculate mean constant error in x axis
means <- aggregate(x~sID, tradat, mean, na.rm = TRUE)
# divide mean constant error by its absolute value to get a value of 1 and the sign of the constant error
signs <- transform(means, sign = x / abs(x))
# multiply the maximum width of the mat by the sign the mean constant error
constants <- transform(signs, constant = 33.02 * sign)
#find rows with missing x-axis data
nasx <- which(is.na(tradat$x))
# match maximum x values with correct sign with missing rows
tradat$x[nasx] <- constants$constant[match(tradat$sID[nasx], constants$sID)]


# Create new column with radial error calculated from the coordinates
tradat$RAD <- sqrt((tradat$y^2)+(tradat$x^2))
# Create new object containing mean transfer radial error for each participant
traRE <- aggregate(tradat$RAD~tradat$FileName+tradat$Grp+tradat$sID, tradat, mean)
#Calculate bivariate variable error
# Create new column with each participant’s x-axis centroid
 tradat$centroidx <- ave(tradat$x, tradat$sID)
# Create new column with each participant’s y-axis centroid
tradat$centroidy <- ave(tradat$y, tradat$sID)
# Create new column with square of deviations from x-axis centroid for each trial
tradat$veX <- (tradat$x-tradat$centroidx)^2
# Create new column with square of deviations from y-axis centroid for each trial
tradat$veY <- (tradat$y-tradat$centroidy)^2
# Create new column with sum of squared deviations
tradat$veSQR <- (tradat$veX+tradat$veY)
# Create new column with mean transfer bivariate variable error for each participant
tradat$BVE <- ave(tradat$veSQR, tradat$sID)^.5
# Create new object containing mean transfer bivariate variable error for each participant 
traBVE <- aggregate(tradat$BVE~tradat$FileName+tradat$Grp+tradat$sID,tradat, mean)
#Rename columns in each object so they are consistent across objects
names(preRE)[names(preRE)=="pretest$FileName"] <-"FileName"
names(preRE)[names(preRE)=="pretest$sID"] <-"sID"
names(preRE)[names(preRE)=="pretest$Grp"] <-"Grp"
names(preRE)[names(preRE)=="pretest$RAD"] <-"RAD"
names(retRE)[names(retRE)=="redat$FileName"] <-"FileName"
names(retRE)[names(retRE)=="redat$sID"] <-"sID"
names(retRE)[names(retRE)=="redat$Grp"] <-"Grp"
names(retRE)[names(retRE)=="redat$RAD"] <-"RAD"
names(traRE)[names(traRE)=="tradat$FileName"] <-"FileName"
names(traRE)[names(traRE)=="tradat$Grp"] <-"Grp"
names(traRE)[names(traRE)=="tradat$sID"] <-"sID"
names(traRE)[names(traRE)=="tradat$RAD"] <-"RAD"
#Rename columns in each object so they are consistent across objects
names(preBVE)[names(preBVE)=="pretest$BVE"] <-"BVE"
names(retBVE)[names(retBVE)=="redat$FileName"] <-"FileName"
names(preBVE)[names(preBVE)=="pretest$sID"] <-"sID"
names(retBVE)[names(retBVE)=="redat$sID"] <-"sID"
names(retBVE)[names(retBVE)=="redat$Grp"] <-"Grp"
names(retBVE)[names(retBVE)=="redat$BVE"] <-"BVE"
names(traBVE)[names(traBVE)=="tradat$FileName"] <-"FileName"
names(traBVE)[names(traBVE)=="tradat$Grp"] <-"Grp"
names(traBVE)[names(traBVE)=="tradat$sID"] <-"sID"
names(traBVE)[names(traBVE)=="tradat$BVE"] <-"BVE"


#---------------------------------------------


##### PRIMARY AND SECONDARY DATA ANALYSES AT FINAL LOOK ##################

## Outlier screening an removal

##Screen for mean radial error outliers at each time point
#pre-test
outliers_mad(preRE$RAD, threshold = 3)
#retention
outliers_mad(retRE$RAD, threshold = 3)
#transfer
outliers_mad(traRE$RAD, threshold = 3)

#Remove outlier based on seven missing pretest trials and six missing retention trials (sID 22)
preREc <- preRE[-c(22),]
retREc <- retRE[-c(22),]
traREc <- traRE[-c(22),]



##Screen for mean bivariate variable error outliers at each time point
#pre-test
outliers_mad(preBVE$BVE, threshold = 3)
#retention
outliers_mad(retBVE$BVE, threshold = 3)
#transfer
outliers_mad(traBVE$BVE, threshold = 3)
#Remove outlier based on too much missing data (sID 22)
preBVEc <- preBVE[-c(22),]
retBVEc <- retBVE[-c(22),]
traBVEc <- traBVE[-c(22),]

## Create radial error df
#rename columns to differentiate times
names(preREc)[names(preREc)=="RAD"] <-"preRE"
names(retREc)[names(retREc)=="RAD"] <-"RetRE"
names(traREc)[names(traREc)=="RAD"] <-"traRE"
# combine pretest, retention, and transfer radial error data into one df
retdat <- cbind(preREc,retREc,traREc)
#rename columns to differentiate times
names(preBVEc)[names(preBVEc)=="BVE"] <-"preBVE"
names(retBVEc)[names(retBVEc)=="BVE"] <-"RetBVE"
names(traBVEc)[names(traBVEc)=="BVE"] <-"traBVE"
# combine pretest, retention, and transfer bivariate variable error data into one df
bvedat <- cbind(preBVEc,retBVEc,traBVEc)


#Create new column with retention and transfer combined as post-test
retdat$post <- (retdat$RetRE+retdat$traRE)/2
# Create Group factor
Group <- as.factor(retdat$Grp)


### Primary confirmatory analysis ###


#Fit ANCOVA with retention test as DV, group as factor and pre-test as the covariate
fit <- aov(RetRE~Group+preRE, data = retdat)
# Generate results based on type III sum of squares (equivalent to SPSS output)
drop1(fit, .~., test = "F")
# Generate estimated marginal means for group
allEffects(fit)
#plot estimated marginal means and pretest-posttest correlation
plot(allEffects(fit))
# Calculate mean and standard deviation of radial error on retention test by group
aggregate(retdat$RetRE, by = list(retdat$Grp), FUN = (mean))
aggregate(retdat$RetRE, by = list(retdat$Grp), FUN = (sd))

### 'Original' confirmatory analyses from preregistration


# Fit ANCOVA with postest as DV, group as factor and pre-test as the covariate
fit <- aov(post~Group+preRE, data = retdat)
# Generate results based on type III sum of squares (equivalent to SPSS output)
drop1(fit, .~., test = "F")
# Generate estimated marginal means for group
allEffects(fit)
#plot estimated marginal means and pretest-posttest correlation
plot(allEffects(fit))
# calculate mean and sd of post-test radial error by group
aggregate(retdat$post, by = list(retdat$Grp), FUN = (mean))
aggregate(retdat$post, by = list(retdat$Grp), FUN = (sd))


### Conduct equivalence test comparing observed effect to lower bound of Daou estimates 


### Combine results from Dauo, Buchanan, et al. 2016 and Dauo et al., 2016 in random effects meta-analysis
# import data
Daou_data <- read_csv("data/Daou_data.csv")
# fit random effects model to data
rma(retg,retv, data = Daou_data)
### Use two one-sided tests method to compare present results to lower 95% CI bound of meta-analytic estimate
dataTOSTtwo(retdat, deps = "RetRE", group = "Grp", var_equal = FALSE, low_eqbound = -0.57, high_eqbound = 0.57, plots = TRUE)

### Conduct exploratory bivariate variable error analysis


#Create new column with retention and transfer combined as post-test
bvedat$post <- (bvedat$RetBVE+bvedat$traBVE)/2
Groupb <- as.factor(bvedat$Grp)
#Fit ANCOVA with post-test as DV, group as factor and pre-test as the covariate
fit2 <- aov(RetBVE~Groupb+preBVE, data = bvedat)
# Generate results based on type III sum of squares (equivalent to SPSS output)
drop1(fit2, .~., test = "F")
# Generate estimated marginal means for group
allEffects(fit2)
#plot estimated marginal means and pretest-posttest correlation
plot(allEffects(fit2))
### Combine BVE results from Dauo, Buchanan, et al. 2016 and Dauo et al., 2016 in random effects meta-analysis
# import data
Daou_data_bve <- read_csv("data/Daou_data_BVE.csv")
# fit random effects model to BE data
rma(retg,retv, data = Daou_data_bve)
### Use two one-sided tests method to compare present results to lower 95% CI bound of meta-analytic estimate
dataTOSTtwo(bvedat, deps = "RetBVE", group = "Grp", var_equal = FALSE, low_eqbound = -0.63, high_eqbound = 0.63, plots = TRUE)


### Secondary analyses ###



## Analysis of acquisition data
## Run mixed ANOVA with radial error as DV, block as a within subjects factor and group as a between subjects factor
res.aov <- anova_test(data = acqREblk, dv = x, wid = sID, between = Grp, within = block) 
get_anova_table(res.aov)
# Get group means and standard deviations for blocks 1,3 and 5

aggregate(acqREblk$x, by = list(acqREblk$block, acqREblk$Grp), FUN = mean)
aggregate(acqREblk$x, by = list(acqREblk$block, acqREblk$Grp), FUN = sd)
# Run mixed ANOVA with bivariate variable error as DV, block as a within subjects factor and group as a between subjects factor
#remove unused column
acqBVEblk <- subset(acqBVEblk, select = -c(x))
bve.acq <- anova_test(data = acqBVEblk, dv = BVE, wid = sID, between = Grp, within = block) 
get_anova_table(bve.acq)

#### Intrinsic Motivation
# Create group factor
Groupm <- as.factor(IMIdat$Grp)
# Fit ANCOVA models to IMI subsections with baseline measures as the covariate
interest <- aov(posINT~Groupm+preINT, data = IMIdat)
useful <- aov(posUSE~Groupm+preUSE, data = IMIdat)
effort <- aov(posEFF~Groupm+preEFF, data = IMIdat)
pressure <- aov(posPRE~Groupm+prePRE, data = IMIdat)
# Compare groups on study time measure
study <- aov(stime~Groupm, data = IMIdat)
# Compare groups on the free recall measure
frecall <- aov(frecall~Groupm, data = IMIdat)

# Generate results based on type III sum of squares (equivalent to SPSS output)
drop1(interest, .~., test = "F")
drop1(useful, .~., test = "F")
drop1(effort, .~., test = "F")
drop1(pressure, .~., test = "F")
drop1(study, .~., test = "F")
drop1(frecall, .~., test = "F")

# Generate estimated marginal means for group for each analysis
allEffects(interest)
allEffects(useful)
allEffects(effort)
allEffects(pressure)
allEffects(study)


#plot estimated marginal means and pretest-posttest correlation
plot(allEffects(interest))
plot(allEffects(useful))
plot(allEffects(effort))
plot(allEffects(pressure))
plot(allEffects(study))


### Practice-trials analysis
# Fill in list of participant IDs and add a zero for their count
ptall <- practice_trials %>% complete(sID = 1:76, fill = list(n = 0))
# Compare groups on number of practice trials
trials <- aov(ptall$n~Groupm)
# View results
summary(trials)
allEffects(trials)
plot(allEffects(trials))


#---------------------------------------

##### Planned 'first look' interim analyses at N = 70


#### DATA WRANGLE #####


### Select first 70 participants for each measure
#pretest radial error
iapreRE <- subset(preREc, sID <= 70)
#pretest bivariate variable error
iapreBVE <- subset(preBVEc, sID <= 70)
#acquisition radial error
iaacqRE <- subset(acqREblk, sID <= 70)
#acquisition bivariate variable error
iaacqBVE <- subset(acqBVEblk, sID <= 70)
#retention radial error
iaretRE <- subset(retREc, sID <= 70)
#retention bivariate variable error
iaretBVE <- subset(retBVEc, sID <= 70)
# transfer variable error
iatraRE <- subset(traREc, sID <= 70)
# transfer bivariate variable error
iatraBVE <- subset(traBVEc, sID <= 70)
#IMI data
iaMOT <- subset(IMIdat, sID <= 70) 
# practice trials
iapractice_trials <- subset(practice_trials, sID <= 70)




## Outlier screening and removal ##

##Screen for mean radial error outliers at each time point
#pre-test
outliers_mad(iapreRE$preRE, threshold = 3)
#retention
outliers_mad(iaretRE$RetRE, threshold = 3)
#transfer
outliers_mad(iatraRE$traRE, threshold = 3)

##Screen for mean bivariate variable error outliers at each time point
#pre-test
outliers_mad(iapreBVE$preBVE, threshold = 3)
#retention
outliers_mad(iaretBVE$RetBVE, threshold = 3)
#transfer
outliers_mad(iatraBVE$traBVE, threshold = 3)
# combine pretest, retention, and transfer radial error data into one df
iaredat <- cbind(iapreRE,iaretRE,iatraRE)
# combine pretest, retention, and transfer bivariate variable error data into one df
iabvedat <- cbind(iapreBVE,iaretBVE,iatraBVE)


#Create new column with retention and transfer combined as post-test
iaredat$post <- (iaredat$RetRE+iaredat$traRE)/2
# Create Group factor
Group <- as.factor(iaredat$Grp)


## Primary confirmatory analysis

#Fit ANCOVA with retention test as DV, group as factor and pre-test as the covariate
iafit1 <- aov(RetRE~Group+preRE, data = iaretdat)
# Generate results based on type III sum of squares (equivalent to SPSS output)
drop1(iafit1, .~., test = "F")
# Generate estimated marginal means for group
allEffects(iafit1)
#plot estimated marginal means and pretest-posttest correlation
plot(allEffects(iafit1))
# Calculate mean and standard deviation of radial error on retention test by group
aggregate(iaretdat$RetRE, by = list(iaretdat$Grp), FUN = (mean))
aggregate(iaretdat$RetRE, by = list(iaretdat$Grp), FUN = (sd))


## 'Original' primary analyses from preregistration 


#Fit ANCOVA with post-test as DV, group as factor and pre-test as the covariate
iafit <- aov(post~Group+preRE, data = iaredat)
# Generate results based on type III sum of squares (equivalent to SPSS output)
drop1(iafit, .~., test = "F")
# Generate estimated marginal means for group
allEffects(iafit)
#plot estimated marginal means and pretest-posttest correlation
plot(allEffects(iafit))


### Repeat with bivariate variable error 
#Create new column with retention and transfer combined as post-test
iabvedat$post <- (iabvedat$RetBVE+iabvedat$traBVE)/2
Groupb <- as.factor(iabvedat$Grp)
#Fit ANCOVA with retention test as DV, group as factor and pre-test as the covariate
iafit2 <- aov(RetBVE~Groupb+preBVE, data = iabvedat)
# Generate results based on type III sum of squares (equivalent to SPSS output)
drop1(iafit2, .~., test = "F")
# Generate estimated marginal means for group
allEffects(iafit2)
#plot estimated marginal means and pretest-posttest correlation
plot(allEffects(iafit2))



## Secondary analyses


## Analysis of acquisition data
## Run mixed ANOVA with radial error as DV, block as a within subjects factor and group as a between subjects factor
iares.aov <- anova_test(data = iaacqRE, dv = x, wid = sID, between = Grp, within = block) 
get_anova_table(iares.aov)
# Run mixed ANOVA with bivariate variable error as DV, block as a within subjects factor and group as a between subjects factor
iabve.acq <- anova_test(data = iaacqBVE, dv = BVE, wid = sID, between = Grp, within = block) 
get_anova_table(iabve.acq)

#### Intrinsic Motivation
# Create group factor
Groupm <- as.factor(iaMOT$Grp)
# Fit ANCOVA models to IMI subsections with baseline measures as the covariate
iainterest <- aov(posINT~Groupm+preINT, data = iaMOT)
iauseful <- aov(posUSE~Groupm+preUSE, data = iaMOT)
iaeffort <- aov(posEFF~Groupm+preEFF, data = iaMOT)
iapressure <- aov(posPRE~Groupm+prePRE, data = iaMOT)

# Compare groups on study time measure
iastudy <- aov(stime~Groupm, data = iaMOT)

# Generate results based on type III sum of squares (equivalent to SPSS output)
drop1(iainterest, .~., test = "F")
drop1(iauseful, .~., test = "F")
drop1(iaeffort, .~., test = "F")
drop1(iapressure, .~., test = "F")
drop1(iastudy, .~., test = "F")

# Generate estimated marginal means for group for each analysis
allEffects(iainterest)
allEffects(iauseful)
allEffects(iaeffort)
allEffects(iapressure)
allEffects(iastudy)

#plot estimated marginal means and pretest-posttest correlation
plot(allEffects(iainterest))
plot(allEffects(iauseful))
plot(allEffects(iaeffort))
plot(allEffects(iapressure))
plot(allEffects(iastudy))

### Practice-trials analysis

# Fill in list of participant IDs and add a zero for their count
iaptall <- iapractice_trials %>% complete(sID = 1:70, fill = list(n = 0))

# Compare groups on number of practice trials
iatrials <- aov(iaptall$n~Groupm)

# View results
summary(iatrials)
allEffects(iatrials)
plot(allEffects(iatrials))
