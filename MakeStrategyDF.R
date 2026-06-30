
# MAKE STERILISATION STRATEGY DATAFRAME # ---------------------------------------------
# R Script used in the main paper 'Modelling sterilisation strategies to maximise population impact and cost-efficiency in free-roaming dog populations'
# to create a dataframe of multiple sterilisation strategies that can be entered into speyFun to evaluate different strategies

# Author: Helen Fielding

# Load packages
library(tidyverse)
path <- 'edit as necessary'

# Set up data frame ####
strategyDF <- data.frame()
nYears <- 17 # just for the record - this is now embedded in function; 6y warm up, 10y output, 1y extra for when neutering starts at end of year
speying <- TRUE

for (maleNeuterImpact in c(0, 0.15, 0.25)){
# Catch return guided ####
desc <- 'CatchReturnGuided'
speyingR <- TRUE
speyingStart <- 1
totalNeuterCap <- NA
speyingTargetC <- NA
speyingTargetR <- NA
speyingRepeat <- NA
speyingInterval <- 0
numTeamsChanges <- FALSE
speyingDuration <- 365


for(speyingMales in c(TRUE, FALSE)){
  for(speyingC in c(TRUE, FALSE)){
  for (numTeams in c(1,3,5)){
    for (speyedPerDay in c(25)){
      for (minCatch in c(5,10)){
        for (neuterBreak in c(30, 90, 5*30, 180, 365,2*365, 5*365)){
          
          if (minCatch == 0) next
          
          i <- paste0('owned', speyingC,
                      'nB', neuterBreak,
                      'mCatch', minCatch,
                      'numTeams', numTeams,
                      '_numTeamsChanges', numTeamsChanges,
                      '_speyingMales', speyingMales,
                      '_y', nYears,
                      '_start', speyingStart,
                      '_pday', speyedPerDay,
                      '_dur', speyingDuration,
                      '_int', speyingInterval,
                      '_tcap', totalNeuterCap,
                      '_rpt', speyingRepeat,
                      '_mni', str_replace(maleNeuterImpact, '\\.', '_'))
          
          print(paste('model', i, Sys.time()))
          
          this_strategyDF <- data.frame(i = i,
                                        desc = desc,
                              nYears = nYears, 
                              speying = speying, 
                              speyingR = speyingR, 
                              speyingC = speyingC,
                              speyingStart = speyingStart, 
                              speyedPerDay = speyedPerDay, 
                              speyingDuration = speyingDuration,
                              speyingRepeat = speyingRepeat,
                              speyingInterval = speyingInterval,
                              speyingTargetR = speyingTargetR,
                              speyingTargetC = speyingTargetC,
                              speyingMales = speyingMales,
                              totalNeuterCap = totalNeuterCap,
                              minCatch = minCatch,
                              neuterBreak = neuterBreak,
                              numTeams = numTeams, 
                              numTeamsChanges = numTeamsChanges,
                              mni = maleNeuterImpact)
          
          strategyDF <- bind_rows(strategyDF, this_strategyDF)
          
        }}}
  }}}

       
# Specify intervals ####
desc <- 'specifiedIntervals'
neuterBreak <- NA
minCatch <- 0 # needs to be a number not NA, will result in no neuterbreaks
speyedPerDay <- 25

for(speyingMales in c(TRUE, FALSE)){
  for(speyingC in c(TRUE, FALSE)){
  for (speyingInterval in c(90, 185, 365,2*365, 5*365, 7*365)){
    for (speyingRepeat in c(NA, 3,5)){
      for (speyingDuration in c(30, 90, 180, 365, 2*365)){
        for (numTeams in c(1,3,5)){
          for(numTeamsChanges in c(TRUE, FALSE)){
            
          if (numTeams == 1 & numTeamsChanges == TRUE) next # skip if 1 team and change teams, as this is the same as if it is teamsChanges == FALSE
            #if(is.na(speyingRepeat) & speyingInterval > 90) next # not sure why this is needed
            
            i <- paste0('owned', speyingC, 
                        'nB', neuterBreak,
                        'mCatch', minCatch,
                        'numTeams', numTeams,
                        '_numTeamsChanges', numTeamsChanges,
                        '_speyingMales', speyingMales,
                        '_y', nYears,
                        '_start', speyingStart,
                        '_pday', speyedPerDay,
                        '_dur', speyingDuration,
                        '_int', speyingInterval,
                        '_tcap', totalNeuterCap,
                        '_rpt', speyingRepeat,
                        '_mni', str_replace(maleNeuterImpact, '\\.', '_'))
            
            print(paste('model', i, Sys.time()))
            
            this_strategyDF <- data.frame(i = i,
                                          desc = desc,
                                          nYears = nYears, 
                                          speying = speying, 
                                          speyingR = speyingR, 
                                          speyingC = speyingC,
                                          speyingStart = speyingStart, 
                                          speyedPerDay = speyedPerDay, 
                                          speyingDuration = speyingDuration,
                                          speyingRepeat = speyingRepeat,
                                          speyingInterval = speyingInterval,
                                          speyingTargetR = speyingTargetR,
                                          speyingTargetC = speyingTargetC,
                                          speyingMales = speyingMales,
                                          totalNeuterCap = totalNeuterCap,
                                          minCatch = minCatch,
                                          neuterBreak = neuterBreak,
                                          numTeams = numTeams, 
                                          numTeamsChanges = numTeamsChanges,
                                          mni = maleNeuterImpact)
            
            strategyDF <- bind_rows(strategyDF, this_strategyDF)
            
          }
          } # num teams changes 
      }}}}}

# Single session ####
# repeats annual neutering till end of model with no intervals
desc <- 'single_session'
neuterBreak <- NA
minCatch <- 0 # needs to be a number not NA
speyingInterval <- 0
speyingRepeat <- 1
speyedPerDay <- 25

for(speyingMales in c(TRUE, FALSE)){
  for(speyingC in c(TRUE, FALSE)){
    for (numTeams in c(1,3,5)){
      for(numTeamsChanges in c(TRUE, FALSE)){
      for (speyingDuration in c(30, 90, 180, 365, 730, 1825, 2555)){
        
        if (numTeams == 1 & numTeamsChanges == TRUE) next # skip if 1 team and change teams, as this is the same as if it is teamsChanges == FALSE
        
              i <- paste0('owned', speyingC, 
                          'nB', neuterBreak,
                          'mCatch', minCatch,
                          'numTeams', numTeams,
                          '_numTeamsChanges', numTeamsChanges,
                          '_speyingMales', speyingMales,
                          '_y', nYears,
                          '_start', speyingStart,
                          '_pday', speyedPerDay,
                          '_dur', speyingDuration,
                          '_int', speyingInterval,
                          '_tcap', totalNeuterCap,
                          '_rpt', speyingRepeat,
                          '_mni', str_replace(maleNeuterImpact, '\\.', '_'))
              
              print(paste('model', i, Sys.time()))
              
              this_strategyDF <- data.frame(i = i,
                                            desc = desc,
                                            nYears = nYears, 
                                            speying = speying, 
                                            speyingR = speyingR, 
                                            speyingC = speyingC,
                                            speyingStart = speyingStart, 
                                            speyedPerDay = speyedPerDay, 
                                            speyingDuration = speyingDuration,
                                            speyingRepeat = speyingRepeat,
                                            speyingInterval = speyingInterval,
                                            speyingTargetR = speyingTargetR,
                                            speyingTargetC = speyingTargetC,
                                            speyingMales = speyingMales,
                                            totalNeuterCap = totalNeuterCap,
                                            minCatch = minCatch,
                                            neuterBreak = neuterBreak,
                                            numTeams = numTeams, 
                                            numTeamsChanges = numTeamsChanges,
                                            mni = maleNeuterImpact)
              
              strategyDF <- bind_rows(strategyDF, this_strategyDF)
              
            }}}
        }}
# Continuous ####
# continuous neutering till end of model with no intervals (neutering for 90days with no interval = cont)
desc <- 'continuous'
neuterBreak <- NA
minCatch <- 0 # needs to be a number not NA
speyingInterval <- 0
speyingRepeat <- NA # means repeats till end
speyingDuration <- 90 # every 90 days but effectively this is continuous
speyedPerDay <- 25

for(speyingMales in c(TRUE, FALSE)){
  for(speyingC in c(TRUE, FALSE)){
    for (numTeams in c(1,3,5)){
      for(numTeamsChanges in c(TRUE, FALSE)){
       
        if (numTeams == 1 & numTeamsChanges == TRUE) next # skip if 1 team and change teams, as this is the same as if it is teamsChanges == FALSE
          i <- paste0('owned', speyingC, 
                      'nB', neuterBreak,
                      'mCatch', minCatch,
                      'numTeams', numTeams,
                      '_numTeamsChanges', numTeamsChanges,
                      '_speyingMales', speyingMales,
                      '_y', nYears,
                      '_start', speyingStart,
                      '_pday', speyedPerDay,
                      '_dur', speyingDuration,
                      '_int', speyingInterval,
                      '_tcap', totalNeuterCap,
                      '_rpt', speyingRepeat,
                      '_mni', str_replace(maleNeuterImpact, '\\.', '_'))
          
          print(paste('model', i, Sys.time()))
          
          this_strategyDF <- data.frame(i = i,
                                        desc = desc,
                                        nYears = nYears, 
                                        speying = speying, 
                                        speyingR = speyingR, 
                                        speyingC = speyingC,
                                        speyingStart = speyingStart, 
                                        speyedPerDay = speyedPerDay, 
                                        speyingDuration = speyingDuration,
                                        speyingRepeat = speyingRepeat,
                                        speyingInterval = speyingInterval,
                                        speyingTargetR = speyingTargetR,
                                        speyingTargetC = speyingTargetC,
                                        speyingMales = speyingMales,
                                        totalNeuterCap = totalNeuterCap,
                                        minCatch = minCatch,
                                        neuterBreak = neuterBreak,
                                        numTeams = numTeams, 
                                        numTeamsChanges = numTeamsChanges,
                                        mni = maleNeuterImpact)
          
          strategyDF <- bind_rows(strategyDF, this_strategyDF)
          
    }}}}

# Different starting months ####
# Specify intervals
speying <- TRUE
speyingDuration <- 180
totalNeuterCap <- NA
speyingTargetC <- NA
speyingTargetR <- NA
speyingRepeat <- 5
desc <- 'ChangeNeuterStart'
speyingInterval <- 365-speyingDuration
speyingMales <- TRUE
speyingC <- TRUE
speyedPerDay <- 25
numTeams <- 3
numTeamsChanges <- TRUE
neuterBreak <- NA
minCatch <- 0 # needs to be a number not NA


for(speyingStart in c(1, round(30.4*1:12))){
              
              i <- paste0('owned', speyingC, 
                          'nB', neuterBreak,
                          'mCatch', minCatch,
                          'numTeams', numTeams,
                          '_numTeamsChanges', numTeamsChanges,
                          '_speyingMales', speyingMales,
                          '_y', nYears,
                          '_start', speyingStart,
                          '_pday', speyedPerDay,
                          '_dur', speyingDuration,
                          '_int', speyingInterval,
                          '_tcap', totalNeuterCap,
                          '_rpt', speyingRepeat,
                          '_mni', str_replace(maleNeuterImpact, '\\.', '_'))
              
              print(paste('model', i, Sys.time()))
              
              this_strategyDF <- data.frame(i = i,
                                            desc = desc,
                                            nYears = nYears, 
                                            speying = speying, 
                                            speyingR = speyingR, 
                                            speyingC = speyingC,
                                            speyingStart = speyingStart, 
                                            speyedPerDay = speyedPerDay, 
                                            speyingDuration = speyingDuration,
                                            speyingRepeat = speyingRepeat,
                                            speyingInterval = speyingInterval,
                                            speyingTargetR = speyingTargetR,
                                            speyingTargetC = speyingTargetC,
                                            speyingMales = speyingMales,
                                            totalNeuterCap = totalNeuterCap,
                                            minCatch = minCatch,
                                            neuterBreak = neuterBreak,
                                            numTeams = numTeams, 
                                            numTeamsChanges = numTeamsChanges,
                                            mni = maleNeuterImpact)
              
              strategyDF <- bind_rows(strategyDF, this_strategyDF)
              
            }

strategyDF%>%count(desc)

# Changing speyed per day ####
desc <- 'speyedPerDay'
speyingR <- TRUE
speyingC <- TRUE
speyingMales <- TRUE
minCatch <- 0
neuterBreak <- NA
speyingStart <- 1
totalNeuterCap <- NA
speyingTargetC <- NA
speyingTargetR <- NA
speyingRepeat <- 3
speyingInterval <- 730
numTeamsChanges <- TRUE
speyingDuration <- 180
numTeams <- 3


for (speyedPerDay in c(5,10,15,25)){
  
  i <- paste0('owned', speyingC,
              'nB', neuterBreak,
              'mCatch', minCatch,
              'numTeams', numTeams,
              '_numTeamsChanges', numTeamsChanges,
              '_speyingMales', speyingMales,
              '_y', nYears,
              '_start', speyingStart,
              '_pday', speyedPerDay,
              '_dur', speyingDuration,
              '_int', speyingInterval,
              '_tcap', totalNeuterCap,
              '_rpt', speyingRepeat,
              '_mni', str_replace(maleNeuterImpact, '\\.', '_'))
  
  print(paste('model', i, Sys.time()))
  
  this_strategyDF <- data.frame(i = i,
                                desc = desc,
                                nYears = nYears, 
                                speying = speying, 
                                speyingR = speyingR, 
                                speyingC = speyingC,
                                speyingStart = speyingStart, 
                                speyedPerDay = speyedPerDay, 
                                speyingDuration = speyingDuration,
                                speyingRepeat = speyingRepeat,
                                speyingInterval = speyingInterval,
                                speyingTargetR = speyingTargetR,
                                speyingTargetC = speyingTargetC,
                                speyingMales = speyingMales,
                                totalNeuterCap = totalNeuterCap,
                                minCatch = minCatch,
                                neuterBreak = neuterBreak,
                                numTeams = numTeams, 
                                numTeamsChanges = numTeamsChanges,
                                mni = maleNeuterImpact)
  
  strategyDF <- bind_rows(strategyDF, this_strategyDF)
  
}







}# end mni
# Make strategy DF ####
strategyDF <- strategyDF%>%
  #distinct(i, .keep_all = TRUE)%>%
  mutate(serial = 1:n())

strategyDF%>%count(desc)
nrow(strategyDF)
saveRDS(strategyDF, paste0(path, 'input/strategyDF_2025.rds'))
