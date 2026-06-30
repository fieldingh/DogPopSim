#License: GPL-3.0

#If you use this model or code in academic work, please cite:

#  Fielding H.R. [2026]. [DogPopSim]. GitHub. [URL/DOI]

# This code is made available under the GNU GPLv3. Commercial use
# is not prohibited by this licence, but redistribution of modified 
# versions must comply with the GPLv3 terms, including preservation 
# of copyright and licence notices and release of corresponding 
# source code.

# Model author: Paul Bessell

# SPEYFUN: MODEL SIMULATION FUNCTION # ------------------------------------------------------------------------- 
# speyFun runs the daily deterministic simulation under a specified 
# sterilisation strategy. The function returns one row per simulated day, 
# including population compartments, births, deaths/mortality rates, 
# abandonment rates, neutering counts, intervention settings and coverage 
# parameters.

# dependent = owned O = confined C
# independent = stray S = roaming R

rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# source Selected Parameters from other code - edit path as necessary
source("input/SelectedParameters.R") # for HPC eddie

# DEFINE FUNCTION INPUTS
speyFun <- function(nYears = 17, # nYears includes 6y warm up, 10y of strategy and an extra year of data as when speyingStart is very late on we need an extra year to get a full year of data
                    speying = TRUE, 
                    speyingR = TRUE, 
                    speyingC = TRUE,
                    speyingMales = TRUE,
                    speyingStart = 1, # start day of sterilisation strategy - day 1
                    speyingInterval = NA, # if you put in interval with no speyingRepeat value (NA), it will repeat speying until the end of the model
                    speyingDuration = 30, # length of sterilisation period
                    speyingRepeat = NA, # na means repeat until end, otherwise number indicates number of repeat sterilisation durations split by speyingInterval
                    speyingTargetR = 0.7, # NOT USED IN PAPER: Target for speying stray dogs - must be less than 1 otherwise messes up calculations as 1-1=0
                    speyingTargetC = 0.5, # NOT USED IN PAPER: Target for speying owned dogs
                    speyedPerDay = NA,# Max capacity of sterilisations that can be done in one day
                    totalNeuterCap = NA, # not used in paper: cap on number of neuters
                    neuterBreak = 0, # interval for catch-guided strategies after sterilisations ceased
                    minCatch = 0,# if catch-guided the threshold number of dogs below which sterilisations cease
                    numTeams = 1, # how many teams start
                    numTeamsChanges = TRUE, # does the number of teams change based on number of dogs captured
                    seasonStartDay = 225, # when does breeding season start
                    maleNeuterImpact = 0.15 # association between male sterilisation coverage and female pregnancy rate
                    ){ 
  
  
  pregnantCompartment <- rep(proportionImpregnated, pregnancy)
  nonPregnantCompartment <- rep(1 - proportionImpregnated, nonpregnant)
  heatCompartment <- heatDuration
  anoestrusCompartment <- anoestrusPeriod
  
  # Seasonal indicator for each day of the simulation. TRUE indicates days 
  # when the rate of transition from anoestrus to heat is multiplied by 
  # seasonUplift.
  inseason <- rep((1:365) %in% seasonStartDay : (seasonStartDay + seasonDuration), nYears) # first year will not see tail end of increase in preg rates if season start day is later in year
  
  # Total number of simulated days. In the current structure, nYears already includes the warm-up period.
  time <- nYears * 365
  warm_up <- 6 # number of years to run the model before any neutering is started - used later in function
  
  # DEFINE OUTPUTS
  outdf <- data.frame("time" = time, 
                      "cFemalePup" = NA,
                      "cMalePup" = NA,
                      "cFemaleJuvenile" = NA,
                      "cMaleJuvenile" = NA,
                      "cHeat" = NA,
                      "cPregnant" = NA,
                      "cNonPregnant" = NA,
                      "cAnoestrus" = NA,
                      "cNeutered" = NA,
                      "cAdultMale" = NA,
                      "cAdultMaleNeutered" = NA,
                      "rFemalePup" = NA,
                      "rMalePup" = NA,
                      "rFemaleJuvenile" = NA,
                      "rMaleJuvenile" = NA,
                      "rHeat" = NA,
                      "rPregnant" = NA,
                      "rNonPregnant" = NA,
                      "rAnoestrus" = NA,
                      "rNeutered" = NA,
                      "rAdultMale" = NA,
                      "rAdultMaleNeutered" = NA,
                      "pupTurfOutMale" = NA,
                      "pupTurfOutFemale" = NA,
                      "juvenileFemaleTurfOut" = NA,
                      "juvenileMaleTurfOut" = NA,
                      "strayPupMortalityMale" = NA,
                      "strayPupMortalityFemale" = NA,
                      "strayAdultFemaleMortality" = NA,
                      "strayAdultMaleMortality" = NA,
                      "Season" = inseason,
                      "currOwnedPop" = NA,
                      "currStrayPop" = NA,
                      "sPupsBorn" = NA, # HF added this line onwards
                      "oPupsBorn" = NA,
                      'speysO' = NA,
                      'castratesO' = NA,
                      'speysS' = NA,
                      'castratesS' = NA,
                      'pupMortalityOwned' = NA,
                      "adultFemaleMortalityRateOwned" = NA,
                      "adultMaleMortalityRateOwned" = NA,
                      'nYears' = NA,
                      'speying' = NA, # So are you going to have speying (not by default syeying starts after four years)
                      'speyingR' = NA, # Do you spey stray dogs
                      'speyingC' = NA, # Do you spey owned dogs
                      'speyingMales' = NA, # Do you spey males
                      'speyingStart' = NA, # Start DAY for speying - can be a vector for multiple speying campaigns
                      'speyingInterval' = NA, # Interval between speying rounds
                      'speyingRepeat' = NA,
                      'speyingDuration' = NA, # How long do you cook them for?
                      'speyedPerDay' = NA, # max number of dogs possible to catch in one day
                      'speyingTargetR' = NA, # Target for speying stray dogs
                      'speyingTargetC' = NA, # Target for speying owned dogs
                      'totalNeuterCap' = NA, # cap for the number of neuters to be performed
                      'heatNeuters' = NA, # number of bitches in heat neutered
                      'pregNeuters' = NA, # number of pregnant bitches neutered
                      'neuterBreak' = NA, # number of days (+1) that neutering is stopped for after they only catch minCatch number of dogs
                      'minCatch' = NA, # below this number of dogs is not finacially viable so take a neuterBreak after this happens
                      'inneuter' = NA, # whether neutering happened on that day or not
                      'numTeams' = NA, # number of catching teams deployed
                      'numTeamsChanges' = NA, # if the model is allowed to increase the number of teams - i.e. is there vet capacity?
                      'numTeamsOrig' = NA, # original and max number of teams allowed - it is whatever numTeams starts as
                      'seasonStartDay' = NA, # the day of the year that the season starts on 225 is goa date (must be 1-365)
                      'maleNeuterImpact' = NA # the impact of male sterilisation on pregnancy rates 0.25 means 1 fewer pregnant female per 4 extra sterilised dogs
                      )
  # currOwnedPop and currStrayPop are updated at the start of each daily time step. 
  # They are used to calculate density-dependent abandonment and mortality.
  currOwnedPop <- ownedPop
  currStrayPop <- strayPop
  
  # DEFINE INTERVENTION SCHEDULE # -----------------------------------------------------------------------
  
  numTeamsOrig <- numTeams # set numTeamsOrig so it records the original max number of teams

  if(!is.na(speyingInterval)) # this clause will break if speyingInterval is NA
    
    inneuter <- c(rep(FALSE, speyingStart-1),# delay speying until the speyingStart day (-1 so it neutering starts on speyingStart day)
                  rep(TRUE, speyingDuration),
                  rep(c(rep(FALSE, speyingInterval), rep(TRUE, speyingDuration)), 
                      length.out = (nYears * 365)-((speyingStart-1)+speyingDuration)) # repeat till the end of sim
    )
  
  # inneuter is a logical vector indicating whether sterilisation is scheduled 
  # on each day. The intervention schedule is created after an initial warm-up 
  # period with no neutering.
  # speyingStart only dictates the first speying operation - timings need to be calculated if the same month needs to start neutering in each year
  inneuter <- c(rep(FALSE,(365*warm_up)), inneuter) # add on no neutering for 6 years instead of replacing, this maintains the speyingStart where speyingInterval is defined
  
  
  # if speyingInterval is defined (i.e. NOT NA)
  
  if(!is.na(speyingRepeat)){
    # Build speying sequence starting on speyingStart
    speySeq <- c(rep(FALSE, speyingStart-1),# delay speying until the speyingStart day (-1 so it neutering starts on speyingStart day)
                 rep(c(rep(TRUE, speyingDuration), rep(FALSE, speyingInterval)), times = speyingRepeat)) 
    # when speyingInterval is 0, this is continuous, which is required for the catch return guided - i.e. for minCatch and neuterBreak to work as they should
    
    # Add warm-up (no neutering)
    inneuter <- c(rep(FALSE,(365*warm_up)), speySeq) # add on no neutering for 6 years
    # Pad the rest of the simulation with FALSE
    total_days <- (warm_up + nYears) * 365
    
    if(length(inneuter) < total_days){
      inneuter <- c(inneuter, rep(FALSE, total_days - length(inneuter)))
    }
    
    # Truncate if too long
    inneuter <- inneuter[1:total_days]
    
  } #end of loop: if speying repeat is defined
  
  # These store female neuter coverage at the start of each sterilisation 
  # period and are used to calculate the daily rate required to reach the 
  # target coverage over speyingDuration days.
   neuteredFemaleOwnedProp <- 0
   neuteredFemaleStrayProp <- 0

# DAILY TIMESTEP # ----------------------------------------------------------
   
  for(i in 1 : time){
    # ABANDONMENT
    totalPups <- sum(ownedCompartmentPupsFemale) +  sum(ownedCompartmentPupsMale)
    pupTurfOutMale <- turfOutFun(pupMortalityOwned, TurfOutCarryingOwned, totalPups / pupCeiling)
    pupTurfOutFemale <- turfOutFun(pupMortalityOwned, TurfOutCarryingOwned, totalPups / pupCeiling) * femaleTurfOutAdjustment
    juvenileFemaleTurfOut <- turfOutFun(adultFemaleMortalityRateOwned, TurfOutCarryingOwned, currOwnedPop / ownedPop) * femaleTurfOutAdjustment
    juvenileMaleTurfOut <- turfOutFun(adultMaleMortalityRateOwned, TurfOutCarryingOwned, currOwnedPop / ownedPop) # ownedPop defined above this loop so currOwnedPop will change with time
    
    # MORTALITY
    strayPupMortalityMale <- mortalityFun(pupMortality, mortalityCarryingStray, currStrayPop / strayPop)
    strayPupMortalityFemale <- mortalityFun(pupMortality, mortalityCarryingStray, currStrayPop / strayPop) * femalePupMortalityUplift
    strayAdultFemaleMortality <- mortalityFun(adultFemaleMortalityRateStray, mortalityCarryingStray, currStrayPop / strayPop)
    strayJuvenileFemaleMortality <- strayAdultFemaleMortality * femalePupMortalityUplift
    strayAdultMaleMortality <- mortalityFun(adultMaleMortalityRateStray, mortalityCarryingStray, currStrayPop / strayPop)
    
    # CALCULATE POPULATION RUNNING TOTALS
    adultFemalesOwned <- heatCompartmentOwned + sum(pregnantCompartmentOwned) + nonPregnantCompartmentOwned + anoestrusCompartmentOwned + neuteredCompartmentOwned
    adultFemalesStray <- heatCompartmentStray + sum(pregnantCompartmentStray) + nonPregnantCompartmentStray + anoestrusCompartmentStray +
      neuteredCompartmentStray
    
    # CALCULATE FEMALE NEUTER COVERAGE (PROPORTION NEUTERED) TO DEFINE SPEYING RATE
    if(i>1){
      if(inneuter[i] & !inneuter[i-1]){ 
        neuteredFemaleOwnedProp <- neuteredCompartmentOwned / adultFemalesOwned
        neuteredFemaleStrayProp <- neuteredCompartmentStray / adultFemalesStray
      }
    }
    adultMalesOwned <- ownedCompartmentAdultMale + ownedCompartmentAdultMaleNeutered
    adultMalesStray <- strayCompartmentAdultMale + strayCompartmentAdultMaleNeutered
    
    # IF SPEYED PER DAY IS NOT DEFINED THEN THIS SETS SPEYING RATE BASED ON TARGET NEUTER COVERAGE (so calculate current neuter coverage to see how far we need to go)
    speyingRateR <- 1 - (1 - speyingTargetR + neuteredFemaleStrayProp) ^ (1 / speyingDuration)
    speyingRateC <- 1 - (1 - speyingTargetC + neuteredFemaleOwnedProp) ^ (1 / speyingDuration)

    # Divide allocation of neuters (speyed per day) proportionally between groups
    if(!is.na(speyedPerDay)){
      currOwnedPopExPups <- currOwnedPop -
        sum(ownedCompartmentPupsFemale) -
        sum(ownedCompartmentPupsMale)
      
      currStrayPopExPups <- currStrayPop -
        sum(strayCompartmentPupsFemale) -
        sum(strayCompartmentPupsMale)
      
      # If speyed per day is defined, rate is based on number of teams and numbers per day
      # other definition is if speys per day are not defined and rate is based on target neuter coverage
      speyingRateC <- (speyedPerDay * numTeams) * (currOwnedPopExPups / (currOwnedPopExPups + currStrayPopExPups)) # This divides sterilisations between owned and stray
      
      # PROPORTIONALLY ALLOCATE SPEYS TO OWNED AND STRAY GROUPS
      # when Owned dogs are not being neutered, all the capacity is allocated to the stray population (not the case vice versa for owned as that scenario not relevant)
      if(speyingC == TRUE){
        speyingRateR <- (speyedPerDay * numTeams) * (currStrayPopExPups / (currOwnedPopExPups + currStrayPopExPups)) # This divides the spey between owned and stray
      }else{
        
      # OR ALLOCATE ALL STERILISATIONS TO STRAY DOGS IF NO OWNED DOGS BEING SPEYED
        speyingRateR <- speyedPerDay * numTeams
      }
      
      speyingRateC <- speyingRateC / (currOwnedPopExPups) # This then gives us a rate
      speyingRateR <- speyingRateR / (currStrayPopExPups)
    }

    # Stop sterilisations if cumulative number of sterilisations has reached totalNeuterCap
    if(!is.na(totalNeuterCap) & sum(outdf$speysO,
          outdf$castratesO,
          outdf$speysS,
          outdf$castratesS, na.rm = TRUE) >= totalNeuterCap) {inneuter[i]<- FALSE}
    
    if(!is.na(speyedPerDay)){
    
      numNeuters <- sum(outdf$speysO[i-1],
        outdf$castratesO[i-1],
        outdf$speysS[i-1],
        outdf$castratesS[i-1], na.rm = TRUE)
    
      # CATCH-RETURN-GUIDED TEAM ADJUSTMENT AND BREAKS # --------------------------------------------------------------------- 
      # If daily capacity is specified, the previous day's catch is used to 
      # adapt the number of teams and/or pause catching for a number of 
      # days defined by neuterBreak.
    
    if(numTeamsChanges){
    if(i>1 & numTeams < numTeamsOrig) # makes sure the number of teams never exceeds that which were first deployed
      {
      if(inneuter[i-1] == TRUE & (numNeuters/numTeams) >= (speyedPerDay*0.9)) {numTeamsNew <- numTeams +1} 
      # add a team if they maxed out the number of dogs they could catch day before
    }
     
      # numTeams is decreased if not enough dogs picked up the previous day, based on minCatch
      # Where there is more than 1 catching team and each team catches less than 10 dogs, the number of teams is reduced by one for the next day
      if(i>1 & numTeams > 1){
        
        if(inneuter[i-1] == TRUE & (numNeuters/numTeams) < 10) {numTeamsNew <- numTeams -1}
        
      } 
      
    }
      
    # If fewer than minCatch dogs were caught on the previous neutering day, 
      # pause neutering for neuterBreak days. With minCatch = 0 this rule is 
      # effectively disabled.
    
    if(i>1){
        if(inneuter[i-1] == TRUE & numNeuters < minCatch) {inneuter[i:(i+neuterBreak)]<- FALSE}
      }
    }
    
    # CALCULATE ALL NEUTER COVERAGES # -------------------------------------------------------
    
    # Neuter coverage is calculated as the proportion neutered among adult 
    # plus juvenile dogs of the relevant sex and ownership/roaming group. 
    # Female neutered compartments contain adult and juvenile females that 
    # have been sterilised and moved into the neutered adult-like compartment.
    # N.B. neuteredCompartmentOwned == owned neutered females
    strayFemaleNC <- neuteredCompartmentStray / (adultFemalesStray + sum(strayCompartmentJuvenileFemale))
    ownedFemaleNC <- neuteredCompartmentOwned / (adultFemalesOwned + sum(ownedCompartmentJuvenileFemale))
    strayMaleNC <- strayCompartmentAdultMaleNeutered / (adultMalesStray + sum(strayCompartmentJuvenileMale))
    ownedMaleNC <- ownedCompartmentAdultMaleNeutered / (adultMalesOwned + sum(ownedCompartmentJuvenileMale))
    
    # Daily capture/neutering rates are reduced as neuter coverage increases, 
    # and set to zero when the relevant group is not targeted, neutering is 
    # not scheduled, or the group-specific coverage cap has been reached.
    # female owned rate based on female owned neuter coverage
    
    cNeuterRateC <- ifelse(speyingC & inneuter[i] & ownedFemaleNC < ownedFemaleNCcap, 
                           speyingRateC * (1 - (ownedFemaleNC)), 0) # higher the neuter coverage, the lower the speying rate
    cNeuterRateR <- ifelse(speyingR & inneuter[i] & strayFemaleNC < strayFemaleNCcap,
                           speyingRateR * (1 - (strayFemaleNC)), 0)
    cNeuterRateCMale <- ifelse(speyingMales, ifelse(speyingC & inneuter[i] & ownedMaleNC < ownedMaleNCcap,
                                                    speyingRateC * (1 - (ownedMaleNC)), 0), 0)
    cNeuterRateRMale <- ifelse(speyingMales, ifelse(speyingR & inneuter[i] & strayMaleNC < strayMaleNCcap, 
                                                    speyingRateR * (1 - (strayMaleNC)), 0), 0)

    # Global switch for sterilisation intervention
    # speying is defined as 0 (no intervention) or 1 (sterilisations)
    cNeuterRateC <- cNeuterRateC * speying
    cNeuterRateR <- cNeuterRateR * speying
    cNeuterRateCMale <- cNeuterRateCMale * speying
    cNeuterRateRMale <- cNeuterRateRMale * speying
    
    # Male sterilisation reduces the probability that females become pregnant. 
    # The reduction is proportional to the fraction of all adult males that are 
    # neutered, scaled by maleNeuterImpact (called male sterilisation impact in paper). 
    # If no males are neutered, this is 1. 
    # With maleNeuterImpact = 0.25, full male neutering would reduce the 
    # pregnancy transition multiplier by 25%.

    mNeuterRateAdj <- 1 - ((ownedCompartmentAdultMaleNeutered + strayCompartmentAdultMaleNeutered) / 
      (ownedCompartmentAdultMaleNeutered + strayCompartmentAdultMaleNeutered + ownedCompartmentAdultMale + strayCompartmentAdultMale) * maleNeuterImpact)
    
    # CALCULATE MOVEMENT BETWEEN SUBPOPULATIONS
    # Owned population -----------------------------------------------------
    
    ownedCompartmentPupsMale1 <- c(pregnantCompartmentOwned[pregnancy] * pupsPerLitter * proportionMalePups,
                                      ownedCompartmentPupsMale[1:pupDuration - 1] - 
                                        ownedCompartmentPupsMale[1:pupDuration - 1] * pupMortalityOwned -
                                        ownedCompartmentPupsMale[1:pupDuration - 1] * pupTurfOutMale)
    
    ownedCompartmentPupsFemale1 <- c(pregnantCompartmentOwned[pregnancy] * pupsPerLitter * (1 - proportionMalePups),
                                        ownedCompartmentPupsFemale[1:pupDuration - 1] - 
                                          ownedCompartmentPupsFemale[1:pupDuration - 1] * pupMortalityOwned -
                                          ownedCompartmentPupsFemale[1:pupDuration - 1] * pupTurfOutFemale)
    
    ownedCompartmentJuvenileMale1 <- c(ownedCompartmentPupsMale[pupDuration], 
                                          ownedCompartmentJuvenileMale[1:juvenileDuration - 1] - 
                                          ownedCompartmentJuvenileMale[1:juvenileDuration - 1] * adultMaleMortalityRateOwned -
                                         ownedCompartmentJuvenileMale[1:juvenileDuration - 1] * juvenileMaleTurfOut -
                                         ownedCompartmentJuvenileMale[1:juvenileDuration - 1] * cNeuterRateCMale)
    
    ownedCompartmentJuvenileFemale1 <- c(ownedCompartmentPupsFemale[pupDuration], 
                                            ownedCompartmentJuvenileFemale[1:juvenileDuration - 1] - 
                                              ownedCompartmentJuvenileFemale[1:juvenileDuration - 1] * adultFemaleMortalityRateOwned -
                                           ownedCompartmentJuvenileFemale[1:juvenileDuration - 1] * juvenileFemaleTurfOut -
                                           ownedCompartmentJuvenileFemale[1:juvenileDuration - 1] * cNeuterRateC)
    
    ownedCompartmentAdultMale1 <- ownedCompartmentAdultMale +
      ownedCompartmentJuvenileMale[juvenileDuration] -
      ownedCompartmentAdultMale * adultMaleMortalityRateOwned -
      ownedCompartmentAdultMale * cNeuterRateCMale
  
    ownedCompartmentAdultMaleNeutered1 <- ownedCompartmentAdultMaleNeutered -
      ownedCompartmentAdultMaleNeutered * adultMaleMortalityRateOwned + 
      ownedCompartmentAdultMale * cNeuterRateCMale +
      sum(ownedCompartmentJuvenileMale[1:juvenileDuration - 1] * cNeuterRateCMale)
      
    heatCompartmentOwned1 <- heatCompartmentOwned +
      ifelse(inseason[i], anoestrusCompartmentOwned * (1 / anoestrusPeriod) * seasonUplift, anoestrusCompartmentOwned * (1 / anoestrusPeriod) * mNeuterRateAdj) -
      heatCompartmentOwned * (1 / heatDuration) -
      heatCompartmentOwned * adultFemaleMortalityRateOwned +
      ownedCompartmentJuvenileFemale[juvenileDuration] -
      heatCompartmentOwned * cNeuterRateC
    
    pregnantCompartmentOwned1 <- c(heatCompartmentOwned * (1 / heatDuration) * proportionImpregnated *  mNeuterRateAdj, pregnantCompartmentOwned[1 : (length(pregnantCompartmentOwned) - 1)]) -
      pregnantCompartmentOwned * adultFemaleMortalityRateOwned -
      pregnantCompartmentOwned * cNeuterRateC
    
    nonPregnantCompartmentOwned1 <- heatCompartmentOwned * (1 / heatDuration) * (1 - proportionImpregnated * mNeuterRateAdj) +  nonPregnantCompartmentOwned * (1 - (1 / nonpregnant)) -
      nonPregnantCompartmentOwned * adultFemaleMortalityRateOwned -
      nonPregnantCompartmentOwned * cNeuterRateC
    
    anoestrusCompartmentOwned1 <- anoestrusCompartmentOwned + 
      pregnantCompartmentOwned[length(pregnantCompartmentOwned)] +
      nonPregnantCompartmentOwned * (1 / nonpregnant) -
      ifelse(inseason[i], anoestrusCompartmentOwned * (1 / anoestrusPeriod) * seasonUplift, anoestrusCompartmentOwned * (1 / anoestrusPeriod) * mNeuterRateAdj) -
      anoestrusCompartmentOwned * adultFemaleMortalityRateOwned -
      anoestrusCompartmentOwned * cNeuterRateC
    
    neuteredCompartmentOwned1 <- neuteredCompartmentOwned - 
      neuteredCompartmentOwned * adultFemaleMortalityRateOwned +
      heatCompartmentOwned * cNeuterRateC +
      sum(pregnantCompartmentOwned * cNeuterRateC) + 
      nonPregnantCompartmentOwned * cNeuterRateC + 
      anoestrusCompartmentOwned * cNeuterRateC +
      sum(ownedCompartmentJuvenileFemale[1:juvenileDuration - 1] * cNeuterRateC)
    
    # number of owned dog speys
    femalesNeuteredO <- heatCompartmentOwned * cNeuterRateC +
      sum(pregnantCompartmentOwned * cNeuterRateC) + 
      nonPregnantCompartmentOwned * cNeuterRateC + 
      anoestrusCompartmentOwned * cNeuterRateC +
      sum(ownedCompartmentJuvenileFemale * cNeuterRateC)
    
    # number of owned castrates in each day
    malesNeuteredO <- ownedCompartmentAdultMale * cNeuterRateCMale +
      sum(ownedCompartmentJuvenileMale * cNeuterRateCMale)
    
    # Stray populations -----------------------------------------------------
    
    
    strayCompartmentPupsMale1 <- c(pregnantCompartmentStray[pregnancy] * pupsPerLitter * proportionMalePups,
                                     strayCompartmentPupsMale[1:pupDuration - 1] - 
                                       strayCompartmentPupsMale[1:pupDuration - 1] * strayPupMortalityMale +
                                       ownedCompartmentPupsMale[1:pupDuration - 1] * pupTurfOutMale)
    
    strayCompartmentPupsFemale1 <- c(pregnantCompartmentStray[pregnancy] * pupsPerLitter * (1 - proportionMalePups),
                                       strayCompartmentPupsFemale[1:pupDuration - 1] - 
                                         strayCompartmentPupsFemale[1:pupDuration - 1] * strayPupMortalityFemale +
                                         ownedCompartmentPupsFemale[1:pupDuration - 1] * pupTurfOutFemale)
    
    strayCompartmentJuvenileMale1 <- c(strayCompartmentPupsMale[pupDuration], 
                                         strayCompartmentJuvenileMale[1:juvenileDuration - 1] - 
                                           strayCompartmentJuvenileMale[1:juvenileDuration - 1] * strayAdultMaleMortality +
                                         ownedCompartmentJuvenileMale[1:juvenileDuration - 1] * juvenileMaleTurfOut -
                                         strayCompartmentJuvenileMale[1:juvenileDuration - 1] * cNeuterRateRMale)
    
    strayCompartmentJuvenileFemale1 <- c(strayCompartmentPupsFemale[pupDuration], 
                                           strayCompartmentJuvenileFemale[1:juvenileDuration - 1] - 
                                             strayCompartmentJuvenileFemale[1:juvenileDuration - 1] * strayJuvenileFemaleMortality +
                                           ownedCompartmentJuvenileFemale[1:juvenileDuration - 1] * juvenileFemaleTurfOut -
                                           strayCompartmentJuvenileFemale[1:juvenileDuration - 1] * cNeuterRateR)
    
    strayCompartmentAdultMale1 <- strayCompartmentAdultMale +
      strayCompartmentJuvenileMale[juvenileDuration] -
      strayCompartmentAdultMale * strayAdultMaleMortality -
      strayCompartmentAdultMale * cNeuterRateRMale
  
    strayCompartmentAdultMaleNeutered1 <- strayCompartmentAdultMaleNeutered -
      strayCompartmentAdultMaleNeutered * strayAdultMaleMortality + # potential issue
      strayCompartmentAdultMale * cNeuterRateRMale +
      sum(strayCompartmentJuvenileMale[1:juvenileDuration - 1] * cNeuterRateRMale)
    
    heatCompartmentStray1 <- heatCompartmentStray +
      ifelse(inseason[i], anoestrusCompartmentStray * (1 / anoestrusPeriod) * seasonUplift, anoestrusCompartmentStray * (1 / anoestrusPeriod) * mNeuterRateAdj) -
      heatCompartmentStray * (1 / heatDuration) -
      heatCompartmentStray * strayAdultFemaleMortality +
      strayCompartmentJuvenileFemale[juvenileDuration] -
      heatCompartmentStray * cNeuterRateR
    
    pregnantCompartmentStray1 <- c(heatCompartmentStray * (1 / heatDuration) * proportionImpregnated  * mNeuterRateAdj, pregnantCompartmentStray[1 : (length(pregnantCompartmentStray) - 1)]) -
      pregnantCompartmentStray * strayAdultFemaleMortality -
      pregnantCompartmentStray * cNeuterRateR
    
    nonPregnantCompartmentStray1 <- heatCompartmentStray * (1 / heatDuration) * (1 - proportionImpregnated * mNeuterRateAdj) +  nonPregnantCompartmentStray * (1-(1/nonpregnant)) -
      nonPregnantCompartmentStray * strayAdultFemaleMortality - 
      nonPregnantCompartmentStray * cNeuterRateR
    
    anoestrusCompartmentStray1 <- anoestrusCompartmentStray + 
      pregnantCompartmentStray[length(pregnantCompartmentStray)] +
      nonPregnantCompartmentStray * (1 / nonpregnant) -
      ifelse(inseason[i], anoestrusCompartmentStray * (1 / anoestrusPeriod) * seasonUplift, anoestrusCompartmentStray * (1 / anoestrusPeriod) * mNeuterRateAdj) -
      anoestrusCompartmentStray * strayAdultFemaleMortality -
      anoestrusCompartmentStray * cNeuterRateR
    
    neuteredCompartmentStray1 <- neuteredCompartmentStray -
      neuteredCompartmentStray * strayAdultFemaleMortality + 
      heatCompartmentStray * cNeuterRateR +
      sum(pregnantCompartmentStray * cNeuterRateR) + 
      nonPregnantCompartmentStray * cNeuterRateR + 
      anoestrusCompartmentStray * cNeuterRateR +
      sum(strayCompartmentJuvenileFemale[1:juvenileDuration - 1] * cNeuterRateR)
    
    # number of stray speys per day
    femalesNeuteredS <- heatCompartmentStray * cNeuterRateR +
      sum(pregnantCompartmentStray * cNeuterRateR) + 
      nonPregnantCompartmentStray * cNeuterRateR + 
      anoestrusCompartmentStray * cNeuterRateR +
      sum(strayCompartmentJuvenileFemale * cNeuterRateR) 
    
    # number of stray males neutered per day
    malesNeuteredS <- strayCompartmentAdultMale * cNeuterRateRMale +
      sum(strayCompartmentJuvenileMale * cNeuterRateRMale)
    
    # OUTPUT ----------------------------------------------------------------------------
    # outdf stores compartment sizes, daily births, daily neuters, mortality 
    # rates, abandonment rates and intervention parameters for each simulated 
    # day. Values are stored before compartments are updated to the next day. # 
    # After output is recorded, all compartment values are replaced by their 
    # next-day values, and current owned/stray population totals are recalculated. # 
    # If numTeamsNew was set during the catch-return-guided logic, numTeams is 
    # updated after the current day's output has been recorded. numTeams Orig 
    # stores the original starting number of teams
    
    outdf[i,] <- c(i, sum(ownedCompartmentPupsFemale) , 
                   sum(ownedCompartmentPupsMale) , 
                   sum(ownedCompartmentJuvenileFemale) , 
                   sum(ownedCompartmentJuvenileMale) , 
                   heatCompartmentOwned ,
                   sum(pregnantCompartmentOwned) ,
                   nonPregnantCompartmentOwned ,
                   anoestrusCompartmentOwned ,
                   neuteredCompartmentOwned,
                   ownedCompartmentAdultMale,
                   ownedCompartmentAdultMaleNeutered,
                   sum(strayCompartmentPupsFemale) , 
                   sum(strayCompartmentPupsMale) , 
                   sum(strayCompartmentJuvenileFemale) , 
                   sum(strayCompartmentJuvenileMale) , 
                   heatCompartmentStray ,
                   sum(pregnantCompartmentStray) ,
                   nonPregnantCompartmentStray ,
                   anoestrusCompartmentStray ,
                   neuteredCompartmentStray,
                   strayCompartmentAdultMale,
                   strayCompartmentAdultMaleNeutered,
                   pupTurfOutMale,
                   pupTurfOutFemale,
                   juvenileFemaleTurfOut,
                   juvenileMaleTurfOut,
                   strayPupMortalityMale,
                   strayPupMortalityFemale,
                   strayAdultFemaleMortality,
                   strayAdultMaleMortality,
                   inseason[i],
                   currOwnedPop,
                   currStrayPop,
                   pregnantCompartmentStray[pregnancy] * pupsPerLitter, # hf added - pups born that day r
                   pregnantCompartmentOwned[pregnancy] * pupsPerLitter, # hf added - pups born that day c
                   femalesNeuteredO,
                   malesNeuteredO,
                   femalesNeuteredS,
                   malesNeuteredS,
                   pupMortalityOwned,
                   adultFemaleMortalityRateOwned,
                   adultMaleMortalityRateOwned,
                   nYears,
                   speying, 
                   speyingR, # Do you spey stray dogs
                   speyingC, # Do you spey owned dogs
                   speyingMales, # Do you spey males
                   speyingStart, # Start day for speying - can be a vector for multiple speying campaigns
                   speyingInterval, # Interval between speying rounds
                   speyingRepeat,
                   speyingDuration, 
                   speyedPerDay,
                   speyingTargetR, 
                   speyingTargetC,
                   totalNeuterCap,
                   (heatCompartmentOwned * cNeuterRateC)+(heatCompartmentStray * cNeuterRateR),
                   sum(pregnantCompartmentOwned * cNeuterRateC)+sum(pregnantCompartmentStray * cNeuterRateR),
                   neuterBreak,
                   minCatch,
                   inneuter[i],
                   numTeams,
                   numTeamsChanges,
                   numTeamsOrig,
                   seasonStartDay,
                   maleNeuterImpact
    )  
    
    # UPDATE SUBPOPULATION VALUES FOR NEXT TIMESTEP ---------------------------------------------------------------------
    ownedCompartmentPupsFemale <- ownedCompartmentPupsFemale1
    ownedCompartmentPupsMale <- ownedCompartmentPupsMale1
    ownedCompartmentJuvenileMale <- ownedCompartmentJuvenileMale1
    ownedCompartmentJuvenileFemale <- ownedCompartmentJuvenileFemale1
    heatCompartmentOwned <- heatCompartmentOwned1
    pregnantCompartmentOwned <- pregnantCompartmentOwned1
    nonPregnantCompartmentOwned <- nonPregnantCompartmentOwned1
    anoestrusCompartmentOwned <- anoestrusCompartmentOwned1
    neuteredCompartmentOwned <- neuteredCompartmentOwned1
    ownedCompartmentAdultMale <- ownedCompartmentAdultMale1
    ownedCompartmentAdultMaleNeutered <- ownedCompartmentAdultMaleNeutered1
    
    strayCompartmentPupsFemale <- strayCompartmentPupsFemale1
    strayCompartmentPupsMale <- strayCompartmentPupsMale1
    strayCompartmentJuvenileMale <- strayCompartmentJuvenileMale1
    strayCompartmentJuvenileFemale <- strayCompartmentJuvenileFemale1
    heatCompartmentStray <- heatCompartmentStray1
    pregnantCompartmentStray <- pregnantCompartmentStray1
    nonPregnantCompartmentStray <- nonPregnantCompartmentStray1
    anoestrusCompartmentStray <- anoestrusCompartmentStray1
    neuteredCompartmentStray <- neuteredCompartmentStray1
    strayCompartmentAdultMale <- strayCompartmentAdultMale1
    strayCompartmentAdultMaleNeutered <- strayCompartmentAdultMaleNeutered1
    
    
    currOwnedPop <- sum(ownedCompartmentPupsFemale) + 
      sum(ownedCompartmentPupsMale) + 
      sum(ownedCompartmentJuvenileFemale) + 
      sum(ownedCompartmentJuvenileMale) + 
      heatCompartmentOwned +
      sum(pregnantCompartmentOwned) +
      nonPregnantCompartmentOwned +
      anoestrusCompartmentOwned +
      ownedCompartmentAdultMale +
      neuteredCompartmentOwned +
      ownedCompartmentAdultMaleNeutered
      
    
    currStrayPop <- sum(strayCompartmentPupsFemale) + 
      sum(strayCompartmentPupsMale) + 
      sum(strayCompartmentJuvenileFemale) + 
      sum(strayCompartmentJuvenileMale) + 
      heatCompartmentStray +
      sum(pregnantCompartmentStray) +
      nonPregnantCompartmentStray +
      anoestrusCompartmentStray +
      strayCompartmentAdultMale +
      neuteredCompartmentStray+
      strayCompartmentAdultMaleNeutered
    
    
    if(exists('numTeamsNew')){
      numTeams <- numTeamsNew # update after both checks and after output dataframe has been filled in for this loop
      rm(numTeamsNew) # so it doesn't exist for next loop
    }
    
  }
  return(outdf)
}
