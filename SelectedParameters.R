#License: GPL-3.0

#If you use this model or code in academic work, please cite:
  
#  Fielding H.R. [2026]. [DogPopSim]. GitHub. [URL/DOI]

# This code is made available under the GNU GPLv3. Commercial use
# is not prohibited by this licence, but redistribution of modified 
# versions must comply with the GPLv3 terms, including preservation 
# of copyright and licence notices and release of corresponding 
# source code.

# Model author: Paul Bessell

## MODEL TIME STRUCTURE AND AGE-CLASS DURATIONS # ------------------------------------------------------------------------- 
# The model runs in daily time steps. Pups remain in pup compartments for # pupDuration days before entering juvenile compartments; juveniles remain in 
# juvenile compartments for juvenileDuration days before entering adult # compartments. 
# nYears is specified inside speyFun(); in the fitted model it includes the 
# 6-year warm-up period, 10 years of intervention/output, and one additional 
# year to capture strategies beginning late in a year.

pupDuration <- 90
juvenileDuration <- 210  

## INITIAL POPULATION SIZE AND SEX STRUCTURE # ------------------------------------------------------------------------- 
# Baseline total dog population. This is split into dependent/owned and 
# independent/stray subpopulations using propOwned.
population <- 6500
propOwned <- 1/3

# Male:female ratios used to initialise the sex structure of owned, stray, 
# and juvenile compartments. Values >1 indicate male-biased populations.
mToFOwned <- 2.5 # ratio of males to females in owned pop
mToFStray <- 1.5
mToFJuvenile <- 1.5 

## BASELINE DAILY MORTALITY RATES # ------------------------------------------------------------------------- 
# Adult mortality rates are expressed as daily probabilities/rates derived 
# from assumed mean adult lifespans in years. 
# For example, 1 / (2.5 * 365) approximates a daily mortality rate equivalent 
# to a 2.5-year mean lifespan.- Reece et al 2008
adultFemaleMortalityRateStray <- 1 / (2.5 * 365) # mortality rate per day taken from
adultMaleMortalityRateStray <- 1 / (3 * 365)
# Owned adult mortality is fixed and does not vary with population density.
adultFemaleMortalityRateOwned <- 1 / (3.5 * 365) 
adultMaleMortalityRateOwned <- 1 / (4 * 365)

# ABANDONMENT Female dogs are assumed to be abandoned/turfed out at a higher rate than 
# males, implemented by multiplying female turf-out rates by this adjustment. 
femaleTurfOutAdjustment <- 2.964 

# Proportion of pups assumed to survive the pup period. This is converted # below into a daily pup mortality rate over pupDuration days.
proprtionPupsSurvive <-0.644

# Pup mortality is derived from survival over the 90-day pup period. 
# This converts a period survival probability into an equivalent daily mortality. 
pupMortality <- -(((1 - proprtionPupsSurvive) ^ (1 / pupDuration)) - 1) 
# Owned pup mortality is lower than stray pup mortality by this adjustment. 
pupMortalityOwnedAdj <- 2.377 

pupMortalityOwned <- pupMortality / pupMortalityOwnedAdj

# Female stray pups/juveniles have higher mortality than males to align with field data, implemented 
# as a multiplier on the baseline pup/adult female mortality terms.
femalePupMortalityUplift <- 1.722

## INITIAL AGE STRUCTURE # ------------------------------------------------------------------------- 
# These values approximate the proportion of the population in juvenile-age 
# compartments based on juvenile duration and adult mortality. 
# The comments should be checked carefully: these are also used to initialise 
# pup numbers, although the variable names refer to juveniles.
FemaleJuvenileStray <- 210 / (1 / adultFemaleMortalityRateStray) # These are the proportion of adults that are juveniles, for now use the same parameter to give the proportion of the popualiton that are pups
FemaleJuvenileOwned <- 210 / (1 / adultFemaleMortalityRateOwned)

ownedPop <- population * propOwned
strayPop <- population - ownedPop

# Pup ceiling represents 12% of the initial owned population and is used as 
# the denominator in density-dependent abandonment of owned pups into the 
# stray population.
pupCeiling <- ownedPop * 0.12 # proportion of pups in the stable population

# Initial pup, juvenile and adult populations are generated separately for 
# owned and stray dogs. 
ownedPopPups <- ownedPop * FemaleJuvenileStray
strayPopPups <- strayPop * FemaleJuvenileOwned

ownedPopJuvenile <- (ownedPop - ownedPopPups) * FemaleJuvenileOwned
strayPopJuvenile <- (strayPop - strayPopPups) * FemaleJuvenileStray

ownedPopAdults <- (ownedPop - ownedPopPups) * (1 - FemaleJuvenileOwned)
strayPopAdults <- (strayPop - strayPopPups) * (1 - FemaleJuvenileStray)

## INITIAL SEX STRUCTURE # ------------------------------------------------------------------------- 
# Pups, juveniles and adults are split into male and female compartments using 
# male:female ratios from field data.

ownedPopPupsMale <- ownedPopPups * mToFJuvenile / (1 + mToFJuvenile)
ownedPopPupsFemale <- ownedPopPups / (1 + mToFOwned)

strayPopPupsMale <- strayPopPups * mToFJuvenile / (1 + mToFJuvenile)
strayPopPupsFemale <- strayPopPups / (1 + mToFJuvenile)


ownedPopJuvenileMale <- ownedPopJuvenile * mToFOwned / (1 + mToFOwned)
ownedPopJuvenileFemale <- ownedPopJuvenile / (1 + mToFOwned)

strayPopJuvenileMale <- strayPopJuvenile * mToFStray / (1 + mToFStray)
strayPopJuvenileFemale <- strayPopJuvenile / (1 + mToFStray)


ownedPopAdultMale <- ownedPopAdults * mToFOwned / (1 + mToFOwned)
ownedPopAdultFemale <- ownedPopAdults / (1 + mToFOwned)

strayPopAdultMale <- strayPopAdults * mToFStray / (1 + mToFStray)
strayPopAdultFemale <- strayPopAdults / (1 + mToFStray)
## CATCH-RETURN-GUIDED DEFAULT # ------------------------------------------------------------------------- 
# Minimum number of dogs caught on the previous day required to continue 
# catching without a break. minCatch = 0 effectively disables catch-break 
# behaviour because daily catch cannot fall below zero. 
minCatch <- 0

# Maximum attainable neuter coverage. These caps represent dogs that are 
strayFemaleNCcap <- 0.95 # unavailable for capture/neutering, uncatchable
ownedFemaleNCcap <- 0.9 # deliberately retained entire, e.g. for breeding, due to owner concerns
strayMaleNCcap <- 0.95
ownedMaleNCcap <- 0.9

# Reproductive parameters # -----------------------------------------------
heatDuration <- 9
anoestrusPeriod <- 180
pregnancy <- 63
nonpregnant <- 63
proportionImpregnated <- 0.35
proportionMalePups <- 0.5
pupsPerLitter <- 6

# Seasonality increases the rate at which females leave anoestrus and enter 
# oestrus for seasonDuration days beginning on seasonStartDay.
seasonStartDay <- 225 # day of the year when season starts i.e. more females comes into oestrus
seasonDuration <- 90
seasonUplift <- 2.5




# Population compartments ------------------------------------------------------------
# Pup and juvenile compartments are represented as vectors, with one element 
# per day of age/stage. Each day, dogs progress one position through the vector. 
# The final element graduates into the next life stage.

ownedCompartmentPupsMale <- rep(ownedPopPupsMale / pupDuration, pupDuration)
ownedCompartmentPupsFemale <- rep(ownedPopPupsFemale / pupDuration, pupDuration)
strayCompartmentPupsMale <- rep(strayPopPupsMale / pupDuration, pupDuration)
strayCompartmentPupsFemale <- rep(strayPopPupsFemale / pupDuration, pupDuration)

ownedCompartmentJuvenileMale <- rep(ownedPopJuvenileMale / juvenileDuration, juvenileDuration)
ownedCompartmentJuvenileFemale <- rep(ownedPopJuvenileFemale / juvenileDuration, juvenileDuration)
strayCompartmentJuvenileMale <- rep(strayPopJuvenileMale / juvenileDuration, juvenileDuration)
strayCompartmentJuvenileFemale <- rep(strayPopJuvenileFemale / juvenileDuration, juvenileDuration)

# Adult males are represented as single total-count (scalar) compartments, rather than
# age-structured vectors. They receive juveniles ageing into adulthood and lose
# dogs through mortality or sterilisation.

ownedCompartmentAdultMale <- ownedPopAdultMale
ownedCompartmentAdultFemale <- ownedPopAdultFemale
ownedCompartmentAdultMaleNeutered <- 0

strayCompartmentAdultMale <- strayPopAdultMale
strayCompartmentAdultFemale <- strayPopAdultFemale
strayCompartmentAdultMaleNeutered <- 0


# Female compartments -----------------------------------------------------
# Adult females are divided among heat, pregnant, non-pregnant, anoestrus and 
# neutered states. Pregnant compartments are represented as a pregnancy-length 
# vector so that births occur after a fixed gestation period.

heatCompartmentOwned <- ownedCompartmentAdultFemale * (heatDuration / 365)
nonPregnantCompartmentOwned <- ownedCompartmentAdultFemale * (nonpregnant / 365) * (1 - proportionImpregnated)
pregnantCompartmentOwned <- ownedCompartmentAdultFemale * (nonpregnant / 365) * proportionImpregnated
pregnantCompartmentOwned <- rep(pregnantCompartmentOwned / pregnancy, pregnancy)
anoestrusCompartmentOwned <- ownedCompartmentAdultFemale - sum(pregnantCompartmentOwned) - nonPregnantCompartmentOwned - heatCompartmentOwned
neuteredCompartmentOwned <- 0

heatCompartmentStray <- strayCompartmentAdultFemale * (heatDuration / 365)
nonPregnantCompartmentStray <- strayCompartmentAdultFemale * (nonpregnant / 365) * (1 - proportionImpregnated)
pregnantCompartmentStray <- strayCompartmentAdultFemale * (nonpregnant / 365) * proportionImpregnated
pregnantCompartmentStray <- rep(pregnantCompartmentStray / pregnancy, pregnancy)
anoestrusCompartmentStray <- strayCompartmentAdultFemale - sum(pregnantCompartmentStray) - nonPregnantCompartmentStray - heatCompartmentStray
neuteredCompartmentStray <- 0

# Independent/Stray threshold/carrying capacity -------------------------------------------------------

ownedThreshold <- ownedPop
strayThreshold <- strayPop


# Independent/Stray mortality function ----------------------------------------------

mortalityCarryingStray <- 0.5
mortalityFun <- function(mortality, prop = 0, adj = 0){
  adjMortality <- (mortality * (1 - prop)) + (mortality * prop * adj)
  return(adjMortality)
}


# Dependent/Owned abandonment/turf-out function ----------------------------------------------

TurfOutCarryingOwned <- 0.755
turfOutFun <- function(mortality, prop = 0, adj = 0){
  adjMortality <- (mortality * prop * adj)
    return(adjMortality)
}


