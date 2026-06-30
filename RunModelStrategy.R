
# Example code to run multiple strategies on HPC # ------------------------------------------------------------

# Author: Helen Fielding

# if (!requireNamespace("tidyverse", quietly = TRUE)) {
#   install.packages("tidyverse")
# }

# Load packages
library(dplyr)
library(ggplot2)

source("/DPMstrategies/input/model_function_speyFun.R")
source('/DPMstrategies/input/OutputFunctions.R')

########
##Args##
########
args <- commandArgs(trailingOnly = TRUE)
##Root ID
j <- as.integer(args[1])
print(j)
# j <- 1 # use this interactively

path <- '/DPMstrategies/input/'
output_path <- '/DPMstrategies/hfieldin/output/'

strategyDF <- readRDS(paste0(path, 'strategyDF_2025.rds'))

# Load metrics from baseline scenario - for plots?
pupDeathsDF <- readRDS(paste0(path, 'pupDeathsDF_baseline.rds'))
metrics_summary_DF <- readRDS(paste0(path, 'metrics_summary_DF_baseline.rds'))
lifeExpDF <- readRDS(paste0(path, 'lifeExpDF_baseline.rds'))


neuterDF <- speyFun(nYears = strategyDF$nYears[j], 
                    speying = strategyDF$speying[j], 
                    speyingR = strategyDF$speyingR[j], 
                    speyingC = strategyDF$speyingC[j],
                    speyingStart = strategyDF$speyingStart[j], 
                    speyedPerDay = strategyDF$speyedPerDay[j], 
                    speyingDuration = strategyDF$speyingDuration[j],
                    speyingRepeat = strategyDF$speyingRepeat[j],
                    speyingInterval = strategyDF$speyingInterval[j],
                    speyingTargetR = strategyDF$speyingTargetR[j],
                    speyingTargetC = strategyDF$speyingTargetC[j],
                    speyingMales = strategyDF$speyingMales[j],
                    totalNeuterCap = strategyDF$totalNeuterCap[j],
                    minCatch = strategyDF$minCatch[j],
                    neuterBreak = strategyDF$neuterBreak[j],
                    numTeams = strategyDF$numTeams[j], 
                    numTeamsChanges = strategyDF$numTeamsChanges[j],
                    maleNeuterImpact = strategyDF$mni[j])

saveRDS(neuterDF, paste0(output_path, 'NeuterDFs/neuterDF_', strategyDF$serial[j], '_', strategyDF$i[j], '.rds'))

print(paste('output saved', strategyDF$i[j], Sys.time()))
