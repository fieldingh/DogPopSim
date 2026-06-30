
# OUTPUT FUNCTIONS # -------------------------------------------------------------
# functions used to gather multiple outputs of speyFun (i.e. multiple strategies) 
# summarise in one line and plot an overview of the strategy.
# 
# NB. These functions are designed to run with multiple strategy outputs from an HPC node ####

# Author: Helen Fielding

library(tidyverse)

path <- 'edit as necessary'


# mutate_cond - tidyverse add on to do conditional mutations ---------------------------------------------------------
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}
# Multiplot title - plot multiple plots ---------------------------------------------------------

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, title = "") {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else if (title == "") {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  } else {
    # Set up the page
    grid.newpage()
    #We add one row for the title
    pushViewport(
      viewport(
        layout = grid.layout(
          nrow(layout) + 1,
          ncol(layout),
          heights = c(1, rep_len(10, ncol(layout)))
        )
      )
    )
    grid.text(label = title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row + 1,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# metricsBatchFUN - make summary metrics for each strategy USED IN WORKFLOW TO GET METRICS -----------------------------------------------

metricsBatchFun <- function(neuterDF = neuterDF, i = i){
  
  output_years <- 10 # number of years to compare output after neutering has started - must be the same for each run of model
  warm_up <- 6
  
  # Estimated costs
  #sxPerDayPerVet <- ifelse(neuterDF$speyingMales[1], 15, 10) # if only doing females, can do fewer sx per day
  
  sxPerDayPerVetF <- 15
  sxPerDayPerVetM <- 30
  
  catchingTeamPerDay <- 644/5
  perVetPerDay <- 157.5/5
  clinicPerDay <- 126/5
  
  perSxF <- 8.4# surgery consumables
  perSxM <- 5.6
  
  ## Demographics
  baseline_metrics <- readRDS(paste0(path, 'baselineMetrics.rds'))%>%
    select(time, basePop = currPop)
  
  # for summary and neuter coverage plot
  metrics1 <- neuterDF%>%
    mutate(date = seq.Date(from = as.Date('2021-01-03'), length.out = n(), by = 1),
           id = i,
           desc = as.character(strategyDF$desc[j]),
           nY = nYears,
           sAdultFemale = rHeat + rPregnant + rNonPregnant + rAnoestrus + rNeutered,
           oAdultFemale = cHeat + cPregnant + cNonPregnant + cAnoestrus + cNeutered,
           oAdultMale = cAdultMale + cAdultMaleNeutered,
           sAdultMale = rAdultMale + rAdultMaleNeutered,
           s_over3 = sAdultMale + rMaleJuvenile + sAdultFemale + rFemaleJuvenile, # inc all neutered pop
           o_over3 = oAdultMale + cMaleJuvenile + oAdultFemale + cFemaleJuvenile, # inc all neutered pop
           s_JuvFemale = rFemaleJuvenile, 
           o_JuvFemale = cFemaleJuvenile, 
           s_JuvMale = rMaleJuvenile , 
           o_JuvMale = cMaleJuvenile, 
           f_nc = (rNeutered+cNeutered)/(sAdultFemale+oAdultFemale+rFemaleJuvenile+cFemaleJuvenile),
           m_nc = (cAdultMaleNeutered + rAdultMaleNeutered)/(oAdultMale + sAdultMale+cMaleJuvenile+rMaleJuvenile),
           o_nc = (cNeutered+cAdultMaleNeutered)/(oAdultMale +cMaleJuvenile+oAdultFemale+cFemaleJuvenile),
           s_nc = (rNeutered + rAdultMaleNeutered)/(sAdultFemale+ sAdultMale+rMaleJuvenile+rFemaleJuvenile),
           s_over3_FemaleDied = (sAdultFemale + rFemaleJuvenile)* strayAdultFemaleMortality,
           o_over3_FemaleDied = (oAdultFemale + cFemaleJuvenile)* adultFemaleMortalityRateOwned,
           o_over3_MaleDied = (oAdultMale + cMaleJuvenile)*adultMaleMortalityRateOwned,
           s_over3_MaleDied = (sAdultMale + rMaleJuvenile)*strayAdultMaleMortality, 
           t_over3_Died = s_over3_FemaleDied + o_over3_FemaleDied + o_over3_MaleDied + s_over3_MaleDied,
           sFemalePupsDied = rFemalePup*strayPupMortalityFemale,
           sMalePupsDied = rMalePup*strayPupMortalityMale,
           oFemalePupsDied = cFemalePup*pupMortalityOwned,
           oMalePupsDied = cMalePup*pupMortalityOwned,
           sPupsDied = sFemalePupsDied+sMalePupsDied,
           oPupsDied = oFemalePupsDied+oMalePupsDied,
           tPupsDied = sPupsDied+oPupsDied,
           oCastrates = castratesO,
           sCastrates = castratesS,
           oSpeys = speysO,
           sSpeys = speysS,
           tSpeys = oSpeys+sSpeys,
           tCastrates = oCastrates + sCastrates,
           tNeuters = tSpeys + tCastrates,
           tPupsBorn = sPupsBorn+oPupsBorn,
           currPop = currStrayPop + currOwnedPop)%>%
    mutate_at(vars(- c(f_nc, m_nc, contains('id'), desc, contains('Mortality'), contains('TurfOut'))), round, 2)%>%
    mutate(sAdults = sAdultFemale + sAdultMale,
           oAdults = oAdultFemale + oAdultMale,
           inNeuter = ifelse(inneuter, 1,0),
           tSpeys = ifelse(tSpeys == 0, NA, tSpeys),
           tCastrates = ifelse(tCastrates == 0, NA, tCastrates),
           tNeuters = ifelse(tNeuters == 0, NA, tNeuters),# this is so I can work out the mean values later on
           vetsPerDayF = tSpeys/sxPerDayPerVetF,
           vetsPerDayM = ifelse(is.na(tCastrates), 0, tCastrates/sxPerDayPerVetM),
           vetsPerDay = ceiling(vetsPerDayM + vetsPerDayF),
           vetCosts = vetsPerDay * perVetPerDay,
           catchCosts = ifelse(inneuter == 1, numTeams*catchingTeamPerDay, 0),
           dayCost = ifelse(inneuter == 1,
                            vetCosts+catchCosts+clinicPerDay+ 
                              ifelse(is.na(tCastrates), 0, (tCastrates * perSxM)) + # when no males neutered this is necessary
                              (tSpeys * perSxF), 0))
  
    # this is needed to make sure the neutering is compared with the equivalent baseline pop
  if(neuterDF$speyingStart[1] > 1){
    # if speying starts NOT on day 1 after warm up, then start the output from wherever neutering starts + 10y
    metrics <- metrics1%>%
      arrange(time)%>%
      mutate(neuterStart = cumsum(inNeuter))%>% # when neuterStart = 1, neutering has started
      filter(neuterStart > 0)%>% # remove all time points before neutering
      mutate(timeAdj = 1:n())%>% # new time from after warm up
      filter(timeAdj <= output_years*365)%>% # selects the 10 years after neutering starts
      left_join(baseline_metrics, by = 'time')%>% # should join with baseline at the right point and compare baseline with strategy pop at same timepoint
      mutate(popDiff = basePop - currPop)
  }else{
    # if speyingStart == 1, so speying starts straight after warm up on day 1 (most cases), take data from day 1 + 10 years
    metrics <- metrics1%>%
      filter(time > warm_up*365 & time <= (warm_up + output_years)*365)%>%
      mutate(timeAdj = 1:n())%>% # new time from after warm up
      left_join(baseline_metrics, by = 'time')%>%
      mutate(popDiff = basePop - currPop)
  }
  
  return(metrics)
}


# popPlotBatchFun - make plot of total population ------------------------------

popPlotBatchFun <- function(metrics = metrics, i = i, dirPlot = dirPlot){
  
  start_day <- 6*365
  
  metrics_baseline <- readRDS(paste0(path, 'baselineMetrics.rds'))%>%
    select(time, basePop = currPop)%>%
    mutate(date = seq.Date(from = as.Date('2021-01-03'), length.out = n(), by = 1))%>%
    filter(time < metrics$nYears[1]*365 & time > start_day)
  
  # make rect
  if(sum(metrics$tNeuters, na.rm = TRUE) > 0){
    
    # not baseline - highlight neuters
    rect_df1 <- metrics%>%
      filter(time > start_day)%>%
      select(time, tNeuters)%>%
      mutate(tNeuters = ifelse(is.na(tNeuters), 0, tNeuters),
             neutersLog = as.numeric(tNeuters > 0))%>%
      mutate(diff_neuters = neutersLog -lag(neutersLog))%>%
      filter(diff_neuters != 0)%>%
      mutate(x = ifelse(diff_neuters == 1, 'xmin', 'xmax'),
             time_grp = ifelse(neutersLog == 1, time, NA))%>%
      fill(time_grp, .direction = 'down')%>%
      tidyr::pivot_wider(names_from = x, values_from = time)%>%
      #mutate(across(.cols = c(xmin, xmax), as.character))%>%
      replace_na(list(xmin = 0, xmax = 0))
    
    if("xmax"%in% colnames(rect_df1)) # when the speying is continuous
    {
      rect_df <- rect_df1%>%
        group_by(time_grp)%>%
        summarise(xmin = max(xmin, na.rm = T),
                  xmax = ifelse(max(xmax, na.rm = T) == 0, max(metrics$time, na.rm = T), max(xmax, na.rm = T)))
    }else{
      rect_df <- rect_df1%>%
        group_by(time_grp)%>%
        summarise(xmin = max(xmin, na.rm = T),
                  xmax = max(metrics$time, na.rm = T))
    }
    
    p1 <- metrics%>%
      filter(time > start_day)%>%
      dplyr::select(date, currPop, basePop, s_over3, o_over3)%>%
      tidyr::pivot_longer(-date, names_to = 'type', values_to = 'pop')%>%
      ggplot()+ 
      geom_col(data = metrics%>%filter(time > start_day), 
               aes(x = date, y = ifelse(is.na(tNeuters), 0, tNeuters*100)), 
               fill = 'lightblue', alpha = 0.5) + 
      #geom_rect(data = rect_df, aes(xmin = xmin, xmax = xmax), ymin = 0, ymax = 5500, fill = 'seagreen', alpha = 0.5)+
      geom_line(aes(x = date, y = pop, colour = type))+
      scale_colour_manual(values = c('coral','navy', "#d8b365", "#5ab4ac"), 
                          labels = c('baseline total', 'total', 'owned 3m+', 'stray 3m+'))+
      scale_y_continuous(limits = c(0,ifelse(max(metrics$currPop)<5500,5500,max(metrics$currPop))),
                         sec.axis = sec_axis(~. /100, name = 'neuters per day'))+ 
      scale_x_date(date_labels = '%Y', date_breaks = '1 year')+
      ylab("number of dogs") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title = element_blank(),
            #legend.position = c(0.5, 0.05),legend.direction = "horizontal"
            legend.position = 'bottom'
      )
    
    p2 <- metrics%>%
      filter(time > start_day)%>%
      dplyr::select(date, o_nc, s_nc ,f_nc, m_nc
      )%>%
      tidyr::pivot_longer(-date, names_to = 'type', values_to = 'neuter_coverage')%>%
      ggplot(aes(x = date))+
      geom_line(aes(y = neuter_coverage, colour = type)) + 
      scale_x_date(date_labels = '%Y', date_breaks = '1 year')+
      scale_y_continuous(limits = c(0,1)) + 
      scale_colour_manual(values = c( 'red', 'blue', "#d8b365", "#5ab4ac"), labels = c('female', 'male', 
                                                                                       'owned', 'stray'))+
      ylab("neuter coverage") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            #legend.position = c(0.12, 0.95),
            legend.position = 'bottom',
            legend.title = element_blank()) # could colour in background on days when speying occurs
    
    
    p3 <- metrics%>%
      #filter(time > start_day)%>%
      ggplot()+
      geom_hline(aes(yintercept = 5))+
      geom_col(aes(x = date, y = speyedPerDay*numTeams), fill = 'gold', alpha = 0.5) + 
      geom_col(aes(x = date, y = tNeuters), colour = 'coral') + 
      scale_x_date(date_labels = '%Y', date_breaks = '1 year')+
      scale_y_continuous() + 
      ylab("Neuters per day (shading: poss # neuters") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    
    png(paste0(dirPlot, '/', str_replace(i, pattern = '\\.', replacement = '_'), '.png'),
        width = 1200, res = 100)
    
    print(multiplot(plotlist = list(p1,p2,p3), cols = 3, title = i))
    dev.off()
    
    endplot <- multiplot(plotlist = list(p1,p2,p3), cols = 3, title = i)
    
  }else{
    # p1 <- metrics%>%
    #   filter(time > start_day)%>%
    #   ggplot() + 
    #   #geom_rect(data = rect_df, aes(xmin = xmin, xmax = xmax), ymin = 0, ymax = 1700, fill = 'seagreen', alpha = 0.5)+
    #   geom_line(data = metrics_baseline%>%filter(time < metrics$nYears[1]*365 & time > start_day), aes(x = time, y = currPop), colour = 'gold')+
    #   geom_line(aes(x = time, y = currPop))+
    #   geom_line(aes(x = time, y = currStrayPop), colour = 'purple')+
    #   geom_line(aes(x = time, y = currOwnedPop), colour = 'coral')+
    #   scale_y_continuous(limits = c(0,ifelse(max(metrics$currPop)<5500,5500,max(metrics$currPop))))+ 
    #   ylab("Total population") + 
    #   theme(legend.position = "bottom")
    
    p1 <- metrics%>%
      filter(time > start_day)%>%
      dplyr::select(date, currPop, basePop, s_over3, o_over3)%>%
      tidyr::pivot_longer(-date, names_to = 'type', values_to = 'pop')%>%
      ggplot()+ 
      geom_line(aes(x = date, y = pop, colour = type))+
      scale_colour_manual(values = c('coral','navy', "#d8b365", "#5ab4ac"), 
                          labels = c('baseline total', 'total', 'owned 3m+', 'stray 3m+'))+
      scale_y_continuous(limits = c(0,ifelse(max(metrics$currPop)<5500,5500,max(metrics$currPop))),
                         sec.axis = sec_axis(~. /100, name = 'neuters per day'))+ 
      scale_x_date(date_labels = '%Y', date_breaks = '1 year')+
      ylab("number of dogs") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.title = element_blank(),
            #legend.position = c(0.5, 0.05),legend.direction = "horizontal"
            legend.position = 'bottom'
      )
    
    endplot <- p1
  }
  
  
  return(#print(endplot)
    print(paste(i, 'plot saved')))}

# summaryFun - make one line summary of model outputs (metricsDF)  ---------------------

summaryFun <- function(metrics = metrics, .i= i){
  
  # get time to half the population/min pop and max female neuter coverage
  if(all(metrics$id != 'baseline_pop')){
    # what is half the starting population?
    t_half_df <- metrics%>%filter(s_over3 <= metrics$s_over3[1]*0.5)
    
    # what is the minimum population of adults and when does it occur? 
    t_min_df <- metrics%>%filter(s_over3 == min(metrics$s_over3))
    t_min <- min(t_min_df$timeAdj)
    
    # what is the max female neuter coverage that occurs during the strategy?
    t_max_fnc <- metrics%>%filter(f_nc == max(metrics$f_nc))
    t_max_fnc <- min(t_max_fnc$timeAdj)
    
    sumTeams <- metrics%>%filter(inneuter == 1)%>%summarise(tTeams = sum(numTeams, na.rm = TRUE))%>%pull(tTeams)
    
    if(nrow(t_half_df) == 0){
      t_half <- NA
      
      
    }else{
      t_half <- min(t_half_df$timeAdj)
    }
  }else{ 
    t_half <- NA
  t_min <- NA
  t_max_fnc <- NA
  sumTeams <- NA
  }

  # Get runs of consecutive 1s or 0s using base::rlinneuter# Get runs of consecutive 1s or 0s using base::rle
  rle_data <- metrics%>%filter(timeAdj >= speyingStart)%>% # remove days if speying starts later in the year
    pull(inneuter)%>%
    rle()
  
  # Convert the RLE output into a tibble: each row is a run
  runs_df <- tibble(
    value = rle_data$values,    # 1 or 0
    length = rle_data$lengths   # how many in a row
  )
  
  # Calculate lengths of consecutive neutering (1) runs
  neutering_lengths <- runs_df %>%
    filter(value == 1) %>%
    pull(length)
  
  # Calculate lengths of consecutive non-neutering (0) runs
  gap_lengths <- runs_df %>%
    filter(value == 0) %>%
    pull(length)
  
# Get summary of this neutering strategy
metrics_summary <- metrics%>%
  #arrange(time)%>%
  #mutate(neuterStart = cumsum(inNeuter))%>% # inNeuter is 1 when neutering should happen - cumulative sum of inNeuter col
  #filter(neuterStart > 0)%>% # this filters out any rows before the neutering started - will result in fewer rows if neutering starts later
  mutate(timeAdj = 1:n())%>% # new time from after warm up
  summarise(last_tNeuters_timeAdj = dplyr::last(timeAdj[!is.na(tNeuters)], default = NA_integer_),
            across(c(tSpeys, tCastrates, tNeuters), ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
            across(c(f_nc,m_nc, tSpeys, tCastrates, tNeuters),~max(.x, na.rm = TRUE), .names = "max_{.col}"),
            across(c(currPop, oAdults, sAdults, s_over3),~min(.x, na.rm = TRUE), .names = "min_{.col}"),
            across(c(#oPupsBorn, sPupsBorn,speysO,castratesO,speysS,castratesS,
              #s_over3_FemaleDied,o_over3_FemaleDied,o_over3_MaleDied,
              #s_over3_MaleDied,sFemalePupsDied,sMalePupsDied,
              #oFemalePupsDied,oMalePupsDied,sPupsDied,oPupsDied,oCastrates,sCastrates,oSpeys,sSpeys,
              tPupsBorn, t_over3_Died,
              tPupsDied,
              tSpeys,tCastrates,tNeuters,popDiff, inNeuter, basePop), ~sum(.x, na.rm = TRUE)),
            tVetsPerDay = sum(vetsPerDay, na.rm = TRUE),
            tCosts = sum(dayCost, na.rm = TRUE),
            tPop = sum(currPop),
            numDays = n(),
            across(c(sAdults, oAdults, s_over3, numTeams, time), 
                   first, .names = "start_{.col}"),
            across(c(id, desc, starts_with('speying'), speyedPerDay, neuterBreak, minCatch, totalNeuterCap, numTeamsOrig, seasonStartDay, maleNeuterImpact), 
                   last),
            across(c(currOwnedPop, currStrayPop, 
                     currPop, s_over3,
                     f_nc, time), 
                   last, .names = "final_{.col}"))%>%
  mutate(tTeams = sumTeams, # I think this is more accurately just total number of teams - not really per day?? jan-22
    t_half = t_half,
    dogsPDay = ifelse(id != 'baseline_pop', popDiff/numDays, NA),
    t_min_s_over3 = t_min,
    t_max_fnc = t_max_fnc,
    last5_pop_slope = (final_s_over3-metrics$s_over3[final_time - (5*365)])/(5*365), # slope from start to finish of last 5 years of model - regardless of when the min-pop was
    from_min_pop_slope = (final_s_over3 - min_s_over3)/(final_time - t_min_s_over3),
    to_min_pop_slope = (min_s_over3 - metrics$s_over3[4*365])/(t_min_s_over3-(4*365)), # if min is the end might get NaN
    #max_fnc_slope = (max_f_nc)/(t_max_fnc-(4*365)),
    #fnc_decline = (max_f_nc - final_f_nc) / (t_max_fnc - final_time),
    strayAdultsUnder50 = min_sAdults < (start_sAdults*0.5),
    # neutering irl
    neuteringRepeatsIRL = runs_df %>%filter(value == 1) %>%nrow(),
    neuteringLength_min = min(neutering_lengths),
    neuteringLength_mean = round(mean(neutering_lengths), 2),
    neuteringLength_max = max(neutering_lengths),
    # days between neutering
    neuteringGap_min = min(gap_lengths),
    neuteringGap_mean = round(mean(gap_lengths), 2),
    neuteringGap_max = max(gap_lengths),
    # did the strategy have a chance to finish?
    singleStrategyLength = ifelse(desc == 'specifiedIntervals', speyingDuration + speyingInterval, as.numeric(NA)),
    strategyLength = ifelse(desc == 'specifiedIntervals' & !is.na(speyingRepeat), singleStrategyLength*speyingRepeat, NA),
    strategyFinished = ifelse(desc == 'specifiedIntervals', strategyLength <= 3650, NA)
    )%>%
  mutate(tTeamsPerDay = tTeams/inNeuter)
  
return(metrics_summary)}

# pupDeathsBatchFun - summarise annual pup mortality ---------------------------


pupDeathsBatchFun <- function(metricsData = metrics, i = i)
{
  pcrit <- 0.4
  
  pupDeathsDF <- metricsData%>%
    mutate(year = ceiling(time/365),
           over3 = s_over3 + o_over3,
           over3Diff = over3 - lag(over3),
           over3_Recruits = lag(t_over3_Died) + over3Diff)%>%
    group_by(year)%>%
    summarise(across(c(tPupsBorn, t_over3_Died, tPupsDied, tNeuters, over3_Recruits), ~sum(.x, na.rm = TRUE)),
              annualPop = mean(currPop), # currPop is stray+owned pop
              lastPop = last(currPop),
              firstPop = first(currPop),
              id = first(id))%>%
    mutate(across(where(is.numeric), round))%>%
    ungroup()%>%
    mutate(num_over3_same = lastPop - over3_Recruits,
           prop_over3_same = num_over3_same/lastPop,
           vc_after_turnover = prop_over3_same*0.7,
           req_vc = pcrit/prop_over3_same)
  
  return(pupDeathsDF)}

# lifeExpBatchFun get life expectancy from birth ---------------------------


lifeExpBatchFun <- function(neuterDF = neuterDF, id = id)
{
  mortalityDF <- neuterDF %>%
    dplyr::select(time, strayPupMortalityMale, 
                  strayAdultMaleMortality,
                  strayPupMortalityFemale,
                  strayAdultFemaleMortality,
                  pupMortalityOwned,
                  adultFemaleMortalityRateOwned,
                  adultMaleMortalityRateOwned) %>%
    mutate(strayPupMortalityMale = 1 - strayPupMortalityMale,
           strayAdultMaleMortality = 1 - strayAdultMaleMortality,
           strayPupMortalityFemale = 1 - strayPupMortalityFemale,
           strayAdultFemaleMortality = 1 - strayAdultFemaleMortality,
           pupMortalityOwned = 1 - pupMortalityOwned,
           adultFemaleMortalityRateOwned = 1 - adultFemaleMortalityRateOwned,
           adultMaleMortalityRateOwned = 1 - adultMaleMortalityRateOwned,
           strayMaleLE = NA,
           strayFemaleLE = NA,
           ownedMaleLE = NA,
           ownedFemaleLE = NA)
  
  # Calculate probability of survival in each compartment (pup, adult, juvenile)
  
  for(i in 1:nrow(mortalityDF)){
    # stray male
    pCum <- cumprod(mortalityDF$strayPupMortalityMale[i : (i + 90)]) # cumulative product of rate of pup surviving each day in pup compartment
    mortalityDF$pVal[i] <- sum(pCum) # sum the cumulative products for a pup to see how many days they will survive (90 days)
    jCum <- cumprod(mortalityDF$strayAdultMaleMortality[(i + 90) : (i + 90 + 210)]) * pCum[length(pCum)] # juv rate of survival given previous pup survival pup survival
    mortalityDF$jVal[i] <- sum(jCum) # juvenile
    aCum <- cumprod(c(mortalityDF$strayAdultMaleMortality[(i + 90 + 210) : nrow(mortalityDF)],
                      rep(mortalityDF$strayAdultMaleMortality[nrow(mortalityDF)], i))) * jCum[length(jCum)] # adult rate given juv rate
    mortalityDF$aVal[i] <- sum(aCum) # adult
    mortalityDF$strayMaleLE[i] <- mortalityDF$pVal[i] + mortalityDF$jVal[i] + mortalityDF$aVal[i] # sum number of days survived all 3 compartments, having been born on day i
    
    # stray female
    pCum <- cumprod(mortalityDF$strayPupMortalityFemale[i : (i + 90)])
    mortalityDF$fpVal[i] <- sum(pCum)
    jCum <- cumprod(mortalityDF$strayAdultFemaleMortality[(i + 90) : (i + 90 + 210)]) * pCum[length(pCum)]
    mortalityDF$fjVal[i] <- sum(jCum)
    aCum <- cumprod(c(mortalityDF$strayAdultFemaleMortality[(i + 90 + 210) : nrow(mortalityDF)],
                      rep(mortalityDF$strayAdultFemaleMortality[nrow(mortalityDF)], i))) * jCum[length(jCum)]
    mortalityDF$faVal[i] <- sum(aCum)
    mortalityDF$strayFemaleLE[i] <- mortalityDF$fpVal[i] + mortalityDF$fjVal[i] + mortalityDF$faVal[i]
    
    # owned male
    pCum <- cumprod(mortalityDF$pupMortalityOwned[i : (i + 90)]) # overall pup mortality here as turf out rates are what differs between m/f in owned pups
    mortalityDF$pVal[i] <- sum(pCum)
    jCum <- cumprod(mortalityDF$adultMaleMortalityRateOwned[(i + 90) : (i + 90 + 210)]) * pCum[length(pCum)]
    mortalityDF$jVal[i] <- sum(jCum)
    aCum <- cumprod(c(mortalityDF$adultMaleMortalityRateOwned[(i + 90 + 210) : nrow(mortalityDF)],
                      rep(mortalityDF$adultMaleMortalityRateOwned[nrow(mortalityDF)], i))) * jCum[length(jCum)]
    mortalityDF$aVal[i] <- sum(aCum)
    mortalityDF$ownedMaleLE[i] <- mortalityDF$pVal[i] + mortalityDF$jVal[i] + mortalityDF$aVal[i]
    
    # owned female
    pCum <- cumprod(mortalityDF$pupMortalityOwned[i : (i + 90)]) # 
    mortalityDF$fpVal[i] <- sum(pCum)
    jCum <- cumprod(mortalityDF$adultFemaleMortalityRateOwned[(i + 90) : (i + 90 + 210)]) * pCum[length(pCum)]
    mortalityDF$fjVal[i] <- sum(jCum)
    aCum <- cumprod(c(mortalityDF$adultFemaleMortalityRateOwned[(i + 90 + 210) : nrow(mortalityDF)],
                      rep(mortalityDF$adultFemaleMortalityRateOwned[nrow(mortalityDF)], i))) * jCum[length(jCum)]
    mortalityDF$faVal[i] <- sum(aCum)
    mortalityDF$ownedFemaleLE[i] <- mortalityDF$fpVal[i] + mortalityDF$fjVal[i] + mortalityDF$faVal[i]
    
  }
  
  mortalityDFG <- mortalityDF %>%
    dplyr::select(time, strayMaleLE, strayFemaleLE, ownedMaleLE, ownedFemaleLE) %>%
    gather(key = "Group", "LE", -time)%>%
    mutate(id = id)
  
  
  
  return(mortalityDFG)}

