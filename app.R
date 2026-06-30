library(shiny)
library(plotly)
library(tidyverse)
library(scales)
library(shinybusy)
library(arrow)
library(DT)

# get data
costDF <- readRDS('data/costDF_app.rds')%>%
  mutate(desc = ifelse(desc == 'specifiedIntervals_noTeamsChange', 'specifiedIntervals', desc))%>%
  mutate(desc1 = ifelse(desc == 'CatchReturnGuided', desc, 'FixedSterilisation'))%>%
  mutate(speyingRepeat = ifelse(is.na(speyingRepeat), 'Rpt till end', as.character(speyingRepeat)))%>%
  mutate(speyingRepeat = ifelse(speyingDuration == 3650, '1', speyingRepeat))%>%
  mutate(numTeamsChanges = ifelse(numTeamsChanges == TRUE, 'Yes', 'No'))

optDF <- costDF%>%filter(strategyTypePlot == 'Optimal')
warm_up <- 6

strategyTypeCols <- c("#E69F00","#F0E442" , "#00cc33", "#0099ff")

sexCols <- c('tomato', 'steelblue')
dependentCols <- c('grey', 'black')

# Functions - read in only the id of the strategy that we want for the plot from arrows file
read_plotdf <- function(desc, ids)
{
  open_dataset(file.path(paste0('data/', desc,"_arrow")))%>% 
    filter(id %in% ids) %>% collect()
}

ui <- fluidPage(
  add_busy_spinner(spin = "fading-circle", position = 'top-left', margins = c(500,400), timeout = 200),
  # ---- Title ----
  titlePanel(
    tags$div(style="display:flex;align-items:center;gap:10px;",
             tags$img(src="dog_edit.svg", style="height:50px;width:auto;"),
             tags$div(
               tags$h2("DogPopSim", style="margin:0;"),
               tags$h4(style="font-weight:300;margin:0;",
                       "Free-roaming dog population sterilisation model")
             )
    )
  ),
  # ---- Description ----
  tags$div(
    style = "max-width: 1000px; margin-bottom: 20px;",
    tags$p(
      "DogPopSim is an interactive tool to explore the demographic and economic impacts 
       of alternative sterilisation strategies on a free-roaming dog population as described in <paper citation>.
       The deterministic mathematical model simulates population dynamics stratified by age, sex and 
       reproductive status and compares outcomes against a no-sterilisation baseline."
    ),
    tags$p(tags$b("Build tab:"), " allows you to define 3 strategies and visualise their impact."
    ),
    tags$p(tags$b("Browse tab:"), " shows a table of tested strategies. Select a strategy to generate plots."
    )
  ),
  tags$hr(),
  # ---- Collapsible methods panel ----
  tags$details(
    tags$summary(tags$b("Model parameters and assumptions")),
    tags$div(
      style = "margin-top: 10px;",
      tags$p(
        "The model is based on dog demographics a in a medium-sized town in southern India with ~110,000 people
        and a human:dog ratio of ~17.25. Field data indicated that a maximum of 5 dog catching teams could be deployed
        and a maximum of 25 dogs could be captured per team per day (Fielding et al, 2023), therefore these are the sterilisation limits used.
        Reproduction occurs all year round but there is an explicit seasonal peak October-January based on field data 
        (Fielding et al., 2021). Free-roaming dependent dogs are directly dependent on humans for resources such as shelter, food 
        and care but have unrestricted access to public spaces. Independent dogs are not directly dependent on humans."
      ),
      tags$b("Outputs"),
      tags$p(
        "Model outputs include total strategy cost, population density reduction, cost-effectiveness metrics,
        and projections of population size and sterilisation coverage over the ten-year simulation. An effective strategy
        reduced the population by over 50% by the end of the simulation and reduced the total population density by more than 40%
        summed over the whole simulation. A high-cost strategy is over US$250,000."
      ),
      tags$p(
        "A full description of the model structure, parameterisation, and assumptions is provided in:",
        tags$br(),
        tags$a(
          href = "#",      # <-- placeholder link
          target = "_blank",
          "Fielding et al. (in draft). Developing free-roaming dog sterilisation strategies to maximise 
          population impact and cost-efficiency. Journal TBC. Link not yet valid"
        )
      ),
      tags$b("References"),
      tags$br(),
      tags$a(
          href = "https://doi.org/10.1016/j.prevetmed.2023.105996",      # <-- placeholder link
          target = "_blank",
          "Fielding, H. R. et al., 2023. Capturing free-roaming dogs for sterilisation: 
          A multi-site study in Goa, India. Prev. Vet. Med. 218, 105996"
        ),
    tags$br(),
    tags$a(
      href = "https://doi.org/10.1016/j.prevetmed.2020.105249",
      target = "_blank",
      "Fielding, H. R. et al., 2021. Timing of reproduction 
      and association with environmental factors in female free-roaming dogs in southern India. 
      Prev. Vet. Med. 187, 105249"
    ),
      
      tags$p(
        style = "font-style: italic;",
        "All results are conditional on model assumptions and should be interpreted comparatively 
         rather than as precise forecasts."
      )
    )
  ),
  
  tags$hr(),
  # ---- CSS for collapsibles and Run buttons ----
  tags$style(HTML("
    /* Collapsible methods panel */
    details summary {
      cursor: pointer;
      font-weight: 600;
      list-style: none;
    }
    details summary::before {
      content: '▶';
      display: inline-block;
      margin-right: 6px;
      transition: transform 0.2s ease;
    }
    details[open] summary::before {
      transform: rotate(90deg);
    }
    details summary:hover {
      color: #007BFF;
    }
    details {
      margin-bottom: 15px;
    }
    details[open] summary {
      margin-bottom: 5px;
    }
    
    /* Run buttons */
    .run-button {
      width: 100%;
      font-size: 18px;
      margin-top: 10px;
    }
  ")),
  
  # Tab 1
  tabsetPanel(
    tabPanel("Build (3 strategies)", 
    tags$p("Use the controls below to define strategy parameters from ",tags$b(
               "top to bottom")," and click the Run button in each panel to generate model outputs. You can compare up to three strategies at once and see plots below."
             ),
  # ---- Three strategy panels horizontally ----
  fluidRow(
    column(4, tags$h4("Strategy 1"), selectInput("desc_ui_1","Strategy approach",choices=list("Fixed sterilisation duration","Catch return guided duration")), radioButtons("speyingC_1","Sterilise dependent population?",choices=list("Yes"=1,"No"=0),selected="1",inline=TRUE), radioButtons("speyingMales_1","Sterilise male FRDs?",choices=list("Yes"=1,"No"=0),selected="1",inline=TRUE), uiOutput("conditional_ui_1"), actionButton("run_btn_1","Run",icon=icon("play"),class="btn-success btn-lg run-button")),
    column(4, tags$h4("Strategy 2"), selectInput("desc_ui_2","Strategy approach",choices=list("Fixed sterilisation duration","Catch return guided duration")), radioButtons("speyingC_2","Sterilise dependent population?",choices=list("Yes"=1,"No"=0),selected="1",inline=TRUE), radioButtons("speyingMales_2","Sterilise male FRDs?",choices=list("Yes"=1,"No"=0),selected="1",inline=TRUE), uiOutput("conditional_ui_2"), actionButton("run_btn_2","Run",icon=icon("play"),class="btn-success btn-lg run-button")),
    column(4, tags$h4("Strategy 3"), selectInput("desc_ui_3","Strategy approach",choices=list("Fixed sterilisation duration","Catch return guided duration")), radioButtons("speyingC_3","Sterilise dependent population?",choices=list("Yes"=1,"No"=0),selected="1",inline=TRUE), radioButtons("speyingMales_3","Sterilise male FRDs?",choices=list("Yes"=1,"No"=0),selected="1",inline=TRUE), uiOutput("conditional_ui_3"), actionButton("run_btn_3","Run",icon=icon("play"),class="btn-success btn-lg run-button"))
  ),
  fluidRow(
    column(4, hr(), uiOutput("outcome_1"), plotOutput("sc_plot_1",height=250), plotOutput("pop_plot_1",height=250)),
    column(4, hr(), uiOutput("outcome_2"), plotOutput("sc_plot_2",height=250), plotOutput("pop_plot_2",height=250)),
    column(4, hr(), uiOutput("outcome_3"), plotOutput("sc_plot_3",height=250), plotOutput("pop_plot_3",height=250))
  ),
  # ---- Collapsible plot details panel ----
  tags$details(
    tags$summary(tags$b("Plot details")),
    tags$div(
      style = "margin-top: 10px;",
      tags$p(tags$b("Upper plot"), " shows sterilisation coverage with different subpopulations indicated by colour. The red shaded region indicates the optimal zone for maximum sterilisation coverage; if female sterilisation coverage lies within this zone at any point, the strategy may be effective, if it never hits this zone, the strategy will not be effective."),
      tags$p(tags$b("Lower plot"), " shows population size with different subpopulations indicated by colour. The baseline scenario population size with no sterilisation is indicated by the dashed red line. Population density reduction is calculated as the summed difference between baseline population size and the strategy population size."),
      tags$br()
    )
  ),
  tags$hr(),
  fluidRow(column(width=12, offset=2, tableOutput("summary_table"))),
  fluidRow(column(width=10, offset=1, plotOutput("all", height=450)))
    ),
  # Tab2
  tabPanel("Browse strategies",
           tags$p("Select a strategy in the table below to see plots of sterilisation coverage and population size over time. Use the drop-down box to filter by strategy outcome."),
           selectInput("browse_class","Filter by strategy outcome",choices=sort(unique(costDF$strategyTypePlot)),selected="Optimal"),
           fluidRow(column(6, plotOutput("browse_sc_plot", height=300)),
                    column(6, plotOutput("browse_pop_plot", height=300))
                    ),
           tags$hr(),
           # ---- Collapsible plot details panel ----
           tags$details(
             tags$summary(tags$b("Plot details")),
             tags$div(
               style = "margin-top: 10px;",
               tags$p(tags$b("Left plot"), " shows sterilisation coverage with different subpopulations indicated by colour. The red shaded region indicates the optimal zone for maximum sterilisation coverage; if female sterilisation coverage lies within this zone at any point, the strategy may be effective, if it never hits this zone, the strategy will not be effective."),
               tags$p(tags$b("Right plot"), " shows population size with different subpopulations indicated by colour. The baseline scenario population size with no sterilisation is indicated by the dashed red line. Population density reduction is calculated as the summed difference between baseline population size and the strategy population size."),
               tags$br()
             )
           ),
           DTOutput("browse_table")
  )
  
  
), 
# ---- Footer ----
tags$footer(
  style = "
      font-size: 12px;
      color: #777;
      margin-top: 20px;
      padding-top: 10px;
      border-top: 1px solid #e5e5e5;
    ",
  tags$p(
    "DogPopSim | Free-roaming dog population sterilisation model | Version 1.0 | If there are any queries, issues or suggestions for v2, please contact: helen.fielding@ed.ac.uk"
  ),
  tags$a(
    href = "https://pixabay.com/vectors/dog-hound-animal-pet-151482/",
    target = '_blank',
    "Dog image by OpenClipart-Vectors via Pixabay" 
  )
))
  # ---- Server ----
server <- function(input, output, session) {
  # Tab 2
  # --- Browse tab data for table (keep id but hide it) ---
  browse_tbl <- reactive({
    req(input$browse_class)
    costDF %>% filter(strategyTypePlot==input$browse_class) %>% arrange(tCostsk) %>%
      transmute(id=id, Class=strategyTypePlot, Approach=desc, Cost_kUSD=tCostsk, Pop_density_reduction=ppd,
                Cost_per_ster=(tCostsk*1000)/tNeuters,
                Sterilise_depFRDs=ifelse(speyingC==1,"Yes","No"),
                Sterilise_males=ifelse(speyingMales==1,"Yes","No"),
                Ster_length=speyingDuration, Ster_interval=speyingInterval, Repeats=speyingRepeat,
                Num_teams=numTeamsOrig, Reduce_teams=numTeamsChanges) %>%
      mutate(Rank=row_number(), .before=1)
  })
  
  output$browse_table <- renderDT({
    datatable(browse_tbl(),
              rownames=FALSE,
              selection="single",
              options=list(pageLength=25, autoWidth=TRUE,
                           columnDefs=list(list(targets=1, visible=FALSE))) # hide id (Rank=0, id=1)
    )
  })
  
  # --- Selected strategy from table ---
  browse_selected <- reactive({
    req(input$browse_table_rows_selected)
    browse_tbl()[input$browse_table_rows_selected, , drop=FALSE]
  })
  
  browse_plotdf <- reactive({
    sel <- browse_selected(); req(nrow(sel)==1)
    # find original row in costDF to get desc (arrow folder key) + id
    row <- costDF %>% filter(id == sel$id) %>% slice(1)
    read_plotdf(row$desc, row$id)
  })
  
  # --- Sterilisation coverage plot (same as panels) ---
  output$browse_sc_plot <- renderPlot({
    req(input$browse_table_rows_selected)
    browse_plotdf() %>% mutate(timeAdj=timeAdj/365) %>%
      dplyr::select(timeAdj, o_nc, s_nc, f_nc, m_nc) %>%
      tidyr::pivot_longer(-timeAdj, names_to="type", values_to="neuter_coverage") %>%
      ggplot(aes(x=timeAdj)) +
      geom_rect(inherit.aes=FALSE,
                data=data.frame(xmin=0,xmax=Inf,ymin=min(optDF$max_f_nc),ymax=max(optDF$max_f_nc)),
                aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax), fill="tomato", alpha=0.2) +
      geom_line(aes(y=neuter_coverage, colour=type)) +
      scale_x_continuous(breaks=seq(1,10,1)) +
      scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) +
      scale_colour_manual(values=c(sexCols, dependentCols),
                          labels=c("Female","Male","Dependent","Independent")) +
      ylab("Sterilisation coverage") + xlab("Year of simulation") +
      theme(legend.position="bottom", legend.title=element_blank())
  })
  
  # --- Population plot (same as panels) ---
  output$browse_pop_plot <- renderPlot({
    req(input$browse_table_rows_selected)
    pdf <- browse_plotdf() %>% mutate(timeAdj=timeAdj/365)
    pdf %>% dplyr::select(timeAdj, currPop, basePop, s_over3, o_over3) %>%
      tidyr::pivot_longer(-timeAdj, names_to="type", values_to="pop") %>%
      ggplot() +
      geom_col(data=pdf, aes(x=timeAdj, y=ifelse(is.na(tNeuters),0,tNeuters*100)),
               fill="lightblue", alpha=1) +
      geom_line(aes(x=timeAdj, y=pop, colour=type, linetype=type)) +
      scale_colour_manual(name="", values=c("#d04e00","#d04e00",dependentCols),
                          labels=c("Baseline total","Total","Dependent 3m+","Independent 3m+")) +
      scale_linetype_manual(name="", values=c("dashed","solid","solid","solid"),
                            labels=c("Baseline total","Total","Dependent 3m+","Independent 3m+")) +
      scale_y_continuous(limits=c(0, ifelse(max(pdf$currPop)<5500,5500,max(pdf$currPop))),
                         sec.axis=sec_axis(~./100, name="Sterilisations per day")) +
      scale_x_continuous(breaks=seq(1,10,1)) +
      ylab("Number of dogs") + xlab("Year of simulation") +
      theme(legend.position="bottom")
  })
  

  
  # Helper: create baseDF for a panel -----------------------------
  baseDF_panel <- function(desc_input, speyingC_input, speyingMales_input) {
    reactive({
      req(desc_input, speyingC_input, speyingMales_input)
      costDF %>%
        filter(
          desc1 %in% if(desc_input=="Catch return guided duration") "CatchReturnGuided" 
          else setdiff(unique(costDF$desc1),"CatchReturnGuided"),
          speyingC == as.numeric(speyingC_input),
          speyingMales == as.numeric(speyingMales_input)
        )
    })
  }
  
  # PANEL 1 -----------------------------
  desc_val_1 <- reactive(if (input$desc_ui_1 == "Catch return guided duration") "CatchReturnGuided" else setdiff(unique(costDF$desc1),"CatchReturnGuided"))
  
  baseDF_1 <- reactive({
    req(input$desc_ui_1, input$speyingC_1, input$speyingMales_1)
    costDF %>% filter(desc1 %in% desc_val_1(), speyingC==as.numeric(input$speyingC_1), speyingMales==as.numeric(input$speyingMales_1))
  })
  
  # Conditional dropdowns
  output$conditional_ui_1 <- renderUI({
    df <- baseDF_1()
    if(nrow(df)==0) return(NULL)
    
    if(any(df$desc1=="CatchReturnGuided")){
      mc <- sort(unique(df$minCatch))
      ntO <- sort(unique(df$numTeamsOrig))
      ntC <- sort(unique(df$numTeamsChanges))
      tagList(
        selectInput("minCatch_1","Capture threshold", choices=mc, selected=mc[1]),
        selectInput("neuterBreak_1","Interval between sterilisations (days)", choices=NULL),
        selectInput("numTeamsOrig_1","Original number of teams", choices=ntO, selected=ntO[1]),
        selectInput("numTeamsChanges_1","Reduce teams if under min catch", choices=ntC, selected=ntC[1])
      )
    } else {
      dur <- sort(unique(df$speyingDuration))
      int <- sort(unique(df$speyingInterval))
      ntO <- sort(unique(df$numTeamsOrig))
      ntC <- sort(unique(df$numTeamsChanges))
      tagList(
        selectInput("speyingDuration_1","Length of sterilisation session (days)", choices=dur, selected=dur[1]),
        selectInput("speyingInterval_1","Interval between sterilisation sessions (days)", choices=int[1]),
        selectInput("speyingRepeat_1","Number of sterilisation sessions", choices=NULL),
        selectInput("numTeamsOrig_1","Original number of teams", choices=ntO, selected=ntO[1]),
        selectInput("numTeamsChanges_1","Reduce teams if catch under threshold", choices=ntC, selected=ntC[1])
      )
    }
  })
  
  # --- Panel 1 cascading dropdowns ---
  # CatchReturnGuided
  observe({
    df <- baseDF_1() %>% filter(desc1=="CatchReturnGuided")
    req(nrow(df) > 0)
    updateSelectInput(session, "minCatch_1", choices=sort(unique(df$minCatch)), selected=sort(unique(df$minCatch))[1])
  })
  observeEvent(input$minCatch_1, {
    df <- baseDF_1() %>% filter(desc1=="CatchReturnGuided", minCatch==input$minCatch_1)
    updateSelectInput(session, "neuterBreak_1", choices=sort(unique(df$neuterBreak)), selected=sort(unique(df$neuterBreak))[1])
  })
  observe({
    df <- baseDF_1() %>% filter(desc1=="CatchReturnGuided")
    req(nrow(df) > 0)
    updateSelectInput(session, "numTeamsOrig_1", choices=sort(unique(df$numTeamsOrig)), selected=sort(unique(df$numTeamsOrig))[1])
    updateSelectInput(session, "numTeamsChanges_1", choices=sort(unique(df$numTeamsChanges)), selected=sort(unique(df$numTeamsChanges))[1])
  })
  
  # FixedSterilisation
  observe({
    df <- baseDF_1() %>% filter(desc1!="CatchReturnGuided")
    req(nrow(df) > 0)
    updateSelectInput(session, "speyingDuration_1", choices=sort(unique(df$speyingDuration)), selected=sort(unique(df$speyingDuration))[1])
  })
  observeEvent(input$speyingDuration_1, {
    df <- baseDF_1() %>% filter(desc1!="CatchReturnGuided", speyingDuration==input$speyingDuration_1)
    updateSelectInput(session, "speyingInterval_1", choices=sort(unique(df$speyingInterval)), selected=sort(unique(df$speyingInterval))[1])
  })
  observeEvent(c(input$speyingDuration_1, input$speyingInterval_1), {
    df <- baseDF_1() %>% filter(desc1!="CatchReturnGuided", speyingDuration==input$speyingDuration_1, speyingInterval==input$speyingInterval_1)
    updateSelectInput(session, "speyingRepeat_1", choices=sort(unique(df$speyingRepeat)), selected=sort(unique(df$speyingRepeat))[1])
  })
  
  ### this can be repeated on _2 etc
  observeEvent(c(input$speyingDuration_1, input$speyingInterval_1, input$speyingRepeat_1), {
    df <- baseDF_1() %>% filter(desc1!="CatchReturnGuided", speyingDuration==input$speyingDuration_1, speyingInterval==input$speyingInterval_1,
                                speyingRepeat == input$speyingRepeat_1)
    updateSelectInput(session, "numTeamsOrig_1", choices=sort(unique(df$numTeamsOrig)), selected=sort(unique(df$numTeamsOrig))[1])
  })
  observeEvent(c(input$speyingDuration_1, input$speyingInterval_1, input$speyingRepeat_1, input$numTeamsOrig_1), {
    df <- baseDF_1() %>% filter(desc1!="CatchReturnGuided", speyingDuration==input$speyingDuration_1, speyingInterval==input$speyingInterval_1,
                                speyingRepeat == input$speyingRepeat_1, numTeamsOrig == input$numTeamsOrig_1)
    updateSelectInput(session, "numTeamsChanges_1", choices=sort(unique(df$numTeamsChanges)), selected=sort(unique(df$numTeamsChanges))[1])
  })
  
  # --- Panel 1 finalDF and outputs ----
  finalDF_1 <- eventReactive(input$run_btn_1, {
    df <- baseDF_1()
    if(any(df$desc1=="CatchReturnGuided")){
      req(input$minCatch_1, input$neuterBreak_1)
      df <- df %>% filter(minCatch==input$minCatch_1, neuterBreak==input$neuterBreak_1)
    } else {
      req(input$speyingDuration_1,input$speyingInterval_1,input$speyingRepeat_1)
      df <- df %>% filter(speyingDuration==input$speyingDuration_1,
                          speyingInterval==input$speyingInterval_1,
                          speyingRepeat==input$speyingRepeat_1)
    }
    req(input$numTeamsOrig_1,input$numTeamsChanges_1)
    df %>% filter(numTeamsOrig==input$numTeamsOrig_1, numTeamsChanges==input$numTeamsChanges_1)
  })
  
  output$outcome_1 <- renderUI({
    req(input$run_btn_1)
    df <- finalDF_1()
    if(nrow(df)==0) return(HTML("No results"))
    HTML(paste0(
      '<b>Strategy outcome</b>: ', df$strategyTypePlot, "<br>",
      '<b>Strategy approach</b>: ', df$desc
    ))
  })

  plotdf_1 <- eventReactive(input$run_btn_1,{
    df<-finalDF_1()
    if(nrow(df)==0) return(HTML("No results"))
    if(nrow(df)>1) return(HTML("More than 1 result"))
    
    read_plotdf(df$desc,df$id)
    })
  
  output$sc_plot_1 <- renderPlot({
    req(input$run_btn_1)
    df <- finalDF_1()
    if(nrow(df)==0) return(HTML("No results"))
    if(nrow(df)>1) return(HTML("More than 1 result"))
    
    plotdf_1()%>%mutate(timeAdj = timeAdj/365)%>%
      dplyr::select(timeAdj, o_nc, s_nc ,f_nc, m_nc)%>%
      tidyr::pivot_longer(-timeAdj, names_to = 'type', values_to = 'neuter_coverage')%>%
      ggplot(aes(x = timeAdj))+
      geom_rect(inherit.aes = FALSE,
                data = data.frame( xmin = 0, xmax = Inf, ymin = min(optDF$max_f_nc), ymax = max(optDF$max_f_nc)),
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'tomato', alpha = 0.2)+
      geom_line(aes(y = neuter_coverage, colour = type)) + scale_x_continuous(breaks = seq(1,10,1))+
      scale_y_continuous(breaks = seq(0,1, 0.2), limits = c(0,1)) +
      scale_colour_manual(values = c( sexCols, dependentCols), labels = c('Female', 'Male', 'Dependent', 'Independent'))+
      ylab("Sterilisation coverage") + xlab('Year of simulation')+ theme(legend.position = 'bottom', legend.title = element_blank())

  })
  
  output$pop_plot_1 <- renderPlot({
    req(input$run_btn_1)
    df <- finalDF_1()
    if(nrow(df)==0) return(NULL)
    
    plotdf_1()%>% mutate(timeAdj = timeAdj/365)%>% 
      dplyr::select(timeAdj, currPop, basePop, s_over3, o_over3 
                    #currOwnedPop, currStrayPop 
      )%>% 
      tidyr::pivot_longer(-timeAdj, names_to = 'type', values_to = 'pop')%>% 
      ggplot()+ 
      geom_col(data = plotdf_1()%>%mutate(timeAdj = timeAdj/365),
               aes(x = timeAdj, y = ifelse(is.na(tNeuters), 0, tNeuters*100)), 
               fill = 'lightblue', alpha = 1) + 
      geom_line(aes(x = timeAdj, y = pop, colour = type, linetype = type))+ 
      scale_colour_manual(name = '', values = c('#d04e00', '#d04e00', dependentCols),
                          labels = c('Baseline total', 'Total', 'Dependent 3m+', 'Independent 3m+'))+
      scale_linetype_manual(name = '', values = c('dashed', 'solid','solid','solid'),
                            labels = c('Baseline total', 'Total', 'Dependent 3m+', 'Independent 3m+'))+
      scale_y_continuous(limits = c(0,ifelse(max(plotdf_1()$currPop)<5500,5500,max(plotdf_1()$currPop))),
                         sec.axis = sec_axis(~. /100, name = 'Sterilisations per day'))+ 
      scale_x_continuous(breaks = seq(1,10,1))+ ylab("Number of dogs") + 
      xlab('Year of simulation')+ 
      theme(legend.position = 'bottom')
    
  })
  
  
  # -----PANEL 2 -----
  
  desc_val_2 <- reactive(if (input$desc_ui_2 == "Catch return guided duration") "CatchReturnGuided" else setdiff(unique(costDF$desc1),"CatchReturnGuided"))
  
  baseDF_2 <- reactive({
    req(input$desc_ui_2, input$speyingC_2, input$speyingMales_2)
    costDF %>% filter(desc1 %in% desc_val_2(), speyingC==as.numeric(input$speyingC_2), speyingMales==as.numeric(input$speyingMales_2))
  })
  
  output$conditional_ui_2 <- renderUI({
    df <- baseDF_2()
    if(nrow(df)==0) return(NULL)
    
    if(any(df$desc1=="CatchReturnGuided")){
      mc <- sort(unique(df$minCatch))
      ntO <- sort(unique(df$numTeamsOrig))
      ntC <- sort(unique(df$numTeamsChanges))
      tagList(
        selectInput("minCatch_2","Capture threshold", choices=mc, selected=mc[1]),
        selectInput("neuterBreak_2","Interval between sterilisations (days)", choices=NULL),
        selectInput("numTeamsOrig_2","Original number of teams", choices=ntO, selected=ntO[1]),
        selectInput("numTeamsChanges_2","Reduce teams if catch under threshold", choices=ntC, selected=ntC[1])
      )
    } else {
      dur <- sort(unique(df$speyingDuration))
      ntO <- sort(unique(df$numTeamsOrig))
      ntC <- sort(unique(df$numTeamsChanges))
      tagList(
        selectInput("speyingDuration_2","Length of sterilisation session (days)", choices=dur, selected=dur[1]),
        selectInput("speyingInterval_2","Interval between sterilisation sessions (days)", choices=NULL),
        selectInput("speyingRepeat_2","Number of sterilisation sessions", choices=NULL),
        selectInput("numTeamsOrig_2","Original number of teams", choices=ntO, selected=ntO[1]),
        selectInput("numTeamsChanges_2","Reduce teams if catch under threshold", choices=ntC, selected=ntC[1])
      )
    }
  })
  
  # ---- Panel 2 cascading updates ----
  
  # ---- CatchReturnGuided branch ----
  observe({
    req(input$desc_ui_2 == "Catch return guided duration")
    df <- baseDF_2() %>% filter(desc1 == "CatchReturnGuided")
    req(nrow(df) > 0)
    mc <- sort(unique(df$minCatch))
    updateSelectInput(session, "minCatch_2", choices = mc, selected = mc[1])
  })
  
  observeEvent(input$minCatch_2, {
    req(input$desc_ui_2 == "Catch return guided duration")
    df <- baseDF_2() %>% filter(desc1 == "CatchReturnGuided", minCatch == input$minCatch_2)
    req(nrow(df) > 0)
    nb <- sort(unique(df$neuterBreak))
    updateSelectInput(session, "neuterBreak_2", choices = nb, selected = nb[1])
  }, ignoreInit = FALSE)
  
  # ---- Fixed sterilisation branch ----
  observe({
    req(input$desc_ui_2 == "Fixed sterilisation duration")
    df <- baseDF_2() %>% filter(desc1 != "CatchReturnGuided")
    req(nrow(df) > 0)
    dur <- sort(unique(df$speyingDuration))
    updateSelectInput(session, "speyingDuration_2", choices = dur, selected = dur[1])
  })
  
  observeEvent(input$speyingDuration_2, {
    req(input$desc_ui_2 == "Fixed sterilisation duration")
    df <- baseDF_2() %>% filter(desc1 != "CatchReturnGuided",
                                speyingDuration == input$speyingDuration_2)
    req(nrow(df) > 0)
    ints <- sort(unique(df$speyingInterval))
    updateSelectInput(session, "speyingInterval_2", choices = ints, selected = ints[1])
  }, ignoreInit = FALSE)
  
  observeEvent(list(input$speyingDuration_2, input$speyingInterval_2), {
    req(input$desc_ui_2 == "Fixed sterilisation duration")
    req(input$speyingDuration_2, input$speyingInterval_2)
    df <- baseDF_2() %>% filter(desc1 != "CatchReturnGuided",
                                speyingDuration == input$speyingDuration_2,
                                speyingInterval == input$speyingInterval_2)
    req(nrow(df) > 0)
    reps <- sort(unique(df$speyingRepeat))
    updateSelectInput(session, "speyingRepeat_2", choices = reps, selected = reps[1])
  }, ignoreInit = FALSE)
  
  observeEvent(c(input$speyingDuration_2, input$speyingInterval_2, input$speyingRepeat_2), {
    df <- baseDF_2() %>% filter(desc1!="CatchReturnGuided", speyingDuration==input$speyingDuration_2, speyingInterval==input$speyingInterval_2,
                                speyingRepeat == input$speyingRepeat_2)
    updateSelectInput(session, "numTeamsOrig_2", choices=sort(unique(df$numTeamsOrig)), selected=sort(unique(df$numTeamsOrig))[1])
  })
  observeEvent(c(input$speyingDuration_2, input$speyingInterval_2, input$speyingRepeat_2, input$numTeamsOrig_2), {
    df <- baseDF_2() %>% filter(desc1!="CatchReturnGuided", speyingDuration==input$speyingDuration_2, speyingInterval==input$speyingInterval_2,
                                speyingRepeat == input$speyingRepeat_2, numTeamsOrig == input$numTeamsOrig_2)
    updateSelectInput(session, "numTeamsChanges_2", choices=sort(unique(df$numTeamsChanges)), selected=sort(unique(df$numTeamsChanges))[1])
  })
  
  finalDF_2 <- eventReactive(input$run_btn_2, {
    # Start from the reactive baseDF for panel 2
    df <- baseDF_2()
    # Branch depending on strategy type
    if(any(df$desc1=="CatchReturnGuided")){
      req(input$minCatch_2, input$neuterBreak_2)
      df <- df %>% filter(minCatch==input$minCatch_2, neuterBreak==input$neuterBreak_2)
    } else {
      req(input$speyingDuration_2,input$speyingInterval_2,input$speyingRepeat_2)
      df <- df %>% filter(speyingDuration==input$speyingDuration_2,
                          speyingInterval==input$speyingInterval_2,
                          speyingRepeat==input$speyingRepeat_2)
    }
    req(input$numTeamsOrig_2,input$numTeamsChanges_2)
    df %>% filter(numTeamsOrig==input$numTeamsOrig_2, numTeamsChanges==input$numTeamsChanges_2)
  })
  
  output$outcome_2 <- renderUI({
    req(input$run_btn_2)
    df <- finalDF_2()
    if(nrow(df)==0) return(HTML("No results"))
    HTML(paste0(
      '<b>Strategy outcome</b>: ', df$strategyTypePlot, "<br>",
      '<b>Strategy approach</b>: ', df$desc
    ))
  })

  plotdf_2 <- eventReactive(input$run_btn_2,{
    df<-finalDF_2()
    if(nrow(df)==0) return(HTML("No results"))
    if(nrow(df)>1) return(HTML("More than 1 result"))
    
    read_plotdf(df$desc,df$id)
  })
  
  output$sc_plot_2 <- renderPlot({
    req(input$run_btn_2)
    df <- finalDF_2()
    if(nrow(df)==0) return(NULL)
    
    plotdf_2()%>% mutate(timeAdj = timeAdj/365)%>% 
      dplyr::select(timeAdj, o_nc, s_nc ,f_nc, m_nc)%>% 
      tidyr::pivot_longer(-timeAdj, names_to = 'type', values_to = 'neuter_coverage')%>%
      ggplot(aes(x = timeAdj))+ 
      geom_rect(inherit.aes = FALSE, 
                data = data.frame( xmin = 0, xmax = Inf, ymin = min(optDF$max_f_nc), ymax = max(optDF$max_f_nc)), 
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'tomato', alpha = 0.2)+ 
      geom_line(aes(y = neuter_coverage, colour = type)) + scale_x_continuous(breaks = seq(1,10,1))+ 
      scale_y_continuous(breaks = seq(0,1, 0.2), limits = c(0,1)) + 
      scale_colour_manual(values = c( sexCols, dependentCols), labels = c('Female', 'Male', 'Dependent', 'Independent'))+ 
      ylab("Sterilisation coverage") + xlab('Year of simulation')+ theme(legend.position = 'bottom', legend.title = element_blank())

    })
  
  output$pop_plot_2 <- renderPlot({
    req(input$run_btn_2)
    df <- finalDF_2()
    if(nrow(df)==0) return(NULL)
    
    plotdf_2()%>% mutate(timeAdj = timeAdj/365)%>% 
      dplyr::select(timeAdj, currPop, basePop, s_over3, o_over3 
                    #currOwnedPop, currStrayPop 
      )%>% 
      tidyr::pivot_longer(-timeAdj, names_to = 'type', values_to = 'pop')%>% 
      ggplot()+ 
      geom_col(data = plotdf_2()%>%mutate(timeAdj = timeAdj/365),
               aes(x = timeAdj, y = ifelse(is.na(tNeuters), 0, tNeuters*100)), 
               fill = 'lightblue', alpha = 1) + 
      geom_line(aes(x = timeAdj, y = pop, colour = type, linetype = type))+ 
      scale_colour_manual(name = '', values = c('#d04e00', '#d04e00', dependentCols),
                          labels = c('Baseline total', 'Total', 'Dependent 3m+', 'Independent 3m+'))+
      scale_linetype_manual(name = '', values = c('dashed', 'solid','solid','solid'),
                            labels = c('Baseline total', 'Total', 'Dependent 3m+', 'Independent 3m+'))+
      scale_y_continuous(limits = c(0,ifelse(max(plotdf_2()$currPop)<5500,5500,max(plotdf_2()$currPop))),
                         sec.axis = sec_axis(~. /100, name = 'Sterilisations per day'))+ 
      scale_x_continuous(breaks = seq(1,10,1))+ ylab("Number of dogs") + 
      xlab('Year of simulation')+ 
      theme(legend.position = 'bottom')
                                                                                                                                                                                                                                                                                          
  })
  
  # PANEL 3 -----
  desc_val_3 <- reactive(if (input$desc_ui_3 == "Catch return guided duration") "CatchReturnGuided" else setdiff(unique(costDF$desc1),"CatchReturnGuided"))
  
  baseDF_3 <- reactive({
    req(input$desc_ui_3, input$speyingC_3, input$speyingMales_3)
    costDF %>% filter(desc1 %in% desc_val_3(), speyingC==as.numeric(input$speyingC_3), speyingMales==as.numeric(input$speyingMales_3))
  })
  
  output$conditional_ui_3 <- renderUI({
    df <- baseDF_3()
    if(nrow(df)==0) return(NULL)
    
    if(any(df$desc1=="CatchReturnGuided")){
      mc <- sort(unique(df$minCatch))
      ntO <- sort(unique(df$numTeamsOrig))
      ntC <- sort(unique(df$numTeamsChanges))
      tagList(
        selectInput("minCatch_3","Capture threshold", choices=mc, selected=mc[1]),
        selectInput("neuterBreak_3","Interval between sterilisations (days)", choices=NULL),
        selectInput("numTeamsOrig_3","Original number of teams", choices=ntO, selected=ntO[1]),
        selectInput("numTeamsChanges_3","Reduce teams if catch under threshold", choices=ntC, selected=ntC[1])
      )
    } else {
      dur <- sort(unique(df$speyingDuration))
      ntO <- sort(unique(df$numTeamsOrig))
      ntC <- sort(unique(df$numTeamsChanges))
      tagList(
        selectInput("speyingDuration_3","Length of sterilisation session (days)", choices=dur, selected=dur[1]),
        selectInput("speyingInterval_3","Interval between sterilisation sessions (days)", choices=NULL),
        selectInput("speyingRepeat_3","Number of sterilisation sessions", choices=NULL),
        selectInput("numTeamsOrig_3","Original number of teams", choices=ntO, selected=ntO[1]),
        selectInput("numTeamsChanges_3","Reduce teams if catch under threshold", choices=ntC, selected=ntC[1])
      )
    }
  })
  
  # Cascading dropdowns for Panel 3
  observe({
    df <- baseDF_3() %>% filter(desc1=="CatchReturnGuided")
    req(nrow(df) > 0)
    updateSelectInput(session, "minCatch_3", choices=sort(unique(df$minCatch)), selected=sort(unique(df$minCatch))[1])
  })
  observeEvent(input$minCatch_3, {
    df <- baseDF_3() %>% filter(desc1=="CatchReturnGuided", minCatch==input$minCatch_3)
    updateSelectInput(session, "neuterBreak_3", choices=sort(unique(df$neuterBreak)), selected=sort(unique(df$neuterBreak))[1])
  })
  observe({
    df <- baseDF_3() %>% filter(desc1=="CatchReturnGuided")
    req(nrow(df) > 0)
    updateSelectInput(session, "numTeamsOrig_3", choices=sort(unique(df$numTeamsOrig)), selected=sort(unique(df$numTeamsOrig))[1])
    updateSelectInput(session, "numTeamsChanges_3", choices=sort(unique(df$numTeamsChanges)), selected=sort(unique(df$numTeamsChanges))[1])
  })
  observe({
    df <- baseDF_3() %>% filter(desc1!="CatchReturnGuided")
    req(nrow(df) > 0)
    updateSelectInput(session, "speyingDuration_3", choices=sort(unique(df$speyingDuration)), selected=sort(unique(df$speyingDuration))[1])
  })
  observeEvent(input$speyingDuration_3, {
    df <- baseDF_3() %>% filter(desc1!="CatchReturnGuided", speyingDuration==input$speyingDuration_3)
    updateSelectInput(session, "speyingInterval_3", choices=sort(unique(df$speyingInterval)), selected=sort(unique(df$speyingInterval))[1])
  })
  observeEvent(c(input$speyingDuration_3, input$speyingInterval_3), {
    df <- baseDF_3() %>% filter(desc1!="CatchReturnGuided", speyingDuration==input$speyingDuration_3, speyingInterval==input$speyingInterval_3)
    updateSelectInput(session, "speyingRepeat_3", choices=sort(unique(df$speyingRepeat)), selected=sort(unique(df$speyingRepeat))[1])
  })
  
  observeEvent(c(input$speyingDuration_3, input$speyingInterval_3, input$speyingRepeat_3), {
    df <- baseDF_3() %>% filter(desc1!="CatchReturnGuided", speyingDuration==input$speyingDuration_3, speyingInterval==input$speyingInterval_3,
                                speyingRepeat == input$speyingRepeat_3)
    updateSelectInput(session, "numTeamsOrig_3", choices=sort(unique(df$numTeamsOrig)), selected=sort(unique(df$numTeamsOrig))[1])
  })
  observeEvent(c(input$speyingDuration_3, input$speyingInterval_3, input$speyingRepeat_3, input$numTeamsOrig_3), {
    df <- baseDF_3() %>% filter(desc1!="CatchReturnGuided", speyingDuration==input$speyingDuration_3, speyingInterval==input$speyingInterval_3,
                                speyingRepeat == input$speyingRepeat_3, numTeamsOrig == input$numTeamsOrig_3)
    updateSelectInput(session, "numTeamsChanges_3", choices=sort(unique(df$numTeamsChanges)), selected=sort(unique(df$numTeamsChanges))[1])
  })
  
  finalDF_3 <- eventReactive(input$run_btn_3, {
    df <- baseDF_3()
    if(any(df$desc1=="CatchReturnGuided")){
      req(input$minCatch_3, input$neuterBreak_3)
      df <- df %>% filter(minCatch==input$minCatch_3, neuterBreak==input$neuterBreak_3)
    } else {
      req(input$speyingDuration_3,input$speyingInterval_3,input$speyingRepeat_3)
      df <- df %>% filter(speyingDuration==input$speyingDuration_3,
                          speyingInterval==input$speyingInterval_3,
                          speyingRepeat==input$speyingRepeat_3)
    }
    req(input$numTeamsOrig_3,input$numTeamsChanges_3)
    df %>% filter(numTeamsOrig==input$numTeamsOrig_3, numTeamsChanges==input$numTeamsChanges_3)
  })
  
  output$outcome_3 <- renderUI({
    req(input$run_btn_3)
    df <- finalDF_3()
    if(nrow(df)==0) return(HTML("No results"))
    HTML(paste0(
      '<b>Strategy outcome</b>: ', df$strategyTypePlot, "<br>",
      '<b>Strategy approach</b>: ', df$desc
    ))
  })
  
  plotdf_3 <- eventReactive(input$run_btn_3,{
    df<-finalDF_3()
    if(nrow(df)==0) return(HTML("No results"))
    if(nrow(df)>1) return(HTML("More than 1 result"))
    
    read_plotdf(df$desc,df$id)
  })
  
  output$sc_plot_3 <- renderPlot({
    req(input$run_btn_3)
    df <- finalDF_3()
    if(nrow(df)==0) return(NULL)
    
    plotdf_3()%>% mutate(timeAdj = timeAdj/365)%>% 
      dplyr::select(timeAdj, o_nc, s_nc ,f_nc, m_nc)%>% 
      tidyr::pivot_longer(-timeAdj, names_to = 'type', values_to = 'neuter_coverage')%>%
      ggplot(aes(x = timeAdj))+ 
      geom_rect(inherit.aes = FALSE, 
                data = data.frame( xmin = 0, xmax = Inf, ymin = min(optDF$max_f_nc), ymax = max(optDF$max_f_nc)), 
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'tomato', alpha = 0.2)+ 
      geom_line(aes(y = neuter_coverage, colour = type)) + scale_x_continuous(breaks = seq(1,10,1))+ 
      scale_y_continuous(breaks = seq(0,1, 0.2), limits = c(0,1)) + 
      scale_colour_manual(values = c( sexCols, dependentCols), labels = c('Female', 'Male', 'Dependent', 'Independent'))+ 
      ylab("Sterilisation coverage") + xlab('Year of simulation')+ theme(legend.position = 'bottom', legend.title = element_blank())
    
  })
  
  output$pop_plot_3 <- renderPlot({
    req(input$run_btn_3)
    df <- finalDF_3()
    if(nrow(df)==0) return(NULL)
    
    plotdf_3()%>% mutate(timeAdj = timeAdj/365)%>% 
      dplyr::select(timeAdj, currPop, basePop, s_over3, o_over3 
                    #currOwnedPop, currStrayPop 
      )%>% 
      tidyr::pivot_longer(-timeAdj, names_to = 'type', values_to = 'pop')%>% 
      ggplot()+ 
      geom_col(data = plotdf_3()%>%mutate(timeAdj = timeAdj/365),
               aes(x = timeAdj, y = ifelse(is.na(tNeuters), 0, tNeuters*100)), 
               fill = 'lightblue', alpha = 1) + 
      geom_line(aes(x = timeAdj, y = pop, colour = type, linetype = type))+ 
      scale_colour_manual(name = '', values = c('#d04e00', '#d04e00', dependentCols),
                          labels = c('Baseline total', 'Total', 'Dependent 3m+', 'Independent 3m+'))+
      scale_linetype_manual(name = '', values = c('dashed', 'solid','solid','solid'),
                            labels = c('Baseline total', 'Total', 'Dependent 3m+', 'Independent 3m+'))+
      scale_y_continuous(limits = c(0,ifelse(max(plotdf_3()$currPop)<5500,5500,max(plotdf_3()$currPop))),
                         sec.axis = sec_axis(~. /100, name = 'Sterilisations per day'))+ 
      scale_x_continuous(breaks = seq(1,10,1))+ ylab("Number of dogs") + 
      xlab('Year of simulation')+ 
      theme(legend.position = 'bottom')
    
  })
  
  output$summary_table <- renderTable({
    req(input$run_btn_1>0 | input$run_btn_2>0 | input$run_btn_3>0)
    fmt <- function(df) c(
      comma(round(df$tCostsk_3dr*1000)),
      round(df$ppd_3dr,2),
      round((df$tCostsk_3dr*1000)/df$tNeuters,2),
      paste0(round(df$propPop)),
      comma(round(df$tSpeys)),
      comma(round(df$tCastrates)),
      comma(round(df$tNeuters))
    )
    vars <- c("Total cost (US$)","Population density reduction","Cost per sterilisation (US$)","% of starting population sterilised","Female sterilisations","Male sterilisations","Total sterilisations")
    s1 <- if(input$run_btn_1>0) fmt(finalDF_1()) else rep("",length(vars))
    s2 <- if(input$run_btn_2>0) fmt(finalDF_2()) else rep("",length(vars))
    s3 <- if(input$run_btn_3>0) fmt(finalDF_3()) else rep("",length(vars))
    data.frame(Metric=vars, `Strategy 1`=s1, `Strategy 2`=s2, `Strategy 3`=s3, check.names=FALSE)
  }, striped=TRUE, bordered=TRUE, spacing="s")
  
  output$all <- renderPlot({
      req(input$run_btn_1>0 | input$run_btn_2>0 | input$run_btn_3>0)
      d1 <- if(input$run_btn_1>0){ f<-finalDF_1(); costDF %>% filter(id %in% f$id) %>% mutate(panel="1") } else NULL
      d2 <- if(input$run_btn_2>0){ f<-finalDF_2(); costDF %>% filter(id %in% f$id) %>% mutate(panel="2") } else NULL
      d3 <- if(input$run_btn_3>0){ f<-finalDF_3(); costDF %>% filter(id %in% f$id) %>% mutate(panel="3") } else NULL
      selectedDF <- bind_rows(d1,d2,d3); req(nrow(selectedDF)>0)
      
      ggplot(costDF)+geom_point(aes(tCostsk,ppd, colour=strategyTypePlot), alpha=0.7)+
        geom_label(data=selectedDF,aes(tCostsk,ppd,label=panel),size=4,fontface="bold")+
        scale_colour_manual(name="",values=strategyTypeCols)+
        xlab("Total cost of campaign (1000 USD)")+ylab("Population density decrease")+
        ggtitle('Selected strategies compared to other tested strategies')+
        theme_dark()
      
    
  })
  
  
} # end server

shinyApp(ui, server)
