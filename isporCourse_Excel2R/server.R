


##===========================  general model setting
if(TRUE){
  saveFig <- FALSE #save figure as external files
  
  # util functions
  if(TRUE){
    round(.125,2)  #quick test: if it is 0.12, need to overwrite for consistency in reporting format across softwares
    round <- function(x, digits=0, formatIt=FALSE) {
      posneg = sign(x)
      z = abs(x)*10^digits
      z = z + 0.5 + sqrt(.Machine$double.eps)
      z = trunc(z)
      z = z/10^digits
      if(formatIt) z <- format(z*posneg, nsmall=digits)
      if(!formatIt) z <- z*posneg
      return(z)
    }
    round(.125,2)  #now it is 0.13
  }
  
  ##===========================  general setting, input data preparation
  nstate <- length(states <- c('Active', 'Remission', 'Hospitalized', 'Death'))
  ntreat <- length(treatList <- c('Endoscopy','Biomarkers'))
  #output variables: Health state Cost:Active/Remission/Hospitalized/Death and treatment, LY, QALY
  nvar <- length(varList <- c(paste0('Cost:',states), 'Cost:Treatment', 'LY', 'QALY')) 
  nstateSquare <- nstate*nstate #size of transition matrix
  
  #-----------------  simulation function
  microSim <- function(pid,   #patient ID: 1 to Cohort_size
                       input, values, #2 interactive variable list needs user input
                       Average_Cost_Endoscopy=NULL,  #placeholder to allow customized values than default
                       Utilities_by_health_state_Active=NULL, #example, can add more variables
                       verbose=FALSE){ 
    
    #--------------------------  Settings
    Cohort_size <- input$Cohort_size  #or increase to a large number, e.g., 5000 for full runs
    Cycle_Length_weeks <- input$cycle_length_weeks
    Time_horizon_years <- input$Time_horizon_years
    Number_of_cycles_per_year <- input$Number_of_cycles_per_year
    Age_at_baseline_years <- input$age_at_baseline
    Percentage_male_at_baseline <- input$percent_male
    ncycle <- 100  #number of simulated cycles
    
    ##------------- map the input: tables
    Clinical_Inputs <- values[["clinical_input_csv"]]
    Life_tables <- values[["life_tables_csv"]]
    ageMax <- max(Life_tables$Age) #max age to be included in time horizon
    
    #--------------------------  Quality Of Life Inputs
    Utilities_by_health_state <- c(Active = input$QoL_Active, 
                                   Remission = input$QoL_Remission, 
                                   Hospitalized = input$QoL_Hospitalized)
    
    Health_State_Costs_input <- c(Active = input$HSC_Active, 
                                  Remission = input$HSC_Remission, 
                                  Hospitalized = input$HSC_Hospitalized, 
                                  Death = input$HSC_Death)
    Average_Cost <- c(Biomarkers=input$Cost_Biomarkers, Endoscopy=input$Cost_Endoscopy)
    
    #++++++++++++++++ replace preset with customized values
    Average_CostUse <- Average_Cost
    if(!is.null(Average_Cost_Endoscopy)) Average_CostUse[['Endoscopy']] <- Average_Cost_Endoscopy
    Utilities_by_health_stateUse <- Utilities_by_health_state
    if(!is.null(Utilities_by_health_state_Active)) 
      Utilities_by_health_stateUse[['Active']] <- Utilities_by_health_state_Active
    
    out <- rep(NA, nvar*ntreat)
    # RN <- Random_Numbers[1:ncycle,] #random numbers generated from Excel model to reproduce result for patient 1
    # RN_age <- RN$Age[1];  RN_sex <- RN$Sex[1]
    if(pid>=1){ #for patients other than the demo, will use R random number generator
      set.seed(pid*5) #for reproducibility
      RN <- matrix(runif(n=ncycle*nstateSquare), ncol=nstateSquare)
      RN_age <- runif(1); RN_sex <- runif(1)
    }
    age0 <- qnorm(p=RN_age, mean=Age_at_baseline_years, sd=Age_at_baseline_years/10)
    male <- qbinom(p=RN_sex, size=1, prob=Percentage_male_at_baseline)
    cycle <- 1:ncycle
    age <- sapply(age0+cycle, function(x) min(x,ageMax))
    Included_in_time_horizon <- as.numeric(c(age0, age)<ageMax)
    inds <- match(floor(age), Life_tables$Age)
    j <- which(names(Life_tables) == ifelse(male==1, 'Male', 'Female'))
    Death_probability <- Life_tables[inds,j]
    
    #-------------------------- Stochastic Matrix construction
    res <- matrix(NA, nvar, ntreat)
    for(treatId in 1:ntreat){ #for each treatment option
      treat <- treatList[treatId]
      # treat <- 'Endoscopy'
      # treat <- 'Biomarkers'
      treat_effect <- with(Clinical_Inputs, Clinical_Inputs[Gender==ifelse(male==1, 'Males','Females')&Type==treat,]) #Treatment Effectiveness
      
      Stochastic_Matrix <- matrix(NA, ncycle, nstateSquare)
      fromIndex <- rep(1:nstate, each=nstate)
      for(i in 1:nstate){ #from state i
        inds <- which(fromIndex==i)
        RN_from <- as.data.frame(RN[,inds]); names(RN_from) <- states
        j <- which(treat_effect$From==states[i])
        si4 <- as.numeric(RN_from$Death < Death_probability | 
                            RN_from$Death < treat_effect[j,'Death']) #Active -> Death
        si2 <- as.numeric(si4!=1 & RN_from$Remission < treat_effect[j,'Remission'])  #Active -> Remission
        si3 <- as.numeric(si4!=1 & si2!=1 & RN_from$Hospitalized < treat_effect[j,'Hospitalized'])  #Active -> Hospitalized
        si1 <- 1-si2-si3-si4 #Active -> Active
        Stochastic_Matrix[,inds] <- cbind(si1, si2, si3, si4)
      }
      Stochastic_Matrix <- rbind(c(1,rep(0,nstateSquare-1)), Stochastic_Matrix) #add initial state
      
      #--------------------------  State occupied at time t
      stateMat <- matrix(NA, 1+ncycle, nstate)
      stateMat[1,] <- c(1,0,0,0) #initial state with Active
      for(i in 1:ncycle){
        stateMat[i+1,] <- rowSums( stateMat[rep(i,nstate),]*matrix(Stochastic_Matrix[i,], ncol=nstate))
      }
      stateTrans <- states[apply(stateMat,1,function(x) which(x>0))] #for output to track states
      
      #-------------------------- Health state cost
      IndexDeath <- which(states=='Death')
      Health_state_cost <- matrix(rep(Health_State_Costs_input, each=1+ncycle), ncol=nstate) * stateMat
      Health_state_cost_death <- ifelse(sum(Health_state_cost[,IndexDeath])>0, Health_State_Costs_input[['Death']], 0)  #Death cost only counted once
      Health_state_cost <- c(colSums(Health_state_cost[,-IndexDeath]), Health_state_cost_death)
      
      Treatment_cost <- Average_CostUse[[treat]]*(1-stateMat[,IndexDeath]) * Included_in_time_horizon
      Life_years <- (1-stateMat[,IndexDeath])
      
      Health_state_utilities <- matrix(rep(Utilities_by_health_stateUse, each=1+ncycle), ncol=nstate-1) * 
        stateMat[,match(names(Utilities_by_health_stateUse), states)]
      
      QALYs <- rowSums(Health_state_utilities) * Included_in_time_horizon
      
      res[,treatId] <- c(Health_state_cost, sum(Treatment_cost), sum(Life_years), sum(QALYs))
    } #end of for loop for each treatment option
    
    out <- as.vector(res)
  }
  
  out2icer <- function(out, printTable=FALSE, fullTable=FALSE){  #convert output to ICER
    rpt <- matrix(colMeans(out), ncol=ntreat); rownames(rpt) <- varList
    inds <- grepl('Cost:',varList)
    Total_cost <- colSums(rpt[inds,])
    i1 <- which(treatList=='Biomarkers') #choose comparator
    i0 <- which(treatList=='Endoscopy') 
    Incre_Total <- Total_cost[i1] - Total_cost[i0]
    Incre_QALY <- rpt['QALY',i1] - rpt['QALY',i0]
    Incre_LY <- rpt['LY',i1] - rpt['LY',i0]
    ICER_QALY <- Incre_Total/Incre_QALY
    ICER_LY <- Incre_Total/Incre_LY
    
    inds <- c(i1,i0)
    tab <- data.frame('Total Cost' = round(Total_cost[inds]), 
                      'Total QALYs' = rpt['QALY',inds], 
                      'Total LYs' = rpt['LY',inds],
                      'Incremental Costs' = c('', round(Incre_Total)), 
                      'Incremental QALYs' = c('', round(Incre_QALY,2)), 
                      'Incremental LYs' = c('', round(Incre_LY,2)),
                      'ICER (per QALY)' = c('', round(ICER_QALY)), 
                      'ICER (per LY)' = c('', round(ICER_LY)),
                      check.names=FALSE)
    row.names(tab) <- treatList[inds]
    if(printTable) print(tab)
    output <- ICER_QALY; if(fullTable) output <- tab
    return(output)
  }
}


##----------------------------  input tables
if(TRUE){
  clinical_input_csv <- read.csv('Clinical_Inputs.csv')
  life_tables_csv <- read.csv('lifeTable.csv')
}


shinyServer(function(input, output, session){
  
  if(TRUE){
    values <- reactiveValues()
    
    ## Handsontable
    observe({
      if (!is.null(input$clinical_input_csv)) {
        clinical_input_csv = hot_to_r(input$clinical_input_csv)
      } else {
        if(is.null(values[["clinical_input_csv"]]))
          clinical_input_csv <- clinical_input_csv
        else
          clinical_input_csv <- values[["clinical_input_csv"]]
      }
      values[["clinical_input_csv"]] <- clinical_input_csv
    })
    output$clinical_input_csv <- renderRHandsontable({
      clinical_input_csv <- values[["clinical_input_csv"]]
      if (!is.null(clinical_input_csv))
        rhandsontable(clinical_input_csv, useTypes=TRUE, stretch="all") %>%
        hot_col(c("Gender","Type","From"), readOnly = TRUE) #fix certain columns (or rows)
    })
    
    observe({
      if (!is.null(input$life_tables_csv)) {
        life_tables_csv = hot_to_r(input$life_tables_csv)
      } else {
        if(is.null(values[["life_tables_csv"]]))
          life_tables_csv <- life_tables_csv
        else
          life_tables_csv <- values[["life_tables_csv"]]
      }
      values[["life_tables_csv"]] <- life_tables_csv
    })
    output$life_tables_csv <- renderRHandsontable({
      life_tables_csv <- values[["life_tables_csv"]]
      if (!is.null(life_tables_csv))
        rhandsontable(life_tables_csv, useTypes=TRUE, stretch="all")
    })
    
  }
  
  
  ##----------------------------  run simulation
  useData <- reactive({
    input$goButton
    isolate({
      if(input$goButton>0){
        Cohort_size <- input$Cohort_size
        
        ptm <- proc.time()[3]  #timing
        out <- matrix(NA, Cohort_size, nvar*ntreat)
        for(i in 1:Cohort_size) out[i,] <- microSim(i, input, values)
        cputime <- as.numeric(proc.time()[3]-ptm)
        cat('\nCPUtime', round(cputime,3), 'seconds: completed!','\n')  
        
        ##===========================  Results
        icer <- out2icer(out, printTable=FALSE, fullTable=TRUE)  
        icer <- as.data.frame(icer)
        
        return(list(out=out, icer=icer))
        
      }
    })
  })
  
  # get tables
  output$tab <- DT::renderDataTable({
    input$goButton
    isolate({
      if(!is.null(useData())){
        tmp <- useData()
        mat <- tmp$icer
        inds <- which(names(mat) %in% c('Total QALYs','Total LYs'))
        mat[,inds] <- round(mat[,inds],2, formatIt=TRUE)
        DT::datatable(mat, options=list(pageLength=12), rownames=TRUE, escape=FALSE)
      }
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste0('Output_', ifelse(is.null(input$Session), Sys.time(), input$Session), '.csv')
    }, 
    content = function(files){
      tmp <- useData()
      mat <- tmp$icer
      write.csv(mat, files, row.names=FALSE, na='')
    }
  )
  
  
  ##------------------------------------------ get figures
  if(FALSE){  #plain figure
    # convergence plot
    output$convergence <- renderPlot({
      input$goButton
      if(!is.null(useData())){
        isolate({
          tmp <- useData()
          out <- tmp$out
          icerVec <- rep(NA, len <- length(knot0 <- round(seq(2,nrow(out),length=20)) ))
          for(i in 1:len) icerVec[i] <- out2icer(out[1:knot0[i],])
          
          par(mfrow=c(1,1), oma=c(1,2,0,0), mar=c(2, 2, 1.4, .3), mgp=c(1.1,.2,0), tck=-0.01,
              cex.axis=.9, cex.main=1, cex.lab=.8, font.lab=2, las=1, xpd=FALSE)
          for(j in 2:ntreat){
            plot(icerVec~knot0, type='n', pch=16, main='Convergence Plot', xlab='', ylab='')
            grid(col='gray50')
            lines(icerVec~knot0, lwd=2)
            points(icerVec~knot0, pch=16)
          }
          par(las=3); mtext(side=c(2), text=c('ICER per QALY'), outer=TRUE, line=1)
          par(las=1); mtext(side=c(1), text=c('Cohort size'), outer=TRUE)
          
        })
      }
    })
    output$convergence.ui <- renderUI({
      input$goButton
      isolate({
        # plotOutput("plot1", height=PlotSize()$height, width=PlotSize()$width)
        plotOutput("convergence", width='70%')  #, height=450, width=640
      })
    })
  }
  
  # convergence plot: interactive plot using R package plotly
  output$plot2 <- renderPlotly({
    input$goButton
    if(!is.null(useData())){
      tmp <- useData()
      out <- tmp$out
      icerVec <- rep(NA, len <- length(knot0 <- round(seq(2,nrow(out),length=20)) ))
      for(i in 1:len) icerVec[i] <- out2icer(out[1:knot0[i],])
      x <- data.frame(average=icerVec, simulation=knot0)
      p = plot_ly(x, x = ~simulation, y = ~average, type = 'scatter', mode = 'lines') %>%
        layout(
          title = "Average of total cost over simulations (ICER per QALY)",
          xaxis = list(title = "Simulation"),
          yaxis = list(title = "Cumulative average of total costs")
        )
      return(p)
    }
  })
  output$convergence.ui <- renderUI({
    input$goButton
    isolate({
      plotlyOutput("plot2", width="90%")
    })
  })
  
  
  
  ##------------------------------------------ sensitivity analysis
  sensitivity <- reactive({
    input$goSensitivity
    isolate({
      if(input$goSensitivity>0){
        
        #---------- create a task matrix to list scenarios
        nam1 <- input$x1nam;  n1 <- input$len1
        nam2 <- input$x2nam;  n2 <- input$len2
        n1 <- length( knot1 <- seq(input$x1min, input$x1max, length=n1) )
        n2 <- length( knot2 <- seq(input$x2min, input$x2max, length=n2) )
        task <- numeric()
        for(i in 1:n1){ 
          for(j in 1:n2){
            for(pid in 1:input$Cohort_size_sen2d){ #patient ID
              task <- rbind(task, c(i, j, pid))
            }
          }
        }
        N <- nrow(task)
        task <- as.data.frame(task)
        names(task) <- c(nam1,nam2,'pid')
        
        # sensitivity analysis
        ptm <- proc.time()[3]  #timing
        outS <- matrix(NA, N, nvar*ntreat)
        for(i in 1:N) 
          outS[i,] <- microSim(task[i,'pid'], input, values, knot1[task[i,nam1]], knot2[task[i,nam2]])
        cputime <- as.numeric(proc.time()[3]-ptm)
        cat('\nCPUtime', round(cputime,3), 'seconds: completed!','\n')  
        
        icerMat <- matrix(NA, n1, n2)
        for(i in 1:n1){
          for(j in 1:n2){
            out_ij <- outS[task[[nam1]]==i & task[[nam2]]==j, ]
            icerMat[i,j] <- out2icer(out_ij)
          }
        }
        
        return(list(icerMat=icerMat, knot1=knot1, knot2=knot2, nam1=nam1, nam2=nam2))
        
      }
    })
  })
  
  # Heatmap
  output$heat <- renderPlot({
    input$goSensitivity
    if(!is.null(sensitivity())){
      isolate({
        tmp <- sensitivity()
        icerMat <- tmp$icerMat; knot1 <- tmp$knot1; knot2 <- tmp$knot2; nam1 <- tmp$nam1; nam2 <- tmp$nam2
        n1 <- length(knot1); n2 <- length(knot2)
        
        #-------------------- 2-d sensitivity analysis with heatmap
        par(mfrow=c(1,1), mar=c(2.2,2.4,1.3,0)+.3, mgp=c(1.4,.2,0), tck=-0.01, cex.axis=.75, cex.main=1.1, cex.lab=1)
        J <- length(cols <- c('#d73027','#f46d43','#fdae61','#fee08b','#ffffbf','#d9ef8b','#a6d96a','#66bd63','#1a9850'))
        customizedCut <- c(2e4,3e5)
        cuts <- sort(c(min(icerMat)-1, quantile(icerMat,c(.05,.25,.5,.75,.975)),0,customizedCut,max(icerMat)+1))
        par('xaxt'='n',yaxt='n')
        image(knot1, knot2, icerMat, col=cols[1:(length(cuts)-1)],
              breaks=cuts,
              xlab=nam1,
              ylab=nam2, 
              main='ICER per QALY'
        )
        par('xaxt'='s',yaxt='s')
        axis(side=1, at=knot1, labels=round(knot1,2, formatIt=TRUE))
        par(las=1); axis(side=2, at=knot2, labels=round(knot2,2, formatIt=TRUE))
        contour(knot1, knot2, icerMat, levels=customizedCut, add=TRUE, labcex=1.2, col='blue', lwd=2)
        z <- as.vector(icerMat); text(rep(knot1, n2), rep(knot2,each=n1), round(z,2), cex=1.1)
      })
    }
  })
  output$heat.ui <- renderUI({
    input$goButton
    isolate({
      plotOutput("heat", width='100%')
    })
  })
  
}) 



