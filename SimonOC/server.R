

# SimonOC: Simon's 2-stage design with Operating Characteristics
# by Zhen Zhang (zhangquake1@outlook.com)

require(shiny)
require(clinfun)

shinyServer(function(input, output, session){
  
  # get data
  useData <- reactive({
    input$goButton
    isolate({
      pu <- input$pu   #Unacceptable
      pa <- input$pa  #Target value (TV)
      e1 <- input$e1  #type 1 error
      e2 <- 1-input$pwr   #power=1-e2
      
      # Simon 2-stage
      tmp <- clinfun::ph2simon(pu=pu, pa=pa, ep1=e1, ep2=e2, nmax=100)
      mat <- as.data.frame(tmp$out)
      design <- mat[order(mat$`EN(p0)`)[1], ]  #optimal design
      design_minimax <- mat[order(mat$n)[1], ]  #minimax design
      
      # compare 2-stage design with 1-stage design based on 1-sided Binomial exact test
      nvec <- 10:100
      cv <- qbinom(1-e1, nvec, pu)  #critical value
      pwr0 <- 1-pbinom(cv, nvec, pa) #power
      # plot(pwr0~nvec, type='l'); points(pwr0~nvec, pch=16); abline(h=1-e2, col='red2') #non-monotonic
      i0 <- which(pwr0>=1-e2)[1]
      n0 <- nvec[i0] #the first one, could have better criterion
      design_1s <- data.frame(r1=NA, n1=NA, r=cv[i0], n=n0, 'EN(p0)'=NA, 'PET(p0)'=NA,
                              check.names=FALSE)
      dsn <- rbind(design, design_minimax, design_1s)
      
      p_rej <- function(pt, des=design){
        r1 <- des[['r1']];  n1 <- des[['n1']];  r <- des[['r']];  n <- des[['n']]  
        pr <- pbinom(r1,n1,pt)
        if(r1+1 <= min(n1,r))
          for(x in (r1+1):min(n1,r))
            pr <- pr + dbinom(x,n1,pt)*pbinom(r-x,n-n1,pt)
        return(pr)
      }
      
      p_go <- function(pt, des=design){
        r1 <- des[['r1']];  n1 <- des[['n1']];  r <- des[['r']];  n <- des[['n']]
        r0 <- round(des[['n']]*pa) - 1
        pr <- 0
        for(x in (r1+1):n1)
          pr <- pr + dbinom(x,n1,pt)*(1-pbinom(r0-x,n-n1,pt))
        return(pr)
      }
      
      dsn$type1_error <- 1-c(p_rej(pu), p_rej(pu, des=design_minimax), pbinom(cv[i0], n0, pu))
      dsn$power <- 1-c(p_rej(pa), p_rej(pa, des=design_minimax), pbinom(cv[i0], n0, pa))
      row.names(dsn) <- c('2-stage optimal','2-stage minimax','1-stage Binomial exact')
      
      return(list(dsn=dsn))
    })
  })
  
  # get fiugres
  output$plot1 <- renderPlot({
    input$goButton
    if(!is.null(useData())){
      isolate({
        pu <- input$pu   #Unacceptable
        pa <- input$pa  #Target value (TV)
        e1 <- input$e1  #type 1 error
        e2 <- 1-input$pwr   #power=1-e2
        
        dsn <- useData()$dsn
        design <- dsn['2-stage optimal',]
        n0 <- dsn['1-stage Binomial exact','n']
        
        p_rej <- function(pt, des=design){
          r1 <- des[['r1']];  n1 <- des[['n1']];  r <- des[['r']];  n <- des[['n']]  
          pr <- pbinom(r1,n1,pt)
          if(r1+1 <= min(n1,r))
            for(x in (r1+1):min(n1,r))
              pr <- pr + dbinom(x,n1,pt)*pbinom(r-x,n-n1,pt)
          return(pr)
        }
        
        p_go <- function(pt, des=design){
          r1 <- des[['r1']];  n1 <- des[['n1']];  r <- des[['r']];  n <- des[['n']]
          r0 <- round(des[['n']]*pa) - 1
          pr <- 0
          for(x in (r1+1):n1)
            pr <- pr + dbinom(x,n1,pt)*(1-pbinom(r0-x,n-n1,pt))
          return(pr)
        }
        
        vec <- seq(input$b1[1], input$b1[2], len=1e3)
        pet <- pbinom(design[['r1']], design[['n1']], vec)
        pwr <- 1-sapply(vec, p_rej)
        ego <- sapply(vec, p_go)
        en <- 1-( design[['n1']] + (1-pet)*(design[['n']]-design[['n1']]) )/design[['n']]
        ego1s <- 1 - pbinom(floor(n0*pa), n0, vec)
        
        par(mar=c(2,2.4,4,.3)+.2, mgp=c(1.3,.3,0), tck=-0.01, cex.axis=1, cex.lab=1.1, cex.main=1.3, las=1, font.lab=2) 
        cols <- c('red','green3','blue','purple3')
        par(yaxt='n')
        plot(1, type='n', xlim=range(vec), ylim=c(0,1), 
             xlab='True Response Rate',   #substitute(paste(a, mu), list(a='True ')), 
             ylab='', #'Probability', 
             main=paste0("Operating Characteristics of Two-Stage Optimal Design\nH0: p<=",round(pu,2)," vs H1: p>=",round(pa,2), "\nStop if responders<=",design[['r1']]," out of ",design[['n1']],", and <=",design[['r']]," out of ",design[['n']], ", ESoE Go if >=",pa))
        par(yaxt='s'); ylabs <- c(0,.05,.25,.5,.75,.9,1); axis(side=2, at=ylabs, labels=paste0(100*ylabs,'%'))
        usr <- par('usr');  xran <- diff(usr[1:2]);  yran <- diff(usr[3:4])
        text(x=usr[1]-0.095*xran, y=usr[3]+.5*yran, labels='Probability', srt=90, font=2, xpd=NA, cex=1.1)
        abline(h=c(0, e1, 0.5, 1-e2), lty=3)
        abline(v=pu, col=cols[1], lty=3)
        abline(v=pa, col=cols[2], lty=3)
        lines(pet~vec, col=cols[1], lwd=2)
        lines(pwr~vec, col=cols[2], lwd=2)
        lines(ego~vec, col=cols[3], lwd=2)
        lines(ego1s~vec, col=cols[3], lwd=2, lty=2)
        lines(en~vec, col=cols[4], lwd=4)
        legend(x=usr[2]-0.28*xran, y=usr[3]+.52*yran, legend=c('Early Termination','Reduce sample %','Continuation','ESoE Go','ESoE Go (1-stage)'), col=cols[c(1,4,2,3,3)], lty=c(1,1,1,1,2), lwd=c(2,4,2,2,2), bg='white', cex=1, bty='n', text.font=2, text.width=4)
        
        return(dsn)
      })
    }
  })

  # render figure for OC curve
  output$plot.ui <- renderUI({
    input$goButton
    isolate({
      plotOutput("plot1", height=400, width=600)
    })
  })
  
  # render table output for design
  output$tab <- DT::renderDataTable({
    input$goButton
    isolate({
      if(!is.null(useData())){
        dsn <- round(useData()$dsn, 3)
        DT::datatable(dsn, options=list(pageLength = 10), rownames=TRUE, escape=FALSE)
      }
    })
  })
  
  # render downloadable output for design
  output$downloadData <- downloadHandler(
    filename = function() { paste('Output.csv', sep='') },   #Sys.Date()
    content = function(files) {
      dsn <- useData()$dsn
      write.csv(mat, files, row.names = FALSE)
    }
  )
  
}) 


