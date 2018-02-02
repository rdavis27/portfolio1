library(shiny)
library(lubridate)
library(quantmod)

shinyServer(function(input, output, session){

    readpass  <- "rpass"
    writepass <- "rwpass"
    portlistDF <- read.csv("portfolios.csv", stringsAsFactors = FALSE)
    updateSelectInput(session,"portfolio",choices = portlistDF$portfolio)
    
    observeEvent(input$portfile, { # new file selected
        if (input$password != writepass){
            print("Operation not allowed.")
            return(NULL)        }
        portfile <- input$portfile
        if (is.null(portfile)) return(NULL)
        print(portfile$datapath)
        tt <- read.csv(portfile$datapath, header = FALSE, stringsAsFactors = FALSE)
        hdr <- tt$V1[1]
        len <- nchar(hdr)
        hr   <- substring(hdr, len-22, len-21)
        min  <- substring(hdr, len-19, len-18)
        ampm <- substring(hdr, len-16, len-15)
        tz   <- substring(hdr, len-13, len-12) # should be ET
        mon  <- substring(hdr, len-9, len-8)
        day  <- substring(hdr, len-6, len-5)
        year <- substring(hdr, len-3)
        datetime <- paste0(year,"-",mon,"-",day," ",hr,":",min," ", ampm, " ", tz)
        #ptime <- as.POSIXlt(tt, tz="EST", "%Y-%m-%d %H:%M %p")
        pp <- read.csv(portfile$datapath, skip = 2, stringsAsFactors = FALSE)
        port <- pp[,c("Symbol","Quantity","Price","Market.Value","Cost.Basis","Reinvest.Dividends.","Description","Security.Type")]
        port <- port[port$Symbol != "Account Total",]
        colnames(port)[6] <- "Reinvest"
        port$Symbol[port$Symbol == "Cash & Money Market"] <- "Cash"
        port$Quantity     <- as.numeric(port$Quantity)
        port$Price        <- as.numeric(gsub("([$,])","",port$Price))
        port$Market.Value <- as.numeric(gsub("([$,])","",port$Market.Value))
        port$Cost.Basis   <- as.numeric(gsub("([$,])","",port$Cost.Basis))
        port$Quantity  [port$Symbol == "Cash"] <- port$Market.Value[port$Symbol == "Cash"]
        port$Price     [port$Symbol == "Cash"] <- 1
        port$Cost.Basis[port$Symbol == "Cash"] <- port$Market.Value[port$Symbol == "Cash"]
        port$Reinvest  [port$Symbol == "Cash"] <- "No"
        portTotal <- sum(port$Market.Value)
        port$Pct.Port <- 100 * port$Market.Value / portTotal
        port$Pct.Gain <- 100 * port$Market.Value / port$Cost.Basis - 100
        port$Dol.Gain <- port$Market.Value - port$Cost.Basis
        port$Pct.Port <- format(round(port$Pct.Port, 2), nsmall = 2)
        port$Pct.Gain <- format(round(port$Pct.Gain, 2), nsmall = 2)
        port$Dol.Gain <- format(round(port$Dol.Gain, 2), nsmall = 2)
        port$Datetime <- datetime
        print(pp)
        print(port)
        write.csv(port, file = paste0(input$portname,".csv"))
        
        portlistDF <- read.csv("portfolios.csv", stringsAsFactors = FALSE)
        portlist <- data.frame(portlistDF$portfolio, stringsAsFactors = FALSE)
        portitem <- data.frame(input$portname, stringsAsFactors = FALSE)
        names(portlist) <- "portfolio"
        names(portitem) <- "portfolio"
        #bind and rewrite only if portitem not already in portlist
        if (!(input$portname %in% portlist$portfolio)){
            portlist <- rbind(portlist, portitem)
            print(portitem)
            write.csv(portlist, file = "portfolios.csv")
            updateSelectInput(session, "portfolio", choices=portlist$portfolio,
                              selected=input$portname)
        }
        print(portlist)
        print(portitem)
    })
    getSpan <- reactive({
        toDate <- NULL
        if (input$span == "1M"){
            fromDate <- Sys.Date() %m-% months(1)
        }
        else if (input$span == "3M"){
            fromDate <- Sys.Date() %m-% months(3)
        }
        else if (input$span == "6M"){
            fromDate <- Sys.Date() %m-% months(6)
        }
        else if (input$span == "YTD"){
            fromDate = today()
            month(fromDate) = 1
            day(fromDate) = 1
        }
        else if (input$span == "1Y"){
            fromDate <- Sys.Date() - years(1)
        }
        else if (input$span == "2Y"){
            fromDate <- Sys.Date() - years(2)
        }
        else if (input$span == "5Y"){
            fromDate <- Sys.Date() - years(5)
        }
        else if (input$span == "10Y"){
            fromDate <- Sys.Date() - years(10)
        }
        else if (input$span == "MAX"){
            fromDate <- NULL
        }
        else{
            fromDate <- input$dateRange[1]
            toDate   <- input$dateRange[2]
        }
        if (is.null(fromDate)){
            strSpan <- paste0(fromDate, "::")
        }
        else{
            strSpan <- paste0(fromDate, "::", toDate)
        }
        strSpan
    })
    getData <- reactive({
        strSpan <- getSpan()
        #print(paste0("strSpan=",strSpan))
        portlistDF <- read.csv("portfolios.csv", stringsAsFactors = FALSE)
        filename <- paste0(input$portfolio, ".csv")
        oo <- read.csv(filename, stringsAsFactors = FALSE)
        pp <- data.frame(oo$Symbol, oo$Quantity, oo$Cost.Basis, stringsAsFactors = FALSE)
        colnames(pp) <- c("Symbol", "Quantity", "Cost.Basis")
        pp$Market.Value <- NA
        pp$Total.Gain <- NA
        pp0 <- data.frame(oo$Symbol, oo$Description, oo$Security.Type, stringsAsFactors = FALSE)
        colnames(pp0) <- c("Symbol", "Description", "Security.Type")
        pp2 <- data.frame("Symbol"=oo$Symbol, stringsAsFactors = FALSE)
        pp2$Day1.Gain   <- NA
        pp2$Day5.Gain   <- NA
        pp2$Select.Gain <- NA
        pp3 <- data.frame("Symbol"=oo$Symbol, stringsAsFactors = FALSE)
        pp3$Day1.Gain   <- NA
        pp3$Day5.Gain   <- NA
        pp3$Select.Gain <- NA
        ttoday <- 0
        tday1  <- 0
        tday5  <- 0
        tfirst <- 0
        tlast  <- 0
        lastdate <<- NULL
        for (i in 1:NROW(pp)){
            symbol <- pp$Symbol[i]
            if (symbol != "Cash"){
                getSymbols(pp$Symbol[i], src = 'yahoo', from='1900-01-01')
                gdata <- get(symbol)
                #lastdate <<- max(lastdate, date(gdata[NROW(gdata)]))
                lastdate <<- date(gdata[NROW(gdata)])
                hdata <- gdata[strSpan]
                if (input$adjusted == TRUE){
                    ptoday <- as.numeric(Ad(gdata[NROW(gdata)]))
                    pday1  <- as.numeric(Ad(gdata[NROW(gdata)-1]))
                    pday5  <- as.numeric(Ad(gdata[NROW(gdata)-5]))
                    pfirst <- as.numeric(Ad(hdata[1]))
                    plast  <- as.numeric(Ad(hdata[NROW(hdata)]))
                }
                else{
                    ptoday <- as.numeric(Cl(gdata[NROW(gdata)]))
                    pday1  <- as.numeric(Cl(gdata[NROW(gdata)-1]))
                    pday5  <- as.numeric(Cl(gdata[NROW(gdata)-5]))
                    pfirst <- as.numeric(Cl(hdata[1]))
                    plast  <- as.numeric(Cl(hdata[NROW(hdata)]))
                }
                pp$Market.Value[i] <- ptoday * pp$Quantity[i]
                #pp$Total.Gain[i]   <- (pricen - price0) * pp$Quantity[i]
                pp$Total.Gain[i] <- ptoday * pp$Quantity[i] - pp$Cost.Basis[i]
                ttoday <- ttoday + ptoday
                tday1  <- tday1  + pday1
                tday5  <- tday5  + pday5
                tfirst <- tfirst + pfirst
                tlast  <- tlast  + plast
                pp2$Day1.Gain[i]   <- (ptoday - pday1) * pp$Quantity[i]
                pp2$Day5.Gain[i]   <- (ptoday - pday5) * pp$Quantity[i]
                pp2$Select.Gain[i] <- (plast - pfirst) * pp$Quantity[i]
                pp3$Day1.Gain[i]   <- 100 * (ptoday - pday1) / pday1
                pp3$Day5.Gain[i]   <- 100 * (ptoday - pday5) / pday5
                pp3$Select.Gain[i] <- 100 * (plast - pfirst) / pfirst
            }
            else{
                pp$Market.Value[i] <- pp$Cost.Basis[i]
                pp$Total.Gain[i]   <- 0
                pp2$Day1.Gain[i]   <- 0
                pp2$Day5.Gain[i]   <- 0
                pp2$Select.Gain[i] <- 0
                pp3$Day1.Gain[i]   <- 0
                pp3$Day5.Gain[i]   <- 0
                pp3$Select.Gain[i] <- 0
                pp0$Description[i] <- pp0$Security.Type[i]
            }
        }
        pp1 <- pp[,c(1,2,4,5)]
        pp1 <- rbind(pp1, c("Total", "", colSums(pp1[,3:4])))
        pp2 <- rbind(pp2, c("Total", colSums(pp2[,2:4])))
        pp3 <- rbind(pp3, c("Total", 100*(ttoday-tday1)/tday1, 100*(ttoday-tday5)/tday5, 100*(tlast-tfirst)/tfirst))
        pp1[,3] <- as.numeric(pp1[,3])
        pp1[,4] <- as.numeric(pp1[,4])
        pp2[,2] <- as.numeric(pp2[,2])
        pp2[,3] <- as.numeric(pp2[,3])
        pp2[,4] <- as.numeric(pp2[,4])
        pp3[,2] <- as.numeric(pp3[,2])
        pp3[,3] <- as.numeric(pp3[,3])
        pp3[,4] <- as.numeric(pp3[,4])
        pp1$Market.Value<- format(round(pp1$Market.Value,2), nsmall = 2)
        pp1$Total.Gain  <- format(round(pp1$Total.Gain,  2), nsmall = 2)
        pp2$Day1.Gain   <- format(round(pp2$Day1.Gain,   2), nsmall = 2)
        pp2$Day5.Gain   <- format(round(pp2$Day5.Gain,   2), nsmall = 2)
        pp2$Select.Gain <- format(round(pp2$Select.Gain, 2), nsmall = 2)
        pp3$Day1.Gain   <- format(round(pp3$Day1.Gain,   2), nsmall = 2)
        pp3$Day5.Gain   <- format(round(pp3$Day5.Gain,   2), nsmall = 2)
        pp3$Select.Gain <- format(round(pp3$Select.Gain, 2), nsmall = 2)
        if (input$span == "Use above dates"){
            span.label <- "Select.Gain"
        }
        else span.label <- paste0(input$span,".Gain")
        colnames(pp2) <- c("Symbol", "1D.Gain", "5D.Gain", span.label)
        colnames(pp3) <- c("Symbol", "1D.Gain", "5D.Gain", span.label)
        pp0 <<- pp0[,c(1,2)]
        pp1 <<- pp1
        pp2 <<- pp2
        pp3 <<- pp3
    })
    output$dollars <- renderPrint({
        if (input$password == readpass | input$password == writepass){
            getData()
            print(pp2)
            cat(paste0("\nlast date = ", lastdate, "\n"))
            cat(file=stderr(), paste0("output$dollars:     ",input$portfolio,"|",input$span,
                                      "|",input$dateRange[1],"|",input$dateRange[2],"\n"))
        }
        else cat(file=stderr(), paste0("##### output$dollars:     invalid password: ",input$password,"\n"))
    })
    output$percent <- renderPrint({
        if (input$password == readpass | input$password == writepass){
            getData()
            print(pp3)
            cat(paste0("\nlast date = ", lastdate, "\n"))
            cat(file=stderr(), paste0("output$percent:     ",input$portfolio,"|",input$span,
                                      "|",input$dateRange[1],"|",input$dateRange[2],"\n"))
        }
        else cat(file=stderr(), paste0("##### output$percent:     invalid password: ",input$password,"\n"))
    })
    output$portfolio <- renderPrint({
        if (input$password == readpass | input$password == writepass){
            getData()
            print(pp1)
            cat(paste0("\nlast date = ", lastdate, "\n"))
            cat(file=stderr(), paste0("output$portfolio:   ",input$portfolio,"|",input$span,
                                      "|",input$dateRange[1],"|",input$dateRange[2],"\n"))
        }
        else cat(file=stderr(), paste0("##### output$portfolio:   invalid password: ",input$password,"\n"))
    })
    output$description <- renderPrint({
        if (input$password == readpass | input$password == writepass){
            getData()
            print(pp0)
            cat(paste0("\nlast date = ", lastdate, "\n"))
            cat(file=stderr(), paste0("output$description: ",input$portfolio,"|",input$span,
                                      "|",input$dateRange[1],"|",input$dateRange[2],"\n"))
        }
        else cat(file=stderr(), paste0("##### output$description: invalid password: ",input$password,"\n"))
        #rr2[,2] <- as.numeric(rr2[,2])
        #rr2[,3] <- as.numeric(rr2[,3])
        #rr2[,4] <- as.numeric(rr2[,4])
        #y <- rbind(rr2, c("Total", colSums(rr2[,2:4])))
    })
})
