#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(deSolve)
library(reshape2)
library(plotly)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
    

    # output$distPlot <- renderPlot({
    # 
    #     df <- data.frame(Gender=c("Male", "Female"),
    #                      Number=c(200, 108))
    #     head(df)
    #     p<-ggplot(data=df, aes(x=Gender, y=Number)) +
    #         geom_bar(stat="identity")
    #     p
    # })
    
    sir <- function(time, state, parameters) {
        with(as.list(c(state, parameters)), {
            #I:Indoor  #BAI DI1 -BFO   -St                                       +Adopt - Stray rate     +Indoor vac -vaccov*                         Sterilized
            ds1FI <- (p*BAI*s2FI)+(p*BEI*s2FI)-m*(1/365)*s1FI-DI1*s1FI           +AR*s1FS-SR*s1FI      -vac.FPI*s1FI                                  -St.FPI*s1FI
            ds2FI <- m*(1/365)*s1FI-m*(1/(365*8))*s2FI-DI2*s2FI                  +AR*s2FS-SR*s2FI      +m*(1/365)*s1FIV+m*(1/365)*s2FIV-vac.FAI*s2FI  -St.FAI*s2FI
            ds3FI <- m*(1/(365*8))*s2FI-m*(1/(365*5))*s3FI-DI3*s3FI              +AR*s3FS-SR*s3FI      +m*(1/365)*s3FIV-vac.FEI*s3FI                  -St.FEI*s3FI
            ds1MI <- ((1-p)*BAI*s2FI)+((1-p)*BEI*s2FI)-m*(1/365)*s1MI-DI1*s1MI   +AR*s1MS-SR*s1MI      -vac.MPI*s1MI                                  -St.MPI*s1MI
            ds2MI <- m*(1/365)*s1MI-m*(1/(365*8))*s2MI-DI2*s2MI                  +AR*s2MS-SR*s2MI      +m*(1/365)*s1MIV+m*(1/365)*s2MIV-vac.MAI*s2MI  -St.MAI*s2MI
            ds3MI <- m*(1/(365*8))*s2MI-m*(1/(365*5))*s3MI-DI3*s3MI              +AR*s3MS-SR*s3MI      +m*(1/365)*s3MIV-vac.MEI*s3MI                  -St.MEI*s3MI     
            
            #O:Outdoor                                                           +Adopt - Stray rate          Indoor vac                             sterilized
            ds1FO <- (p*BAO*s2FO)+(p*BEO*s2FO)-m*(1/365)*s1FO-DO1*s1FO           +AR*s1FS-SR*s1FO       -vac.FPO*s1FO                                  -St.FPO*s1FO
            ds2FO <- m*(1/365)*s1FO-m*(1/(365*8))*s2FO-DO2*s2FO                  +AR*s2FS-SR*s2FO       +m*(1/365)*s1FOV+m*(1/365)*s2FOV-vac.FAO*s2FO  -St.FAO*s2FO
            ds3FO <- m*(1/(365*8))*s2FO-m*(1/(365*5))*s3FO-DO3*s3FO              +AR*s3FS-SR*s3FO       +m*(1/365)*s3FOV-vac.FEO*s3FO                  -St.FEO*s3FO
            ds1MO <- ((1-p)*BAO*s2FO)+((1-p)*BEO*s2FO)-m*(1/365)*s1MO-DO1*s1MO   +AR*s1MS-SR*s1MO       -vac.MPO*s1MO                                  -St.MPO*s1MO
            ds2MO <- m*(1/365)*s1MO-m*(1/(365*8))*s2MO-DO2*s2MO                  +AR*s2MS-SR*s2MO       +m*(1/365)*s1MOV+m*(1/365)*s2MOV-vac.MAO*s2MO  -St.MAO*s2MO
            ds3MO <- m*(1/(365*8))*s2MO-m*(1/(365*5))*s3MO-DO3*s3MO              +AR*s3MS-SR*s3MO       +m*(1/365)*s3MOV-vac.MEO*s3MO                  -St.MEO*s3MO  
            
            #S:Stray
            #include Stray rate((5.02/1000)/365)-Adopt rate ((0.78/1000)/365)                                  Stray vac                                      sterilized                  
            ds1FS <- (p*BAS*s2FS)+(p*BES*s2FS)-m*(1/365)*s1FS-DS1*s1FS           +SR*s1FI+SR*s1FO-AR*s1FS      -vac.FPS*s1FS                                  -St.FPS*s1FS    
            ds2FS <- m*(1/365)*s1FS-m*(1/(365*8))*s2FS-DS2*s2FS                  +SR*s2FI+SR*s2FO-AR*s2FS      +m*(1/365)*s1FSV+m*(1/365)*s2FSV-vac.FAS*s2FS  -St.FAS*s2FS
            ds3FS <- m*(1/(365*8))*s2FS-m*(1/(365*5))*s3FS-DS3*s3FS              +SR*s3FI+SR*s3FO-AR*s3FS      +m*(1/365)*s3FSV-vac.FES*s3FS                  -St.FES*s3FS
            ds1MS <- ((1-p)*BAS*s2FS)+((1-p)*BES*s2FS)-m*(1/365)*s1MS-DS1*s1MS   +SR*s1MI+SR*s1MO-AR*s1MS      -vac.MPS*s1MS                                  -St.MPS*s1MS
            ds2MS <- m*(1/365)*s1MS-m*(1/(365*8))*s2MS-DS2*s2MS                  +SR*s2MI+SR*s2MO-AR*s2MS      +m*(1/365)*s1MSV+m*(1/365)*s2MSV-vac.MAS*s2MS  -St.MAS*s2MS
            ds3MS <- m*(1/(365*8))*s2MS-m*(1/(365*5))*s3MS-DS3*s3MS              +SR*s3MI+SR*s3MO-AR*s3MS      +m*(1/365)*s3MSV-vac.MES*s3MS                  -St.MES*s3MS
            
            #ISt: Indoor Sterilization *120/365>St coverage                                    Indoor sterilization Vac                                       Vac before Sterilized
            ds1FISt <- St.FPI*s1FI-m*(1/365)*s1FISt-DI1*s1FISt                                 -vac.FPISt*s1FISt                                              #+St.FPIV*s1FIV
            ds2FISt <- m*(1/365)*s1FISt+St.FAI*s2FI-m*(1/(365*8))*s2FISt-DI2*s2FISt            +m*(1/365)*s1FIStV+m*(1/365)*s2FIStV-vac.FAISt*s2FISt          #+St.FAIV*s2FIV
            ds3FISt <- m*(1/(365*8))*s2FISt+St.FEI*s3FI-m*(1/(365*5))*s3FISt-DI3*s3FISt        +m*(1/365)*s3FIStV-vac.FEISt*s3FISt                            #+St.FEIV*s3FIV
            ds1MISt <- St.MPI*s1MI-m*(1/365)*s1MISt-DI1*s1MISt                                 -vac.MPISt*s1MISt                                              #+St.MPIV*s1MIV
            ds2MISt <- m*(1/365)*s1MISt+St.MAI*s2MI-m*(1/(365*8))*s2MISt-DI2*s2MISt            +m*(1/365)*s1MIStV+m*(1/365)*s2MIStV-vac.MAISt*s2MISt          #+St.MAIV*s2MIV
            ds3MISt <- m*(1/(365*8))*s2MISt+St.MEI*s3MI-m*(1/(365*5))*s3MISt-DI3*s3MISt        +m*(1/365)*s3MIStV-vac.MEISt*s3MISt                            #+St.MEIV*s2MIV
            
            #Ost : Outdoor Sterilization                                                       Outdoor Sterilization vac                                      Sterilized after vac
            ds1FOSt <- St.FPO*s1FO-m*(1/365)*s1FOSt-DO1*s1FOSt                                 -vac.FPOSt*s1FOSt                                              #+St.FPOV*s1FOV
            ds2FOSt <- m*(1/365)*s1FOSt+St.FAO*s2FO-m*(1/(365*8))*s2FOSt-DO2*s2FOSt            +m*(1/365)*s1FOStV+m*(1/365)*s2FOStV-vac.FAOSt*s2FOSt          #+St.FAOV*s2FOV
            ds3FOSt <- m*(1/(365*8))*s2FOSt+St.FEO*s3FO-m*(1/(365*5))*s3FOSt-DO3*s3FOSt        +m*(1/365)*s3FOStV-vac.FEOSt*s3FOSt                            #+St.FEOV*s3FOV
            ds1MOSt <- St.MPO*s1MO-m*(1/365)*s1MOSt-DO1*s1MOSt                                 -vac.MPOSt*s1MOSt                                              #+St.MPOV*s1MOV
            ds2MOSt <- m*(1/365)*s1MOSt+St.MAO*s2MO-m*(1/(365*8))*s2MOSt-DO2*s2MOSt            +m*(1/365)*s1MOStV+m*(1/365)*s2MOStV-vac.MAOSt*s2MOSt          #+St.MAOV*s2MOV
            ds3MOSt <- m*(1/(365*8))*s2MOSt+St.MEO*s3MO-m*(1/(365*5))*s3MOSt-DO3*s3MOSt        +m*(1/365)*s3MOStV-vac.MEOSt*s3MOSt                            #+St.MEOV*s3MOV
            
            #SSt : Stray Sterilization                                                          Stray Sterilization vac                                       Sterilized after vac
            ds1FSSt <- St.FPS*s1FS-m*(1/365)*s1FSSt-DS1*s1FSSt                                  -vac.FPSSt*s1FSSt     
            ds2FSSt <- m*(1/365)*s1FSSt+St.FAS*s2FS-m*(1/(365*8))*s2FSSt-DS2*s2FSSt             +m*(1/365)*s1FSStV+m*(1/365)*s2FSStV-vac.FASSt*s2FSSt      
            ds3FSSt <- m*(1/(365*8))*s2FSSt+St.FES*s3FO-m*(1/(365*5))*s3FSSt-DS3*s3FSSt         +m*(1/365)*s3FSStV-vac.FESSt*s3FSSt      
            ds1MSSt <- St.MAS*s1MS-m*(1/365)*s1MSSt-DS1*s1MSSt                                  -vac.MPSSt*s1MSSt     
            ds2MSSt <- m*(1/365)*s1MSSt+St.MPS*s2MS-m*(1/(365*8))*s2MSSt-DS2*s2MSSt             +m*(1/365)*s1MSStV+m*(1/365)*s2MSStV-vac.MASSt*s2MSSt    
            ds3MSSt <- m*(1/(365*8))*s2MSSt+St.MES*s3MS-m*(1/(365*5))*s3MSSt-DS3*s3MSSt         +m*(1/365)*s3MSStV-vac.MESSt*s3MSSt
            
            ##############                
            #Vac indoor   Vaccov flow in - vac flow out          Sterilized
            ds1FIV <- vac.FPI*s1FI-m*(1/365)*s1FIV-DI1*s1FIV    -St.FPIV*s1FIV
            ds2FIV <- vac.FAI*s2FI-m*(1/365)*s2FIV-DI2*s2FIV    -St.FAIV*s2FIV
            ds3FIV <- vac.FEI*s3FI-m*(1/365)*s3FIV-DI3*s3FIV    -St.FEIV*s3FIV
            ds1MIV <- vac.MPI*s1MI-m*(1/365)*s1MIV-DI1*s1MIV    -St.MPIV*s1MIV
            ds2MIV <- vac.MAI*s2MI-m*(1/365)*s2MIV-DI2*s2MIV    -St.MAIV*s2MIV
            ds3MIV <- vac.MEI*s3MI-m*(1/365)*s3MIV-DI3*s3MIV    -St.MEIV*s3MIV
            
            #vac outdoor                                         Sterilized
            ds1FOV <- vac.FPO*s1FO-m*(1/365)*s1FOV-DO1*s1FOV    -St.FPOV*s1FOV
            ds2FOV <- vac.FAO*s2FO-m*(1/365)*s2FOV-DO2*s2FOV    -St.FAOV*s2FOV
            ds3FOV <- vac.FEO*s3FO-m*(1/365)*s3FOV-DO3*s3FOV    -St.FEOV*s3FOV
            ds1MOV <- vac.MPO*s1MO-m*(1/365)*s1MOV-DO1*s1MOV    -St.MPOV*s1MOV
            ds2MOV <- vac.MAO*s2MO-m*(1/365)*s2MOV-DO2*s2MOV    -St.MAOV*s2MOV
            ds3MOV <- vac.MEO*s3MO-m*(1/365)*s3MOV-DO3*s3MOV    -St.MEOV*s3MOV
            
            #vac stray                                           Sterilized
            ds1FSV <- vac.FPS*s1FS-m*(1/365)*s1FSV-DS1*s1FSV    -St.FPSV*s1FSV
            ds2FSV <- vac.FAS*s2FS-m*(1/365)*s2FSV-DS2*s2FSV    -St.FASV*s2FSV
            ds3FSV <- vac.FES*s3FS-m*(1/365)*s3FSV-DS3*s3FSV    -St.FESV*s3FSV
            ds1MSV <- vac.MPS*s1MS-m*(1/365)*s1MSV-DS1*s1MSV    -St.MPSV*s1MSV
            ds2MSV <- vac.MAS*s2MS-m*(1/365)*s2MSV-DS2*s2MSV    -St.MASV*s2MSV
            ds3MSV <- vac.MES*s3MS-m*(1/365)*s3MSV-DS3*s3MSV    -St.MESV*s3MSV
            
            #vac in indoor sterilized dogs                                 vac before sterilized
            ds1FIStV <- vac.FPISt*s1FISt-m*(1/365)*s1FIStV-DI1*s1FIStV     +St.FPIV*s1FIV
            ds2FIStV <- vac.FAISt*s2FISt-m*(1/365)*s2FIStV-DI2*s2FIStV     +St.FAIV*s2FIV
            ds3FIStV <- vac.FEISt*s3FISt-m*(1/365)*s3FIStV-DI3*s3FIStV     +St.FEIV*s3FIV
            ds1MIStV <- vac.MPISt*s1MISt-m*(1/365)*s1MIStV-DI1*s1MIStV     +St.MPIV*s1MIV
            ds2MIStV <- vac.MAISt*s2MISt-m*(1/365)*s2MIStV-DI2*s2MIStV     +St.MAIV*s2MIV
            ds3MIStV <- vac.MEISt*s3MISt-m*(1/365)*s3MIStV-DI3*s3MIStV     +St.MEIV*s3MIV
            
            #vac outdoor sterilized                                        vac before sterilized
            ds1FOStV <- vac.FPOSt*s1FOSt-m*(1/365)*s1FOStV-DI1*s1FOStV     +St.FPOV*s1FOV
            ds2FOStV <- vac.FAOSt*s2FOSt-m*(1/365)*s2FOStV-DI2*s2FOStV     +St.FAOV*s2FOV
            ds3FOStV <- vac.FEOSt*s3FOSt-m*(1/365)*s3FOStV-DI3*s3FOStV     +St.FEOV*s3FOV
            ds1MOStV <- vac.MPOSt*s1FOSt-m*(1/365)*s1FOStV-DI1*s1MOStV     +St.MPOV*s1MOV
            ds2MOStV <- vac.MAOSt*s2FOSt-m*(1/365)*s2FOStV-DI2*s2MOStV     +St.MAOV*s2MOV
            ds3MOStV <- vac.MEOSt*s3FOSt-m*(1/365)*s3FOStV-DI3*s3MOStV     +St.MEOV*s3MOV
            
            #vac stray sterilized                                          vac before sterilized
            ds1FSStV <- vac.FPSSt*s1FSSt-m*(1/365)*s1FSStV-DS1*s1FSStV     +St.FPSV*s1FSV
            ds2FSStV <- vac.FASSt*s2FSSt-m*(1/365)*s2FSStV-DS2*s2FSStV     +St.FASV*s2FSV
            ds3FSStV <- vac.FESSt*s3FSSt-m*(1/365)*s3FSStV-DS3*s3FSStV     +St.FESV*s3FSV
            ds1MSStV <- vac.MPSSt*s1MSSt-m*(1/365)*s1MSStV-DS1*s1MSStV     +St.MPSV*s1MSV
            ds2MSStV <- vac.MASSt*s2MSSt-m*(1/365)*s2MSStV-DS2*s2MSStV     +St.MASV*s2MSV
            ds3MSStV <- vac.MESSt*s3MSSt-m*(1/365)*s3MSStV-DS3*s3MSStV     +St.MESV*s3MSV
            
            
            
            return(list(c(ds1FI,ds2FI,ds3FI, 
                          ds1MI,ds2MI,ds3MI, 
                          ds1FO,ds2FO,ds3FO, 
                          ds1MO,ds2MO,ds3MO, 
                          ds1FS,ds2FS,ds3FS, 
                          ds1MS,ds2MS,ds3MS,
                          
                          ds1FISt,ds2FISt,ds3FISt,
                          ds1MISt,ds2MISt,ds3MISt,
                          ds1FOSt,ds2FOSt,ds3FOSt,
                          ds1MOSt,ds2MOSt,ds3MOSt,
                          ds1FSSt,ds2FSSt,ds3FSSt,
                          ds1MSSt,ds2MSSt,ds3MSSt,
                          
                          
                          ds1FIV,ds2FIV,ds3FIV,ds1MIV,ds2MIV,ds3MIV,
                          ds1FOV,ds2FOV,ds3FOV,ds1MOV,ds2MOV,ds3MOV,
                          ds1FSV,ds2FSV,ds3FSV,ds1MSV,ds2MSV,ds3MSV,    
                          ds1FIStV,ds2FIStV,ds3FIStV,ds1MIStV,ds2MIStV,ds3MIStV,
                          ds1FOStV,ds2FOStV,ds3FOStV,ds1MOStV,ds2MOStV,ds3MOStV,
                          ds1FSStV,ds2FSStV,ds3FSStV,ds1MSStV,ds2MSStV,ds3MSStV
                          
            )))
        })
    }
    
    init <- c(s1FI = 200, s2FI = 0,s3FI = 00, 
              s1MI = 200, s2MI = 00,s3MI = 00,
              s1FO = 300, s2FO = 00,s3FO = 00, 
              s1MO = 300, s2MO = 00,s3MO = 00,
              s1FS = 400, s2FS = 00,s3FS = 00, 
              s1MS = 400, s2MS = 00,s3MS = 00,
              #Sterilization 1-dogs proportion
              s1FISt=0,s2FISt=0,s3FISt=0, 
              s1MISt=0,s2MISt=0,s3MISt=0,
              s1FOSt=0,s2FOSt=0,s3FOSt=0,
              s1MOSt=0,s2MOSt=0,s3MOSt=0,
              s1FSSt=0,s2FSSt=0,s3FSSt=0,
              s1MSSt=0,s2MSSt=0,s3MSSt=0,
              #Vac
              s1FIV=0,s2FIV=0,s3FIV=0,s1MIV=0,s2MIV=0,s3MIV=0,
              s1FOV=0,s2FOV=0,s3FOV=0,s1MOV=0,s2MOV=0,s3MOV=0,
              s1FSV=0,s2FSV=0,s3FSV=0,s1MSV=0,s2MSV=0,s3MSV=0,    
              s1FIStV=0,s2FIStV=0,s3FIStV=0,s1MIStV=0,s2MIStV=0,s3MIStV=0,
              s1FOStV=0,s2FOStV=0,s3FOStV=0,s1MOStV=0,s2MOStV=0,s3MOStV=0,
              s1FSStV=0,s2FSStV=0,s3FSStV=0,s1MSStV=0,s2MSStV=0,s3MSStV=0
    )
    
    parameters <- c(BAI = (92/300)/365, BAO= 0, BAS = 0,
                    BEI = 0, BEO = 0, BES = 0 ,
                    DI1 = 0, DI2 = 0, DI3 = 0, 
                    DO1 = 0, DO2 = 0, DO3 = 0, 
                    DS1 = 0, DS2 = 0, DS3 = 0,
                    #m multiplier
                    m =5,
                    #p proportion (Female&Male F:M ratio)
                    p=0.5,
                    #SR stray rate (abandon),AR (Adopted rate)
                    SR= 0, AR= 0,
                    #St. sterilization rate (by gender, age, type, vac)
                    St.FPI = 0, St.FAI = 0, St.FEI = 0, St.MPI = 0, St.MAI = 0, St.MEI = 0, 
                    St.FPO = 0, St.FAO = 0, St.FEO = 0, St.MPO = 0, St.MAO = 0, St.MEO = 0,
                    St.FPS = 0, St.FAS = 0, St.FES = 0, St.MPS = 0, St.MAS = 0, St.MES = 0,
                    
                    St.FPIV = 0, St.FAIV = 0, St.FEIV = 0, St.MPIV = 0, St.MAIV = 0, St.MEIV = 0, 
                    St.FPOV = 0, St.FAOV = 0, St.FEOV = 0, St.MPOV = 0, St.MAOV = 0, St.MEOV = 0,
                    St.FPSV = 0, St.FASV = 0, St.FESV = 0, St.MPSV = 0, St.MASV = 0, St.MESV = 0,
                    
                    
                    #vac.rate (by gender, age, type)
                    vac.FPI = 0, vac.FAI = 0, vac.FEI = 0, vac.MPI = 0, vac.MAI = 0, vac.MEI = 0,
                    vac.FPO = 0, vac.FAO = 0, vac.FEO = 0, vac.MPO = 0, vac.MAO = 0, vac.MEO = 0,
                    vac.FPS = 0, vac.FAS = 0, vac.FES = 0, vac.MPS = 0, vac.MAS = 0, vac.MES = 0,
                    
                    vac.FPISt = 0, vac.FAISt = 0, vac.FEISt = 0, vac.MPISt = 0, vac.MAISt = 0, vac.MEISt = 0,
                    vac.FPOSt = 0, vac.FAOSt = 0, vac.FEOSt = 0, vac.MPOSt = 0, vac.MAOSt = 0, vac.MEOSt = 0,
                    vac.FPSSt = 0, vac.FASSt = 0, vac.FESSt = 0, vac.MPSSt = 0, vac.MASSt = 0, vac.MESSt = 0 
                    
    )     
    
    out_df <- reactive({
        
        times <- seq(0, 365*20, by = 1)
        out <- as.data.frame(ode(y = init, times = times, func = sir, parms = parameters),method="euler")
        
        out_df <- as.data.frame(out)
        out_df
    })
    
    output$PopIndoorPlot <- renderPlotly({
        withProgress(message = 'Progress indicators', {
        out_Female<- out_df()[,1:4]
        out_Female$time <- out_Female$time/365
        out_Male<- out_df()[,c(1,5:7)]
        out_Male$time <- out_Male$time/365
        names(out_Female)[2] <- "Female Puppy Indoor dogs"
        names(out_Female)[3] <- "Female Adult Indoor dogs"
        names(out_Female)[4] <- "Female Older Indoor dogs"
        names(out_Male)[2] <- "Male Puppy Indoor dog"
        names(out_Male)[3] <- "Male Adult Indoor dogs"
        names(out_Male)[4] <- "Male Older Indoor dogs"
        
        
        genderF <- rep("Female",length(out_Female[,1]))
        genderM <- rep("Male",length(out_Male[,1]))
        
        out_Female_melt <- reshape2::melt(out_Female, id="time")
        age <- c(rep("Puppy",length(out_Female_melt[,1])/3),rep("Adult",length(out_Female_melt[,1])/3),rep("Older",length(out_Female_melt[,1])/3))
        out_Female_melt <-data.frame(out_Female_melt,gender=genderF,age=age)
        out_Male_melt <- reshape2::melt(out_Male, id="time")
        out_Male_melt <- data.frame(out_Male_melt,gender=genderM,age=age)
        out_all_melt <- as.data.frame(rbind(out_Female_melt,out_Male_melt))
        
        p <-ggplot(data = out_all_melt) + 
            labs( x = "Year", y = "Number")+
            geom_line(mapping = aes(x = time, y = value,color = gender,linetype = age),size = 1)+
            theme(axis.title = element_text(size = 20))+
            theme(axis.text = element_text(size = 15, colour="black"))+ 
            theme(legend.title = element_blank())
        ggplotly(p)%>%
            layout(legend = list(font = list(size = 15) ))
        
        })
        
    })
    
    # Test Table
    # output$table <- DT::renderDataTable({
    #         out_Female<- out_df()[,1:4]
    #         out_Female$time <- out_Female$time/365
    #         out_Male<- out_df()[,c(1,5:7)]
    #         out_Male$time <- out_Male$time/365
    #         names(out_Female)[2] <- "Female Puppy Indoor dogs"
    #         names(out_Female)[3] <- "Female Adult Indoor dogs"
    #         names(out_Female)[4] <- "Female Older Indoor dogs"
    #         names(out_Male)[2] <- "Male Puppy Indoor dog"
    #         names(out_Male)[3] <- "Male Adult Indoor dogs"
    #         names(out_Male)[4] <- "Male Older Indoor dogs"
    # 
    #         genderF <- rep("Female",length(out_Female[,1]))
    #         genderM <- rep("Male",length(out_Male[,1]))
    #        
    #         
    # 
    #         # out_Female_melt <- reshape2::melt(out_Female, id="time")
    #         # out_Female_melt <-data.frame(out_Female_melt,type=genderF)
    #         # out_Male_melt <- reshape2::melt(out_Male, id="time")
    #         # out_Male_melt <- data.frame(out_Male_melt,type=genderM)
    #         # out_all_melt <- as.data.frame(rbind(out_Female_melt,out_Male_melt))
    #         
    #         out_Female_melt <- reshape2::melt(out_Female, id="time")
    #         age <- c(rep("Puppy",length(out_Female_melt[,1])/3),rep("Adult",length(out_Female_melt[,1])/3),rep("Older",length(out_Female_melt[,1])/3))
    #         out_Female_melt <-data.frame(out_Female_melt,gender=genderF,age=age)
    #         out_Male_melt <- reshape2::melt(out_Male, id="time")
    #         out_Male_melt <- data.frame(out_Male_melt,gender=genderM,age=age)
    #         out_all_melt <- as.data.frame(rbind(out_Female_melt,out_Male_melt))
    # 
    #         DT::datatable(out_all_melt,
    #                       rownames = FALSE
    #         )
    # 
    # })
    
    output$PopOutdoorPlot <- renderPlotly({
        withProgress(message = 'Progress indicators', {
            out<- out_df()
            out$time <- out$time/365
            # names(out)[8] <- "Female Puppy Outdoor dogs"
            # names(out)[9] <- "Female Adult Outdoor dogs"
            # names(out)[10] <- "Female Older Outdoor dogs"
            # names(out)[11] <- "Male Puppy Outdoor dogs"
            # names(out)[12] <- "Male Adult Outdoor dogs"
            # names(out)[13] <- "Male Older Outdoor dogs"
            
            out_Female<- out_df()[c(1,8:10)]
            out_Female$time <- out_Female$time/365
            out_Male<- out_df()[,c(1,11:13)]
            out_Male$time <- out_Male$time/365
            names(out_Female)[2] <- "Female Puppy Outdoor dogs"
            names(out_Female)[3] <- "Female Adult Outdoor dogs"
            names(out_Female)[4] <- "Female Older Outdoor dogs"
            names(out_Male)[2] <- "Male Puppy Outdoor dogs"
            names(out_Male)[3] <- "Male Adult Outdoor dogs"
            names(out_Male)[4] <- "Male Older Outdoor dogs"
            
            genderF <- rep("Female",length(out_Female[,1]))
            genderM <- rep("Male",length(out_Male[,1]))
            
            out_Female_melt <- reshape2::melt(out_Female, id="time")
            age <- c(rep("Puppy",length(out_Female_melt[,1])/3),rep("Adult",length(out_Female_melt[,1])/3),rep("Older",length(out_Female_melt[,1])/3))
            out_Female_melt <-data.frame(out_Female_melt,gender=genderF,age=age)
            out_Male_melt <- reshape2::melt(out_Male, id="time")
            out_Male_melt <- data.frame(out_Male_melt,gender=genderM,age=age)
            out_all_melt <- as.data.frame(rbind(out_Female_melt,out_Male_melt))
            
            
            
            p <-ggplot(data = out_all_melt) + 
                labs( x = "Year", y = "Number")+
                geom_line(mapping = aes(x = time, y = value,color = gender,linetype = age),size = 1)+
                theme(axis.title = element_text(size = 20))+
                theme(axis.text = element_text(size = 15, colour="black"))+ 
                theme(legend.title = element_blank())
            ggplotly(p)%>%
                layout(legend = list(font = list(size = 15) ))
            
        })
        
    })
    

    
    output$PopStrayPlot <- renderPlotly({
        withProgress(message = 'Progress indicators', {
            out<- out_df()
            out$time <- out$time/365
            # names(out)[14] <- "Female Puppy Stray dogs"
            # names(out)[15] <- "Female Adult Stray dogs"
            # names(out)[16] <- "Female Older Stray dogs"
            # names(out)[17] <- "Male Puppy Stray dog"
            # names(out)[18] <- "Male Adult Stray dogs"
            # names(out)[19] <- "Male Older Stray dogs"
            
            out_Female<- out_df()[c(1,14:16)]
            out_Female$time <- out_Female$time/365
            out_Male<- out_df()[,c(1,17:19)]
            out_Male$time <- out_Male$time/365
            names(out_Female)[2] <- "Female Puppy Stray dogs"
            names(out_Female)[3] <- "Female Adult Stray dogs"
            names(out_Female)[4] <- "Female Older Stray dogs"
            names(out_Male)[2] <- "Male Puppy Stray dogs"
            names(out_Male)[3] <- "Male Adult Stray dogs"
            names(out_Male)[4] <- "Male Older Stray dogs"

            
            genderF <- rep("Female",length(out_Female[,1]))
            genderM <- rep("Male",length(out_Male[,1]))
            
            out_Female_melt <- reshape2::melt(out_Female, id="time")
            age <- c(rep("Puppy",length(out_Female_melt[,1])/3),rep("Adult",length(out_Female_melt[,1])/3),rep("Older",length(out_Female_melt[,1])/3))
            out_Female_melt <-data.frame(out_Female_melt,gender=genderF,age=age)
            out_Male_melt <- reshape2::melt(out_Male, id="time")
            out_Male_melt <- data.frame(out_Male_melt,gender=genderM,age=age)
            out_all_melt <- as.data.frame(rbind(out_Female_melt,out_Male_melt))
            
            p <-ggplot(data = out_all_melt) + 
                labs( x = "Year", y = "Number")+
                geom_line(mapping = aes(x = time, y = value,color = gender,linetype = age),size = 1)+
                theme(axis.title = element_text(size = 20))+
                theme(axis.text = element_text(size = 15, colour="black"))+ 
                theme(legend.title = element_blank())
            ggplotly(p)%>%
                layout(legend = list(font = list(size = 15) ))
            
            # plot(year, out[,"s1FI"]
            #      ,col = "1",
            #      xlab="Time (Year)",ylab="Number of dogs", type = "l",
            #      ylim=c(0,500),xlim = c(0,20),main = "Type of dogs")
            # lines(year, out[,"s2FI"], col="2")
            # lines(year, out[,"s3FI"], col="3")
            # lines(year, out[,"s1MI"], col="4")
            # lines(year, out[,"s2MI"], col="5")
            # lines(year, out[,"s3MI"], col="6")
            # legend("topright", legend=c("FI Pups", "FI Adults", "FI Older","MI Pups", "MI Adults", "MI Older"),
            #        col=c("black", "red", "Green","blue","cyan","violet"), 
            #        bty="n",#type of box to be drawn around the legend. The allowed values are "o" (the default) and "n" no box
            #        lwd = 0.8,#the line types and widths for lines appearing in the legend
            #        cex=0.8)
        })
        
    })
    
    output$SterilizationPlot <- renderPlotly({
        withProgress(message = 'Progress indicators', {
            out<- out_df()
            out$time <- out$time/365
            
        names(out)[20] <- "Female Puppy Indoor dogs"
        names(out)[21] <- "Female Adult Indoor dogs"
        names(out)[22] <- "Female Older Indoor dogs"
        names(out)[23] <- "Male Puppy Indoor dog"
        names(out)[24] <- "Male Adult Indoor dogs"
        names(out)[25] <- "Male Older Indoor dogs"
        
        
        out_melt <- reshape2::melt(out[,c(1,20:25)], id="time")
        
        p <-ggplot(data = out_melt) + 
            labs( x = "Year", y = "Number")+
            geom_line(mapping = aes(x = time, y = value,color = variable),size = 1)+
            theme(axis.title = element_text(size = 20))+
            theme(axis.text = element_text(size = 15, colour="black"))+ 
            theme(legend.title = element_blank())
        ggplotly(p)%>%
            layout(legend = list(font = list(size = 15) ))
        
        
        # plot(out$time, out[,20]
        #      ,col = "1",
        #      xlab="Time (days)",ylab="Number of dogs", type = "l",
        #      ylim=c(0,500),xlim = c(0,(365*20)),main = "Type of dogs")
        # lines(times, out[,21], col="2")
        # lines(times, out[,22], col="3")
        # lines(times, out[,23], col="4")
        # lines(times, out[,24], col="5")
        # lines(times, out[,25], col="6")
        # legend("topright", legend=c("FI Pups", "FI Adults", "FI Older","MI Pups", "MI Adults", "MI Older"),
        #        col=c("black", "red", "Green","blue","cyan","violet"), 
        #        bty="n",#type of box to be drawn around the legend. The allowed values are "o" (the default) and "n" no box
        #        lwd = 0.8,#the line types and widths for lines appearing in the legend
        #        cex=0.8)
        })
    })
    
    #Indoor
    observeEvent(input$FemaleIndoorRate, 
                 updateSliderInput(session, "MaleIndoorRate", value = 100-input$FemaleIndoorRate)
    )
    
    observeEvent(input$MaleIndoorRate, 
                 updateSliderInput(session, "FemaleIndoorRate", value = 100-input$MaleIndoorRate)
    )
    
    observeEvent(input$PuppyIndoor, 
                 updateSliderInput(session, "AdultIndoor", value = 100-input$PuppyIndoor-input$OlderIndoor)
    )
    
    observeEvent(input$AdultIndoor, 
                 updateSliderInput(session, "OlderIndoor", value = 100-input$PuppyIndoor-input$AdultIndoor)
    )
    
    observeEvent(input$OlderIndoor, 
                 updateSliderInput(session, "PuppyIndoor", value = 100-input$AdultIndoor-input$OlderIndoor)
    )
    #Outdoor
    observeEvent(input$FemaleOutdoorRate, 
                 updateSliderInput(session, "MaleOutdoorRate", value = 100-input$FemaleOutdoorRate)
    )
    
    observeEvent(input$MaleOutdoorRate, 
                 updateSliderInput(session, "FemaleOutdoorRate", value = 100-input$MaleOutdoorRate)
    )
    
    observeEvent(input$PuppyOutdoor, 
                 updateSliderInput(session, "AdultOutdoor", value = 100-input$PuppyOutdoor-input$OlderOutdoor)
    )
    
    observeEvent(input$AdultOutdoor, 
                 updateSliderInput(session, "OlderOutdoor", value = 100-input$PuppyOutdoor-input$AdultOutdoor)
    )
    
    observeEvent(input$OlderOutdoor, 
                 updateSliderInput(session, "PuppyOutdoor", value = 100-input$AdultOutdoor-input$OlderOutdoor)
    )
    
    #Stray
    observeEvent(input$FemaleStrayRate, 
                 updateSliderInput(session, "MaleStrayRate", value = 100-input$FemaleStrayRate)
    )
    
    observeEvent(input$MaleStrayRate, 
                 updateSliderInput(session, "FemaleStrayRate", value = 100-input$MaleStrayRate)
    )
    
    observeEvent(input$PuppyStray, 
                 updateSliderInput(session, "AdultStray", value = 100-input$PuppyStray-input$OlderStray)
    )
    
    observeEvent(input$AdultStray, 
                 updateSliderInput(session, "OlderStray", value = 100-input$PuppyStray-input$AdultStray)
    )
    
    observeEvent(input$OlderStray, 
                 updateSliderInput(session, "PuppyStray", value = 100-input$AdultStray-input$OlderStray)
    )
    

})
