#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinybusy)
library(ggplot2)
library(deSolve)
library(reshape2)
library(plotly)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
    
    doRun <- reactiveValues(run = F)
    hide("PopIndoorPlot")
    hide("PopOutdoorPlot")
    hide("PopStrayPlot")
    hide("TotalPop")
    
    hide("SterilizationPlot")
    hide("SterilizationPlot2")
    hide("SterilizationPlot3")
    
    
    observeEvent(input$run,{
        show("PopIndoorPlot")
        show("PopOutdoorPlot")
        show("PopStrayPlot")
        show("TotalPop")
        
        show("SterilizationPlot")
        show("SterilizationPlot2")
        show("SterilizationPlot3")
    })
    
    sir <- function(time, state, parameters) {
        with(as.list(c(state, parameters)), {
            
            #####Indoor population dynamics######################################################################################
            #Indoor puppy equations = part1: [(p*Birth rate) 4 groups flow in from un-vaccinated and vaccinated Adults&Elder female - multiplier(m=5)*puppy 1 year(1/365) - death(DI1)*puppy]
            #Below#                   part2: +adopted ratio from stray to indoor (q)*adopted rate AR*stray puppy - abandon (SR*indoor puppy)
            #                         part3: -vaccinated (vac.FPI) and moved to vaccine stage or waning to adult stage 1 year (1/365), do not back to puppy stage 
            #                         part4: -sterilized (St.FPI) and moved to sterilization stage, do not back to puppy stage 
            
            #I:Indoor Female  part1 Birth -Aging -Death                                               part2 +Adopt - Stray rate        part3 -vac rate flow out     part4 -Sterilized
            ds1FI <- (p*BAI*s2FI)+(p*BEI*s3FI)+(p*BAI*s2FIV)+(p*BEI*s3FIV)-m*(1/365)*s1FI-DI1*s1FI           +(q*AR*s1FS)-SR*s1FI            -vac.FPI*s1FI                -St.FPI*s1FI
            
            #Indoor adult equations = part1: multiplier(m=5)*puppy 1 year(1/365) move in - multiplier(m=5)*adult stage (1/365) 8 years moving out - death
            #                         part2: +adopted ratio from stray to indoor (q*AR) - abandon SR*adults
            #                         part3: -vaccinated (vac.FAI) and moved to vaccine stage or waning back to adult stage 1 year (1/365)
            #                         part4: -sterilized (St.FAI) and moved to sterilization stage, do not back to adult stage 
            
            #Female Adult part1 move in - aging - death                    part2 +Adopt - Stray rate   part3 waning moving back from puppy&adult - vaccinated   part4 -sterilized   
            ds2FI <- m*(1/365)*s1FI-m*(1/(365*8))*s2FI-DI2*s2FI                  +(q*AR*s2FS)-SR*s2FI      +(1/365)*s1FIV+(1/365)*s2FIV-vac.FAI*s2FI                  -St.FAI*s2FI
            
            #Indoor elder equations = part1: +multiplier(m=5)*adults 8 year(1/(365*8)) move in - Death
            #                         part2: +adopted ratio from stray to indoor (q*AR) - abandon SR*Elders
            #                         part3: -vaccinated (vac.FEI) and moved to vaccine stage or waning back to Elder stage 1 year (1/365)
            #                         part4: -sterilized (St.FEI) and moved to sterilization stage, do not back to elder stage 
            
            #Female Elder part1 move in - death                           part2 +Adopt - Stray rate   part3 +waning moving back - vaccinated        part4 -sterilized   
            ds3FI <- m*(1/(365*8))*s2FI-DI3*s3FI-m*(1/(365*5))*s3FI             +(q*AR*s3FS)-SR*s3FI        +(1/365)*s3FIV-vac.FEI*s3FI                    -St.FEI*s3FI
            
            #Indoor puppy equations = part1: [male ratio(1-p)*Birth rate) 4 groups flow in from un-vaccinated and vaccinated Adults&Elder female - multiplier(m=5)*puppy 1 year(1/365) - death(DI1)*puppy]  
            #                         part2: +adopted ratio from stray to indoor (q)*AR - abandon SR*puppy
            #                         part3: -vaccinated (vac.MPI) and moved to vaccine stage or waning back to adult stage, do not back to puppy stage
            #                         part4: -sterilized (St.MPI) and moved to sterilization stage, do not back to adult stage 
            
            #Male puppy part1 Birth - aging - death                                                                   part2 +Adopt - Stray rate    part3  -vaccinated        part4 -sterilized   
            ds1MI <- ((1-p)*BAI*s2FI)+((1-p)*BEI*s3FI)+((1-p)*BAI*s2FIV)+((1-p)*BEI*s3FIV)-m*(1/365)*s1MI-DI1*s1MI          +(q*AR*s1MS)-SR*s1MI          -vac.MPI*s1MI            -St.MPI*s1MI
            
            #Indoor adult equations = part1: multiplier(m=5)*puppy 1 year(1/365) move in - multiplier(m=5)*adult stage (1/365) 8 years moving out - death
            #                         part2: +adopted ratio from stray to indoor (q)*AR - abandon SR*adults
            #                         part3: -vaccinated (vac.FAI) and moved to vaccine stage or vanning back to adult stage again
            #                         part4: -sterilized (St.FAI) and moved to sterilization stage, do not back to adult stage 
            
            #Male Adult part1 move in - aging - death                      part2 +Adopt - Stray rate   part3 waning moving back from puppy&adult - vaccinated      part4 -sterilized   
            ds2MI <- m*(1/365)*s1MI-m*(1/(365*8))*s2MI-DI2*s2MI                  +(q*AR*s2MS)-SR*s2MI      +(1/365)*s1MIV+(1/365)*s2MIV-vac.MAI*s2MI                     -St.MAI*s2MI
            
            #Indoor elder equations = part1: +multiplier(m=5)*adults 8 year(1/(365*8)) adult 8 years move in - Death
            #                         part2: +adopted ratio from stray to indoor (q)*AR - abandon SR
            #                         part3: -vaccinated (vac.FEI) and moved to vaccine stage or waning back to Elder stage again
            #                         part4: -sterilized (St.FEI) and moved to sterilization stage, do not back to elder stage 
            
            #Male Elder part1 move in - death                              part2 +Adopt - Stray rate   part3 +waning moving back - vaccinated        part4 -sterilized   
            ds3MI <- m*(1/(365*8))*s2MI-DI3*s3MI-m*(1/(365*5))*s3MI              +(q*AR*s3MS)-SR*s3MI        +(1/365)*s3MIV-vac.MEI*s3MI                   -St.MEI*s3MI
            
            
            #####Outdoor population dynamics#######################################################################################################
            #outdoor puppy equations = part1: [(p*Birth rate) 4 groups flow in from un-vaccinated and vaccinated Adults&Elder female - multiplier(m=5)*puppy 1 year(1/365) - death(DO1)*puppy]
            #                          part2: +adopted ratio (1-q) from stray to outdoor (1-q)*AR - abandon SR*Puppy
            #                          part3: -vaccinated (vac.FPI) and moved to vaccine stage or waning to adult stage 1 year (1/365), do not back to puppy stage 
            #                          part4: -sterilized (St.FPI) and moved to sterilization stage, do not back to puppy stage 
            
            #Female puppy part1 Birth -Aging -Death                                                    part2 +Adopt - Stray rate           part3 -vac rate flow out     part4 -Sterilized
            ds1FO <- (p*BAO*s2FO)+(p*BEO*s3FO)+(p*BAO*s2FOV)+(p*BEO*s3FOV)-m*(1/365)*s1FO-DO1*s1FO           +((1-q)*AR*s1FS)-SR*s1FO            -vac.FPO*s1FO                -St.FPO*s1FO
            
            #Outdoor adult equations = part1: multiplier(m=5)*puppy 1 year(1/365) move in - multiplier(m=5)*adult stage (1/365) 8 years moving out - death
            #                          part2: +adopted ratio (1-q) from stray to outdoor (1-q)*AR - abandon SR*Adults
            #                          part3: -vaccinated (vac.FAO) and moved to vaccine stage or waning back to adult stage again
            #                          part4: -sterilized (St.FAO) and moved to sterilization stage, do not back to adult stage 
            
            #Female adult part1 move in - aging - death                    part2 +Adopt ratio(1-q) - Stray rate    part3 +waning moving back from puppy&adult - vaccinated    part4 -sterilized   
            ds2FO <- m*(1/365)*s1FO-m*(1/(365*8))*s2FO-DO2*s2FO                  +((1-q)*AR*s2FS)-SR*s2FO                +(1/365)*s1FOV+(1/365)*s2FOV-vac.FAO*s2FO                  -St.FAO*s2FO
            
            #Outdoor elder equations = part1: +multiplier(m=5)*adults 8 year(1/(365*8)) adult 8 years move in - Death
            #                          part2: +adopted ratio (1-q) from stray to outdoor (1-q)*AR - abandon SR*Elder
            #                          part3: -vaccinated (vac.FEO) and moved to vaccine stage or waning back to Elder stage again
            #                          part4: -sterilized (St.FEO) and moved to sterilization stage, do not back to elder stage 
            
            #Female Elder part1 move in - death                            part2 +Adopt - Stray rate           part3 +waning moving back - vaccinated                part4 -sterilized   
            ds3FO <- m*(1/(365*8))*s2FO-DO3*s3FO-m*(1/(365*5))*s3FO              +((1-q)*AR*s3FS)-SR*s3FO            +(1/365)*s3FOV-vac.FEO*s3FO                            -St.FEO*s3FO
            
            #outdoor puppy equations = part1: [(1-p*Birth rate) 4 groups flow in from un-vaccinated and vaccinated Adults&Elder female - multiplier(m=5)*puppy 1 year(1/365) - death(DO1)*puppy]
            #                          part2: +adopted ratio (1-q) from stray to outdoor (1-q)*AR - abandon SR*Puppy
            #                          part3: -vaccinated (vac.MPO) and moved to vaccine stage or waning to adult stage 1 year (1/365), do not back to puppy stage 
            #                          part4: -sterilized (St.MPO) and moved to sterilization stage, do not back to puppy stage 
            
            #Male puppy part1 Birth -Aging -Death                                                                  part2 +Adopt ratio (1-q) - Stray rate  part3 -vac rate flow out     part4 -Sterilized
            ds1MO <- ((1-p)*BAO*s2FO)+((1-p)*BEO*s3FO)+((1-p)*BAO*s2FOV)+((1-p)*BEO*s3FOV)-m*(1/365)*s1MO-DO1*s1MO       +((1-q)*AR*s1MS)-SR*s1MO                -vac.MPO*s1MO                -St.MPO*s1MO
            
            #Outdoor adult equations = part1: multiplier(m=5)*puppy 1 year(1/365) move in - multiplier(m=5)*adult stage (1/365) 8 years moving out - death
            #                          part2: +adopted ratio (1-q) from stray to outdoor (1-q)*AR - abandon SR*Adults
            #                          part3: -vaccinated (vac.MAO) and moved to vaccine stage or waning back to adult stage from puppy&adult 
            #                          part4: -sterilized (St.MAO) and moved to sterilization stage, do not back to adult stage 
            
            #male adult part1 move in - aging - death                    part2 +Adopt ratio (1-q) - Stray rate   part3 +waning moving back - vaccinated           part4 -sterilized   
            ds2MO <- m*(1/365)*s1MO-m*(1/(365*8))*s2MO-DO2*s2MO                +((1-q)*AR*s2MS)-SR*s2MO                +(1/365)*s1MOV+(1/365)*s2MOV-vac.MAO*s2MO        -St.MAO*s2MO
            
            #Outdoor elder equations = part1: multiplier(m=5)*puppy 1 year(1/365) move in - multiplier(m=5)*adult stage (1/365) 8 years moving out - death
            #                          part2: +adopted ratio (1-q) from stray to outdoor (1-q)*AR - abandon SR*Adults
            #                          part3: -vaccinated (vac.MEO) and moved to vaccine stage or waning back to adult stage again
            #                          part4: -sterilized (St.MEO) and moved to sterilization stage, do not back to adult stage 
            
            #male elder part1 move in - aging - death                    part2 +Adopt ratio (1-q) - Stray rate   part3 +waning moving back - vaccinated          part4 -sterilized   
            ds3MO <- m*(1/(365*8))*s2MO-DO3*s3MO-m*(1/(365*5))*s3MO            +((1-q)*AR*s3MS)-SR*s3MO                +(1/365)*s3MOV-vac.MEO*s3MO                     -St.MEO*s3MO 
            
            #####S:Stray#########################################################################################################################
            #Stray dog dynamics Female (p):Male (1-p) ratio
            #Stray puppy equations = part1: [(p*Birth rate) 4 groups flow in from un-vaccinated and vaccinated Adults&Elder female - multiplier(m=5)*puppy 1 year(1/365) - death(DS1)*puppy]
            #Below#                  part2: +abandon (SR) from Indoor&Outdoor - adopted AR
            #                        part3: -vaccinated (vac.FPS) and moved to vaccine stage or waning to adult stage 1 year (1/365), do not back to puppy stage 
            #                        part4: -sterilized (St.FPS) and moved to sterilization stage, do not back to puppy stage 
            
            #Female puppy part1 Birth -Aging -Death                                                      part2 +Abandon from Indoor&Outdoor - adopted AR    part3 -vac rate flow out     part4 -Sterilized
            ds1FS <- (p*BAS*s2FS)+(p*BES*s3FS)+(p*BAS*s2FSV)+(p*BES*s3FSV)-m*(1/365)*s1FS-DS1*s1FS             +SR*s1FI+SR*s1FO-AR*s1FS                           -vac.FPS*s1FS                -St.FPS*s1FS
            
            #Stray adult equations = part1: multiplier(m=5)*puppy 1 year(1/365) move in - multiplier(m=5)*adult stage (1/365) 8 years moving out - death
            #                        part2: +abandon (SR) from Indoor&Outdoor - adopted AR
            #                        part3: -vaccinated (vac.FAS) and moved to vaccine stage or waning back to adult stage from puppy&adult
            #                        part4: -sterilized (St.FAS) and moved to sterilization stage, do not back to adult stage 
            
            #Female adult part1 move in - aging - death                    part2 +Abandon from Indoor&Outdoor SR- adopted AR    part3 +waning moving back from puppy&adult - vaccinated    part4 -sterilized   
            ds2FS <- m*(1/365)*s1FS-m*(1/(365*8))*s2FS-DS2*s2FS                  +SR*s2FI+SR*s2FO-AR*s2FS                             +(1/365)*s1FSV+(1/365)*s2FSV-vac.FAS*s2FS                  -St.FAS*s2FS
            
            #Stray elder equations = part1: +multiplier(m=5)*adults 8 year(1/(365*8)) adult 8 years move in - Death
            #                        part2: +abandon (SR) from Indoor&Outdoor - adopted AR
            #                        part3: -vaccinated (vac.FES) and moved to vaccine stage or waning back to Elder stage again
            #                        part4: -sterilized (St.FES) and moved to sterilization stage, do not back to elder stage 
            
            #Female Elder part1 move in - death                            part2 +Abandon from Indoor&Outdoor SR- adopted AR    part3 +waning moving back - vaccinated         part4 -sterilized   
            ds3FS <- m*(1/(365*8))*s2FS-DS3*s3FS-m*(1/(365*5))*s3FS              +SR*s3FI+SR*s3FO-AR*s3FS                             +(1/365)*s3FSV-vac.FES*s3FS                    -St.FES*s3FS
            
            #Stray puppy equations = part1: [male ratio (1-p*Birth rate) 4 groups flow in from un-vaccinated and vaccinated Adults&Elder female - multiplier(m=5)*puppy 1 year(1/365) - death(DS1)*puppy]
            #Below#                  part2: +abandon (SR) from Indoor&Outdoor - adopted AR
            #                        part3: -vaccinated (vac.MPS) and moved to vaccine stage or waning to adult stage 1 year (1/365), do not back to puppy stage 
            #                        part4: -sterilized (St.MPS) and moved to sterilization stage, do not back to puppy stage 
            
            #Male puppy part1 Birth -Aging -Death                                                                   part2 +Abandon from Indoor&Outdoor - adopted AR    part3 -vac rate flow out        part4 -Sterilized
            ds1MS <- ((1-p)*BAS*s2FS)+((1-p)*BES*s3FS)+((1-p)*BAS*s2FSV)+((1-p)*BES*s3FSV)-m*(1/365)*s1MS-DS1*s1MS        +SR*s1MI+SR*s1MO-AR*s1MS                           -vac.MPS*s1MS                   -St.MPS*s1MS
            
            #Stray adult equations = part1: multiplier(m=5)*puppy 1 year(1/365) move in - multiplier(m=5)*adult stage (1/365) 8 years moving out - death
            #                        part2: +abandon (SR) from Indoor&Outdoor - adopted AR
            #                        part3: -vaccinated (vac.MAS) and moved to vaccine stage or waning back to adult stage from puppy&adult
            #                        part4: -sterilized (St.MAS) and moved to sterilization stage, do not back to adult stage 
            
            #Female adult part1 move in - aging - death                    part2 +Abandon from Indoor&Outdoor SR- adopted AR    part3 +waning moving back from puppy&adult - vaccinated       part4 -sterilized   
            ds2MS <- m*(1/365)*s1MS-m*(1/(365*8))*s2MS-DS2*s2MS                  +SR*s2MI+SR*s2MO-AR*s2MS                             +(1/365)*s1MSV+(1/365)*s2MSV-vac.MAS*s2MS                     -St.MAS*s2MS
            
            #Stray elder equations = part1: +multiplier(m=5)*adults 8 year(1/(365*8)) adult 8 years move in - Death
            #                        part2: +abandon (SR) from Indoor&Outdoor - adopted AR
            #                        part3: -vaccinated (vac.FES) and moved to vaccine stage or waning back to Elder stage again
            #                        part4: -sterilized (St.FES) and moved to sterilization stage, do not back to elder stage 
            
            #male Elder part1 move in - death                            part2 +Abandon from Indoor&Outdoor SR- adopted AR    part3 +waning moving back - vaccinated         part4 -sterilized   
            ds3MS <- m*(1/(365*8))*s2MS-DS3*s3MS-m*(1/(365*5))*s3MS             +SR*s3MI+SR*s3MO-AR*s3MS                            +(1/365)*s3MSV-vac.MES*s3MS                    -St.MES*s3MS
            
            
            #################################################################################################################################    
            #### Sterilization compartment ##################################################################################################
            #################################################################################################################################
            #ISt: Indoor sterilization    
            #Indoor puppy Sterilization = part1: +St.rate*puppy from baseline - aging moving out - Death
            #                             part2: -vaccinated puppy (vac rate), waning to sterilized adult, do not back to puppy stage
            
            #female puppy part1                                                   part2 -Indoor sterilization Vac
            ds1FISt <- St.FPI*s1FI-(m*(1/365)*s1FISt)-DI1*s1FISt                        -vac.FPISt*s1FISt                                              
            
            #Indoor adults Sterilization = part1: +St.rate*adult from baseline + aging from puppy  - aging moving out to elder - Death
            #                              part2: -vaccinated (vac rate), waning from vaccinated sterilized puppy&adult 
            
            #female adult part1                                                      part2 +waning -Indoor sterilization Vac
            ds2FISt <- m*(1/365)*s1FISt+St.FAI*s2FI-m*(1/(365*8))*s2FISt-DI2*s2FISt        +(1/365)*s1FIStV+(1/365)*s2FIStV-vac.FAISt*s2FISt 
            
            #Indoor elder Sterilization = part1: +St.rate*elder from baseline + aging from adult - Death
            #                             part2: -vaccinated (vac rate), waning from vaccinated sterilized elder 
            
            #female elder part1                                                     part2 +waning -Indoor sterilization Vac
            ds3FISt <- m*(1/(365*8))*s2FISt+St.FEI*s3FI-DI3*s3FISt-m*(1/(365*5))*s3FISt                        +(1/365)*s3FIStV-vac.FEISt*s3FISt                           
            
            #Indoor puppy Sterilization = part1: +St.rate*puppy from baseline - aging moving out - Death
            #                             part2: -vaccinated puppy (vac rate), waning to sterilized adult, do not back to puppy stage
            
            #male puppy part1                                                   part2 -Indoor sterilization Vac
            ds1MISt <- St.MPI*s1MI-m*(1/365)*s1MISt-DI1*s1MISt                         -vac.MPISt*s1MISt                                            
            
            #Indoor adults Sterilization = part1: +St.rate*adult from baseline + aging from puppy  - aging moving out to elder - Death
            #                              part2: -vaccinated (vac rate), waning from vaccinated sterilized puppy&adult 
            
            #male adult part1                                                            part2 +waning -Indoor sterilization Vac
            ds2MISt <- m*(1/365)*s1MISt+St.MAI*s2MI-m*(1/(365*8))*s2MISt-DI2*s2MISt            +(1/365)*s1MIStV+(1/365)*s2MIStV-vac.MAISt*s2MISt
            
            #Indoor elder Sterilization = part1: +St.rate*elder from baseline + aging from adult - Death
            #                             part2: -vaccinated (vac rate), waning from vaccinated sterilized elder 
            
            #male elder part1                                                            part2 +waning -Indoor sterilization Vac
            ds3MISt <- m*(1/(365*8))*s2MISt+St.MEI*s3MI-DI3*s3MISt-m*(1/(365*5))*s3MISt                              +(1/365)*s3MIStV-vac.MEISt*s3MISt                            
            
            
            #Outdoor Sterilization########################################################
            #OSt: Outdoor sterilization    
            #same pattern with Indoor sterilization
            
            #Ost : Outdoor Sterilization                                                       Outdoor Sterilization vac +  flow in from vac.st waning time **no multiplier                                    
            ds1FOSt <- St.FPO*s1FO-m*(1/365)*s1FOSt-DO1*s1FOSt                                 -vac.FPOSt*s1FOSt                                             
            ds2FOSt <- m*(1/365)*s1FOSt+St.FAO*s2FO-m*(1/(365*8))*s2FOSt-DO2*s2FOSt            +(1/365)*s1FOStV+(1/365)*s2FOStV-vac.FAOSt*s2FOSt        
            ds3FOSt <- m*(1/(365*8))*s2FOSt+St.FEO*s3FO-DO3*s3FOSt-m*(1/(365*5))*s3FOSt        +(1/365)*s3FOStV-vac.FEOSt*s3FOSt                            
            ds1MOSt <- St.MPO*s1MO-m*(1/365)*s1MOSt-DO1*s1MOSt                                 -vac.MPOSt*s1MOSt                                              
            ds2MOSt <- m*(1/365)*s1MOSt+St.MAO*s2MO-m*(1/(365*8))*s2MOSt-DO2*s2MOSt            +(1/365)*s1MOStV+(1/365)*s2MOStV-vac.MAOSt*s2MOSt          
            ds3MOSt <- m*(1/(365*8))*s2MOSt+St.MEO*s3MO-DO3*s3MOSt-m*(1/(365*5))*s3MOSt                               +(1/365)*s3MOStV-vac.MEOSt*s3MOSt                            
            
            #Stray Sterilization######################################################## 
            #SSt: Stray Sterilization
            #same pattern with Indoor sterilization        
            #SSt : Stray Sterilization                                                          Stray Sterilization vac                                       
            ds1FSSt <- St.FPS*s1FS-m*(1/365)*s1FSSt-DS1*s1FSSt                                  -vac.FPSSt*s1FSSt     
            ds2FSSt <- m*(1/365)*s1FSSt+St.FAS*s2FS-m*(1/(365*8))*s2FSSt-DS2*s2FSSt             +(1/365)*s1FSStV+(1/365)*s2FSStV-vac.FASSt*s2FSSt      
            ds3FSSt <- m*(1/(365*8))*s2FSSt+St.FES*s3FO-DS3*s3FSSt-m*(1/(365*5))*s3FSSt         +(1/365)*s3FSStV-vac.FESSt*s3FSSt      
            ds1MSSt <- St.MAS*s1MS-m*(1/365)*s1MSSt-DS1*s1MSSt                                  -vac.MPSSt*s1MSSt     
            ds2MSSt <- m*(1/365)*s1MSSt+St.MPS*s2MS-m*(1/(365*8))*s2MSSt-DS2*s2MSSt             +(1/365)*s1MSStV+(1/365)*s2MSStV-vac.MASSt*s2MSSt    
            ds3MSSt <- m*(1/(365*8))*s2MSSt+St.MES*s3MS-DS3*s3MSSt-m*(1/(365*5))*s3MSSt         +(1/365)*s3MSStV-vac.MESSt*s3MSSt
            
            ######################################################################################################################################
            # Vaccination in baseline dynamics ###################################################################################################
            ######################################################################################################################################
            # IV: Indoor vaccination#################
            # Indoor puppy vaccine equations = part1: +vac in from baseline - 1/365 waning to baseline - Death
            #                                  part2: -sterilization after vaccination, moving out to "vaccination&sterilization" compartment below, do not directly moving back to "Sterilization" compartment
            #                                  part3: aging dynamic with multiplier (m=5)
            
            #Vac indoor part1 Vaccov flow in - vac flow out - death      part2 -Sterilized       part3 Aging dynamics
            ds1FIV <- vac.FPI*s1FI-(1/365)*s1FIV-DI1*s1FIV                     -St.FPIV*s1FIV          -(m*(1/365)*s1FIV)
            ds2FIV <- vac.FAI*s2FI-(1/365)*s2FIV-DI2*s2FIV                     -St.FAIV*s2FIV          +(m*(1/365)*s1FIV)-(m*(1/365*8)*s2FIV)
            ds3FIV <- vac.FEI*s3FI-(1/365)*s3FIV-DI3*s3FIV                     -St.FEIV*s3FIV          +(m*(1/365*8))*s2FIV-m*(1/(365*5))*s3FIV
            ds1MIV <- vac.MPI*s1MI-(1/365)*s1MIV-DI1*s1MIV                     -St.MPIV*s1MIV          -(m*(1/365)*s1MIV)
            ds2MIV <- vac.MAI*s2MI-(1/365)*s2MIV-DI2*s2MIV                     -St.MAIV*s2MIV          +(m*(1/365)*s1MIV)-(m*(1/365*8))*s2MIV
            ds3MIV <- vac.MEI*s3MI-(1/365)*s3MIV-DI3*s3MIV                     -St.MEIV*s3MIV          +(m*(1/365*8))*s2MIV-m*(1/(365*5))*s3MIV
            
            # OV: Outdoor vaccination############### 
            #same pattern with indoor vaccine
            
            #Vac outdoor part1 Vaccov flow in - vac flow out - death      part2 -Sterilized       part3 Aging dynamics
            ds1FOV <- vac.FPO*s1FO-(1/365)*s1FOV-DO1*s1FOV                     -St.FPOV*s1FOV          -(m*(1/365)*s1FOV)
            ds2FOV <- vac.FAO*s2FO-(1/365)*s2FOV-DO2*s2FOV                     -St.FAOV*s2FOV          +(m*(1/365)*s1FOV)-(m*(1/365*8)*s2FOV)
            ds3FOV <- vac.FEO*s3FO-(1/365)*s3FOV-DO3*s3FOV                     -St.FEOV*s3FOV          +(m*(1/365*8)*s2FOV)-m*(1/(365*5))*s3FOV
            ds1MOV <- vac.MPO*s1MO-(1/365)*s1MOV-DO1*s1MOV                     -St.MPOV*s1MOV          -(m*(1/365)*s1MOV)
            ds2MOV <- vac.MAO*s2MO-(1/365)*s2MOV-DO2*s2MOV                     -St.MAOV*s2MOV          +(m*(1/365)*s1MOV)-(m*(1/365*8))*s2MOV
            ds3MOV <- vac.MEO*s3MO-(1/365)*s3MOV-DO3*s3MOV                     -St.MEOV*s3MOV          +(m*(1/365*8))*s2MOV-m*(1/(365*5))*s3MOV
            
            # SV: Stray vaccination##################
            #same pattern with indoor vaccine
            
            #Vac Stray part1 Vaccov flow in - vac flow out - death      part2 -Sterilized       part3 Aging dynamics
            ds1FSV <- vac.FPS*s1FS-(1/365)*s1FSV-DS1*s1FSV                    -St.FPSV*s1FSV           -(m*(1/365)*s1FSV)
            ds2FSV <- vac.FAS*s2FS-(1/365)*s2FSV-DS2*s2FSV                    -St.FASV*s2FSV           +(m*(1/365)*s1FSV)-(m*(1/365*8))*s2FSV
            ds3FSV <- vac.FES*s3FS-(1/365)*s3FSV-DS3*s3FSV                    -St.FESV*s3FSV           +(m*(1/365*8)*s2FSV)-m*(1/(365*5))*s3FSV
            ds1MSV <- vac.MPS*s1MS-(1/365)*s1MSV-DS1*s1MSV                    -St.MPSV*s1MSV           -(m*(1/365)*s1MSV)
            ds2MSV <- vac.MAS*s2MS-(1/365)*s2MSV-DS2*s2MSV                    -St.MASV*s2MSV           +(m*(1/365)*s1MSV)-(m*(1/365*8))*s2MSV
            ds3MSV <- vac.MES*s3MS-(1/365)*s3MSV-DS3*s3MSV                    -St.MESV*s3MSV           +(m*(1/365*8))*s2MSV-m*(1/(365*5))*s3MSV
            
            ######################################################################################################################################
            # Vaccination and Sterilization compartment ###################################################################################################
            ######################################################################################################################################
            # StV: Sterilization vaccination #################
            # Indoor puppy vaccinated&sterilized equations = part1: +vac in from sterilization - 1/365 waning to "sterilization" compartment - Death
            #                                                part2: +sterilization after vaccination, -1/365 waning directly moving back to "Sterilization" compartment
            #                                                part3: aging dynamic with multiplier (m=5)
            
            #vaccine and sterilized indoor: part1                               part2 +vac.gr before sterilize     part3 Aging dynamics
            ds1FIStV <- vac.FPISt*s1FISt-(1/365)*s1FIStV-DI1*s1FIStV                 +St.FPIV*s1FIV                    -m*(1/365)*s1FIStV
            ds2FIStV <- vac.FAISt*s2FISt-(1/365)*s2FIStV-DI2*s2FIStV                 +St.FAIV*s2FIV                    +(m*(1/365)*s1FIStV)-(m*(1/365*8)*s2FIStV)
            ds3FIStV <- vac.FEISt*s3FISt-(1/365)*s3FIStV-DI3*s3FIStV                 +St.FEIV*s3FIV                    +(m*(1/365*8))*s2FIStV-m*(1/(365*5))*s3FIStV
            ds1MIStV <- vac.MPISt*s1MISt-(1/365)*s1MIStV-DI1*s1MIStV                 +St.MPIV*s1MIV                    -m*(1/365)*s1MIStV
            ds2MIStV <- vac.MAISt*s2MISt-(1/365)*s2MIStV-DI2*s2MIStV                 +St.MAIV*s2MIV                    +(m*(1/365)*s1MIStV)-(m*(1/365*8)*s2MIStV)
            ds3MIStV <- vac.MEISt*s3MISt-(1/365)*s3MIStV-DI3*s3MIStV                 +St.MEIV*s3MIV                    +(m*(1/365*8)*s2MIStV)-m*(1/(365*5))*s3MIStV
            
            
            #vaccine and sterilized outdoor: part1                              part2 +vac.gr before sterilize     part3 Aging dynamics
            ds1FOStV <- vac.FPOSt*s1FOSt-(1/365)*s1FOStV-DO1*s1FOStV                  +St.FPOV*s1FOV                   -m*(1/365)*s1FOStV
            ds2FOStV <- vac.FAOSt*s2FOSt-(1/365)*s2FOStV-DO2*s2FOStV                  +St.FAOV*s2FOV                   +(m*(1/365)*s1FOStV)-(m*(1/365*8))*s2FOStV
            ds3FOStV <- vac.FEOSt*s3FOSt-(1/365)*s3FOStV-DO3*s3FOStV                  +St.FEOV*s3FOV                   +(m*(1/365*8))*s2FOStV-m*(1/(365*5))*s3FOStV
            ds1MOStV <- vac.MPOSt*s1MOSt-(1/365)*s1MOStV-DO1*s1MOStV                  +St.MPOV*s1MOV                   -m*(1/365)*s1MOStV
            ds2MOStV <- vac.MAOSt*s2MOSt-(1/365)*s2MOStV-DO2*s2MOStV                  +St.MAOV*s2MOV                   +(m*(1/365)*s1MOStV)-(m*(1/365*8))*s2MOStV
            ds3MOStV <- vac.MEOSt*s3MOSt-(1/365)*s3MOStV-DO3*s3MOStV                  +St.MEOV*s3MOV                   +(m*(1/365*8))*s2MOStV-m*(1/(365*5))*s3MOStV
            
            #vaccine and sterilized stray: part1                                part2 +vac.gr before sterilize     part3 Aging dynamics
            ds1FSStV <- vac.FPSSt*s1FSSt-(1/365)*s1FSStV-DS1*s1FSStV                +St.FPSV*s1FSV                     -m*(1/365)*s1FSStV
            ds2FSStV <- vac.FASSt*s2FSSt-(1/365)*s2FSStV-DS2*s2FSStV                +St.FASV*s2FSV                     +(m*(1/365)*s1FSStV)-(m*(1/365*8))*s2FSStV
            ds3FSStV <- vac.FESSt*s3FSSt-(1/365)*s3FSStV-DS3*s3FSStV                +St.FESV*s3FSV                     +(m*(1/365*8))*s2FSStV-m*(1/(365*5))*s3FSStV
            ds1MSStV <- vac.MPSSt*s1MSSt-(1/365)*s1MSStV-DS1*s1MSStV                +St.MPSV*s1MSV                     -m*(1/365)*s1MSStV
            ds2MSStV <- vac.MASSt*s2MSSt-(1/365)*s2MSStV-DS2*s2MSStV                +St.MASV*s2MSV                     +(m*(1/365)*s1MSStV)-(m*(1/365*8))*s2MSStV
            ds3MSStV <- vac.MESSt*s3MSSt-(1/365)*s3MSStV-DS3*s3MSStV                +St.MESV*s3MSV                     +(m*(1/365*8)*s2MSStV)-m*(1/(365*5))*s3MSStV
            
            
            
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
    
    init <- reactive({
            c(s1FI = input$IndoorPop*input$FemaleIndoorRate/100*input$PuppyIndoor/100, 
              s2FI = input$IndoorPop*input$FemaleIndoorRate/100*input$AdultIndoor/100,
              s3FI = input$IndoorPop*input$FemaleIndoorRate/100*input$OlderIndoor/100, 
              s1MI = input$IndoorPop*input$MaleIndoorRate/100*input$PuppyIndoor/100, 
              s2MI = input$IndoorPop*input$MaleIndoorRate/100*input$AdultIndoor/100,
              s3MI = input$IndoorPop*input$MaleIndoorRate/100*input$OlderIndoor/100,
              
              s1FO = input$OutdoorPop*input$FemaleOutdoorRate/100*input$PuppyOutdoor/100, 
              s2FO = input$OutdoorPop*input$FemaleOutdoorRate/100*input$AdultOutdoor/100,
              s3FO = input$OutdoorPop*input$FemaleOutdoorRate/100*input$OlderOutdoor/100, 
              s1MO = input$OutdoorPop*input$MaleOutdoorRate/100*input$PuppyOutdoor/100, 
              s2MO = input$OutdoorPop*input$MaleOutdoorRate/100*input$AdultOutdoor/100,
              s3MO = input$OutdoorPop*input$MaleOutdoorRate/100*input$OlderOutdoor/100,
              
              s1FS = input$StrayPop*input$FemaleStrayRate/100*input$PuppyStray/100,  
              s2FS = input$StrayPop*input$FemaleStrayRate/100*input$AdultStray/100,
              s3FS = input$StrayPop*input$FemaleStrayRate/100*input$OlderStray/100, 
              s1MS = input$StrayPop*input$MaleStrayRate/100*input$PuppyStray/100, 
              s2MS = input$StrayPop*input$MaleStrayRate/100*input$AdultStray/100,
              s3MS = input$StrayPop*input$MaleStrayRate/100*input$OlderStray/100,
              
              #Sterilization 1-dogs proportion
              s1FISt= 000,s2FISt=0,s3FISt=0, 
              s1MISt= 000,s2MISt=0,s3MISt=0,
              s1FOSt= 000,s2FOSt=0,s3FOSt=0,
              s1MOSt= 000,s2MOSt=0,s3MOSt=0,
              s1FSSt= 000,s2FSSt=0,s3FSSt=0,
              s1MSSt= 000,s2MSSt=0,s3MSSt=0,
              #Vac
              s1FIV= 000,s2FIV=0,s3FIV=0,
              s1MIV= 000,s2MIV=0,s3MIV=0,
              s1FOV= 000,s2FOV=0,s3FOV=0,
              s1MOV= 000,s2MOV=0,s3MOV=0,
              s1FSV= 000,s2FSV=0,s3FSV=0,
              s1MSV= 000,s2MSV=0,s3MSV=0,    
              s1FIStV= 000,s2FIStV=0,s3FIStV=0,
              s1MIStV= 000,s2MIStV=0,s3MIStV=0,
              s1FOStV= 000,s2FOStV=0,s3FOStV=0,
              s1MOStV= 000,s2MOStV=0,s3MOStV=0,
              s1FSStV= 000,s2FSStV=0,s3FSStV=0,
              s1MSStV= 000,s2MSStV=0,s3MSStV=0
    )
    })
    
    parameters <- reactive({
                  c(BAI = input$BirthAdultIndoor/365, 
                    BAO = input$BirthAdultOutdoor/365, 
                    BAS = input$BirthAdultStray/365,
                    
                    BEI = input$BirthOlderIndoor/365, 
                    BEO = input$BirthOlderOutdoor/365, 
                    BES = input$BirthOlderStray/365,
                    
                    DI1 = input$DeathPuppyIndoor/365, 
                    DI2 = input$DeathAdultIndoor/365, 
                    DI3 = input$DeathOlderIndoor/365, 
                    
                    DO1 = input$DeathPuppyOutdoor/365,
                    DO2 = input$DeathPuppyOutdoor/365,
                    DO3 = input$DeathPuppyOutdoor/365,
                    
                    DS1 = input$DeathPuppyStray/365, 
                    DS2 = input$DeathPuppyStray/365, 
                    DS3 = input$DeathPuppyStray/365,
                    
                    #m multiplier
                    m = 5,
                    #p proportion (Female&Male F:M ratio)
                    p = 0.5,  
                    #q Indoor:Outdoor ratio from adopted dogs
                    q = 0,
                    #SR stray rate (abandon),AR (Adopted rate)
                    SR = 0, AR = 0,
                    #St. sterilization rate (by gender, age, type, vac)
                    St.FPI = input$SterFemalePuppyIndoor/100/365, 
                    St.FAI = input$SterFemaleAdultIndoor/100/365, 
                    St.FEI = input$SterFemaleOlderIndoor/100/365,
                    
                    St.MPI = input$SterMalePuppyIndoor/100/365, 
                    St.MAI = input$SterMaleAdultIndoor/100/365, 
                    St.MEI = input$SterMaleOlderIndoor/100/365,
                    
                    St.FPO = input$SterFemalePuppyOutdoor/100/365, 
                    St.FAO = input$SterFemaleAdultOutdoor/100/365, 
                    St.FEO = input$SterFemaleOlderOutdoor/100/365,
                    
                    St.MPO = input$SterMalePuppyOutdoor/100/365, 
                    St.MAO = input$SterMaleAdultOutdoor/100/365, 
                    St.MEO = input$SterMaleOlderOutdoor/100/365,
                    
                    St.FPS = input$SterFemalePuppyStray/100/365, 
                    St.FAS = input$SterFemaleAdultStray/100/365, 
                    St.FES = input$SterFemaleOlderStray/100/365,
                    
                    St.MPS = input$SterMalePuppyStray/100/365, 
                    St.MAS = input$SterMaleAdultStray/100/365, 
                    St.MES = input$SterMaleOlderStray/100/365,
                    
                    St.FPIV = input$SterFemalePuppyIndoor/100/365, 
                    St.FAIV = input$SterFemaleAdultIndoor/100/365, 
                    St.FEIV = input$SterFemaleOlderIndoor/100/365,
                    
                    St.MPIV = input$SterMalePuppyIndoor/100/365, 
                    St.MAIV = input$SterMaleAdultIndoor/100/365, 
                    St.MEIV = input$SterMaleOlderIndoor/100/365,
                    
                    St.FPOV = input$SterFemalePuppyOutdoor/100/365, 
                    St.FAOV = input$SterFemaleAdultOutdoor/100/365, 
                    St.FEOV = input$SterFemaleOlderOutdoor/100/365,
                    
                    St.MPOV = input$SterMalePuppyOutdoor/100/365, 
                    St.MAOV = input$SterMaleAdultOutdoor/100/365, 
                    St.MEOV = input$SterMaleOlderOutdoor/100/365,
                    
                    St.FPSV = input$SterFemalePuppyStray/100/365, 
                    St.FASV = input$SterFemaleAdultStray/100/365,
                    St.FESV = input$SterFemaleOlderStray/100/365,
                    
                    St.MPSV = input$SterMalePuppyStray/100/365, 
                    St.MASV = input$SterMaleAdultStray/100/365, 
                    St.MESV = input$SterMaleOlderStray/100/365,
                    
                    
                    #vac.rate (by gender, age, type)
                    vac.FPI = input$VacPuppyIndoor/100, 
                    vac.FAI = input$VacAdultIndoor/100, 
                    vac.FEI = input$VacOlderIndoor/100, 
                    vac.MPI = input$VacPuppyIndoor/100, 
                    vac.MAI = input$VacAdultIndoor/100, 
                    vac.MEI = input$VacOlderIndoor/100,
                    
                    vac.FPO = input$VacPuppyOutdoor/100, 
                    vac.FAO = input$VacAdultOutdoor/100, 
                    vac.FEO = input$VacOlderOutdoor/100, 
                    vac.MPO = input$VacPuppyOutdoor/100, 
                    vac.MAO = input$VacAdultOutdoor/100, 
                    vac.MEO = input$VacOlderOutdoor/100,
                    
                    vac.FPS = input$VacPuppyStray/100,
                    vac.FAS = input$VacAdultStray/100, 
                    vac.FES = input$VacOlderStray/100, 
                    vac.MPS = input$VacPuppyStray/100, 
                    vac.MAS = input$VacAdultStray/100, 
                    vac.MES = input$VacOlderStray/100,
                    
                    vac.FPISt = input$VacPuppyIndoor/100, 
                    vac.FAISt = input$VacAdultIndoor/100, 
                    vac.FEISt = input$VacOlderIndoor/100, 
                    vac.MPISt = input$VacPuppyIndoor/100, 
                    vac.MAISt = input$VacAdultIndoor/100, 
                    vac.MEISt = input$VacOlderIndoor/100,
                    
                    vac.FPOSt = input$VacPuppyOutdoor/100, 
                    vac.FAOSt = input$VacAdultOutdoor/100,
                    vac.FEOSt = input$VacOlderOutdoor/100,
                    vac.MPOSt = input$VacPuppyOutdoor/100, 
                    vac.MAOSt = input$VacAdultOutdoor/100, 
                    vac.MEOSt = input$VacOlderOutdoor/100,
                    
                    vac.FPSSt = input$VacPuppyStray/100, 
                    vac.FASSt = input$VacAdultStray/100, 
                    vac.FESSt = input$VacOlderStray/100,
                    
                    vac.MPSSt = input$VacPuppyStray/100, 
                    vac.MASSt = input$VacAdultStray/100, 
                    vac.MESSt = input$VacOlderStray/100 
                    
    )    
    })
    
    out_df <- reactive({
        
        times <- seq(0, 365*20, by = 1)
        out <- as.data.frame(ode(y = init(), times = times, func = sir, parms = parameters()),method="euler")
        
        out_df <- as.data.frame(out)
        out_df
    })
    
    
    output$PopIndoorPlot <- renderPlot({

        input$run
        show_modal_spinner()

        isolate({

        par(mfrow = c(3, 1))
        layout(matrix(c(1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,2,2,3,3,0), nrow = 4, ncol = 5, byrow = TRUE))
            out<- out_df()
        out$time <- out$time/365

        max_y <- max(c(out[,"s1FI"],out[,"s2FI"],out[,"s3FI"],out[,"s1MI"],out[,"s2MI"],out[,"s3MI"]), na.rm = TRUE) +10
        
        total_dog <- out[,"s1FI"] + out[,"s2FI"] + out[,"s3FI"] + out[,"s1MI"] + out[,"s2MI"] + out[,"s3MI"]
        max_total_dog <- max(out[,"s1FI"] + out[,"s2FI"] + out[,"s3FI"] + out[,"s1MI"] + out[,"s2MI"] + out[,"s3MI"])
        plot(out[,"time"], out[,"s1FI"]
             ,col = "red",
             xlab="Time (Years)",ylab="Number of dogs", type = "l",lwd = 3,lty = 1,
             cex.lab=1.7,cex.axis=1.5, cex.main=3,ylim=c(0,max_y),main = "Type of dogs")
        lines(out[,"time"], out[,"s2FI"], col="red" ,lwd = 3,lty=5)
        lines(out[,"time"], out[,"s3FI"], col="red" ,lwd = 3,lty=3)
        lines(out[,"time"], out[,"s1MI"], col="blue" ,lwd = 3,lty=1)
        lines(out[,"time"], out[,"s2MI"], col="blue" ,lwd = 3,lty=5)
        lines(out[,"time"], out[,"s3MI"], col="blue" ,lwd = 3,lty=3)
        par(new=TRUE)
        plot(out[,"time"], total_dog, pch=15,  xlab="", ylab="", ylim=c(0,max_total_dog), 
             axes=FALSE, type="b", col="Green")
        ## a little farther out (line=4) to make room for labels
        mtext("Total dog",side=4,col="Green",line=4) 
        axis(4, ylim=c(0,max_total_dog), col="Green",col.axis="Green",las=1)
        plot.new()
        legend("center", legend=c("Puppy", "Adults", "Older"),
               lty=1:3, 
               bty="n",#type of box to be drawn around the legend. The allowed values are "o" (the default) and "n" no box
               cex = 2,
               lwd = 2,
        ) 
        plot.new()
        legend("center", legend=c("Female", "Male","Total"),
               fill= c("red","blue","green"),
               bty="n",#type of box to be drawn around the legend. The allowed values are "o" (the default) and "n" no box
               cex = 2
        )

        })
        remove_modal_spinner()
    })
    
    output$PopOutdoorPlot <- renderPlot({
        input$run
        show_modal_spinner()
        isolate({
        out<- out_df()
        out$time <- out$time/365
        par(mfrow = c(3, 1))
        layout(matrix(c(1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,2,2,3,3,0), nrow = 4, ncol = 5, byrow = TRUE))
        max_y <- max(c(out[,"s1FO"],out[,"s2FO"],out[,"s3FO"],out[,"s1MO"],out[,"s2MO"],out[,"s3MO"]), na.rm = TRUE) +10
        total_dog <- out[,"s1FI"] + out[,"s2FI"] + out[,"s3FI"] + out[,"s1MI"] + out[,"s2MI"] + out[,"s3MI"]
        max_total_dog <- max(out[,"s1FI"] + out[,"s2FI"] + out[,"s3FI"] + out[,"s1MI"] + out[,"s2MI"] + out[,"s3MI"])
        plot(out[,"time"], out[,"s1FO"]
             ,col = "red",
             xlab="Time (Years)",ylab="Number of dogs", type = "l",lwd = 3,lty = 1,
             cex.lab=1.7,cex.axis=1.5, cex.main=3,ylim=c(0,max_y),main = "Type of dogs")
        lines(out[,"time"], out[,"s2FO"], col="red" ,lwd = 3,lty=2)
        lines(out[,"time"], out[,"s3FO"], col="red" ,lwd = 3,lty=3)
        lines(out[,"time"], out[,"s1MO"], col="blue" ,lwd = 3,lty=1)
        lines(out[,"time"], out[,"s2MO"], col="blue" ,lwd = 3,lty=2)
        lines(out[,"time"], out[,"s3MO"], col="blue" ,lwd = 3,lty=3)
        par(new=TRUE)
        plot(out[,"time"], total_dog, pch=15,  xlab="", ylab="", ylim=c(0,max_total_dog), 
             axes=FALSE, type="b", col="Green")
        ## a little farther out (line=4) to make room for labels
        mtext("Total dog",side=4,col="Green",line=4) 
        axis(4, ylim=c(0,max_total_dog), col="Green",col.axis="Green",las=1)
        plot.new()
        legend("center", legend=c("Puppy", "Adults", "Older"),
               lty=1:3, 
               bty="n",#type of box to be drawn around the legend. The allowed values are "o" (the default) and "n" no box
               cex = 2,
               lwd = 2
        ) 
        plot.new()
        legend("center", legend=c("Female", "Male"),
               fill= c("red","blue"), 
               bty="n",#type of box to be drawn around the legend. The allowed values are "o" (the default) and "n" no box
               cex = 3
        ) 
        })
        remove_modal_spinner()
    })
    
    output$PopStrayPlot <- renderPlot({
        input$run
        show_modal_spinner()
        isolate({
        out<- out_df()
        out$time <- out$time/365
        par(mfrow = c(3, 1))
        layout(matrix(c(1,1,1,1,0,1,1,1,1,0,1,1,1,1,0,2,2,3,3,0), nrow = 4, ncol = 5, byrow = TRUE))
        max_y <- max(c(out[,"s1FS"],out[,"s2FS"],out[,"s3FS"],out[,"s1MS"],out[,"s2MS"],out[,"s3MS"]), na.rm = TRUE) +10
        total_dog <- out[,"s1FI"] + out[,"s2FI"] + out[,"s3FI"] + out[,"s1MI"] + out[,"s2MI"] + out[,"s3MI"]
        max_total_dog <- max(out[,"s1FI"] + out[,"s2FI"] + out[,"s3FI"] + out[,"s1MI"] + out[,"s2MI"] + out[,"s3MI"])
        plot(out[,"time"], out[,"s1FS"]
             ,col = "red",
             xlab="Time (Years)",ylab="Number of dogs", type = "l",lwd = 3,lty = 1,
             cex.lab=1.7,cex.axis=1.5, cex.main=3,ylim=c(0,max_y),main = "Type of dogs")
        lines(out[,"time"], out[,"s2FS"], col="red" ,lwd = 3,lty=2)
        lines(out[,"time"], out[,"s3FS"], col="red" ,lwd = 3,lty=3)
        lines(out[,"time"], out[,"s1MS"], col="blue" ,lwd = 3,lty=1)
        lines(out[,"time"], out[,"s2MS"], col="blue" ,lwd = 3,lty=2)
        lines(out[,"time"], out[,"s3MS"], col="blue" ,lwd = 3,lty=3)
        par(new=TRUE)
        plot(out[,"time"], total_dog, pch=15,  xlab="", ylab="", ylim=c(0,max_total_dog), 
             axes=FALSE, type="b", col="Green")
        ## a little farther out (line=4) to make room for labels
        mtext("Total dog",side=4,col="Green",line=4) 
        axis(4, ylim=c(0,max_total_dog), col="Green",col.axis="Green",las=1)
        plot.new()
        legend("center", legend=c("Puppy", "Adults", "Older"),
               lty=1:3, 
               bty="n",#type of box to be drawn around the legend. The allowed values are "o" (the default) and "n" no box
               cex = 2,
               lwd = 2
        ) 
        plot.new()
        
        legend("center", legend=c("Female", "Male"),
               fill= c("red","blue"), 
               bty="n",#type of box to be drawn around the legend. The allowed values are "o" (the default) and "n" no box
               cex = 3
        ) 
        })
        remove_modal_spinner()
    })
    
    output$TotalPop <- renderPlot({
        input$run
        show_modal_spinner()
        isolate({
            out<- out_df()
            out$time <- out$time/365
            Sys.sleep(3)
            Pop <- data.frame(time = out$time,pop = round(rowSums(out[,-1])))

            plot(Pop[,"time"], Pop[,"pop"] 
                 ,col = "1",cex.lab=1.7,cex.axis=1.5, cex.main=3,
                 xlab="Time (days)",ylab="Number of dogs", type = "l")
        })
        remove_modal_spinner()
    })
    
    
    output$SterilizationPlot <- renderPlotly({
        input$run
        show_modal_spinner()
        isolate({
            
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
        
       p<- ggplotly(p)%>%
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
        remove_modal_spinner()
        p
    })
    
    output$SterilizationPlot2 <- renderPlotly({
        input$run
        show_modal_spinner()
        isolate({
            
            out<- out_df()
            out$time <- out$time/365
            
            names(out)[26] <- "Female Puppy Outdoor dogs"
            names(out)[27] <- "Female Adult Outdoor dogs"
            names(out)[28] <- "Female Older Outdoor dogs"
            names(out)[29] <- "Male Puppy Outdoor dog"
            names(out)[30] <- "Male Adult Outdoor dogs"
            names(out)[31] <- "Male Older Outdoor dogs"
            
            
            out_melt <- reshape2::melt(out[,c(1,26:31)], id="time")
            
            p <-ggplot(data = out_melt) + 
                labs( x = "Year", y = "Number")+
                geom_line(mapping = aes(x = time, y = value,color = variable),size = 1)+
                theme(axis.title = element_text(size = 20))+
                theme(axis.text = element_text(size = 15, colour="black"))+ 
                theme(legend.title = element_blank())
            
            p <- ggplotly(p)%>%
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
        remove_modal_spinner()
        p
    })
    
    output$SterilizationPlot3 <- renderPlotly({
        input$run
        show_modal_spinner()
        isolate({
            
            out<- out_df()
            out$time <- out$time/365
            
            names(out)[32] <- "Female Puppy Stray dogs"
            names(out)[33] <- "Female Adult Stray dogs"
            names(out)[34] <- "Female Older Stray dogs"
            names(out)[35] <- "Male Puppy Stray dog"
            names(out)[36] <- "Male Adult Stray dogs"
            names(out)[37] <- "Male Older Stray dogs"
            
            
            out_melt <- reshape2::melt(out[,c(1,32:37)], id="time")
            
            p <-ggplot(data = out_melt) + 
                labs( x = "Year", y = "Number")+
                geom_line(mapping = aes(x = time, y = value,color = variable),size = 1)+
                theme(axis.title = element_text(size = 20))+
                theme(axis.text = element_text(size = 15, colour="black"))+ 
                theme(legend.title = element_blank())
            
            p <- ggplotly(p)%>%
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
        remove_modal_spinner()
        p
        
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
    #Population dog
    observeEvent(input$IndoorDogRate, 
                 updateSliderInput(session, "OutdoorDogRate", value = 100-input$IndoorDogRate-input$StrayDogRate)
    )
    
    observeEvent(input$OutdoorDogRate, 
                 updateSliderInput(session, "StrayDogRate", value = 100-input$IndoorDogRate-input$OutdoorDogRate)
    )
    
    observeEvent(input$StrayDogRate, 
                 updateSliderInput(session, "IndoorDogRate", value = 100-input$OutdoorDogRate-input$StrayDogRate)
    )
    
    output$Indoor_pop <- renderUI({

        tagList(
            tags$div(class = "inline",
        tags$h2(
        numericInput("IndoorPop", "Number of Indoor Dog :", 600,
                     min = 0, max = NA)
            ))
        )
    })
    
    output$Outdoor_pop <- renderUI({
        tagList(
            tags$div(class = "inline",
                     tags$h2(
                         numericInput("OutdoorPop", "Number of Outdoor Dog :", 600,
                                      min = 0, max = NA)
                     ))
        )
    })
    
    output$Stray_pop <- renderUI({
        tagList(
            tags$div(class = "inline",
                     tags$h2(
                         numericInput("StrayPop", "Number of Stray Dog :", 600,
                                      min = 0, max = NA)
                     ))
        )
    })
    
    outputOptions(output, "Indoor_pop",  suspendWhenHidden = FALSE)
    outputOptions(output, "Outdoor_pop",  suspendWhenHidden = FALSE)
    outputOptions(output, "Stray_pop",  suspendWhenHidden = FALSE)
    
    output$Indoor_pop_type <- renderUI({
        FemalePuppyIndoor <- round(input$IndoorPop*input$FemaleIndoorRate/100*input$PuppyIndoor/100)
        FemaleAdultIndoor <- round(input$IndoorPop*input$FemaleIndoorRate/100*input$AdultIndoor/100)
        FemaleOlderIndoor <- round(input$IndoorPop*input$FemaleIndoorRate/100*input$OlderIndoor/100)
        MalePuppyIndoor <- round(input$IndoorPop*input$MaleIndoorRate/100*input$PuppyIndoor/100 )
        MaleAdultIndoor <- round(input$IndoorPop*input$MaleIndoorRate/100*input$AdultIndoor/100)
        MaleOlderIndoor <- round(input$IndoorPop*input$MaleIndoorRate/100*input$OlderIndoor/100)
        tagList(
            tags$div(
                tags$div(class="col-sm-6",
                tags$h3(paste0("Female Puppy Indoor : ",FemalePuppyIndoor)),
                tags$h3(paste0("Female Adult Indoor : ",FemaleAdultIndoor)),
                tags$h3(paste0("Female Older Indoor : ",FemaleOlderIndoor)),
                ),
                tags$div(class="col-sm-6",
                tags$h3(paste0("Male Puppy Indoor : ",MalePuppyIndoor)),
                tags$h3(paste0("Male Adult Indoor : ",MaleAdultIndoor)),
                tags$h3(paste0("Male Older Indoor : ",MaleOlderIndoor)),
                )
            )
        )
    })
    
    output$Outdoor_pop_type <- renderUI({
        FemalePuppyOutdoor <- round(input$OutdoorPop*input$FemaleOutdoorRate/100*input$PuppyOutdoor/100)
        FemaleAdultOutdoor <- round(input$OutdoorPop*input$FemaleOutdoorRate/100*input$AdultOutdoor/100)
        FemaleOlderOutdoor <- round(input$OutdoorPop*input$FemaleOutdoorRate/100*input$OlderOutdoor/100)
        MalePuppyOutdoor <- round(input$OutdoorPop*input$MaleOutdoorRate/100*input$PuppyOutdoor/100 )
        MaleAdultOutdoor <- round(input$OutdoorPop*input$MaleOutdoorRate/100*input$AdultOutdoor/100)
        MaleOlderOutdoor <- round(input$OutdoorPop*input$MaleOutdoorRate/100*input$OlderOutdoor/100)
        tagList(
            tags$div(
                tags$div(class="col-sm-6",
                         tags$h3(paste0("Female Puppy Outdoor : ",FemalePuppyOutdoor)),
                         tags$h3(paste0("Female Adult Outdoor : ",FemaleAdultOutdoor)),
                         tags$h3(paste0("Female Older Outdoor : ",FemaleOlderOutdoor)),
                ),
                tags$div(class="col-sm-6",
                         tags$h3(paste0("Male Puppy Outdoor : ",MalePuppyOutdoor)),
                         tags$h3(paste0("Male Adult Outdoor : ",MaleAdultOutdoor)),
                         tags$h3(paste0("Male Older Outdoor : ",MaleOlderOutdoor)),
                )
            )
        )
    })
    
    output$Stray_pop_type <- renderUI({
        FemalePuppyStray <- round(input$StrayPop*input$FemaleStrayRate/100*input$PuppyStray/100)
        FemaleAdultStray <- round(input$StrayPop*input$FemaleStrayRate/100*input$AdultStray/100)
        FemaleOlderStray <- round(input$StrayPop*input$FemaleStrayRate/100*input$OlderStray/100)
        MalePuppyStray <- round(input$StrayPop*input$MaleStrayRate/100*input$PuppyStray/100 )
        MaleAdultStray <- round(input$StrayPop*input$MaleStrayRate/100*input$AdultStray/100)
        MaleOlderStray <- round(input$StrayPop*input$MaleStrayRate/100*input$OlderStray/100)
        tagList(
            tags$div(
                tags$div(class="col-sm-6",
                         tags$h3(paste0("Female Puppy Stray : ",FemalePuppyStray)),
                         tags$h3(paste0("Female Adult Stray : ",FemaleAdultStray)),
                         tags$h3(paste0("Female Older Stray : ",FemaleOlderStray)),
                ),
                tags$div(class="col-sm-6",
                         tags$h3(paste0("Male Puppy Stray : ",MalePuppyStray)),
                         tags$h3(paste0("Male Adult Stray : ",MaleAdultStray)),
                         tags$h3(paste0("Male Older Stray : ",MaleOlderStray)),
                )
            )
        )
    })
    
    
    
    output$popIndoor <- renderText({
        pop_indoor <- input$Pop*input$IndoorDogRate/100
        paste0("Indoor dog : " , pop_indoor)
    })
    
    output$popOutdoor <- renderText({
        pop_outdoor <- input$Pop*input$OutdoorDogRate/100
        paste0("Outdoor dog : " , pop_outdoor)
    })
    
    output$popStray <- renderText({
        pop_Stray <- input$Pop*input$StrayDogRate/100
        paste0("Stray dog : " , pop_Stray)
    })

})
