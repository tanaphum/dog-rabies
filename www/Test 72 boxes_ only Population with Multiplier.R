#SetDirectory
setwd("E:/Tropical Medicine/RB Project/R/RB Beginner")

#####
#All types
#s1: Puppy, s2: Adult, s3: Old
#F: Female
#M: Male
#B1: I&O Birth rate
#B2: Stray Birth

library(deSolve)
sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    #I:Indoor  #B1 DI1 -BFO   -St                  +Adopt - Stray rate    +Indoor vac -vaccov*                 Sterilized
    ds1FI <- B1*s2FI-m*(1/365)*s1FI-DI1*s1FI           +AR*s1FS-SR*s1FI                -vaccov*s1FI              -St.rate*s1FI
    ds2FI <- m*(1/365)*s1FI-m*(1/(365*8))*s2FI-DI2*s2FI    +AR*s2FS-SR*s2FI    +(1/365)*s1FIV+(1/365)*s2FIV-vaccov*s2FI  -St.rate*s2FI
    ds3FI <- m*(1/(365*8))*s2FI-m*(1/(365*5))*s3FI-DI3*s3FI  +AR*s3FS-SR*s3FI    +(1/365)*s3FIV-vaccov*s3FI              -St.rate*s3FI
    ds1MI <- B1*s2FI-m*(1/365)*s1MI-DI1*s1MI           +AR*s1MS-SR*s1MI                -vaccov*s1MI              -St.rate*s1MI
    ds2MI <- m*(1/365)*s1MI-m*(1/(365*8))*s2MI-DI2*s2MI    +AR*s2MS-SR*s2MI    +1/365*s1MIV+1/365*s2MIV-vaccov*s2MI  -St.rate*s2MI
    ds3MI <- m*(1/(365*8))*s2MI-m*(1/(365*5))*s3MI-DI3*s3MI  +AR*s3MS-SR*s3MI    +1/365*s3MIV-vaccov*s3MI              -St.rate*s3MI     
    
    #O:Outdoor                                     +Adopt - Stray rate        Indoor vac                       sterilized
    ds1FO <- B1*s2FO-m*(1/365)*s1FO-DO1*s1FO           +AR*s1FS-SR*s1FO                -vaccov*s1FO              -St.rate*s1FO
    ds2FO <- m*(1/365)*s1FO-m*(1/(365*8))*s2FO-DO2*s2FO    +AR*s2FS-SR*s2FO    +1/365*s1FOV+1/365*s2FOV-vaccov*s2FO  -St.rate*s2FO
    ds3FO <- m*(1/(365*8))*s2FO-m*(1/(365*5))*s3FO-DO3*s3FO  +AR*s3FS-SR*s3FO    +1/365*s3FOV-vaccov*s3FO              -St.rate*s3FO
    ds1MO <- B1*s2FO-m*(1/365)*s1MO-DO1*s1MO           +AR*s1MS-SR*s1MO                -vaccov*s1MO              -St.rate*s1MO
    ds2MO <- m*(1/365)*s1MO-m*(1/(365*8))*s2MO-DO2*s2MO    +AR*s2MS-SR*s2MO    +1/365*s1MOV+1/365*s2MOV-vaccov*s2MO  -St.rate*s2MO
    ds3MO <- m*(1/(365*8))*s2MO-m*(1/(365*5))*s3MO-DO3*s3MO  +AR*s3MS-SR*s3MO    +1/365*s3MOV-vaccov*s3MO              -St.rate*s3MO  
    
    #S:Stray
    #include Stray rate((5.02/1000)/365)-Adopt rate ((0.78/1000)/365)                Stray vac                        sterilized                  
    ds1FS <- B2*s2FS-m*(1/365)*s1FS-DS*s1FS           +SR*s1FI+SR*s1FO-AR*s1FS              -vaccov1*s1FS              -St.rate1*s1FS    
    ds2FS <- m*(1/365)*s1FS-m*(1/(365*8))*s2FS-DS*s2FS    +SR*s2FI+SR*s2FO-AR*s2FS  +1/365*s1FSV+1/365*s2FSV-vaccov1*s2FS  -St.rate1*s2FS
    ds3FS <- m*(1/(365*8))*s2FS-m*(1/(365*5))*s3FS-DS*s3FS  +SR*s3FI+SR*s3FO-AR*s3FS  +1/365*s3FSV-vaccov1*s3FS              -St.rate1*s3FS
    ds1MS <- B2*s2FS-m*(1/365)*s1MS-DS*s1MS           +SR*s1MI+SR*s1MO-AR*s1MS              -vaccov1*s1MS              -St.rate1*s1MS
    ds2MS <- m*(1/365)*s1MS-m*(1/(365*8))*s2MS-DS*s2MS    +SR*s2MI+SR*s2MO-AR*s2MS  +1/365*s1MSV+1/365*s2MSV-vaccov1*s2MS  -St.rate1*s2MS
    ds3MS <- m*(1/(365*8))*s2MS-m*(1/(365*5))*s3MS-DS*s3MS  +SR*s3MI+SR*s3MO-AR*s3MS  +1/365*s3MSV-vaccov1*s3MS              -St.rate1*s3MS
    
    #ISt: Indoor Sterilization *120/365>St coverage                     Indoor sterilization Vac
    ds1FISt <- St.rate*s1FI-(1/365)*s1FISt-DI1*s1FISt                                 -vaccov*s1FISt
    ds2FISt <- (1/365)*s1FISt+St.rate*s2FI-1/(365*8)*s2FISt-DI2*s2FISt  +1/365*s1FIStV+1/365*s2FIStV-vaccov*s2FISt     
    ds3FISt <- 1/(365*8)*s2FISt+St.rate*s3FI-DI3*s3FISt                 +1/365*s3FIStV-vaccov*s3FISt
    ds1MISt <- St.rate*s1MI-(1/365)*s1MISt-DI1*s1MISt                               -vaccov*s1MISt     
    ds2MISt <- (1/365)*s1MISt+St.rate*s2MI-1/(365*8)*s2MISt-DI2*s2MISt  +1/365*s1MIStV+1/365*s2MIStV-vaccov*s2MISt     
    ds3MISt <- 1/(365*8)*s2MISt+St.rate*s3MI-DI3*s3MISt                 +1/365*s3MIStV-vaccov*s3MISt    
    
    #Ost : Outdoor Sterilization                                        Outdoor Sterilization vac
    ds1FOSt <- St.rate*s1FO-(1/365)*s1FOSt-DO1*s1FOSt                                 -vaccov*s1FOSt     
    ds2FOSt <- (1/365)*s1FOSt+St.rate*s2FO-1/(365*8)*s2FOSt-DO2*s2FOSt  +1/365*s1FOStV+1/365*s2FOStV-vaccov*s2FOSt      
    ds3FOSt <- 1/(365*8)*s2FOSt+St.rate*s3FO-DO3*s3FOSt                 +1/365*s3FOStV-vaccov*s3FOSt      
    ds1MOSt <- St.rate*s1MO-(1/365)*s1MOSt-DO1*s1MOSt                                 -vaccov*s1MOSt       
    ds2MOSt <- (1/365)*s1MOSt+St.rate*s2MO-1/(365*8)*s2MOSt-DO2*s2MOSt  +1/365*s1MOStV+1/365*s2MOStV-vaccov*s2MOSt       
    ds3MOSt <- 1/(365*8)*s2MOSt+St.rate*s3MO-DO3*s3MOSt                 +1/365*s3MOStV-vaccov*s3MOSt       
    
    #SSt : Stray Sterilization                                          Stray Sterilization vac
    ds1FSSt <- St.rate*s1FS-(1/365)*s1FSSt-DS*s1FSSt                                   -vaccov1*s1FSSt     
    ds2FSSt <- (1/365)*s1FSSt+St.rate*s2FS-1/(365*8)*s2FSSt-DS*s2FSSt    +1/365*s1FSStV+1/365*s2FSStV-vaccov1*s2FSSt      
    ds3FSSt <- 1/(365*8)*s2FSSt+St.rate*s3FO-DS*s3FSSt                   +1/365*s3FSStV-vaccov1*s3FSSt      
    ds1MSSt <- St.rate*s1MS-(1/365)*s1MSSt-DS*s1MSSt                                   -vaccov1*s1MSSt     
    ds2MSSt <- (1/365)*s1MSSt+St.rate*s2MS-1/(365*8)*s2MSSt-DS*s2MSSt    +1/365*s1MSStV+1/365*s2MSStV-vaccov1*s2MSSt    
    ds3MSSt <- 1/(365*8)*s2MSSt+St.rate*s3MS-DS*s3MSSt                   +1/365*s3MSStV-vaccov1*s3MSSt
    
    ##############                
    #Vac indoor   Vaccov flow in - vac flow out
    ds1FIV <- vaccov*s1FI-1/365*s1FIV
    ds2FIV <- vaccov*s2FI-1/365*s2FIV
    ds3FIV <- vaccov*s3FI-1/365*s3FIV
    ds1MIV <- vaccov*s1MI-1/365*s1MIV
    ds2MIV <- vaccov*s2MI-1/365*s2MIV
    ds3MIV <- vaccov*s3MI-1/365*s3MIV
    
    #vac outdoor
    ds1FOV <- vaccov*s1FO-1/365*s1FOV
    ds2FOV <- vaccov*s2FO-1/365*s2FOV
    ds3FOV <- vaccov*s3FO-1/365*s3FOV
    ds1MOV <- vaccov*s1MO-1/365*s1MOV
    ds2MOV <- vaccov*s2MO-1/365*s2MOV
    ds3MOV <- vaccov*s3MO-1/365*s3MOV
    
    #vac stray
    ds1FSV <- vaccov1*s1FS-1/365*s1FSV
    ds2FSV <- vaccov1*s2FS-1/365*s2FSV
    ds3FSV <- vaccov1*s3FS-1/365*s3FSV
    ds1MSV <- vaccov1*s1MS-1/365*s1MSV
    ds2MSV <- vaccov1*s2MS-1/365*s2MSV
    ds3MSV <- vaccov1*s3MS-1/365*s3MSV
    
    #vac in indoor sterilized dogs
    ds1FIStV <- vaccov*s1FISt-1/365*s1FIStV
    ds2FIStV <- vaccov*s2FISt-1/365*s2FIStV
    ds3FIStV <- vaccov*s3FISt-1/365*s3FIStV
    ds1MIStV <- vaccov*s1MISt-1/365*s1MIStV
    ds2MIStV <- vaccov*s2MISt-1/365*s2MIStV
    ds3MIStV <- vaccov*s3MISt-1/365*s3MIStV
    
    #vac outdoor sterilized
    ds1FOStV <- vaccov*s1FOSt-1/365*s1FOStV
    ds2FOStV <- vaccov*s2FOSt-1/365*s2FOStV
    ds3FOStV <- vaccov*s3FOSt-1/365*s3FOStV
    ds1MOStV <- vaccov*s1FOSt-1/365*s1FOStV
    ds2MOStV <- vaccov*s2FOSt-1/365*s2FOStV
    ds3MOStV <- vaccov*s3FOSt-1/365*s3FOStV
    
    #vac stray sterilized
    ds1FSStV <- vaccov1*s1FSSt-1/365*s1FSStV
    ds2FSStV <- vaccov1*s2FSSt-1/365*s2FSStV
    ds3FSStV <- vaccov1*s3FSSt-1/365*s3FSStV
    ds1MSStV <- vaccov1*s1MSSt-1/365*s1MSStV
    ds2MSStV <- vaccov1*s2MSSt-1/365*s2MSStV
    ds3MSStV <- vaccov1*s3MSSt-1/365*s3MSStV
    
    
    
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
#Proportion**** 
#Own 309, stray 12 assumed 2 each age
init <- c(s1FI = 400, s2FI = 00,s3FI = 00, 
          s1MI = 00, s2MI = 00,s3MI = 00,
          s1FO = 00, s2FO = 00,s3FO = 00, 
          s1MO = 00, s2MO = 00,s3MO = 00,
          s1FS = 00, s2FS = 00,s3FS = 00, 
          s1MS = 00, s2MS = 00,s3MS = 00,
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

#B rate No.Newborn/year
#St.rate =OwnD, St.rate1=StrayD
parameters <- c(B1 = 0, B2= 0,
                DI1 = 0, DI2= 0, DI3= 0, 
                DO1 = 0, DO2= 0, DO3= 0, 
                DS = 0, 
                #m multiplier
                m =5,
                #SR=0,AR=0
                SR= 0, AR= 0,
                St.rate=0, St.rate1=0, vaccov=0, vaccov1=0
)   
times <- seq(0, 365*20, by = 1)
out <- as.data.frame(ode(y = init, times = times, func = sir, parms = parameters),method="euler")
out$time <- NULL

out[7301,]

#out 1-3 Female, 4-6 Male Indoor
plot(times, out[,1]
     ,col = "1",
     xlab="Time (days)",ylab="Number of dogs", type = "l",
     ylim=c(0,500),xlim = c(0,(365*20)),main = "Type of dogs")
lines(times, out[,2], col="2")
lines(times, out[,3], col="3")
lines(times, out[,4], col="4")
lines(times, out[,5], col="5")
lines(times, out[,6], col="6")

#out 7-9 Female, 10-12 Male Outdoor
plot(times, out[,7]
     ,col = "7",
     xlab="Time (days)",ylab="Number of dogs", type = "l",
     ylim=c(0,500),xlim = c(0,(365*20)),main = "Type of dogs")

lines(times, out[,8], col="8")
lines(times, out[,9], col="9")
lines(times, out[,10], col="10")
lines(times, out[,11], col="11")
lines(times, out[,12], col="12")

#out 13-15 Female, 16-18 Male Stray
plot(times, out[,13]
     ,col = "13",
     xlab="Time (days)",ylab="Number of dogs", type = "l",
     ylim=c(0,500),xlim = c(0,(365*20)),main = "Type of dogs")
lines(times, out[,14], col="14")
lines(times, out[,15], col="15")
lines(times, out[,16], col="16")
lines(times, out[,17], col="17")
lines(times, out[,18], col="18")

#out 19-21 Female, 22-24 Male Indoor with Sterilization 
plot(times, out[,19]
     ,col = "19",
     xlab="Time (days)",ylab="Number of dogs", type = "l",
     ylim=c(0,500),xlim = c(0,(365*20)),main = "Type of dogs")
lines(times, out[,20], col="20")
lines(times, out[,21], col="21")
lines(times, out[,22], col="22")
lines(times, out[,23], col="23")
lines(times, out[,24], col="24")


#out 25-27 Female, 28-30 Male Outdoor with Sterilization 
plot(times, out[,25]
     ,col = "25",
     xlab="Time (days)",ylab="Number of dogs", type = "l",
     ylim=c(0,500),xlim = c(0,(365*20)),main = "Type of dogs")
lines(times, out[,26], col="26")
lines(times, out[,27], col="27")
lines(times, out[,28], col="28")
lines(times, out[,29], col="29")
lines(times, out[,30], col="30")

#out 31-33 Female, 34-36 Male Stray with Sterilization 
plot(times, out[,31]
     ,col = "31",
     xlab="Time (days)",ylab="Number of dogs", type = "l",
     ylim=c(0,500),xlim = c(0,(365*20)),main = "Type of dogs")
lines(times, out[,32], col="32")
lines(times, out[,33], col="33")
lines(times, out[,34], col="34")
lines(times, out[,35], col="35")
lines(times, out[,36], col="36")

#out 37-39 Female, 40-42 Male Indoor vaccination
plot(times, out[,37]
     ,col = "37",
     xlab="Time (days)",ylab="Number of dogs", type = "l",
     ylim=c(0,500),xlim = c(0,(365*20)),main = "Type of dogs")
lines(times, out[,38], col="38")
lines(times, out[,39], col="39")
lines(times, out[,40], col="40")
lines(times, out[,41], col="41")
lines(times, out[,42], col="42")

sum(out[,1:3])

death <- 1/(365*5)*out[,"s3FI"]
sum(death)
