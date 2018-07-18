setwd("C:/Users/mabdelgadi/Downloads/Private/Role_event/003-day")
data3.original <- read.csv("simDataLong_new.csv",sep = ';',header = TRUE)

#p1 <- data3.original[1:7,]
#p2 <- data3.original[8:14,]
#test <- cbind(p1,p2)
get_matrix <- function(p1){
  i11 <- i12 <- i22 <- i23 <- i33 <- i34 <- i44 <- i45 <- i55 <- 0
  for (x in 1:6){
    if((p1[x:x,3:3] =="NL") & (p1[(x+1):(x+1),3:3] =="NL")){
      i11 = i11+1
    }
    else if((p1[x:x,3:3] =="NL") & (p1[(x+1):(x+1),3:3] =="NLtoMCI")){
      i12 = i12+1
    }
    else if((p1[x:x,3:3] =="NLtoMCI") & (p1[(x+1):(x+1),3:3] =="NLtoMCI")){
      i22 = i22+1
    }
    else if((p1[x:x,3:3] =="NLtoMCI") & (p1[(x+1):(x+1),3:3] =="MCI")){
      i23 = i23+1
    }
    else if((p1[x:x,3:3] =="MCI") & (p1[(x+1):(x+1),3:3] =="MCI")){
      i33 = i33+1
    }
    else if((p1[x:x,3:3] =="MCI") & (p1[(x+1):(x+1),3:3] =="MCItoAD")){
      i34 = i34+1
    }
    else if((p1[x:x,3:3] =="MCItoAD") & (p1[(x+1):(x+1),3:3] =="MCItoAD")){
      i44 = i44+1
    }
    else if((p1[x:x,3:3] =="MCItoAD") & (p1[(x+1):(x+1),3:3] =="AD")){
      i45 = i45+1
    }
    else if((p1[x:x,3:3] =="AD") & (p1[(x+1):(x+1),3:3] =="AD")){
      i55 = i55+1
    }
  }
  i11 <- i11/nrow(p1[p1$STATE == "NL",])
  i12 <- i12/nrow(p1[p1$STATE == "NL",])
  i22 <- i22/nrow(p1[p1$STATE == "NLtoMCI",])
  i23 <- i23/nrow(p1[p1$STATE == "NLtoMCI",])
  i33 <- i33/nrow(p1[p1$STATE == "MCI",])
  i34 <- i34/nrow(p1[p1$STATE == "MCI",])
  i44 <- i44/nrow(p1[p1$STATE == "MCItoAD",])
  i45 <- i45/nrow(p1[p1$STATE == "MCItoAD",])
  i55 <- i55/nrow(p1[p1$STATE == "AD",])
  return(c(i11,i12,i22,i23,i33,i34,i44,i45,i55))
}
counter = 0
for(i in seq(from=1, to=nrow(data3.original), by=7)){
  print(get_matrix(data3.original[i:(i+6),]))
  counter =counter+ 1
}

