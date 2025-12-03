#we consider all the cars are mouving at the same speed, 
#and each car takes a decision depending its position 
x <- 0:100
y <- -50:50
j<-0


simulation_time <- as.numeric(readline(prompt="Enter the time of simulation ; "))
turning_pt1<-55
turning_pt2<-51
lightcolor<- "green"

#library(gifski)
#  save_gif({
for(i in 1:simulation_time){

  #point(0,0.3, pch=6, col="blue" cex=5)
  
   # drawing the four lines which represent roads
  plot(c(0,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-55,50), xlim=c(0,100)) 
  lines(c(0,100), c(5,5), col="yellow", lwd=15)
  
  lines(c(50,50), c(-50,50), col="yellow", lwd=15)
  lines(c(55,55), c(-50,50), col="black", lwd=15)
    
  
                #cars from the left
  #add car1 from left
  
  if(i>=50 && i<=100){
    lightcolor <- "red" #1 represent the red color
  }else{
    lightcolor <- "blue"
    j <- i-50
  }
  if(lightcolor == 1){ 
    points(x[50], y[51],pch=20, col="red", cex=2)
  }else{
    points(x[j], y[51],pch=20, col="red", cex=2)
  }
  
  #add car2 and car3 from right
  if(i<=turning_pt1){
    points(x[i]-3, y[51], pch=20, col="green", cex=2)
   }else{
     points(x[turning_pt1], y[51+(i-turning_pt1)], pch=20, col="green", cex=2)
   }
    if(i<=turning_pt2){
      points(x[i]-7, y[51], pch=20, col="violet", cex=2)
    }else{
      points(x[turning_pt2], y[51+(turning_pt2-i)], pch=20, col="violet", cex=2)
    }
    
                    #cars from the right
    #add car1 from right
    points(x[100-i], y[56],pch=20, col="red", cex=2)
    
    #add car2 and car3 from right
    if(i<=turning_pt1){
      points(x[100-i]-6, y[56], pch=20, col="cyan", cex=2)
    }else{
      points(x[turning_pt1], y[56+(i-turning_pt2)], pch=20, col="cyan", cex=2)
    }
  
    if(i<=turning_pt2){
        points(x[100-i]-3, y[56], pch=20, col="blue", cex=2)
      }else{
        points(x[turning_pt2], y[56+(turning_pt2-i)], pch=20, col="blue", cex=2)
      }

    
    Sys.sleep(0.04)
}

#}, gif_file="myanimation.gif", width=600, height=600, delay=1)