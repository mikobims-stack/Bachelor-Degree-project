move_bycicle <- function(simulation_time, direction_from, direction_to){

              #controlling input time to be numeric from 100 to 200
  if(!is.numeric(simulation_time)){
    stop("Error : Enter value from 100 to 200")
  } 

              #controlling input direction left, right, up or down
  direction <- c("Left", "Right", "Up", "Down")
  
  if(!(is.character(direction_from)) || is.null(direction_from) ||  !(direction_from %in% direction)){
    choice <- menu(direction, title = "Select where the car is coming from")
    direction_from <- direction[choice]
  }

  if(!(is.character(direction_to)) || is.null(direction_to) ||  !(direction_to %in% direction) ){
    choice <- menu(direction, title = "Select where the car is going to")
    direction_to <- direction[choice]
  }
  
  x <- -100:100
  y <- x
 
     
  if(direction_from == "Left"){
    for(i in 1:simulation_time){
      
                    #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
                    #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
                    #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
                      #color light
     
      
      if(direction_to == "Left"){
        
        if(i<90){
                  
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
        
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
              #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
          
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
  
        }else if(i>=83 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[85], y[100],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
      
          points(x[90-i+141], y[105],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[85], y[100],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[85+i-141], y[100],pch=20, col="red", cex=2)
        }
      }
      
      if(direction_to == "Up"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[85], y[100],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          if(i-56<105){
            points(x[85+i-141], y[100],pch=20, col="red", cex=2)
          }else{
            points(x[105], y[i-61],pch=20, col="red", cex=2)
          }
        }
      }
      
      if(direction_to == "Down"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[100], y[183-i],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
            
          points(x[100], y[183-i],pch=20, col="red", cex=2)
        }
      }
      
      Sys.sleep(0.1)
    
    }
    print("simulation from left")
  }
  if(direction_from == "Right"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<80){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[200-i], y[105],pch=20, col="red", cex=2)
          
        }else if(i>=100 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[120], y[105],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[140-(i-120)], y[105],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<80){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[200-i], y[105],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[120], y[105],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[i-20], y[100],pch=20, col="red", cex=2)
        }
      }
      
      if(direction_to == "Up"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[195-i], y[105],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[105], y[i+14 ],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[105], y[i+14 ],pch=20, col="red", cex=2)
          
        }
      }
      
      if(direction_to == "Down"){
        
        if(i<85){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[200-i], y[105],pch=20, col="red", cex=2)
          
        }else if(i>=90 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[115], y[105],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          
          if(i<=145){
            points(x[250-i], y[105],pch=20, col="red", cex=2)
          }else{
            points(x[100], y[250-i],pch=20, col="red", cex=2)
          }
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from RIGHT")
  }
  if(direction_from == "Up"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[200-i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[195-i], y[105],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[195-i], y[105 ],pch=20, col="red", cex=2)
          
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[i+10], y[100],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[i+10], y[100 ],pch=20, col="red", cex=2)
          
        }
      }
      
      if(direction_to == "Up"){
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[105], y[i+10],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[105], y[i+10],pch=20, col="red", cex=2)
          
        }
      }
      
      if(direction_to == "Down"){
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }
        
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
  }
  if(direction_from == "Down"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          if(i<=105){
            points(x[105], y[i],pch=20, col="red", cex=2)
          }else{
            points(x[211-i], y[106],pch=20, col="red", cex=2)
          }
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[211-i], y[106],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          if(i<=100){
            points(x[105], y[i],pch=20, col="red", cex=2)
          }else{
            points(x[i+4], y[100],pch=20, col="red", cex=2)
          }
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[i+4], y[100],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Up"){
        if(i<94){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }
      }
      
      if(direction_to == "Down"){
        if(i<94){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          if(i<=100){
            points(x[105], y[i],pch=20, col="red", cex=2)
          }else{
            points(x[100], y[200-i],pch=20, col="red", cex=2)  
          }
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[100], y[200-i],pch=20, col="red", cex=2)  
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
  }
  
}
move_bike <- function(simulation_time, direction_from, direction_to){
  
  #controlling input time to be numeric from 100 to 200
  if(!is.numeric(simulation_time)){
    stop("Error : Enter value from 100 to 200")
  } 
  
  #controlling input direction left, right, up or down
  direction <- c("Left", "Right", "Up", "Down")
  
  if(!(is.character(direction_from)) || is.null(direction_from) ||  !(direction_from %in% direction)){
    choice <- menu(direction, title = "Select where the car is coming from")
    direction_from <- direction[choice]
  }
  
  if(!(is.character(direction_to)) || is.null(direction_to) ||  !(direction_to %in% direction) ){
    choice <- menu(direction, title = "Select where the car is going to")
    direction_to <- direction[choice]
  }
  
  x <- -100:100
  y <- x
  
  
  if(direction_from == "Left"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[85], y[100],pch=20, col="red", cex=2)
          Sys.sleep(0.3)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[90-i+141], y[105],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[85], y[100],pch=20, col="red", cex=2)
          Sys.sleep(0.3)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[85+i-141], y[100],pch=20, col="red", cex=2)
        }
      }
      
      if(direction_to == "Up"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[85], y[100],pch=20, col="red", cex=2)
          Sys.sleep(0.3)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          if(i-56<105){
            points(x[85+i-141], y[100],pch=20, col="red", cex=2)
          }else{
            points(x[105], y[i-61],pch=20, col="red", cex=2)
          }
        }
      }
      
      if(direction_to == "Down"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){ # here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[100], y[183-i],pch=20, col="red", cex=2)
          sys.system(0.3)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[100], y[183-i],pch=20, col="red", cex=2)
        }
      }
      
      Sys.sleep(0.1)
      
    }
    print("simulation from left")
  }
  if(direction_from == "Right"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<80){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[200-i], y[105],pch=20, col="red", cex=2)
          
        }else if(i>=100 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[120], y[105],pch=20, col="red", cex=2)
          sys.sleep(0.3)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[140-(i-120)], y[105],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<80){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[200-i], y[105],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[120], y[105],pch=20, col="red", cex=2)
          sys.sleep(0.3)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[i-20], y[100],pch=20, col="red", cex=2)
        }
      }
      
      if(direction_to == "Up"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[195-i], y[105],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[105], y[i+14 ],pch=20, col="red", cex=2)
          Sys.sleep(0.3)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[105], y[i+14 ],pch=20, col="red", cex=2)
          
        }
      }
      
      if(direction_to == "Down"){
        
        if(i<85){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[200-i], y[105],pch=20, col="red", cex=2)
          
        }else if(i>=90 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[115], y[105],pch=20, col="red", cex=2)
          sys.sleep(0.3)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          
          if(i<=145){
            points(x[250-i], y[105],pch=20, col="red", cex=2)
          }else{
            points(x[100], y[250-i],pch=20, col="red", cex=2)
          }
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from RIGHT")
  }
  if(direction_from == "Up"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[200-i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[195-i], y[105],pch=20, col="red", cex=2)
          sys.sleep(0.3)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[195-i], y[105 ],pch=20, col="red", cex=2)
          
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[i+10], y[100],pch=20, col="red", cex=2)
          sys.sleep(0.3)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[i+10], y[100 ],pch=20, col="red", cex=2)
          
        }
      }
      
      if(direction_to == "Up"){
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[105], y[i+10],pch=20, col="red", cex=2)
          sys.sleep(0.3)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[105], y[i+10],pch=20, col="red", cex=2)
          
        }
      }
      
      if(direction_to == "Down"){
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
          sys.sleep(0.3)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }
        
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
  }
  if(direction_from == "Down"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          if(i<=105){
            points(x[105], y[i],pch=20, col="red", cex=2)
          }else{
            points(x[211-i], y[106],pch=20, col="red", cex=2)
          }
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[211-i], y[106],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          if(i<=100){
            points(x[105], y[i],pch=20, col="red", cex=2)
          }else{
            points(x[i+4], y[100],pch=20, col="red", cex=2)
          }
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[i+4], y[100],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Up"){
        if(i<94){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }
      }
      
      if(direction_to == "Down"){
        if(i<94){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          if(i<=100){
            points(x[105], y[i],pch=20, col="red", cex=2)
          }else{
            points(x[100], y[200-i],pch=20, col="red", cex=2)  
          }
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[100], y[200-i],pch=20, col="red", cex=2)  
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
  }
  
}
move_car <- function(simulation_time, direction_from, direction_to){
  
  #controlling input time to be numeric from 100 to 200
  if(!is.numeric(simulation_time)){
    stop("Error : Enter value from 100 to 200")
  } 
  
  #controlling input direction left, right, up or down
  direction <- c("Left", "Right", "Up", "Down")
  
  if(!(is.character(direction_from)) || is.null(direction_from) ||  !(direction_from %in% direction)){
    choice <- menu(direction, title = "Select where the car is coming from")
    direction_from <- direction[choice]
  }
  
  if(!(is.character(direction_to)) || is.null(direction_to) ||  !(direction_to %in% direction) ){
    choice <- menu(direction, title = "Select where the car is going to")
    direction_to <- direction[choice]
  }
  
  x <- -100:100
  y <- x
  
  
  if(direction_from == "Left"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[85], y[100],pch=20, col="red", cex=2)
          sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[90-i+141], y[105],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[85], y[100],pch=20, col="red", cex=2)
          Sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[85+i-141], y[100],pch=20, col="red", cex=2)
        }
      }
      
      if(direction_to == "Up"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[85], y[100],pch=20, col="red", cex=2)
          Sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          if(i-56<105){
            points(x[85+i-141], y[100],pch=20, col="red", cex=2)
          }else{
            points(x[105], y[i-61],pch=20, col="red", cex=2)
          }
        }
      }
      
      if(direction_to == "Down"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[100], y[183-i],pch=20, col="red", cex=2)
          sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[100], y[183-i],pch=20, col="red", cex=2)
        }
      }
      
      Sys.sleep(0.1)
      
    }
    print("simulation from left")
  }
  if(direction_from == "Right"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<80){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[200-i], y[105],pch=20, col="red", cex=2)
          
        }else if(i>=100 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[120], y[105],pch=20, col="red", cex=2)
          sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[140-(i-120)], y[105],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<80){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[200-i], y[105],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[120], y[105],pch=20, col="red", cex=2)
          sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[i-20], y[100],pch=20, col="red", cex=2)
        }
      }
      
      if(direction_to == "Up"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[195-i], y[105],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[105], y[i+14 ],pch=20, col="red", cex=2)
          sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[105], y[i+14 ],pch=20, col="red", cex=2)
          
        }
      }
      
      if(direction_to == "Down"){
        
        if(i<85){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[200-i], y[105],pch=20, col="red", cex=2)
          
        }else if(i>=90 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[115], y[105],pch=20, col="red", cex=2)
          sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          
          if(i<=145){
            points(x[250-i], y[105],pch=20, col="red", cex=2)
          }else{
            points(x[100], y[250-i],pch=20, col="red", cex=2)
          }
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from RIGHT")
  }
  if(direction_from == "Up"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[200-i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[195-i], y[105],pch=20, col="red", cex=2)
          sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[195-i], y[105 ],pch=20, col="red", cex=2)
          
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[i+10], y[100],pch=20, col="red", cex=2)
          sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[i+10], y[100 ],pch=20, col="red", cex=2)
          
        }
      }
      
      if(direction_to == "Up"){
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[105], y[i+10],pch=20, col="red", cex=2)
          sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[105], y[i+10],pch=20, col="red", cex=2)
          
        }
      }
      
      if(direction_to == "Down"){
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
          sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }
        
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
  }
  if(direction_from == "Down"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          if(i<=105){
            points(x[105], y[i],pch=20, col="red", cex=2)
          }else{
            points(x[211-i], y[106],pch=20, col="red", cex=2)
          }
          sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[211-i], y[106],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          if(i<=100){
            points(x[105], y[i],pch=20, col="red", cex=2)
          }else{
            points(x[i+4], y[100],pch=20, col="red", cex=2)
          }
          sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[i+4], y[100],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Up"){
        if(i<94){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[105], y[i],pch=20, col="red", cex=2)
          sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }
      }
      
      if(direction_to == "Down"){
        if(i<94){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){# here the vehicle reaches the junction
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          if(i<=100){
            points(x[105], y[i],pch=20, col="red", cex=2)
          }else{
            points(x[100], y[200-i],pch=20, col="red", cex=2)  
          }
          sys.sleep(0.6)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[100], y[200-i],pch=20, col="red", cex=2)  
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
  }
  
}
move_truk <- function(simulation_time, direction_from, direction_to){
  
  #controlling input time to be numeric from 100 to 200
  if(!is.numeric(simulation_time)){
    stop("Error : Enter value from 100 to 200")
  } 
  
  #controlling input direction left, right, up or down
  direction <- c("Left", "Right", "Up", "Down")
  
  if(!(is.character(direction_from)) || is.null(direction_from) ||  !(direction_from %in% direction)){
    choice <- menu(direction, title = "Select where the car is coming from")
    direction_from <- direction[choice]
  }
  
  if(!(is.character(direction_to)) || is.null(direction_to) ||  !(direction_to %in% direction) ){
    choice <- menu(direction, title = "Select where the car is going to")
    direction_to <- direction[choice]
  }
  
  x <- -100:100
  y <- x
  
  
  if(direction_from == "Left"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[85], y[100],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[90-i+141], y[105],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[85], y[100],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[85+i-141], y[100],pch=20, col="red", cex=2)
        }
      }
      
      if(direction_to == "Up"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[85], y[100],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          if(i-56<105){
            points(x[85+i-141], y[100],pch=20, col="red", cex=2)
          }else{
            points(x[105], y[i-61],pch=20, col="red", cex=2)
          }
        }
      }
      
      if(direction_to == "Down"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[100], y[183-i],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[100], y[183-i],pch=20, col="red", cex=2)
        }
      }
      
      Sys.sleep(0.1)
      
    }
    print("simulation from left")
  }
  if(direction_from == "Right"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<80){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[200-i], y[105],pch=20, col="red", cex=2)
          
        }else if(i>=100 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[120], y[105],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[140-(i-120)], y[105],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<80){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[200-i], y[105],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[120], y[105],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[i-20], y[100],pch=20, col="red", cex=2)
        }
      }
      
      if(direction_to == "Up"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[195-i], y[105],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[105], y[i+14 ],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[105], y[i+14 ],pch=20, col="red", cex=2)
          
        }
      }
      
      if(direction_to == "Down"){
        
        if(i<85){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[200-i], y[105],pch=20, col="red", cex=2)
          
        }else if(i>=90 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[115], y[105],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          
          if(i<=145){
            points(x[250-i], y[105],pch=20, col="red", cex=2)
          }else{
            points(x[100], y[250-i],pch=20, col="red", cex=2)
          }
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from RIGHT")
  }
  if(direction_from == "Up"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[200-i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[195-i], y[105],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[195-i], y[105 ],pch=20, col="red", cex=2)
          
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[i+10], y[100],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[i+10], y[100 ],pch=20, col="red", cex=2)
          
        }
      }
      
      if(direction_to == "Up"){
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[105], y[i+10],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[105], y[i+10],pch=20, col="red", cex=2)
          
        }
      }
      
      if(direction_to == "Down"){
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }
        
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
  }
  if(direction_from == "Down"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          if(i<=105){
            points(x[105], y[i],pch=20, col="red", cex=2)
          }else{
            points(x[211-i], y[106],pch=20, col="red", cex=2)
          }
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[211-i], y[106],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          if(i<=100){
            points(x[105], y[i],pch=20, col="red", cex=2)
          }else{
            points(x[i+4], y[100],pch=20, col="red", cex=2)
          }
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[i+4], y[100],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Up"){
        if(i<94){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }
      }
      
      if(direction_to == "Down"){
        if(i<94){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          if(i<=100){
            points(x[105], y[i],pch=20, col="red", cex=2)
          }else{
            points(x[100], y[200-i],pch=20, col="red", cex=2)  
          }
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[100], y[200-i],pch=20, col="red", cex=2)  
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
  }
  
}
move_all_vehicules <- function(simulation_time, direction_from, direction_to){
  
  #controlling input time to be numeric from 100 to 200
  if(!is.numeric(simulation_time)){
    stop("Error : Enter value from 100 to 200")
  } 
  
  #controlling input direction left, right, up or down
  direction <- c("Left", "Right", "Up", "Down")
  
  if(!(is.character(direction_from)) || is.null(direction_from) ||  !(direction_from %in% direction)){
    choice <- menu(direction, title = "Select where the car is coming from")
    direction_from <- direction[choice]
  }
  
  if(!(is.character(direction_to)) || is.null(direction_to) ||  !(direction_to %in% direction) ){
    choice <- menu(direction, title = "Select where the car is going to")
    direction_to <- direction[choice]
  }
  
  x <- -100:100
  y <- x
  
  
  if(direction_from == "Left"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[85], y[100],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[90-i+141], y[105],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[85], y[100],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[85+i-141], y[100],pch=20, col="red", cex=2)
        }
      }
      
      if(direction_to == "Up"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[85], y[100],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          if(i-56<105){
            points(x[85+i-141], y[100],pch=20, col="red", cex=2)
          }else{
            points(x[105], y[i-61],pch=20, col="red", cex=2)
          }
        }
      }
      
      if(direction_to == "Down"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[i], y[100],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[100], y[183-i],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[100], y[183-i],pch=20, col="red", cex=2)
        }
      }
      
      Sys.sleep(0.1)
      
    }
    print("simulation from left")
  }
  if(direction_from == "Right"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<80){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[200-i], y[105],pch=20, col="red", cex=2)
          
        }else if(i>=100 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[120], y[105],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[140-(i-120)], y[105],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<80){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[200-i], y[105],pch=20, col="red", cex=2)
          
        }else if(i>=83 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[120], y[105],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[i-20], y[100],pch=20, col="red", cex=2)
        }
      }
      
      if(direction_to == "Up"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[195-i], y[105],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[105], y[i+14 ],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[105], y[i+14 ],pch=20, col="red", cex=2)
          
        }
      }
      
      if(direction_to == "Down"){
        
        if(i<85){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[200-i], y[105],pch=20, col="red", cex=2)
          
        }else if(i>=90 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[115], y[105],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          
          if(i<=145){
            points(x[250-i], y[105],pch=20, col="red", cex=2)
          }else{
            points(x[100], y[250-i],pch=20, col="red", cex=2)
          }
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from RIGHT")
  }
  if(direction_from == "Up"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[200-i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[195-i], y[105],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[195-i], y[105 ],pch=20, col="red", cex=2)
          
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[i+10], y[100],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[i+10], y[100 ],pch=20, col="red", cex=2)
          
        }
      }
      
      if(direction_to == "Up"){
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[105], y[i+10],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[105], y[i+10],pch=20, col="red", cex=2)
          
        }
      }
      
      if(direction_to == "Down"){
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>=91 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[100], y[190-i],pch=20, col="red", cex=2)
        }
        
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
  }
  if(direction_from == "Down"){
    for(i in 1:simulation_time){
      
      #road from left to right and vice versa
      plot(c(-100,100), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100)) 
      lines(c(-100,100), c(5,5), col="yellow", lwd=15)
      
      #road from up to down and vice versa
      lines(c(0,0), c(-100,100), col="yellow", lwd=15)
      lines(c(5,5), c(-100,100), col="black", lwd=15)
      
      #drawing the main junction  
      points(x[104], y[104],pch=20, col="white", cex=15)
      
      #color light
      
      
      if(direction_to == "Left"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          if(i<=105){
            points(x[105], y[i],pch=20, col="red", cex=2)
          }else{
            points(x[211-i], y[106],pch=20, col="red", cex=2)
          }
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[211-i], y[106],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Right"){
        
        if(i<90){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          if(i<=100){
            points(x[105], y[i],pch=20, col="red", cex=2)
          }else{
            points(x[i+4], y[100],pch=20, col="red", cex=2)
          }
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[i+4], y[100],pch=20, col="red", cex=2)
        }
      }
      
      
      if(direction_to == "Up"){
        if(i<94){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }
      }
      
      if(direction_to == "Down"){
        if(i<94){
          
          if(i>50){
            points(x[87], y[85],pch=15, col="red", cex=2)
            points(x[121], y[120],pch=15, col="red", cex=2)
            
            points(x[94], y[137],pch=15, col="green", cex=2)
            points(x[113], y[75],pch=15, col="green", cex=2)
          }else{
            
            #color light
            points(x[87], y[85],pch=15, col="green", cex=2)
            points(x[121], y[120],pch=15, col="green", cex=2)
            
            points(x[94], y[137],pch=15, col="red", cex=2)
            points(x[113], y[75],pch=15, col="red", cex=2)
            
          }
          
          points(x[105], y[i],pch=20, col="red", cex=2)
        }else if(i>=95 && i<=140){
          points(x[87], y[85],pch=15, col="red", cex=2)
          points(x[121], y[120],pch=15, col="red", cex=2)
          
          points(x[94], y[137],pch=15, col="green", cex=2)
          points(x[113], y[75],pch=15, col="green", cex=2)
          if(i<=100){
            points(x[105], y[i],pch=20, col="red", cex=2)
          }else{
            points(x[100], y[200-i],pch=20, col="red", cex=2)  
          }
        }else if(i>140){
          points(x[87], y[85],pch=15, col="green", cex=2)
          points(x[121], y[120],pch=15, col="green", cex=2)
          
          points(x[94], y[137],pch=15, col="red", cex=2)
          points(x[113], y[75],pch=15, col="red", cex=2)
          
          points(x[100], y[200-i],pch=20, col="red", cex=2)  
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
  }
  
}
move_bike(200,"Left","UP")