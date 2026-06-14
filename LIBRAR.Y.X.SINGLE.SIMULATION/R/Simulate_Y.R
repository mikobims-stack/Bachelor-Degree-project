#' Creation of the main axes that forms the four way direction
#'
#' This Procedure create the Roads view from the Four way direction
#' this procedure is automatically call once the simulation of a particular vehicle is lunched.
#'
#' @param x,y the dimension or size of the roads from east to west (x) and from north to south(y).
#' @return No return value
#' @export
roads_four_ways <- function(x,y){
  #road from left to right and vice versa
  plot(c(-100,0), c(0,0), type ="l", col="black", lwd=15, axes=FALSE, ylim=c(-100,100), xlim=c(-100,100))
  lines(c(-100,0), c(5,5), col="yellow", lwd=15)
  
  #road from up to down and vice versa
  lines(c(0,0), c(-100,100), col="yellow", lwd=15)
  lines(c(5,5), c(-100,100), col="black", lwd=15)
  
  #drawing the main junction
  points(x[104], y[104],pch=10, col="white", cex=15)
}

#' Creation of the traffic light
#'
#' This procedure creates the traffic light  on the 4 way direction road and it is incharge of the
#' green light.
#' this procedure is use in other procedure and is not accessible by the public
#' this procedure is automatically call once the simulation of a particular vehicle is lunched.
#'
#' @param are the coordinates of the traffic light on the roads in other word its location.
#' @return No return value
light_color_green_left <-function(x,y){
  points(x[87], y[85],pch=15, col="green", cex=2)
  points(x[121], y[120],pch=15, col="green", cex=2)
  
  points(x[94], y[137],pch=15, col="red", cex=2)
  points(x[113], y[75],pch=15, col="red", cex=2)
}

#' Creation of the traffic light
#'
#' This procedure creates the traffic light  on the 4 way direction road and it is incharge of the
#' red light.
#'  this procedure is use in the other procedure and is not accessible by the public
#' this procedure is automatically call once the simulation of a particular vehicle is lunched.

#' @param are the coordinates of the traffic light on the roads in other word its location.
#' @return No return value

light_color_red_left <-function(x,y){
  points(x[87], y[85],pch=15, col="red", cex=2)
  points(x[121], y[120],pch=15, col="red", cex=2)
  
  points(x[94], y[137],pch=15, col="green", cex=2)
  points(x[113], y[75],pch=15, col="green", cex=2)
}

#' Displacement of the vehicles from east
#'
#' This procedure is used inside the procedures;
#' move_bicycle()
#' move_bike()
#' move_car()
#' move_truck()
#' it indicate the vehicle coming from the east and it shows it current position.
#' this procedure is not accessible by the public
#' this procedure is automatically call once the simulation of a particular vehicle is lunched.
#'
#' @param i,x,y i is the timing of the overall simulation, x,y correspond
#' to the current position of the vehicle.
#'
#' @return No return value

from_east <- function(i, x, y){
  
  if(i>50){
    light_color_red_left(x,y)
  }else{
    light_color_green_left(x,y)
  }
  
  points(x[i], y[100],pch=10, col="red", cex=2)
  
}

#' Displacement of the vehicles from north
#'
#' This procedure is used inside the procedures;
#' move_bicycle()
#' move_bike()
#' move_car()
#' move_truck()
#' it indicate the vehicle coming from the north and it shows it current position.
#' this procedure is not accessible by the public
#' this procedure is automatically call once the simulation of a particular vehicle is lunched.
#'
#' @param i,x,y i is the timing of the overall simulation, x,y correspond
#' to the current position of the vehicle.
#'
#' @return No return value

from_north <- function(i, x, y){
  if(i>50){
    light_color_red_left(x,y)
  }else{
    light_color_green_left(x,y)
  }
  
  points(x[100], y[200-i],pch=10, col="red", cex=2)
  
}

#' Displacement of the vehicles from south
#'
#' This procedure is used inside the procedures;
#' move_bicycle()
#' move_bike()
#' move_car()
#' move_truck()
#' it indicate the vehicle coming from the south and it shows it current position.
#' this procedure is not accessible by the public
#' this procedure is automatically call once the simulation of a particular vehicle is lunched.
#'
#' @param i,x,y i is the timing of the overall simulation, x,y correspond
#' to the current position of the vehicle.
#'
#' @return No return value

from_south <- function(i, x, y){
  
  if(i>50){
    light_color_red_left(x,y)
  }else{
    light_color_green_left(x,y)
  }
  
  points(x[105], y[i],pch=10, col="red", cex=2)
  
}

#' Displacement of a bicycle
#'
#' This procedure shows the simulation of a bicycle coming from any of the four direction of the road
#' move_east()
#' move_north()
#' move_south()
#'
#' @param simulation_time,direction_from,direction_to
#' simulation_time is the duration of the simulation. The simulation time must be an integer ranging from 100 upward.
#' For a better simulation, use 200 for the timing.
#' direction_from is the initial position of the vehicle, the acceptable input are;
#' "EAST", "NORTH", "SOUTH"
#' direction_to is the final destination of the vehicle or the direction the vehicle will
#' take once it reaches the junction. The acceptable input are; "EAST", "NORTH", "SOUTH"
#'
#' @return No return value
#' @export
move_bicycle_Y <- function(simulation_time, direction_from, direction_to){
  
  #controlling input time to be numeric from 100 to 200
  if(!is.numeric(simulation_time)){
    stop("Error : Enter value from 100 to 200")
  }
  
  #controlling input direction left, right, up or down
  direction <- c("EAST", "NORTH", "SOUTH")
  
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
  
  
  if(direction_from == "EAST"){
    for(i in 1:simulation_time){
      
      roads_four_ways(x,y)
      if(direction_to == "EAST"){
        if(i<90){
          from_east(i,x,y)
        }else if(i>=84 && i<=140){
          light_color_red_left(x,y)
          
          points(x[85], y[100],pch=10, col="red", cex=2)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[90-i+141], y[105],pch=10, col="red", cex=2)
        }
        
      }
      
      if(direction_to == "NORTH"){
        
        if(i<90){
          from_east(i,x,y)
        }else if(i>=83 && i<=140){
          light_color_red_left(x,y)
          points(x[85], y[100],pch=10, col="red", cex=2)
        }else if(i>140){
          light_color_green_left(x,y)
          if(i-56<105){
            points(x[85+i-141], y[100],pch=10, col="red", cex=2)
          }else{
            points(x[105], y[i-61],pch=10, col="red", cex=2)
          }
        }
      }
      
      if(direction_to == "SOUTH"){
        
        if(i<90){
          from_east(i,x,y)
        }else if(i>=83 && i<=140){
          light_color_red_left(x,y)
          points(x[100], y[183-i],pch=10, col="red", cex=2)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[100], y[183-i],pch=10, col="red", cex=2)
        }
      }
      
      Sys.sleep(0.1)
      
    }
    print("simulation from left")
  }
  if(direction_from == "NORTH"){
    for(i in 1:simulation_time){
      
      roads_four_ways(x,y)
      
      
      if(direction_to == "EAST"){
        
        if(i<90){
          from_north(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          points(x[195-i], y[105],pch=10, col="red", cex=2)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[195-i], y[105 ],pch=10, col="red", cex=2)
        }
      }
      if(direction_to == "NORTH"){
        if(i<90){
          from_north(i,x,y)
        }else if(i>=91 && i<=140){
          light_color_red_left(x,y)
          points(x[105], y[i+10],pch=10, col="red", cex=2)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[105], y[i+10],pch=10, col="red", cex=2)
        }
      }
      if(direction_to == "SOUTH"){
        if(i<90){
          from_north(i,x,y)
        }else if(i>=91 && i<=140){
          light_color_red_left(x,y)
          points(x[100], y[190-i],pch=10, col="red", cex=2)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[100], y[190-i],pch=10, col="red", cex=2)
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
  }
  if(direction_from == "SOUTH"){
    for(i in 1:simulation_time){
      
      roads_four_ways(x,y)
      
      
      if(direction_to == "EAST"){
        
        if(i<90){
          from_south(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          if(i<=105){
            points(x[105], y[i],pch=10, col="red", cex=2)
          }else{
            points(x[211-i], y[106],pch=10, col="red", cex=2)
          }
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[211-i], y[106],pch=10, col="red", cex=2)
        }
      }
      if(direction_to == "NORTH"){
        if(i<94){
          from_south(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          points(x[105], y[i],pch=10, col="red", cex=2)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[105], y[i],pch=10, col="red", cex=2)
        }
      }
      if(direction_to == "SOUTH"){
        if(i<94){
          from_south(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          if(i<=100){
            points(x[105], y[i],pch=10, col="red", cex=2)
          }else{
            points(x[100], y[200-i],pch=10, col="red", cex=2)
          }
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[100], y[200-i],pch=10, col="red", cex=2)
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
    
  }
  
}

#' Displacement of a bike
#'
#' This procedure shows the simulation of a bike coming from any of the four direction of the road
#' move_east()
#' move_north()
#' move_south()
#'
#' @param simulation_time,direction_from,direction_to
#' simulation_time is the duration of the simulation. The simulation time must be an integer ranging from 100 upward.
#' For a better simulation, use 200 for the timing.
#' direction_from is the initial position of the vehicle, the acceptable input are;
#' "EAST", "NORTH", "SOUTH"
#' direction_to is the final destination of the vehicle or the direction the vehicle will
#' take once it reaches the junction. The acceptable input are; "EAST", "NORTH", "SOUTH"
#'
#' @return No return value
#' @export
move_bike_Y <- function(simulation_time, direction_from, direction_to){
  
  #controlling input time to be numeric from 100 to 200
  if(!is.numeric(simulation_time)){
    stop("Error : Enter value from 100 to 200")
  }
  
  #controlling input direction left, right, up or down
  direction <- c("EAST",  "NORTH", "SOUTH")
  
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
  
  
  if(direction_from == "EAST"){
    for(i in 1:simulation_time){
      
      roads_four_ways(x,y)
      if(direction_to == "EAST"){
        if(i<90){
          from_east(i,x,y)
        }else if(i>=84 && i<=140){
          light_color_red_left(x,y)
          points(x[85], y[100],pch=16, col="red", cex=2)
          Sys.sleep(0.3)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[90-i+141], y[105],pch=16, col="red", cex=2)
        }
        
      }
      
      
      
      if(direction_to == "NORTH"){
        
        if(i<90){
          from_east(i,x,y)
        }else if(i>=83 && i<=140){
          light_color_red_left(x,y)
          points(x[85], y[100],pch=16, col="red", cex=2)
          Sys.sleep(0.3)
        }else if(i>140){
          light_color_green_left(x,y)
          if(i-56<105){
            points(x[85+i-141], y[100],pch=16, col="red", cex=2)
          }else{
            points(x[105], y[i-61],pch=16, col="red", cex=2)
          }
        }
      }
      
      if(direction_to == "SOUTH"){
        
        if(i<90){
          from_east(i,x,y)
        }else if(i>=83 && i<=140){
          light_color_red_left(x,y)
          points(x[100], y[183-i],pch=16, col="red", cex=2)
          Sys.sleep(0.3)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[100], y[183-i],pch=16, col="red", cex=2)
        }
      }
      
      Sys.sleep(0.1)
      
    }
    print("simulation from left")
  }
  if(direction_from == "NORTH"){
    for(i in 1:simulation_time){
      
      roads_four_ways(x,y)
      
      
      if(direction_to == "EAST"){
        
        if(i<90){
          from_north(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          points(x[195-i], y[105],pch=16, col="red", cex=2)
          Sys.sleep(0.3)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[195-i], y[105 ],pch=16, col="red", cex=2)
        }
      }
      if(direction_to == "NORTH"){
        if(i<90){
          from_north(i,x,y)
        }else if(i>=91 && i<=140){
          light_color_red_left(x,y)
          points(x[105], y[i+10],pch=16, col="red", cex=2)
          Sys.sleep(0.3)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[105], y[i+10],pch=16, col="red", cex=2)
        }
      }
      if(direction_to == "SOUTH"){
        if(i<90){
          from_north(i,x,y)
        }else if(i>=91 && i<=140){
          light_color_red_left(x,y)
          points(x[100], y[190-i],pch=16, col="red", cex=2)
          Sys.sleep(0.3)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[100], y[190-i],pch=16, col="red", cex=2)
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
  }
  if(direction_from == "SOUTH"){
    for(i in 1:simulation_time){
      
      roads_four_ways(x,y)
      
      
      if(direction_to == "EAST"){
        
        if(i<90){
          from_south(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          if(i<=105){
            points(x[105], y[i],pch=16, col="red", cex=2)
          }else{
            points(x[211-i], y[106],pch=16, col="red", cex=2)
          }
          Sys.sleep(0.3)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[211-i], y[106],pch=16, col="red", cex=2)
        }
      }
      if(direction_to == "NORTH"){
        if(i<94){
          from_south(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          points(x[105], y[i],pch=16, col="red", cex=2)
          Sys.sleep(0.3)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[105], y[i],pch=16, col="red", cex=2)
        }
      }
      if(direction_to == "SOUTH"){
        if(i<94){
          from_south(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          if(i<=100){
            points(x[105], y[i],pch=16, col="red", cex=2)
          }else{
            points(x[100], y[200-i],pch=16, col="red", cex=2)
          }
          Sys.sleep(0.3)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[100], y[200-i],pch=16, col="red", cex=2)
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
    
  }
  
}

#' Displacement of a car
#'
#' This procedure shows the simulation of a car coming from any of the four direction of the road
#' move_east()
#' move_north()
#' move_south()
#'
#' @param simulation_time,direction_from,direction_to
#' simulation_time is the duration of the simulation. The simulation time must be an integer ranging from 100 upward.
#' For a better simulation, use 200 for the timing.
#' direction_from is the initial position of the vehicle, the acceptable input are;
#' "EAST",  "NORTH", "SOUTH"
#' direction_to is the final destination of the vehicle or the direction the vehicle will
#' take once it reaches the junction. The acceptable input are; "EAST", "NORTH", "SOUTH"
#'
#' @return No return value
#' @export
move_car_Y <- function(simulation_time, direction_from, direction_to){
  
  #controlling input time to be numeric from 100 to 200
  if(!is.numeric(simulation_time)){
    stop("Error : Enter value from 100 to 200")
  }
  
  #controlling input direction left, right, up or down
  direction <- c("EAST", "NORTH", "SOUTH")
  
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
  
  
  if(direction_from == "EAST"){
    for(i in 1:simulation_time){
      
      roads_four_ways(x,y)
      if(direction_to == "EAST"){
        if(i<90){
          from_east(i,x,y)
        }else if(i>=84 && i<=140){
          light_color_red_left(x,y)
          points(x[85], y[100],pch=18, col="red", cex=2)
          Sys.sleep(0.6)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[90-i+141], y[105],pch=18, col="red", cex=2)
        }
        
      }
      
      
      if(direction_to == "NORTH"){
        
        if(i<90){
          from_east(i,x,y)
        }else if(i>=83 && i<=140){
          light_color_red_left(x,y)
          points(x[85], y[100],pch=18, col="red", cex=2)
          Sys.sleep(0.6)
        }else if(i>140){
          light_color_green_left(x,y)
          if(i-56<105){
            points(x[85+i-141], y[100],pch=18, col="red", cex=2)
          }else{
            points(x[105], y[i-61],pch=18, col="red", cex=2)
          }
        }
      }
      
      if(direction_to == "SOUTH"){
        
        if(i<90){
          from_east(i,x,y)
        }else if(i>=83 && i<=140){
          light_color_red_left(x,y)
          points(x[100], y[183-i],pch=18, col="red", cex=2)
          Sys.sleep(0.6)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[100], y[183-i],pch=18, col="red", cex=2)
        }
      }
      
      Sys.sleep(0.1)
      
    }
    print("simulation from left")
  }
  if(direction_from == "NORTH"){
    for(i in 1:simulation_time){
      
      roads_four_ways(x,y)
      
      
      if(direction_to == "EAST"){
        
        if(i<90){
          from_north(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          points(x[195-i], y[105],pch=18, col="red", cex=2)
          Sys.sleep(0.6)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[195-i], y[105 ],pch=18, col="red", cex=2)
        }
      }
      if(direction_to == "NORTH"){
        if(i<90){
          from_north(i,x,y)
        }else if(i>=91 && i<=140){
          light_color_red_left(x,y)
          points(x[105], y[i+10],pch=18, col="red", cex=2)
          Sys.sleep(0.6)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[105], y[i+10],pch=18, col="red", cex=2)
        }
      }
      if(direction_to == "SOUTH"){
        if(i<90){
          from_north(i,x,y)
        }else if(i>=91 && i<=140){
          light_color_red_left(x,y)
          points(x[100], y[190-i],pch=18, col="red", cex=2)
          Sys.sleep(0.6)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[100], y[190-i],pch=18, col="red", cex=2)
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
  }
  if(direction_from == "SOUTH"){
    for(i in 1:simulation_time){
      
      roads_four_ways(x,y)
      
      
      if(direction_to == "EAST"){
        
        if(i<90){
          from_south(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          if(i<=105){
            points(x[105], y[i],pch=18, col="red", cex=2)
          }else{
            points(x[211-i], y[106],pch=18, col="red", cex=2)
          }
          Sys.sleep(0.6)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[211-i], y[106],pch=18, col="red", cex=2)
        }
      }
      if(direction_to == "NORTH"){
        if(i<94){
          from_south(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          points(x[105], y[i],pch=18, col="red", cex=2)
          Sys.sleep(0.6)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[105], y[i],pch=18, col="red", cex=2)
        }
      }
      if(direction_to == "SOUTH"){
        if(i<94){
          from_south(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          if(i<=100){
            points(x[105], y[i],pch=18, col="red", cex=2)
          }else{
            points(x[100], y[200-i],pch=18, col="red", cex=2)
          }
          Sys.sleep(0.6)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[100], y[200-i],pch=18, col="red", cex=2)
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
    
  }
  
}

#' Displacement of a truck
#'
#' This procedure shows the simulation of a truck coming from any of the four direction of the road
#' move_east()
#' move_north()
#' move_south()
#'
#' @param simulation_time,direction_from,direction_to
#' simulation_time is the duration of the simulation. The simulation time must be an integer ranging from 100 upward.
#' For a better simulation, use 200 for the timing.
#' direction_from is the initial position of the vehicle, the acceptable input are;
#' "EAST",  "NORTH", "SOUTH"
#' direction_to is the final destination of the vehicle or the direction the vehicle will
#' take once it reaches the junction. The acceptable input are; "EAST",  "NORTH", "SOUTH"
#'
#' @return No return value
#' @export
move_truck_Y <- function(simulation_time, direction_from, direction_to){
  
  #controlling input time to be numeric from 100 to 200
  if(!is.numeric(simulation_time)){
    stop("Error : Enter value from 100 to 200")
  }
  
  #controlling input direction left, right, up or down
  direction <- c("EAST", "NORTH", "SOUTH")
  
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
  
  
  if(direction_from == "EAST"){
    for(i in 1:simulation_time){
      
      roads_four_ways(x,y)
      if(direction_to == "EAST"){
        if(i<90){
          from_east(i,x,y)
        }else if(i>=84 && i<=140){
          light_color_red_left(x,y)
          points(x[85], y[100],pch=15, col="red", cex=2)
          Sys.sleep(1)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[90-i+141], y[105],pch=15, col="red", cex=2)
        }
        
      }
      
      
      if(direction_to == "NORTH"){
        
        if(i<90){
          from_east(i,x,y)
        }else if(i>=83 && i<=140){
          light_color_red_left(x,y)
          points(x[85], y[100],pch=15, col="red", cex=2)
          Sys.sleep(1)
        }else if(i>140){
          light_color_green_left(x,y)
          if(i-56<105){
            points(x[85+i-141], y[100],pch=15, col="red", cex=2)
          }else{
            points(x[105], y[i-61],pch=15, col="red", cex=2)
          }
        }
      }
      
      if(direction_to == "SOUTH"){
        
        if(i<90){
          from_east(i,x,y)
        }else if(i>=83 && i<=140){
          light_color_red_left(x,y)
          points(x[100], y[183-i],pch=15, col="red", cex=2)
          Sys.sleep(1)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[100], y[183-i],pch=15, col="red", cex=2)
        }
      }
      
      Sys.sleep(0.1)
      
    }
    print("simulation from left")
  }
  if(direction_from == "NORTH"){
    for(i in 1:simulation_time){
      
      roads_four_ways(x,y)
      
      
      if(direction_to == "EAST"){
        
        if(i<90){
          from_north(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          points(x[195-i], y[105],pch=15, col="red", cex=2)
          Sys.sleep(1)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[195-i], y[105 ],pch=15, col="red", cex=2)
        }
      }
      if(direction_to == "NORTH"){
        if(i<90){
          from_north(i,x,y)
        }else if(i>=91 && i<=140){
          light_color_red_left(x,y)
          points(x[105], y[i+10],pch=15, col="red", cex=2)
          Sys.sleep(1)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[105], y[i+10],pch=15, col="red", cex=2)
        }
      }
      if(direction_to == "SOUTH"){
        if(i<90){
          from_north(i,x,y)
        }else if(i>=91 && i<=140){
          light_color_red_left(x,y)
          points(x[100], y[190-i],pch=15, col="red", cex=2)
          Sys.sleep(1)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[100], y[190-i],pch=15, col="red", cex=2)
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
  }
  if(direction_from == "SOUTH"){
    for(i in 1:simulation_time){
      
      roads_four_ways(x,y)
      
      
      if(direction_to == "EAST"){
        
        if(i<90){
          from_south(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          if(i<=105){
            points(x[105], y[i],pch=15, col="red", cex=2)
          }else{
            points(x[211-i], y[106],pch=15, col="red", cex=2)
          }
          Sys.sleep(1)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[211-i], y[106],pch=15, col="red", cex=2)
        }
      }
      if(direction_to == "NORTH"){
        if(i<94){
          from_south(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          points(x[105], y[i],pch=15, col="red", cex=2)
          Sys.sleep(1)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[105], y[i],pch=15, col="red", cex=2)
        }
      }
      if(direction_to == "SOUTH"){
        if(i<94){
          from_north(i,x,y)
        }else if(i>=95 && i<=140){
          light_color_red_left(x,y)
          if(i<=100){
            points(x[105], y[i],pch=15, col="red", cex=2)
          }else{
            points(x[100], y[200-i],pch=15, col="red", cex=2)
          }
          Sys.sleep(1)
        }else if(i>140){
          light_color_green_left(x,y)
          points(x[100], y[200-i],pch=15, col="red", cex=2)
        }
      }
      
      
      
      Sys.sleep(0.1)
      
    }
    print("simulation from up")
    
  }
  
}
