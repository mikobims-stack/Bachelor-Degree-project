# assigning the interval for our different road
x <- -100:100
y <- x

#constructing the data type
type = c("bicycle", "bike", "car", "truck")
destination <- c("left", "right", "straight", "return")
origine <- c("east", "west", "north", "south")
road_axes <- c("east", "west", "north", "south")
Initial_position <- c("100","200","300","400","500","600","700","800","900",
  "1000", "1100","1200", "1300", "1400","1500","1600","1700","1800","1900", "2000")

vehicle <- data.frame(Vehicle_Type = character(),
  Vehicle_Origine = character(),
  vehicle_initial = integer(),
  vehicle_id = character(),
  Vehicle_Destination = character(),
  vehicle_speed = integer(),
  vehicle_priority = logical(),
  vehicle_special_priority = logical()
)

#' Constructing points on the EAST, WEST, NORTH and SOUTH road and the ROUNDABOUT.
#'
#' This portion of the code creates a list of displacement point starting from east, west, north and south.
#' this procedure is automatically call once the simulation of a particular vehicle is lunched.
#'
#' @param  it uses the x and y coordinates from east to west (x) and from north to south(y).
#' @return No return value because it not a function nor procedure
#' @export

  #constructing table that contains displacement points of road EAST_1 and EAST_2
  Default_point_on_road_east_1 <- data.frame(X_coordinate = integer(),Y_coordinate = integer(),ID = character(),Index = integer(),Occupied = logical())
  Default_point_on_road_east_2 <- data.frame(X_coordinate = integer(),Y_coordinate = integer(),ID = character(),Index = integer(),Occupied = logical())

  #constructing table that contains displacement points of road WEST_1 and WEST_2
  Default_point_on_road_west_1 <- data.frame(X_coordinate = integer(),Y_coordinate = integer(),ID = character(), Index = integer(),Occupied = logical())
  Default_point_on_road_west_2 <- data.frame(X_coordinate = integer(),Y_coordinate = integer(),ID = character(), Index = integer(),Occupied = logical())

  #constructing table that contains displacement points of road NORTH_1 and NORTH_2
  Default_point_on_road_north_1 <- data.frame(X_coordinate = integer(),Y_coordinate = integer(),ID = character(),Index = integer(),Occupied = logical())
  Default_point_on_road_north_2 <- data.frame(X_coordinate = integer(),Y_coordinate = integer(),ID = character(),Index = integer(),Occupied = logical())

  #constructing table that contains displacement points of road SOUTH_1 and SOUTH_2
  Default_point_on_road_south_1 <- data.frame(X_coordinate = integer(),Y_coordinate = integer(),ID = character(),Index = integer(),Occupied = logical())
  Default_point_on_road_south_2 <- data.frame(X_coordinate = integer(),Y_coordinate = integer(),ID = character(),Index = integer(),Occupied = logical())

  #constructing table that contains displacement points of the ROUNDABOUT
  Default_point_on_roundabout <- data.frame(X_coordinate = integer(),Y_coordinate = integer(),ID = character(),Index = integer(),Occupied = logical())


  #assigning displacement point to the table of road EAST_1 and EAST_2
  x_point_1 <- -100
  x_point_2 <- -10

  for (i in 1:19) {
    Default_point_on_road_east_1 = rbind(
      Default_point_on_road_east_1,
      data.frame(
        X_coordinate = x_point_1,
        Y_coordinate = -3.5,
        ID = paste0("E1_", i),
        Index = i,
        Occupied = FALSE
      )
    )
    x_point_1 <- x_point_1 + 5

    Default_point_on_road_east_2 = rbind(
      Default_point_on_road_east_2,
      data.frame(
        X_coordinate = x_point_2,
        Y_coordinate = 3.5,
        ID = paste0("E2_", i),
        Index = i,
        Occupied = FALSE
      )
    )
    x_point_2 <- x_point_2 - 5
  }

  #assigning displacement point to the table of road WEST_1 and WEST_2
  x_point_3 <- 100
  x_point_4 <- 10

  for (i in 1:19) {
    Default_point_on_road_west_1 = rbind(
      Default_point_on_road_west_1,
      data.frame(
        X_coordinate = x_point_3,
        Y_coordinate = 3.5,
        ID = paste0("W1_", i),
        Index = i,
        Occupied = FALSE
      )
    )
    x_point_3 <- x_point_3 - 5

    Default_point_on_road_west_2 = rbind(
      Default_point_on_road_west_2,
      data.frame(
        X_coordinate = x_point_4,
        Y_coordinate = -3.5,
        ID = paste0("W2_", i),
        Index = i,
        Occupied = FALSE
      )
    )
    x_point_4 <- x_point_4 + 5
  }

  #assigning displacement point to the table of road NORTH_1 and NORTH_2
  y_point_1 <- 100
  y_point_2 <- 10

  for (i in 1:19) {
    Default_point_on_road_north_1 = rbind(
      Default_point_on_road_north_1,
      data.frame(
        X_coordinate = -4.5,
        Y_coordinate = y_point_1,
        ID = paste0("N1_", i),
        Index = i,
        Occupied = FALSE
      )
    )
    y_point_1 <- y_point_1 - 5

    Default_point_on_road_north_2 = rbind(
      Default_point_on_road_north_2,
      data.frame(
        X_coordinate = 2.5,
        Y_coordinate = y_point_2,
        ID = paste0("N2_", i),
        Index = i,
        Occupied = FALSE
      )
    )
    y_point_2 <- y_point_2 + 5
  }


  #assigning displacement point to the table of road SOUTH_1 and SOUTH_2
  y_point_3 <- -100
  y_point_4 <- -10

  for (i in 1:19) {
    Default_point_on_road_south_1 = rbind(
      Default_point_on_road_south_1,
      data.frame(
        X_coordinate = -1.5,
        Y_coordinate = y_point_3,
        ID = paste0("S1_", i),
        Index = i,
        Occupied = FALSE
      )
    )
    y_point_3 <- y_point_3 + 5

    Default_point_on_road_south_2 = rbind(
      Default_point_on_road_south_2,
      data.frame(
        X_coordinate = -4.5,
        Y_coordinate = y_point_4,
        ID = paste0("S2_", i),
        Index = i,
        Occupied = FALSE
      )
    )
    y_point_4 <- y_point_4 - 5
  }

  #assigning displacement point to the table of ROUNDABOUT
  Default_point_on_roundabout = rbind(
    Default_point_on_roundabout,
    data.frame(
      X_coordinate = -2.5,
      Y_coordinate = -2.5,
      ID = "R11",
      Index = 1,
      Occupied = FALSE
    )
  )
  Default_point_on_roundabout = rbind(
    Default_point_on_roundabout,
    data.frame(
      X_coordinate = 2.5,
      Y_coordinate = -2.5,
      ID = "R12",
      Index = 2,
      Occupied = FALSE
    )
  )
  Default_point_on_roundabout = rbind(
    Default_point_on_roundabout,
    data.frame(
      X_coordinate = 2.5,
      Y_coordinate = 2.5,
      ID = "R13",
      Index = 3,
      Occupied = FALSE
    )
  )
  Default_point_on_roundabout = rbind(
    Default_point_on_roundabout,
    data.frame(
      X_coordinate = -2.5,
      Y_coordinate = 2.5,
      ID = "R14",
      Index = 4,
      Occupied = FALSE
    )
  )
  Default_point_on_roundabout = rbind(
    Default_point_on_roundabout,
    data.frame(
      X_coordinate = 0,
      Y_coordinate = 0,
      ID = "R15",
      Index = 5,
      Occupied = FALSE
    )
  )

  #Print the values of different tables
  print(Default_point_on_road_east_1)
  print(Default_point_on_road_east_2)
  print(Default_point_on_road_west_1)
  print(Default_point_on_road_west_2)
  print(Default_point_on_road_north_1)
  print(Default_point_on_road_north_2)
  print(Default_point_on_road_south_1)
  print(Default_point_on_road_south_2)
  print(Default_point_on_roundabout)

  save(
    Default_point_on_road_east_1,
    Default_point_on_road_east_2,
    Default_point_on_road_north_1,
    Default_point_on_road_north_2,
    Default_point_on_road_south_1,
    Default_point_on_road_south_2,
    Default_point_on_road_west_1,
    Default_point_on_road_west_2,
    Default_point_on_roundabout,
    file = "TABLE-POINT.RData"
  )


#' procedure for Constructing the four way road on road
#'
#' This procedure creates the traffic light  on the 4 way direction road and it is in charge of the
#' green light.
#' This procedure is use in other procedure and is not accessible by the public
#'
#'
#' This procedure is automatically call once the simulation of a particular vehicle is lunched.
#' @param x,y the dimension or size of the roads from east to west (x) and from north to south(y).
#' @return No return value
#' @export
roads_four_ways <- function(x, y) {
  #road from left to right and vice-versa
  plot(
    c(-100, 100),
    c(-5, -5),
    type = "l",
    col = "black",
    lwd = 20,
    axes = FALSE,
    ylim = c(-100, 100),
    xlim = c(-100, 100)
  )
  lines(c(-100, 100), c(5, 5), col = "yellow", lwd = 20)

  #road from up to down and vice versa
  lines(c(-5, -5),
        c(-100, 100),
        col = "yellow",
        lwd = 20)
  lines(c(0, 0), c(-100, 100), col = "black", lwd = 20)

  #drawing the main junction
  points(x[100],
         y[100],
         pch = 20,
         col = "white",
         cex = 10)
}

#' Function for parameters
#' This function take in an empty table and ask the user for a certaine list of parameters.
#' The parameters a filled into the table.
#'
#' @param vehicle A data frame to be filled
#' @return  A data frame containing the enter parameters by the user
#' @export
Enter_parameter <- function(vehicle) {
  #constructing table to collect information for each vehicle

  # collect data for the number of vehicles on each way
  for (i in 1:4) {
    cat("For Road at the ", origine[i])
    repeat
    {
      number_v =  as.numeric(readline(prompt = "Enter the number of vehicles \n"))
      if(!is.na(number_v) && number_v<20 && number_v >= 0){
        break
      }
      cat("\n Number of vehicles must be between 1 and 20 \n")
    }
    if(number_v !=0){
      for (j in 1:number_v) {
        print("\n vehicle ", i)
        Type <- type[menu(type, title = "\n Select the type of vehicle")]
        Origine <- origine[i]
        Id <- readline(prompt = "Enter the vehicles Matricule \n")
        Destination <- destination[menu(destination, title = "Select the destination of the vehicle")]
        Initial <- menu(Initial_position, title = "Select initial position from junction (in hundred meter)")
        Speed <- readline(prompt = "Enter the vehicles speed from 1 to 10 (in tens kilometer)  \n")
        Priority <- FALSE
        Special_priority <- FALSE

        vehicle <- rbind(
          vehicle,
          data.frame(
            Vehicle_Type = Type,
            Vehicle_Origine = Origine,
            vehicle_id = Id,
            Vehicle_Destination = Destination,
            vehicle_speed = Speed,
            vehicle_initial = Initial,
            vehicle_priority = Priority,
            vehicle_special_priority = Special_priority
          )
        )
      }
    }
  }
  return(vehicle)
}

#' Verification of Matricule
#' This function checks if two vehicle have the same identification number.
#' In case two vehicles have the same identification number, the user is ask to change the identification number.
#'
#' @param vehicle A data frame containing a list of parameters entered by the user.
#' @return  A data frame containing the entered parameters by the user
#' @export
check_Id <- function(vehicle) {
  if(nrow(vehicle)>1){
  for (i in 1:(nrow(vehicle) - 1)) {
    for (j in (i + 1):nrow(vehicle)) {
      if (vehicle$vehicle_id[i] == vehicle$vehicle_id[j]) {
        cat("Vehicles have same matricule, enter new matricules for vehicles")
        vehicle$vehicle_id[i] <- readline(prompt = "Enter Matricule of vehicle.... \n")
        vehicle$vehicle_id[j] <- readline(prompt = "Enter Matricule for vehicle.... \n")
        cat("\n Run the check_id() function again....\n")
      }
    }
  }
  }
  return(vehicle)
}

#' Verification of Vehicle Initial position
#' This function checks if two vehicle have the same Initial Position.
#' In case two vehicles have the same Initial Position, the user is ask to change the Initial position.
#'
#' @param vehicle A data frame containing a list of parameters entered by the user.
#' @return  A data frame containing the entered parameters by the user
#' @export
check_Initial <- function(vehicle) {
  if(nrow(vehicle)>1){
  for (i in 1:(nrow(vehicle) - 1)) {
    for (j in (i + 1):nrow(vehicle)) {
      if (vehicle$Vehicle_Origine[i] == vehicle$Vehicle_Origine[j]) {
        if (vehicle$vehicle_initial[i] == vehicle$vehicle_initial[j]) {
          cat(
            "Vehicles are at same position, enter new position for ",
            vehicle$vehicle_id[i],
            " and ",
            vehicle$vehicle_id[j]
          )
          cat("\n Update initial position for ",
              vehicle$vehicle_initial[i])
          vehicle$vehicle_initial[i] <- menu(Initial_position, title = "\n Select initial position from junction (in hundred meter)")
          cat("\n Update initial position for ",
              vehicle$vehicle_initial[j])
          vehicle$vehicle_initial[j] <- menu(Initial_position, title = "\n Select initial position from junction (in hundred meter)")
          cat("\n Run the check_speed() function again")
        }
      }

    }
  }
  }
  return(vehicle)
}

#' Display a vehicle
#' This method Display a vehicle at it initial position on the Display four road side.
#' The shape of each vehicle is display and each vehicle is represented by a particular shape.
#'
#' @param vehicle A data frame containing a list of parameters entered by the user.
#' @return  No value, produces a display of each component at rest.
#' @export
Display_at_initial <- function(vehicle, point_east, point_west, point_north, point_south){
  for (i in 1:nrow(vehicle)){
    j <- vehicle$vehicle_initial[i]
    if(vehicle$Vehicle_Type[i] == "bicycle"){
      shape <- 10
    }else if(vehicle$Vehicle_Type[i] == "bike"){
      shape <- 16
    }else if(vehicle$Vehicle_Type[i]=="car"){
      shape <- 18
    }else if(vehicle$Vehicle_Type[i]=="truck"){
      shape <- 15
    }

    if(vehicle$Vehicle_Origine[i] == "east"){
      points(point_east$X_coordinate[j], point_east$Y_coordinate[j],pch=shape, col="red", cex=2)
      point_east$Occupied[j] <- TRUE

    }
    if(vehicle$Vehicle_Origine[i] == "west"){
      points(point_west$X_coordinate[j], point_west$Y_coordinate[j],pch=shape, col="blue", cex=2)
      point_west$Occupied[j] <- TRUE
    }
    if(vehicle$Vehicle_Origine[i] == "north"){
      points(point_north$X_coordinate[j], point_north$Y_coordinate[j],pch=shape, col="magenta", cex=2)
      point_north$Occupied[j] <- TRUE
    }
    if(vehicle$Vehicle_Origine[i] == "south"){
      points(point_south$X_coordinate[j], point_south$Y_coordinate[j],pch=shape, col="cyan", cex=1.5)
      point_south$Occupied[j] <- TRUE
    }
  }
  return(list(point_east =  point_east, point_west =  point_west,point_north =  point_north,point_south =  point_south))
}

#' Simulate vehicle movement
#' This method shows how vehicles will move from it initial position to it final position according to the parameter given by the user.
#' The shape of each vehicle is display and each vehicle is represented by a particular shape.
#'
#' @param vehicle A data frame containing a list of parameters entered by the user.
#' @return  No value, shows a simulation of the given parameter.
#' @export

Simulation_X <- function(x,y,vehicle,point_E1, point_E2, point_W1, point_W2, point_N1, point_N2, point_S1, point_S2, point_R){
  repeat{
    simulation_time =  as.numeric(readline(prompt = "Enter the time of simulation between 100 and 200 units \n"))
    if(simulation_time>=100 && simulation_time <= 200 && !is.na(simulation_time)){
      break
    }
    cat("\n Simulation time must be between 100 and 200")
  }
  path1 <- data.frame(value = logical(), value_left = logical()) #variables that controls different patern
  path2 <- data.frame(value = logical(), value_left = logical())
  path3 <- data.frame(value = logical(), value_left = logical())
 for (i in 1:4) {
  path1 <- rbind(path1, data.frame(value = FALSE, value_left = FALSE))
  path2 <- rbind(path2, data.frame(value = FALSE, value_left = FALSE))
  path3 <- rbind(path3, data.frame(value = FALSE, value_left = FALSE))
 }
  for (j in 1:simulation_time) {
    roads_four_ways(x,y)
    for (i in 1:nrow(vehicle)) {
      if(vehicle$Vehicle_Type[i] == "bicycle"){
        shape <- 10
      }else if(vehicle$Vehicle_Type[i] == "bike"){
        shape <- 16
      }else if(vehicle$Vehicle_Type[i]=="car"){
        shape <- 18
      }else if(vehicle$Vehicle_Type[i]=="truck"){
        shape <- 15
      }
      if (vehicle$Vehicle_Origine[i] == "east") {

        if (vehicle$vehicle_initial[i]+1 < 20) {
          #run vehicles before roundabout
          z <- vehicle$vehicle_initial[i]

          if (!point_E1$Occupied[z+1]) {
              points(point_E1$X_coordinate[z + 1],point_E1$Y_coordinate[z + 1],pch = shape,col = "red",cex = 1.5)
              point_E1$Occupied[z] <- FALSE
              point_E1$Occupied[z+1] <- TRUE
              vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i]+1
          } else{
            points(point_E1$X_coordinate[z],point_E1$Y_coordinate[z],pch = shape,col = "red",cex = 1.5)
            point_E1$Occupied[z] <- TRUE
          }
        } else {

          # vehicle that returns
          if(vehicle$Vehicle_Destination[i]== "return"){
           if(vehicle$vehicle_initial[i]==19){
             z <- vehicle$vehicle_initial[i]
             if(!point_R$Occupied[1]){
               points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "red",cex = 1.5)
               point_R$Occupied[1] <- TRUE
               point_E1$Occupied[z] <- FALSE
               vehicle$vehicle_initial[i] <- 20 #indicate if vehicles reaches the 1st point of junction
             }else {
               points(point_E1$X_coordinate[z],point_E1$Y_coordinate[z],pch = shape,col = "red",cex = 1.5)
               point_E1$Occupied[z] <- TRUE
             }
           } else if(vehicle$vehicle_initial[i]==20){
             z <- vehicle$vehicle_initial[i]
             if(!point_R$Occupied[5]){ #after 1st point of junction check middle point 5 if free
               points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "red",cex = 1.5)
               point_R$Occupied[5] <- TRUE
               point_R$Occupied[1] <- FALSE
               vehicle$vehicle_initial[i] <- 21 # indicates that vehicles has reache middle point
             }else if(!point_R$Occupied[2]){ #else check if point 2 is free
               points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "red",cex = 1.5)
               point_R$Occupied[2] <- TRUE
               point_R$Occupied[1] <- FALSE
               path1$value[1] <- TRUE
               vehicle$vehicle_initial[i] <- 21
             }else if(!point_R$Occupied[4]){
               points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "red",cex = 1.5)
               point_R$Occupied[4] <- TRUE
               point_R$Occupied[1] <- FALSE
               vehicle$vehicle_initial[i] <- 22 #if vehicle move to point 4
             }else{
               points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "red",cex = 1.5)
               point_R$Occupied[1] <- TRUE
             }
           }else if(vehicle$vehicle_initial[i]==21 || path1$value[1] == TRUE){
             z <- vehicle$vehicle_initial[i]
             if(path1$value[1] == FALSE){ #if vehicle move to middle then execute this
             if(!point_R$Occupied[4]){
               points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "red",cex = 1.5)
               point_R$Occupied[4] <- TRUE
               point_R$Occupied[5] <- FALSE
               vehicle$vehicle_initial[i] <- 22 #if vehicle move to point 4
             }else {
               points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "red",cex = 1.5)
               point_R$Occupied[5] <- TRUE
             }
             }else if(path1$value[1] == TRUE){ # else if vehicle move to point 2 execute this
               if(!point_R$Occupied[5]){
                 points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "red",cex = 1.5)
                 point_R$Occupied[5] <- TRUE
                 point_R$Occupied[2] <- FALSE

                 vehicle$vehicle_initial[i] <- 21
                 path1$value[1] <- FALSE
               }else {
                 points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "red",cex = 1.5)
                 point_R$Occupied[2] <- TRUE
               }
             }

           }else
             if(vehicle$vehicle_initial[i]==22){
             #procedure that displace a vehicle on E2

             if(!point_E2$Occupied[1]){
               points(point_E2$X_coordinate[1],point_E2$Y_coordinate[1],pch = shape,col = "red",cex = 1.5)
               point_R$Occupied[4] <- FALSE
               point_E2$Occupied[1] <- TRUE
               vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
             }else{
               points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "red",cex = 1.5)
               point_R$Occupied[4] <- TRUE
             }
           }else if(vehicle$vehicle_initial[i]>22 && vehicle$vehicle_initial[i]<41 ){
             if(!point_E2$Occupied[vehicle$vehicle_initial[i]-21]){
               points(point_E2$X_coordinate[vehicle$vehicle_initial[i]-21],point_E2$Y_coordinate[vehicle$vehicle_initial[i]-21],pch = shape,col = "red",cex = 1.5)
               point_E2$Occupied[vehicle$vehicle_initial[i]-21-1] <- FALSE
               point_E2$Occupied[vehicle$vehicle_initial[i]-21] <- TRUE
               vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
             }else{
               points(point_E2$X_coordinate[vehicle$vehicle_initial[i]-21-1],point_E2$Y_coordinate[vehicle$vehicle_initial[i]-21-1],pch = shape,col = "red",cex = 1.5)
               point_E2$Occupied[vehicle$vehicle_initial[i]-21-1] <- TRUE
             }
           }
          }else
            if(vehicle$Vehicle_Destination[i] == "straight"){  # ################################# continue check from here #################################################

              if(vehicle$vehicle_initial[i]==19){
                z <- vehicle$vehicle_initial[i]
                if(!point_R$Occupied[1]){
                  points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "red",cex = 1.5)
                  point_R$Occupied[1] <- TRUE
                  point_E1$Occupied[z] <- FALSE
                  vehicle$vehicle_initial[i] <- 20
                }else {
                  points(point_E1$X_coordinate[z],point_E1$Y_coordinate[z],pch = shape,col = "red",cex = 1.5)
                  point_E1$Occupied[z] <- TRUE
                }
              } else if(vehicle$vehicle_initial[i]==20){
                z <- vehicle$vehicle_initial[i]
                if(!point_R$Occupied[2]){
                  points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "red",cex = 1.5)
                  point_R$Occupied[2] <- TRUE
                  point_R$Occupied[1] <- FALSE
                  vehicle$vehicle_initial[i] <- 21
                }else if(!point_R$Occupied[5]){
                  points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "red",cex = 1.5)
                  path2$value[1] <- TRUE
                  point_R$Occupied[5] <- TRUE
                  point_R$Occupied[1] <- FALSE
                }else{
                  points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "red",cex = 1.5)
                  point_R$Occupied[1] <- TRUE
                }
              }else if(vehicle$vehicle_initial[i]==21 || path2$value[1] == TRUE){
                  #procedure that displace a vehicle on w2
                  if(path2$value[1] == FALSE){
                   if(!point_W2$Occupied[1]){
                    points(point_W2$X_coordinate[1],point_W2$Y_coordinate[1],pch = shape,col = "red",cex = 1.5)
                    point_R$Occupied[2] <- FALSE
                    point_W2$Occupied[1] <- TRUE
                    vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                  }else{
                    points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "red",cex = 1.5)
                    point_R$Occupied[2] <- TRUE
                  }
                  }else if(path2$value[1] == TRUE){
                    if(!point_R$Occupied[2]){
                      points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "red",cex = 1.5)
                      path2$value[1] <- FALSE
                      point_R$Occupied[2] <- TRUE
                      point_R$Occupied[5] <- FALSE
                      vehicle$vehicle_initial[i] <- 21
                    }
                }
                }else if(vehicle$vehicle_initial[i]>21 && vehicle$vehicle_initial[i]<40 ){
                  if(!point_W2$Occupied[vehicle$vehicle_initial[i]-20]){
                    points(point_W2$X_coordinate[vehicle$vehicle_initial[i]-20],point_W2$Y_coordinate[vehicle$vehicle_initial[i]-20],pch = shape,col = "red",cex = 1.5)
                    point_W2$Occupied[vehicle$vehicle_initial[i]-20-1] <- FALSE
                    point_E2$Occupied[vehicle$vehicle_initial[i]-20] <- TRUE
                    vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                  }else{
                    points(point_W2$X_coordinate[vehicle$vehicle_initial[i]-20-1],point_W2$Y_coordinate[vehicle$vehicle_initial[i]-20-1],pch = shape,col = "red",cex = 1.5)
                    point_W2$Occupied[vehicle$vehicle_initial[i]-20-1] <- TRUE
                  }
                }

            }else
              if(vehicle$Vehicle_Destination[i] == "left"){

                if(vehicle$vehicle_initial[i]==19){
                  z <- vehicle$vehicle_initial[i]
                  if(!point_R$Occupied[1]){
                    points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "red",cex = 1.5)
                    point_R$Occupied[1] <- TRUE
                    point_E1$Occupied[z] <- FALSE
                    vehicle$vehicle_initial[i] <- 20
                  }else {
                    points(point_E1$X_coordinate[z],point_E1$Y_coordinate[z],pch = shape,col = "red",cex = 1.5)
                    point_E1$Occupied[z] <- TRUE
                  }
                } else if(vehicle$vehicle_initial[i]==20){
                  if(!point_R$Occupied[5]){
                    points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "red",cex = 1.5)
                    point_R$Occupied[5] <- TRUE
                    point_R$Occupied[1] <- FALSE
                    path1$value_left[1] <- TRUE
                    vehicle$vehicle_initial[i] <- 21
                  }else if(!point_R$Occupied[4]){
                    points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "red",cex = 1.5)
                    point_R$Occupied[4] <- TRUE
                    point_R$Occupied[1] <- FALSE
                    vehicle$vehicle_initial[i] <- 21
                    path2$value_left[1]<-TRUE
                  }else if(!point_R$Occupied[2]){
                    points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "red",cex = 1.5)
                    point_R$Occupied[2] <- TRUE
                    point_R$Occupied[1] <- FALSE
                    path3$value_left[1] <- TRUE
                    vehicle$vehicle_initial[i] <- 21
                  }else {
                    points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "red",cex = 1.5)
                    point_R$Occupied[1] <- TRUE
                  }
                }else if(vehicle$vehicle_initial[i]==21){
                  z <- vehicle$vehicle_initial[i]
                    if(path1$value_left[1]){
                      if(!point_R$Occupied[3]){
                        points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "red",cex = 1.5)
                        point_R$Occupied[3] <- TRUE
                        point_R$Occupied[5] <- FALSE
                        vehicle$vehicle_initial[i] <- 22
                      }
                    }else if(path2$value_left[1]){
                      if(!point_R$Occupied[3]){
                        points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "red",cex = 1.5)
                        point_R$Occupied[3] <- TRUE
                        point_R$Occupied[4] <- FALSE
                        vehicle$vehicle_initial[i] <- 22
                      }
                    }else if(path3$value_left[1]){
                      if(!point_R$Occupied[3]){
                        points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "red",cex = 1.5)
                        point_R$Occupied[3] <- TRUE
                        point_R$Occupied[2] <- FALSE
                        vehicle$vehicle_initial[i] <- 22
                      }
                    }
                }else
                  if(vehicle$vehicle_initial[i]==22){
                    #procedure that displace a vehicle on N2

                    if(!point_N2$Occupied[1]){
                      points(point_N2$X_coordinate[1],point_N2$Y_coordinate[1],pch = shape,col = "red",cex = 1.5)
                      point_R$Occupied[3] <- FALSE
                      point_N2$Occupied[1] <- TRUE
                      vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                    }else{
                      points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "red",cex = 1.5)
                      point_R$Occupied[3] <- TRUE
                    }
                  }else if(vehicle$vehicle_initial[i]>22 && vehicle$vehicle_initial[i]<41 ){
                    if(!point_N2$Occupied[vehicle$vehicle_initial[i]-21]){
                      points(point_N2$X_coordinate[vehicle$vehicle_initial[i]-21],point_N2$Y_coordinate[vehicle$vehicle_initial[i]-21],pch = shape,col = "red",cex = 1.5)
                      point_N2$Occupied[vehicle$vehicle_initial[i]-21-1] <- FALSE
                      point_N2$Occupied[vehicle$vehicle_initial[i]-21] <- TRUE
                      vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                    }else{
                      points(point_N2$X_coordinate[vehicle$vehicle_initial[i]-21-1],point_N2$Y_coordinate[vehicle$vehicle_initial[i]-21-1],pch = shape,col = "red",cex = 1.5)
                      point_N2$Occupied[vehicle$vehicle_initial[i]-21-1] <- TRUE
                    }
                  }

              }else
                if(vehicle$Vehicle_Destination[i] == "right"){

                  if(vehicle$vehicle_initial[i]==19){
                    z <- vehicle$vehicle_initial[i]
                    if(!point_R$Occupied[1]){
                      points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "red",cex = 1.5)
                      point_R$Occupied[1] <- TRUE
                      point_E1$Occupied[z] <- FALSE
                      vehicle$vehicle_initial[i] <- 20
                    }else {
                      points(point_E1$X_coordinate[z],point_E1$Y_coordinate[z],pch = shape,col = "red",cex = 1.5)
                      point_E1$Occupied[z] <- TRUE
                    }
                  } else
                    if(vehicle$vehicle_initial[i]==20){
                      #procedure that displace a vehicle on S2

                      if(!point_S2$Occupied[1]){
                        points(point_S2$X_coordinate[1],point_S2$Y_coordinate[1],pch = shape,col = "red",cex = 1.5)
                        point_R$Occupied[1] <- FALSE
                        point_S2$Occupied[1] <- TRUE
                        vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                      }else{
                        points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "red",cex = 1.5)
                        point_R$Occupied[1] <- TRUE
                      }
                    }else if(vehicle$vehicle_initial[i]>20 && vehicle$vehicle_initial[i]<39 ){
                      if(!point_S2$Occupied[vehicle$vehicle_initial[i]-19]){
                        points(point_S2$X_coordinate[vehicle$vehicle_initial[i]-19],point_S2$Y_coordinate[vehicle$vehicle_initial[i]-19],pch = shape,col = "red",cex = 1.5)
                        point_S2$Occupied[vehicle$vehicle_initial[i]-19-1] <- FALSE
                        point_S2$Occupied[vehicle$vehicle_initial[i]-19] <- TRUE
                        vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                      }else{
                        points(point_S2$X_coordinate[vehicle$vehicle_initial[i]-19-1],point_S2$Y_coordinate[vehicle$vehicle_initial[i]-19-1],pch = shape,col = "red",cex = 1.5)
                        point_S2$Occupied[vehicle$vehicle_initial[i]-19-1] <- TRUE
                      }
                    }

                }


        }
      }else
        if(vehicle$Vehicle_Origine[i] == "west"){


          if (vehicle$vehicle_initial[i]+1 < 20) {
            #run vehicles before roundabout
            z <- vehicle$vehicle_initial[i]

            if (!point_W1$Occupied[z+1]) {
              points(point_W1$X_coordinate[z + 1],point_W1$Y_coordinate[z + 1],pch = shape,col = "blue",cex = 1.5)
              point_W1$Occupied[z] <- FALSE
              point_W1$Occupied[z+1] <- TRUE
              vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i]+1
            } else if (point_W1$Occupied[z + 1]) {
              points(point_W1$X_coordinate[z],point_W1$Y_coordinate[z],pch = shape,col = "blue",cex = 1.5)
              point_W1$Occupied[z] <- TRUE
            }
          } else {

            # vehicle that returns
            if(vehicle$Vehicle_Destination[i]== "return"){
              if(vehicle$vehicle_initial[i]==19){
                z <- vehicle$vehicle_initial[i]
                if(!point_R$Occupied[3]){
                  points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "blue",cex = 1.5)
                  point_R$Occupied[3] <- TRUE
                  point_W1$Occupied[z] <- FALSE
                  vehicle$vehicle_initial[i] <- 20
                }else {
                  points(point_W1$X_coordinate[z],point_W1$Y_coordinate[z],pch = shape,col = "blue",cex = 1.5)
                  point_W1$Occupied[z] <- TRUE
                }
              } else if(vehicle$vehicle_initial[i]==20){
                z <- vehicle$vehicle_initial[i]
                if(!point_R$Occupied[5]){
                  points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "blue",cex = 1.5)
                  point_R$Occupied[5] <- TRUE
                  point_R$Occupied[3] <- FALSE
                  vehicle$vehicle_initial[i] <- 21
                }else if(!point_R$Occupied[4]){
                  points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "blue",cex = 1.5)
                  point_R$Occupied[4] <- TRUE
                  point_R$Occupied[3] <- FALSE
                  path1$value[2] <- TRUE
                }else if(!point_R$Occupied[2]){
                  points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "blue",cex = 1.5)
                  point_R$Occupied[2] <- TRUE
                  point_R$Occupied[3] <- FALSE
                  vehicle$vehicle_initial[i] <- 22
                }else{
                  points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "blue",cex = 1.5)
                  point_R$Occupied[3] <- TRUE
                }
              }else if(vehicle$vehicle_initial[i]==21 || path1$value[2] == TRUE){
                z <- vehicle$vehicle_initial[i]
                if(path1$value[2]==FALSE){
                if(!point_R$Occupied[2]){
                  points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "blue",cex = 1.5)
                  point_R$Occupied[2] <- TRUE
                  point_R$Occupied[5] <- FALSE
                  vehicle$vehicle_initial[i] <- 22
                }else {
                  points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "blue",cex = 1.5)
                  point_R$Occupied[5] <- TRUE
                }
                }else if(path1$value[2]==TRUE){
                  if(!point_R$Occupied[5]){
                    points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "blue",cex = 1.5)
                    point_R$Occupied[5] <- TRUE
                    point_R$Occupied[4] <- FALSE
                    path1$value[2] <- FALSE
                  }else{
                      points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "blue",cex = 1.5)
                      point_R$Occupied[4] <- TRUE
                  }
                }
              }else
                if(vehicle$vehicle_initial[i]==22){
                  #procedure that displace a vehicle on W2

                  if(!point_W2$Occupied[1]){
                    points(point_W2$X_coordinate[1],point_W2$Y_coordinate[1],pch = shape,col = "blue",cex = 1.5)
                    point_R$Occupied[2] <- FALSE
                    point_W2$Occupied[1] <- TRUE
                    vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                  }else{
                    points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "blue",cex = 1.5)
                    point_R$Occupied[3] <- TRUE
                  }
                }else if(vehicle$vehicle_initial[i]>22 && vehicle$vehicle_initial[i]<41 ){
                  if(!point_W2$Occupied[vehicle$vehicle_initial[i]-21]){
                    points(point_W2$X_coordinate[vehicle$vehicle_initial[i]-21],point_W2$Y_coordinate[vehicle$vehicle_initial[i]-21],pch = shape,col = "blue",cex = 1.5)
                    point_W2$Occupied[vehicle$vehicle_initial[i]-21-1] <- FALSE
                    point_W2$Occupied[vehicle$vehicle_initial[i]-21] <- TRUE
                    vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                  }else{
                    points(point_W2$X_coordinate[vehicle$vehicle_initial[i]-21-1],point_W2$Y_coordinate[vehicle$vehicle_initial[i]-21-1],pch = shape,col = "blue",cex = 1.5)
                    point_W2$Occupied[vehicle$vehicle_initial[i]-21-1] <- TRUE
                  }
                }
            }else
              if(vehicle$Vehicle_Destination[i] == "straight"){

                if(vehicle$vehicle_initial[i]==19){
                  z <- vehicle$vehicle_initial[i]
                  if(!point_R$Occupied[3]){
                    points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "blue",cex = 1.5)
                    point_R$Occupied[3] <- TRUE
                    point_W1$Occupied[z] <- FALSE
                    vehicle$vehicle_initial[i] <- 20
                  }else {
                    points(point_W1$X_coordinate[z],point_W1$Y_coordinate[z],pch = shape,col = "blue",cex = 1.5)
                    point_W1$Occupied[z] <- TRUE
                  }
                } else if(vehicle$vehicle_initial[i]==20){
                  z <- vehicle$vehicle_initial[i]
                  if(!point_R$Occupied[4]){
                    points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "blue",cex = 1.5)
                    point_R$Occupied[4] <- TRUE
                    point_R$Occupied[3] <- FALSE
                    vehicle$vehicle_initial[i] <- 21
                  }else if(!point_R$Occupied[5]){
                    points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "blue",cex = 1.5)
                    point_R$Occupied[5] <- TRUE
                    point_R$Occupied[3] <- FALSE
                    path2$value[2] <- TRUE
                  }else {
                    points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "blue",cex = 1.5)
                    point_R$Occupied[3] <- TRUE
                  }
                }else if(vehicle$vehicle_initial[i]==21 || path2$value[2]==TRUE){
                  #procedure that displace a vehicle on E2
                  if(path2$value[2]==FALSE){
                  if(!point_E2$Occupied[1]){
                    points(point_E2$X_coordinate[1],point_E2$Y_coordinate[1],pch = shape,col = "blue",cex = 1.5)
                    point_R$Occupied[4] <- FALSE
                    point_E2$Occupied[1] <- TRUE
                    vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                  }else{
                    points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "blue",cex = 1.5)
                    point_R$Occupied[4] <- TRUE
                  }
                  }else if(path2$value[2]==TRUE){
                    if(!point_R$Occupied[4]){
                      points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "blue",cex = 1.5)
                      point_R$Occupied[4] <- TRUE
                      point_R$Occupied[5] <- FALSE
                      path2$value[2]<- FALSE
                    }else{
                      points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "blue",cex = 1.5)
                      point_R$Occupied[5] <- TRUE
                      }
                  }
                }else if(vehicle$vehicle_initial[i]>21 && vehicle$vehicle_initial[i]<40 ){
                  if(!point_E2$Occupied[vehicle$vehicle_initial[i]-20]){
                    points(point_E2$X_coordinate[vehicle$vehicle_initial[i]-20],point_E2$Y_coordinate[vehicle$vehicle_initial[i]-20],pch = shape,col = "blue",cex = 1.5)
                    point_E2$Occupied[vehicle$vehicle_initial[i]-20-1] <- FALSE
                    point_E2$Occupied[vehicle$vehicle_initial[i]-20] <- TRUE
                    vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                  }else{
                    points(point_E2$X_coordinate[vehicle$vehicle_initial[i]-20-1],point_E2$Y_coordinate[vehicle$vehicle_initial[i]-20-1],pch = shape,col = "blue",cex = 1.5)
                    point_W2$Occupied[vehicle$vehicle_initial[i]-20-1] <- TRUE
                  }
                }

              }else
                if(vehicle$Vehicle_Destination[i] == "left"){

                  if(vehicle$vehicle_initial[i]==19){
                    z <- vehicle$vehicle_initial[i]
                    if(!point_R$Occupied[3]){
                      points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "blue",cex = 1.5)
                      point_R$Occupied[3] <- TRUE
                      point_W1$Occupied[z] <- FALSE
                      vehicle$vehicle_initial[i] <- 20
                    }else {
                      points(point_W1$X_coordinate[z],point_W1$Y_coordinate[z],pch = shape,col = "blue",cex = 1.5)
                      point_W1$Occupied[z] <- TRUE
                    }
                  } else if(vehicle$vehicle_initial[i]==20){
                    z <- vehicle$vehicle_initial[i]
                    if(!point_R$Occupied[5]){
                      points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "blue",cex = 1.5)
                      point_R$Occupied[5] <- TRUE
                      point_R$Occupied[3] <- FALSE
                      vehicle$vehicle_initial[i] <- 21
                      path1$value_left[2] <- TRUE
                    }else  if(!point_R$Occupied[2]){
                      points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "blue",cex = 1.5)
                      point_R$Occupied[2] <- TRUE
                      point_R$Occupied[3] <- FALSE
                      vehicle$vehicle_initial[i] <- 21
                      path2$value_left[2] <- TRUE
                    }else if(!point_R$Occupied[4]){
                      points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "blue",cex = 1.5)
                      point_R$Occupied[4] <- TRUE
                      point_R$Occupied[3] <- FALSE
                      path3$value_left[2]<-TRUE
                      vehicle$vehicle_initial[i] <- 21
                    }else{
                      points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "blue",cex = 1.5)
                      point_R$Occupied[3] <- TRUE
                    }
                  }else if(vehicle$vehicle_initial[i]==21){
                    if(path1$value_left[2]){
                      if(!point_R$Occupied[1]){
                        points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "blue",cex = 1.5)
                        point_R$Occupied[1] <- TRUE
                        point_R$Occupied[5] <- FALSE
                        vehicle$vehicle_initial[i] <- 22
                      }
                    }else if(path2$value_left[2]){
                      if(!point_R$Occupied[1]){
                        points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "blue",cex = 1.5)
                        point_R$Occupied[1] <- TRUE
                        point_R$Occupied[2] <- FALSE
                        vehicle$vehicle_initial[i] <- 22
                      }
                    }else if(path3$value_left[2]){
                      if(!point_R$Occupied[1]){
                        points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "blue",cex = 1.5)
                        point_R$Occupied[1] <- TRUE
                        point_R$Occupied[4] <- FALSE
                        vehicle$vehicle_initial[i] <- 22
                      }
                    }
                  }else
                    if(vehicle$vehicle_initial[i]==22){
                      #procedure that displace a vehicle on S2

                      if(!point_S2$Occupied[1]){
                        points(point_S2$X_coordinate[1],point_S2$Y_coordinate[1],pch = shape,col = "blue",cex = 1.5)
                        point_R$Occupied[1] <- FALSE
                        point_S2$Occupied[1] <- TRUE
                        vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                      }else{
                        points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "blue",cex = 1.5)
                        point_R$Occupied[1] <- TRUE
                      }
                    }else if(vehicle$vehicle_initial[i]>22 && vehicle$vehicle_initial[i]<41 ){
                      if(!point_S2$Occupied[vehicle$vehicle_initial[i]-21]){
                        points(point_S2$X_coordinate[vehicle$vehicle_initial[i]-21],point_S2$Y_coordinate[vehicle$vehicle_initial[i]-21],pch = shape,col = "blue",cex = 1.5)
                        point_S2$Occupied[vehicle$vehicle_initial[i]-21-1] <- FALSE
                        point_S2$Occupied[vehicle$vehicle_initial[i]-21] <- TRUE
                        vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                      }else{
                        points(point_S2$X_coordinate[vehicle$vehicle_initial[i]-21-1],point_S2$Y_coordinate[vehicle$vehicle_initial[i]-21-1],pch = shape,col = "blue",cex = 1.5)
                        point_S2$Occupied[vehicle$vehicle_initial[i]-21-1] <- TRUE
                      }
                    }

                }else
                  if(vehicle$Vehicle_Destination[i] == "right"){

                    if(vehicle$vehicle_initial[i]==19){
                      z <- vehicle$vehicle_initial[i]
                      if(!point_R$Occupied[3]){
                        points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "blue",cex = 1.5)
                        point_R$Occupied[3] <- TRUE
                        point_W1$Occupied[z] <- FALSE
                        vehicle$vehicle_initial[i] <- 20
                      }else {
                        points(point_W1$X_coordinate[z],point_W1$Y_coordinate[z],pch = shape,col = "blue",cex = 1.5)
                        point_W1$Occupied[z] <- TRUE
                      }
                    } else
                      if(vehicle$vehicle_initial[i]==20){
                        #procedure that displace a vehicle on N2

                        if(!point_N2$Occupied[1]){
                          points(point_N2$X_coordinate[1],point_N2$Y_coordinate[1],pch = shape,col = "blue",cex = 1.5)
                          point_R$Occupied[3] <- FALSE
                          point_N2$Occupied[1] <- TRUE
                          vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                        }else{
                          points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "blue",cex = 1.5)
                          point_R$Occupied[3] <- TRUE
                        }
                      }else if(vehicle$vehicle_initial[i]>20 && vehicle$vehicle_initial[i]<39 ){
                        if(!point_N2$Occupied[vehicle$vehicle_initial[i]-19]){
                          points(point_N2$X_coordinate[vehicle$vehicle_initial[i]-19],point_N2$Y_coordinate[vehicle$vehicle_initial[i]-19],pch = shape,col = "blue",cex = 1.5)
                          point_N2$Occupied[vehicle$vehicle_initial[i]-19-1] <- FALSE
                          point_N2$Occupied[vehicle$vehicle_initial[i]-19] <- TRUE
                          vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                        }else{
                          points(point_N2$X_coordinate[vehicle$vehicle_initial[i]-19-1],point_N2$Y_coordinate[vehicle$vehicle_initial[i]-19-1],pch = shape,col = "blue",cex = 1.5)
                          point_N2$Occupied[vehicle$vehicle_initial[i]-19-1] <- TRUE
                        }
                      }

                  }
          }

        }else
          if(vehicle$Vehicle_Origine[i] == "north"){
            if (vehicle$vehicle_initial[i]+1 < 20) {
              #run vehicles before roundabout
              z <- vehicle$vehicle_initial[i]

              if (!point_N1$Occupied[z+1]) {
                points(point_N1$X_coordinate[z + 1],point_N1$Y_coordinate[z + 1],pch = shape,col = "magenta",cex = 1.5)
                point_N1$Occupied[z] <- FALSE
                point_N1$Occupied[z+1] <- TRUE
                vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i]+1
              } else if (point_N1$Occupied[z + 1]) {
                points(point_N1$X_coordinate[z],point_N1$Y_coordinate[z],pch = shape,col = "magenta",cex = 1.5)
                point_N1$Occupied[z] <- TRUE
              }
            } else {

              # vehicle that returns
              if(vehicle$Vehicle_Destination[i]== "return"){
                if(vehicle$vehicle_initial[i]==19){
                  z <- vehicle$vehicle_initial[i]
                  if(!point_R$Occupied[4]){
                    points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "magenta",cex = 1.5)
                    point_R$Occupied[4] <- TRUE
                    point_N1$Occupied[z] <- FALSE
                    vehicle$vehicle_initial[i] <- 20
                  }else {
                    points(point_N1$X_coordinate[z],point_N1$Y_coordinate[z],pch = shape,col = "magenta",cex = 1.5)
                    point_E1$Occupied[z] <- TRUE
                  }
                } else if(vehicle$vehicle_initial[i]==20){
                  z <- vehicle$vehicle_initial[i]
                  if(!point_R$Occupied[5]){
                    points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "magenta",cex = 1.5)
                    point_R$Occupied[5] <- TRUE
                    point_R$Occupied[4] <- FALSE
                    vehicle$vehicle_initial[i] <- 21
                  }else if(!point_R$Occupied[1]){
                    points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "magenta",cex = 1.5)
                    point_R$Occupied[1] <- TRUE
                    point_R$Occupied[4] <- FALSE
                    path1$value[3] <- TRUE
                  }else if(!point_R$Occupied[3]){
                    points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "magenta",cex = 1.5)
                    point_R$Occupied[3] <- TRUE
                    point_R$Occupied[4] <- FALSE
                    vehicle$vehicle_initial[i] <- 22
                  }else{
                    points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "magenta",cex = 1.5)
                    point_R$Occupied[4] <- TRUE
                  }
                }else if(vehicle$vehicle_initial[i]==21 || path1$value[3] == TRUE){
                  z <- vehicle$vehicle_initial[i]
                  if(path1$value[3] == FALSE){
                  if(!point_R$Occupied[3]){
                    points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "magenta",cex = 1.5)
                    point_R$Occupied[3] <- TRUE
                    point_R$Occupied[5] <- FALSE
                    vehicle$vehicle_initial[i] <- 22
                  }else {
                    points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "magenta",cex = 1.5)
                    point_R$Occupied[5] <- TRUE
                  }
                  }else if(path1$value[3]==TRUE){
                    if(!point_R$Occupied[5]){
                      points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "magenta",cex = 1.5)
                      point_R$Occupied[5] <- TRUE
                      point_R$Occupied[1] <- FALSE
                      path1$value[3]<-FALSE
                    }else {
                      points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "magenta",cex = 1.5)
                      point_R$Occupied[1] <- TRUE
                    }
                  }
                }else
                  if(vehicle$vehicle_initial[i]==22){
                    #procedure that displace a vehicle on N2

                    if(!point_N2$Occupied[1]){
                      points(point_N2$X_coordinate[1],point_N2$Y_coordinate[1],pch = shape,col = "magenta",cex = 1.5)
                      point_R$Occupied[3] <- FALSE
                      point_N2$Occupied[1] <- TRUE
                      vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                    }else{
                      points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "magenta",cex = 1.5)
                      point_R$Occupied[3] <- TRUE
                    }
                  }else if(vehicle$vehicle_initial[i]>22 && vehicle$vehicle_initial[i]<41 ){
                    if(!point_N2$Occupied[vehicle$vehicle_initial[i]-21]){
                      points(point_N2$X_coordinate[vehicle$vehicle_initial[i]-21],point_N2$Y_coordinate[vehicle$vehicle_initial[i]-21],pch = shape,col = "magenta",cex = 1.5)
                      point_N2$Occupied[vehicle$vehicle_initial[i]-21-1] <- FALSE
                      point_N2$Occupied[vehicle$vehicle_initial[i]-21] <- TRUE
                      vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                    }else{
                      points(point_N2$X_coordinate[vehicle$vehicle_initial[i]-21-1],point_N2$Y_coordinate[vehicle$vehicle_initial[i]-21-1],pch = shape,col = "magenta",cex = 1.5)
                      point_N2$Occupied[vehicle$vehicle_initial[i]-21-1] <- TRUE
                    }
                  }
              }else
                if(vehicle$Vehicle_Destination[i] == "straight"){

                  if(vehicle$vehicle_initial[i]==19){
                    z <- vehicle$vehicle_initial[i]
                    if(!point_R$Occupied[4]){
                      points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "magenta",cex = 1.5)
                      point_R$Occupied[4] <- TRUE
                      point_N1$Occupied[z] <- FALSE
                      vehicle$vehicle_initial[i] <- 20
                    }else {
                      points(point_N1$X_coordinate[z],point_N1$Y_coordinate[z],pch = shape,col = "magenta",cex = 1.5)
                      point_N1$Occupied[z] <- TRUE
                    }
                  } else if(vehicle$vehicle_initial[i]==20){
                    z <- vehicle$vehicle_initial[i]
                    if(!point_R$Occupied[1]){
                      points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "magenta",cex = 1.5)
                      point_R$Occupied[1] <- TRUE
                      point_R$Occupied[4] <- FALSE
                      vehicle$vehicle_initial[i] <- 21
                    }else if(!point_R$Occupied[5]){
                      points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "magenta",cex = 1.5)
                      point_R$Occupied[5] <- TRUE
                      point_R$Occupied[4] <- FALSE
                      path2$value[3] <- TRUE
                    }else {
                      points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "magenta",cex = 1.5)
                      point_R$Occupied[4] <- TRUE
                    }
                  }else if(vehicle$vehicle_initial[i]==21 || path2$value[3]==TRUE){
                    #procedure that displace a vehicle on S2
                  if(path2$value[3]==FALSE){
                    if(!point_S2$Occupied[1]){
                      points(point_S2$X_coordinate[1],point_S2$Y_coordinate[1],pch = shape,col = "magenta",cex = 1.5)
                      point_R$Occupied[1] <- FALSE
                      point_S2$Occupied[1] <- TRUE
                      vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                    }else{
                      points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "magenta",cex = 1.5)
                      point_R$Occupied[1] <- TRUE
                    }
                  }else if(path2$value[3]==TRUE){
                    if(!point_R$Occupied[1]){
                      points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "magenta",cex = 1.5)
                      point_R$Occupied[1] <- TRUE
                      point_R$Occupied[5] <- FALSE
                      path2$value[3]<- FALSE
                    }else{
                        points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "magenta",cex = 1.5)
                        point_R$Occupied[5] <- TRUE
                    }
                  }
                  }else if(vehicle$vehicle_initial[i]>21 && vehicle$vehicle_initial[i]<40 ){
                    if(!point_S2$Occupied[vehicle$vehicle_initial[i]-20]){
                      points(point_S2$X_coordinate[vehicle$vehicle_initial[i]-20],point_S2$Y_coordinate[vehicle$vehicle_initial[i]-20],pch = shape,col = "magenta",cex = 1.5)
                      point_S2$Occupied[vehicle$vehicle_initial[i]-20-1] <- FALSE
                      point_S2$Occupied[vehicle$vehicle_initial[i]-20] <- TRUE
                      vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                    }else{
                      points(point_S2$X_coordinate[vehicle$vehicle_initial[i]-20-1],point_S2$Y_coordinate[vehicle$vehicle_initial[i]-20-1],pch = shape,col = "magenta",cex = 1.5)
                      point_S2$Occupied[vehicle$vehicle_initial[i]-20-1] <- TRUE
                    }
                  }

                }else
                  if(vehicle$Vehicle_Destination[i] == "left"){

                    if(vehicle$vehicle_initial[i]==19){
                      z <- vehicle$vehicle_initial[i]
                      if(!point_R$Occupied[4]){
                        points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "magenta",cex = 1.5)
                        point_R$Occupied[4] <- TRUE
                        point_N1$Occupied[z] <- FALSE
                        vehicle$vehicle_initial[i] <- 20
                      }else {
                        points(point_N1$X_coordinate[z],point_N1$Y_coordinate[z],pch = shape,col = "magenta",cex = 1.5)
                        point_N1$Occupied[z] <- TRUE
                      }
                    } else if(vehicle$vehicle_initial[i]==20){
                      z <- vehicle$vehicle_initial[i]
                      if(!point_R$Occupied[5]){
                        points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "magenta",cex = 1.5)
                        point_R$Occupied[5] <- TRUE
                        point_R$Occupied[4] <- FALSE
                        path1$value_left[3]<-TRUE
                        vehicle$vehicle_initial[i] <- 21
                      }else if(!point_R$Occupied[3]){
                        points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "magenta",cex = 1.5)
                        point_R$Occupied[3] <- TRUE
                        point_R$Occupied[4] <- FALSE
                        vehicle$vehicle_initial[i] <- 21
                        path2$value_left[3]<-TRUE
                      }else if(!point_R$Occupied[1]){
                        points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "magenta",cex = 1.5)
                        point_R$Occupied[1] <- TRUE
                        point_R$Occupied[4] <- FALSE
                        path3$value_left[3] <- TRUE
                        vehicle$vehicle_initial[i] <- 21
                      }else {
                        points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "magenta",cex = 1.5)
                        point_R$Occupied[4] <- TRUE
                      }
                    }else if(vehicle$vehicle_initial[i]==21){
                      if(path1$value_left[3]){
                        if(!point_R$Occupied[2]){
                          points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "magenta",cex = 1.5)
                          point_R$Occupied[2] <- TRUE
                          point_R$Occupied[5] <- FALSE
                          vehicle$vehicle_initial[i] <- 22
                        }
                      }else if(path2$value_left[3]){
                        if(!point_R$Occupied[2]){
                          points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "magenta",cex = 1.5)
                          point_R$Occupied[2] <- TRUE
                          point_R$Occupied[3] <- FALSE
                          vehicle$vehicle_initial[i] <- 22
                        }
                      }else if(path3$value_left[3]){
                        if(!point_R$Occupied[2]){
                          points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "magenta",cex = 1.5)
                          point_R$Occupied[2] <- TRUE
                          point_R$Occupied[1] <- FALSE
                          vehicle$vehicle_initial[i] <- 22
                        }
                      }
                    }else if(vehicle$vehicle_initial[i]==22){
                        #procedure that displace a vehicle on W2
                        if(!point_W2$Occupied[1]){
                          points(point_W2$X_coordinate[1],point_W2$Y_coordinate[1],pch = shape,col = "magenta",cex = 1.5)
                          point_W2$Occupied[1] <- TRUE
                          point_R$Occupied[2] <- FALSE
                          vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                        }else{
                          points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "magenta",cex = 1.5)
                          point_R$Occupied[2] <- TRUE
                        }
                      }else if(vehicle$vehicle_initial[i]>22 && vehicle$vehicle_initial[i]<41 ){
                        if(!point_W2$Occupied[vehicle$vehicle_initial[i]-21]){
                          points(point_W2$X_coordinate[vehicle$vehicle_initial[i]-21],point_W2$Y_coordinate[vehicle$vehicle_initial[i]-21],pch = shape,col = "magenta",cex = 1.5)
                          point_W2$Occupied[vehicle$vehicle_initial[i]-21-1] <- FALSE
                          point_W2$Occupied[vehicle$vehicle_initial[i]-21] <- TRUE
                          vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                        }else{
                          points(point_W2$X_coordinate[vehicle$vehicle_initial[i]-21-1],point_W2$Y_coordinate[vehicle$vehicle_initial[i]-21-1],pch = shape,col = "magenta",cex = 1.5)
                          point_W2$Occupied[vehicle$vehicle_initial[i]-21-1] <- TRUE
                        }
                      }
                  }else
                    if(vehicle$Vehicle_Destination[i] == "right"){

                      if(vehicle$vehicle_initial[i]==19){
                        z <- vehicle$vehicle_initial[i]
                        if(!point_R$Occupied[4]){
                          points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "magenta",cex = 1.5)
                          point_R$Occupied[4] <- TRUE
                          point_N1$Occupied[z] <- FALSE
                          vehicle$vehicle_initial[i] <- 20
                        }else {
                          points(point_N1$X_coordinate[z],point_N1$Y_coordinate[z],pch = shape,col = "magenta",cex = 1.5)
                          point_N1$Occupied[z] <- TRUE
                        }
                      } else
                        if(vehicle$vehicle_initial[i]==20){
                          #procedure that displace a vehicle on E2

                          if(!point_E2$Occupied[1]){
                            points(point_E2$X_coordinate[1],point_E2$Y_coordinate[1],pch = shape,col = "magenta",cex = 1.5)
                            point_R$Occupied[4] <- FALSE
                            point_E2$Occupied[1] <- TRUE
                            vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                          }else{
                            points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "magenta",cex = 1.5)
                            point_R$Occupied[4] <- TRUE
                          }
                        }else if(vehicle$vehicle_initial[i]>20 && vehicle$vehicle_initial[i]<39 ){
                          if(!point_E2$Occupied[vehicle$vehicle_initial[i]-19]){
                            points(point_E2$X_coordinate[vehicle$vehicle_initial[i]-19],point_E2$Y_coordinate[vehicle$vehicle_initial[i]-19],pch = shape,col = "magenta",cex = 1.5)
                            point_E2$Occupied[vehicle$vehicle_initial[i]-19-1] <- FALSE
                            point_E2$Occupied[vehicle$vehicle_initial[i]-19] <- TRUE
                            vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                          }else{
                            points(point_E2$X_coordinate[vehicle$vehicle_initial[i]-19-1],point_E2$Y_coordinate[vehicle$vehicle_initial[i]-19-1],pch = shape,col = "magenta",cex = 1.5)
                            point_E2$Occupied[vehicle$vehicle_initial[i]-19-1] <- TRUE
                          }
                        }

                    }


            }

          }else
            if(vehicle$Vehicle_Origine[i] == "south"){

              if (vehicle$vehicle_initial[i]+1 < 20) {
                #run vehicles before roundabout
                z <- vehicle$vehicle_initial[i]

                if (!point_S1$Occupied[z+1]) {
                  points(point_S1$X_coordinate[z + 1],point_S1$Y_coordinate[z + 1],pch = shape,col = "cyan",cex = 1.5)
                  point_S1$Occupied[z] <- FALSE
                  point_S1$Occupied[z+1] <- TRUE
                  vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i]+1
                } else if (point_S1$Occupied[z + 1]) {
                  points(point_S1$X_coordinate[z],point_S1$Y_coordinate[z],pch = shape,col = "cyan",cex = 1.5)
                  point_S1$Occupied[z] <- TRUE
                }
              } else {

                # vehicle that returns
                if(vehicle$Vehicle_Destination[i]== "return"){
                  if(vehicle$vehicle_initial[i]==19){
                    z <- vehicle$vehicle_initial[i]
                    if(!point_R$Occupied[2]){
                      points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "cyan",cex = 1.5)
                      point_R$Occupied[2] <- TRUE
                      point_S1$Occupied[z] <- FALSE
                      vehicle$vehicle_initial[i] <- 20
                    }else {
                      points(point_S1$X_coordinate[z],point_S1$Y_coordinate[z],pch = shape,col = "cyan",cex = 1.5)
                      point_S1$Occupied[z] <- TRUE
                    }
                  } else if(vehicle$vehicle_initial[i]==20){
                    z <- vehicle$vehicle_initial[i]
                    if(!point_R$Occupied[5]){
                      points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "cyan",cex = 1.5)
                      point_R$Occupied[5] <- TRUE
                      point_R$Occupied[2] <- FALSE
                      vehicle$vehicle_initial[i] <- 21
                    }else if(!point_R$Occupied[3]){
                      points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "cyan",cex = 1.5)
                      point_R$Occupied[3] <- TRUE
                      point_R$Occupied[2] <- FALSE
                      path1$value[4]<-TRUE
                    }else if(!point_R$Occupied[1]){
                      points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "cyan",cex = 1.5)
                      point_R$Occupied[1] <- TRUE
                      point_R$Occupied[2] <- FALSE
                      vehicle$vehicle_initial[i] <- 22
                    }else{
                      points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "cyan",cex = 1.5)
                      point_R$Occupied[2] <- TRUE
                    }
                  }else if(vehicle$vehicle_initial[i]==21 || path1$value[4]==TRUE){
                    z <- vehicle$vehicle_initial[i]
                    if(path1$value[4] == FALSE){
                    if(!point_R$Occupied[1]){
                      points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "cyan",cex = 1.5)
                      point_R$Occupied[1] <- TRUE
                      point_R$Occupied[5] <- FALSE
                      vehicle$vehicle_initial[i] <- 22
                    }else {
                      points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "cyan",cex = 1.5)
                      point_R$Occupied[5] <- TRUE
                    }
                    }else if(path1$value[4]==TRUE){
                      if(!point_R$Occupied[5]){
                        points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "cyan",cex = 1.5)
                        point_R$Occupied[5] <- TRUE
                        point_R$Occupied[3] <- FALSE
                        path1$value[4]<-FALSE
                      }else {
                        points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "cyan",cex = 1.5)
                        point_R$Occupied[3] <- TRUE
                      }
                      }
                  }else
                    if(vehicle$vehicle_initial[i]==22){
                      #procedure that displace a vehicle on S2

                      if(!point_S2$Occupied[1]){
                        points(point_S2$X_coordinate[1],point_S2$Y_coordinate[1],pch = shape,col = "cyan",cex = 1.5)
                        point_R$Occupied[1] <- FALSE
                        point_S2$Occupied[1] <- TRUE
                        vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                      }else{
                        points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "cyan",cex = 1.5)
                        point_R$Occupied[1] <- TRUE
                      }
                    }else if(vehicle$vehicle_initial[i]>22 && vehicle$vehicle_initial[i]<41 ){
                      if(!point_S2$Occupied[vehicle$vehicle_initial[i]-21]){
                        points(point_S2$X_coordinate[vehicle$vehicle_initial[i]-21],point_S2$Y_coordinate[vehicle$vehicle_initial[i]-21],pch = shape,col = "cyan",cex = 1.5)
                        point_S2$Occupied[vehicle$vehicle_initial[i]-21-1] <- FALSE
                        point_S2$Occupied[vehicle$vehicle_initial[i]-21] <- TRUE
                        vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                      }else{
                        points(point_S2$X_coordinate[vehicle$vehicle_initial[i]-21-1],point_S2$Y_coordinate[vehicle$vehicle_initial[i]-21-1],pch = shape,col = "cyan",cex = 1.5)
                        point_S2$Occupied[vehicle$vehicle_initial[i]-21-1] <- TRUE
                      }
                    }
                }else
                  if(vehicle$Vehicle_Destination[i] == "straight"){

                    if(vehicle$vehicle_initial[i]==19){
                      z <- vehicle$vehicle_initial[i]
                      if(!point_R$Occupied[2]){
                        points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "cyan",cex = 1.5)
                        point_R$Occupied[2] <- TRUE
                        point_S1$Occupied[z] <- FALSE
                        vehicle$vehicle_initial[i] <- 20
                      }else {
                        points(point_S1$X_coordinate[z],point_S1$Y_coordinate[z],pch = shape,col = "cyan",cex = 1.5)
                        point_S1$Occupied[z] <- TRUE
                      }
                    } else if(vehicle$vehicle_initial[i]==20){
                      z <- vehicle$vehicle_initial[i]
                      if(!point_R$Occupied[3]){
                        points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "cyan",cex = 1.5)
                        point_R$Occupied[3] <- TRUE
                        point_R$Occupied[2] <- FALSE
                        vehicle$vehicle_initial[i] <- 21
                      }else if(!point_R$Occupied[5]){
                        points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "cyan",cex = 1.5)
                        point_R$Occupied[5] <- TRUE
                        point_R$Occupied[2] <- FALSE
                        path2$value[4]<-TRUE
                      }else {
                        points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "cyan",cex = 1.5)
                        point_R$Occupied[2] <- TRUE
                      }
                    }else if(vehicle$vehicle_initial[i]==21 || path2$value[4]==TRUE){
                      #procedure that displace a vehicle on N2
                      if(path2$value[4]==FALSE){
                      if(!point_N2$Occupied[1]){
                        points(point_N2$X_coordinate[1],point_N2$Y_coordinate[1],pch = shape,col = "cyan",cex = 1.5)
                        point_R$Occupied[3] <- FALSE
                        point_N2$Occupied[1] <- TRUE
                        vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                      }else{
                        points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "cyan",cex = 1.5)
                        point_R$Occupied[3] <- TRUE
                      }
                      }else if(path2$value[4]==TRUE){
                        if(!point_R$Occupied[3]){
                          points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "cyan",cex = 1.5)
                          point_R$Occupied[3] <- TRUE
                          point_R$Occupied[5] <- FALSE
                          path2$value[4] <- FALSE
                        }else{
                          points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "cyan",cex = 1.5)
                          point_R$Occupied[5] <- TRUE
                        }
                        }
                    }else if(vehicle$vehicle_initial[i]>21 && vehicle$vehicle_initial[i]<40 ){
                      if(!point_N2$Occupied[vehicle$vehicle_initial[i]-20]){
                        points(point_N2$X_coordinate[vehicle$vehicle_initial[i]-20],point_N2$Y_coordinate[vehicle$vehicle_initial[i]-20],pch = shape,col = "cyan",cex = 1.5)
                        point_N2$Occupied[vehicle$vehicle_initial[i]-20-1] <- FALSE
                        point_N2$Occupied[vehicle$vehicle_initial[i]-20] <- TRUE
                        vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                      }else{
                        points(point_N2$X_coordinate[vehicle$vehicle_initial[i]-20-1],point_N2$Y_coordinate[vehicle$vehicle_initial[i]-20-1],pch = shape,col = "cyan",cex = 1.5)
                        point_N2$Occupied[vehicle$vehicle_initial[i]-20-1] <- TRUE
                      }
                    }

                  }else
                    if(vehicle$Vehicle_Destination[i] == "left"){

                      if(vehicle$vehicle_initial[i]==19){
                        z <- vehicle$vehicle_initial[i]
                        if(!point_R$Occupied[2]){
                          points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "cyan",cex = 1.5)
                          point_R$Occupied[2] <- TRUE
                          point_S1$Occupied[z] <- FALSE
                          vehicle$vehicle_initial[i] <- 20

                        }else {
                          points(point_S1$X_coordinate[z],point_S1$Y_coordinate[z],pch = shape,col = "cyan",cex = 1.5)
                          point_S1$Occupied[z] <- TRUE
                        }
                      } else if(vehicle$vehicle_initial[i]==20){
                        z <- vehicle$vehicle_initial[i]
                        if(!point_R$Occupied[5]){
                          points(point_R$X_coordinate[5],point_R$Y_coordinate[5],pch = shape,col = "cyan",cex = 1.5)
                          point_R$Occupied[5] <- TRUE
                          point_R$Occupied[2] <- FALSE
                          vehicle$vehicle_initial[i] <- 21
                          path1$value_left[4] <- TRUE
                        }else  if(!point_R$Occupied[1]){
                          points(point_R$X_coordinate[1],point_R$Y_coordinate[1],pch = shape,col = "cyan",cex = 1.5)
                          point_R$Occupied[1] <- TRUE
                          point_R$Occupied[2] <- FALSE
                          vehicle$vehicle_initial[i] <- 21
                          path2$value_left[4] <- TRUE
                        }else if(!point_R$Occupied[3]){
                          points(point_R$X_coordinate[3],point_R$Y_coordinate[3],pch = shape,col = "cyan",cex = 1.5)
                          point_R$Occupied[3] <- TRUE
                          point_R$Occupied[2] <- FALSE
                          path3$value_left[4]<- TRUE
                          vehicle$vehicle_initial[i] <- 21
                        }else{
                          points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "cyan",cex = 1.5)
                          point_R$Occupied[2] <- TRUE
                        }
                      }else if(vehicle$vehicle_initial[i]==21){
                        if(path1$value_left[4]){
                          if(!point_R$Occupied[4]){
                            points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "cyan",cex = 1.5)
                            point_R$Occupied[4] <- TRUE
                            point_R$Occupied[5] <- FALSE
                            vehicle$vehicle_initial[i] <- 22
                          }
                        }else if(path2$value_left[4]){
                          if(!point_R$Occupied[4]){
                            points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "cyan",cex = 1.5)
                            point_R$Occupied[4] <- TRUE
                            point_R$Occupied[1] <- FALSE
                            vehicle$vehicle_initial[i] <- 22
                          }
                        }else if(path3$value_left[4]){
                          if(!point_R$Occupied[4]){
                            points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "cyan",cex = 1.5)
                            point_R$Occupied[4] <- TRUE
                            point_R$Occupied[3] <- FALSE
                            vehicle$vehicle_initial[i] <- 22
                          }
                        }
                      }else
                        if(vehicle$vehicle_initial[i]==22){
                          #procedure that displace a vehicle on E2

                          if(!point_E2$Occupied[1]){
                            points(point_E2$X_coordinate[1],point_E2$Y_coordinate[1],pch = shape,col = "cyan",cex = 1.5)
                            point_R$Occupied[4] <- FALSE
                            point_E2$Occupied[1] <- TRUE
                            vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                          }else{
                            points(point_R$X_coordinate[4],point_R$Y_coordinate[4],pch = shape,col = "cyan",cex = 1.5)
                            point_R$Occupied[4] <- TRUE
                          }
                        }else if(vehicle$vehicle_initial[i]>22 && vehicle$vehicle_initial[i]<41 ){
                          if(!point_E2$Occupied[vehicle$vehicle_initial[i]-21]){
                            points(point_E2$X_coordinate[vehicle$vehicle_initial[i]-21],point_E2$Y_coordinate[vehicle$vehicle_initial[i]-21],pch = shape,col = "cyan",cex = 1.5)
                            point_E2$Occupied[vehicle$vehicle_initial[i]-21-1] <- FALSE
                            point_E2$Occupied[vehicle$vehicle_initial[i]-21] <- TRUE
                            vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                          }else{
                            points(point_E2$X_coordinate[vehicle$vehicle_initial[i]-21-1],point_E2$Y_coordinate[vehicle$vehicle_initial[i]-21-1],pch = shape,col = "cyan",cex = 1.5)
                            point_E2$Occupied[vehicle$vehicle_initial[i]-21-1] <- TRUE
                          }
                        }

                    }else
                      if(vehicle$Vehicle_Destination[i] == "right"){

                        if(vehicle$vehicle_initial[i]==19){
                          z <- vehicle$vehicle_initial[i]
                          if(!point_R$Occupied[2]){
                            points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "cyan",cex = 1.5)
                            point_R$Occupied[2] <- TRUE
                            point_S1$Occupied[z] <- FALSE
                            vehicle$vehicle_initial[i] <- 20
                          }else {
                            points(point_S1$X_coordinate[z],point_S1$Y_coordinate[z],pch = shape,col = "cyan",cex = 1.5)
                            point_S1$Occupied[z] <- TRUE
                          }
                        } else
                          if(vehicle$vehicle_initial[i]==20){
                            #procedure that displace a vehicle on W2
                            if(!point_W2$Occupied[1]){
                              points(point_W2$X_coordinate[1],point_W2$Y_coordinate[1],pch = shape,col = "cyan",cex = 1.5)
                              point_R$Occupied[2] <- FALSE
                              point_W2$Occupied[1] <- TRUE
                              vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                            }else{
                              points(point_R$X_coordinate[2],point_R$Y_coordinate[2],pch = shape,col = "cyan",cex = 1.5)
                              point_R$Occupied[2] <- TRUE
                            }
                          }else if(vehicle$vehicle_initial[i]>20 && vehicle$vehicle_initial[i]<39 ){
                            if(!point_W2$Occupied[vehicle$vehicle_initial[i]-19]){
                              points(point_W2$X_coordinate[vehicle$vehicle_initial[i]-19],point_W2$Y_coordinate[vehicle$vehicle_initial[i]-19],pch = shape,col = "cyan",cex = 1.5)
                              point_W2$Occupied[vehicle$vehicle_initial[i]-19-1] <- FALSE
                              point_W2$Occupied[vehicle$vehicle_initial[i]-19] <- TRUE
                              vehicle$vehicle_initial[i] <- vehicle$vehicle_initial[i] + 1
                            }else{
                              points(point_W2$X_coordinate[vehicle$vehicle_initial[i]-19-1],point_W2$Y_coordinate[vehicle$vehicle_initial[i]-19-1],pch = shape,col = "cyan",cex = 1.5)
                              point_W2$Occupied[vehicle$vehicle_initial[i]-19-1] <- TRUE
                            }
                          }

                      }


              }


            }else{
              print("No simulation please")
            }

    }
    Sys.sleep(1)


  }
}



#portion of code to be remove from here it is a test of procedures and functions

#draw the road for simulation
roads_four_ways(x, y)
#ask user different information containing the simulation
vehicle <- Enter_parameter(vehicle)
#verifies the Matricul and initial position
vehicle <- check_Id(vehicle)
vehicle <- check_Initial(vehicle)

#it display the vehicles at it initial and save an
Update_state <- Display_at_initial(vehicle,
                   Default_point_on_road_east_1,Default_point_on_road_west_1,
                   Default_point_on_road_north_1, Default_point_on_road_south_1)

Default_point_on_road_east_1 <- Update_state$point_east
Default_point_on_road_west_1 <- Update_state$point_west
Default_point_on_road_north_1 <- Update_state$point_north
Default_point_on_road_south_1 <- Update_state$point_south

#make the simulation of the whole system
Simulation_X(x,y,vehicle,
             Default_point_on_road_east_1,Default_point_on_road_east_2,
             Default_point_on_road_west_1,Default_point_on_road_west_2,
             Default_point_on_road_north_1,Default_point_on_road_north_2,
             Default_point_on_road_south_1,Default_point_on_road_south_2, Default_point_on_roundabout)
