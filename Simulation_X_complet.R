# assigning the interval for our different road
x <- -100:100
y <- x
#procedure for constructing different parameters
Construct <- function() {
  #constructing the data type
  type = c("bicycle", "bike", "car", "truck")
  destination <- c("left", "right", "straight", "return")
  origine <- c("north", "south", "east", "west")
  light_color <- c("green", "red", "orange")
  road_axes <- c("north", "south", "east", "west")

  #constructing table to collect information for each vehicle
  vehicle <- data.frame(
    Vehicle_Type = character(),
    Vehicle_Origine = character(),
    Vehicle_Destination = character(),
    vehicel_id = character(),
    vehicle_speed = integer(),
    vehicle_priority = logical(),
    vehicle_special_priority = logical()
  )

  #Constructing points on the EAST, WEST, NORTH and SOUTH road and the ROUNDABOUT.

  #constructing table that contains displacement points of road EAST_1 and EAST_2
  Default_point_on_road_east_1 <- data.frame(X_coordinate = integer(), Y_coordinate = integer())
  Default_point_on_road_east_2 <- data.frame(X_coordinate = integer(), Y_coordinate = integer())

  #constructing table that contains displacement points of road WEST_1 and WEST_2
  Default_point_on_road_west_1 <- data.frame(X_coordinate = integer(), Y_coordinate = integer())
  Default_point_on_road_west_2 <- data.frame(X_coordinate = integer(), Y_coordinate = integer())

  #constructing table that contains displacement points of road NORTH_1 and NORTH_2
  Default_point_on_road_north_1 <- data.frame(X_coordinate = integer(), Y_coordinate = integer())
  Default_point_on_road_north_2 <- data.frame(X_coordinate = integer(), Y_coordinate = integer())

  #constructing table that contains displacement points of road SOUTH_1 and SOUTH_2
  Default_point_on_road_south_1 <- data.frame(X_coordinate = integer(), Y_coordinate = integer())
  Default_point_on_road_south_2 <- data.frame(X_coordinate = integer(), Y_coordinate = integer())

  #constructing table that contains displacement points of the ROUNDABOUT
  Default_point_on_roundabout <- data.frame(X_coordinate = integer(), Y_coordinate = integer())


  #assigning displacement point to the table of road EAST_1 and EAST_2
  x_point_1 <- -100
  x_point_2 <- -100

  for (i in 1:19) {
    Default_point_on_road_east_1 = rbind(
      Default_point_on_road_east_1,
      data.frame(X_coordinate = x_point_1, Y_coordinate = -2.5)
    )
    x_point_1 <- x_point_1 + 5

    Default_point_on_road_east_2 = rbind(
      Default_point_on_road_east_2,
      data.frame(X_coordinate = x_point_2, Y_coordinate = 2.5)
    )
    x_point_2 <- x_point_2 + 5
  }

  #assigning displacement point to the table of road WEST_1 and WEST_2
  x_point_3 <- 10
  x_point_4 <- 10

  for (i in 1:19) {
    Default_point_on_road_west_1 = rbind(
      Default_point_on_road_west_1,
      data.frame(X_coordinate = x_point_3, Y_coordinate = -2.5)
    )
    x_point_3 <- x_point_3 + 5

    Default_point_on_road_west_2 = rbind(
      Default_point_on_road_west_2,
      data.frame(X_coordinate = x_point_4, Y_coordinate = 2.5)
    )
    x_point_4 <- x_point_4 + 5
  }

  #assigning displacement point to the table of road NORTH_1 and NORTH_2
  y_point_1 <- 10
  y_point_2 <- 10

  for (i in 1:19) {
    Default_point_on_road_north_1 = rbind(
      Default_point_on_road_north_1,
      data.frame(X_coordinate = -2.5, Y_coordinate = y_point_1)
    )
    y_point_1 <- y_point_1 + 5

    Default_point_on_road_north_2 = rbind(
      Default_point_on_road_north_2,
      data.frame(X_coordinate = 2.5, Y_coordinate = y_point_2)
    )
    y_point_2 <- y_point_2 + 5
  }


  #assigning displacement point to the table of road SOUTH_1 and SOUTH_2
  y_point_3 <- -100
  y_point_4 <- -100

  for (i in 1:19) {
    Default_point_on_road_south_1 = rbind(
      Default_point_on_road_south_1,
      data.frame(X_coordinate = -2.5, Y_coordinate = y_point_3)
    )
    y_point_3 <- y_point_3 + 5

    Default_point_on_road_south_2 = rbind(
      Default_point_on_road_south_2,
      data.frame(X_coordinate = 2.5, Y_coordinate = y_point_4)
    )
    y_point_4 <- y_point_4 + 5
  }

  #assigning displacement point to the table of ROUNDABOUT
  Default_point_on_roundabout = rbind( Default_point_on_roundabout, data.frame(X_coordinate =-2.5, Y_coordinate = 2.5))
  Default_point_on_roundabout = rbind( Default_point_on_roundabout, data.frame(X_coordinate =-2.5, Y_coordinate = -2.5))
  Default_point_on_roundabout = rbind( Default_point_on_roundabout, data.frame(X_coordinate =2.5, Y_coordinate = 2.5))
  Default_point_on_roundabout = rbind( Default_point_on_roundabout, data.frame(X_coordinate =2.5, Y_coordinate = -2.5))
  Default_point_on_roundabout = rbind( Default_point_on_roundabout, data.frame(X_coordinate =0, Y_coordinate = 0))

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

  save(Default_point_on_road_east_1, Default_point_on_road_east_2,
       Default_point_on_road_north_1, Default_point_on_road_north_2,
       Default_point_on_road_south_1, Default_point_on_road_south_2,
       Default_point_on_road_west_1, Default_point_on_road_west_2,
       Default_point_on_roundabout, file = "TABLE-POINT.RData")

}

#Constructing the four way road on road
roads_four_ways <- function(x, y) {
  #road from left to right and vice versa
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
  points(x[104],
         y[104],
         pch = 20,
         col = "white",
         cex = 15)
}

#procedure that displace a vehicle according to it speed and priority
# also verify available displacement point in order to cross the roundabout


#test of procedures
Construct()
roads_four_ways(x,y)


#points(Default_point_on_road_east_1[1,1], Default_point_on_road_east_1[1,2], pch = 20, col = "blue", cex = 2)
