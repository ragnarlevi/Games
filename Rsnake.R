library(grDevices)

snake_game <- function(x= 16, y = 16, isWall = TRUE) {
  # plot - True means user controls, otherwise this could be solved with a solver function
  x11() # close with dev.off()
  # change to even numbers
  x <- x + x%%2
  y <- y + y%%2
  
  drawsnake <- function(movement, snake){
    movement <- tolower(movement)
    rect(1-0.6+0.1, y+1.5, x, y+2.5 , col="white", lty="blank")
    text(0, y+2, paste("Score:",score),pos=4)
    text(x/4, y+2, paste("Movements:",movements),pos=4)
    if(isWall){
      text(3*x/4, y+2, paste("Wall is on"),pos=4)
    }else{
      text(3*x/4, y+2, paste("Wall is off"),pos=4)
    }
    newhead <- switch(movement, 
                      "right" = c(snake[[1]][1]+1, snake[[1]][2]),
                      "left" = c(snake[[1]][1]-1, snake[[1]][2]),
                      "up"= c(snake[[1]][1], snake[[1]][2]+1),
                      "down"= c(snake[[1]][1], snake[[1]][2]-1),
                      c(snake[[1]][1], snake[[1]][2])) # (if we are not moving we dont do anything)
    if(!identical(newhead, snake[[1]])){
      snake <- append(list(newhead), snake)
      # remove last observations  and clear the rectangle
      rx <- snake[[length(snake)]][1]
      ry <- snake[[length(snake)]][2]
      rect(rx-0.5, ry-0.5, rx+0.5, ry+0.5,col="white", lty="blank")
      snake <- snake[1:(length(snake)-1)]
      # when we do blank we can accidentaly erase stuff
      # so we ad the lines again
      rect(0-0.6,0-0.6,x+0.6,y+0.6)
    }
    
    # if there is no wall we need
    # to do extra stuff
    if(!isWall){
      head <- c(snake[[1]][1], snake[[1]][2])
      if(head[1] > x){
        # we have crashed right side
        snake[[1]] <- c(0, snake[[1]][2])
      }else if(head[1] < 0){
        #we have crashed into left side
        snake[[1]] <- c(x, snake[[1]][2])
      }else if(head[2] > y){
        # we have crashed the top side
        snake[[1]] <- c(snake[[1]][1], 0)
      }else if(head[2] < 0){
        # we have crashed the bottom
        snake[[1]] <- c(snake[[1]][1], y)
      }
    }
    
    
    cols <- c("gray", rep("white", length(snake)-1))
    sapply(1:length(snake), function(x){
      rx <- snake[[x]][1]
      ry <- snake[[x]][2]
      rect(rx-0.4, ry-0.4, rx+0.4, ry+0.4,col=cols[x])
    })

    return(snake)
    
  }
  generateFood <- function(snake, food){
    # we only create food if there is no food on the map
    if(is.null(food)){
      boolean <- T
      
      while(any(boolean)){
        rx <- floor(runif(n = 1,min = 0, max = x ))
        ry <- floor(runif(n = 1,min = 0, max = y ))
        food <- c(rx,ry)
        
        boolean <- sapply(1:length(snake), function(x){
          identical(food, snake[[x]])
        })
      }
      rect(rx-0.2, ry - 0.2, rx + 0.2, ry + 0.2, col = "red") 
    }
    
    return(food)
    
  }
  eatFood <- function(snake, food){
    if(identical(snake[[1]], food)){
      food <<- NULL
      score <<- score + 1
      
      tail <- tailAfterEating(snake)
      snake <<- append(snake, list(tail))
    }
    
    
  }
  tailAfterEating <- function(snake){
    # enough to look at the last two spots
    n <- length(snake)
    if(snake[[n]][1] == snake[[n-1]][1]){
      # they bot have the same x cordinate
      if(snake[[n]][2] - snake[[n-1]][2] > 0 ){
        # moving down
        tail <- c(snake[[n]][1], snake[[n]][2] +1 )
      }else{
        # moving up
        tail <- c(snake[[n]][1], snake[[n]][2] -1 )
      }
    }else{
      # they have the same y-coordinate
      if(snake[[n]][1] - snake[[n-1]][1] > 0 ){
        # moving to the left
        tail <- c(snake[[n]][1]+1, snake[[n]][2] )
      }else{
        # moving right
        tail <- c(snake[[n]][1]-1, snake[[n]][2] )
      }
      
    }
    
    return(tail)
    
  }
  crashReport <- function(isWall, snake){
    len <- length(snake)
    head <- c(snake[[1]][1], snake[[1]][2])
      # check if head has collided with body
      # head not included !
    crash <- any(sapply(2:len, function(x){
      identical(head, snake[[x]])
      }))
      
    # if the there is a wall wee need to check if
    # we have crashed
    if(isWall){
      if(head[1] > x){
        # we have crashed right side
        crash <- T
      }else if(head[1] < 0){
        #we have crashed into left side
        crash <- T
      }else if(head[2] > y){
        # we have crashed the top side
        crash <- T
      }else if(head[2] < 0){
        # we have crashed the bottom
        crash <- T
      }
    }
    
    return(crash)
  }
  keydown <- function(key){
    return(key)
  }
  onIdle <- function(){
    return("asdf")
  }
  
  # initialize data
  origin <- c(floor(x/2), floor(y/2))
  snake <- list(origin, c(origin[1]-1, origin[2]), c(origin[1]-2, origin[2]))
  food <- NULL
  score <- 0
  movements <- 0
  lastkey <- "Begin" # last direction
  # create the canvas
  plot(origin[1],origin[2],type="n", main="Snake Game", xlab="", ylab="", asp=1, axes=FALSE, xlim=c(-2,x+2), ylim=c(-2,y+2))
  rect(0-0.6,0-0.6,x+0.6,y+0.6)
  setGraphicsEventHandlers(onKeybd=keydown)
  drawsnake(movement = lastkey, snake = snake)
  
  crash <- F
  while(lastkey != "q" & !crash){
    lastkey <- getGraphicsEvent(onIdle = onIdle, onKeybd = keydown)
    movements <- movements + 1
    snake <- drawsnake(movement = lastkey, snake = snake)
    food <- generateFood(snake = snake, food = food)
    eatFood(snake = snake, food =food)
    crash <- crashReport(isWall = isWall, snake = snake)
  }
  if(crash){
    text(x = x/3.5, y = origin[2],"Game Over",pos=4, col = "red", cex= 2)
  }
  if(lastkey == "q"){
    text(x = x/3.5, y = origin[2],"User quit",pos=4, col = "blue", cex = 2)
  }
  
}

snake_game(isWall = T)
  


