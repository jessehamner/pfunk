# R functions to make parliament diagrams
# These are really verbose but do a nice job, and there's no external calls 
# except to the (shape) library (and that's mostly optional)
#
#

# There's a related python script to do something similar to this:
# https://github.com/slashme/parliamentdiagram/blob/master/newarch.py
# 
# But I didn't use or port any of his code. 

require(shape)

# TODO:
# for sets with many (four or more?) 1- or 2-member parties, 
# cut the set of parties with three or fewer members into two sets
# of approximately half each. 
# Then submit both sets (or one matrix, or whatever) to the apportionThisRow
# function, so that it can use set 1 for row 1, and set 2 for row 2, and ignore
# any subsequent rows in the apportionment process. 
#
# Consider adding a background image (via SVG or a cropped PNG); this would require
# a concave hull around the (guaranteed contiguous!) set of balls for any one party,
# and R only provides for convex hulls. There is a javascript codebase to make one,
# however, and porting it might not be terrible.


# Some defaults, for aesthetics in the plot:
balldiameter <- 0.95            
ballspacing <- 1.0              

ballsum <-(0)
proportion <- 1/3               # proportion of inner radius (empty) to outer radius
goldenratio <- (1 + sqrt(5))/2  # maybe I should try phi? might make Tufte happy.
proportion <- 2 - goldenratio   # There, that's better.
fontsize=14


circlesPerShell <- function (shell, circlediameter, radiusmultiplier) {
  A <- floor(pi * shell * radiusmultiplier / circlediameter)
  B <- (pi * shell * radiusmultiplier / circlediameter) %% 1
  theta <- getTheta(shell * radiusmultiplier, circlediameter) # IN RADIANS
  return (list(A, B, theta))
}


getTheta <- function(radius, circlediameter) {
  theta <- asin(circlediameter * 0.5 /radius)
  return(theta) # IN RADIANS
}


getPhi <- function(balls, theta){
  radlength <- pi - (2 * theta)
  phi <- radlength / (balls - 1)
  return (phi) # In RADIANS
}


popOneAndRecompute <- function(row, frame, circlediameter) {
  if (frame[row,]$balls > 2 ) {
    frame[row,]$balls <- (frame[row,]$balls - 1)
    balls <- frame[row,]$balls
    theta <- getTheta(row, circlediameter) # IN RADIANS
    radlength <- pi - 2 * theta
    phi <- getPhi(balls,theta)
    frame[row,]$arcsep <- phi
  }
  
  print(frame)
  return(frame)
}

#   shell balls      frac      theta    arcsep
# 4 4       9   0.2277585 0.11903088 0.3629414


# figure the position of one ball:
ballPosition <- function (radius, balldiameter, position, theta, arcsep) {
  
  # First ball center is only at the theta angle, which *should* put the bottom 
  # flush with (y=0)
  angle <- (position - 1) * arcsep + theta
  
  # We'll go counter-clockwise from 0 radians
  x <- radius * cos(angle) 
  y <- radius * sin(angle)
  
  return(list(x,y) )
}


# Here's the logic:
# largest group 
# third largest group
# next smallest group
# ...
# second largest group
# helps contiguity of small parties where many parties exist. Shouldn't affect two party systems at all
orderPartiesForPrinting <- function (parties) {
  vect <- (c(which.max(parties$count), order(parties$count)))
  vect[length(vect)] <- NA
  vect <- vect[1:(length(vect) - 1)]   
  return(vect)  
}


orderPFP <- function(somevector) {
  v <- as.vector(somevector)
  vect <- as.vector(order(v, decreasing=T))
  vect[length(v)+1] <- vect[2]
  vect <- vect[-2]
  return(vect)  
}


#     skip if parties$remaining is zero
#     IF shell is greater than parties$remaining, THEN print one on this line no matter what, and decrement "remaining". 
#     ELSE IF shell is NOT greater than parties[party,]$remaining THEN 
#       get a naive count of circles to print for the party, on this line, as a proportion of their party
#       add the fraction overflow from the previous line
#       figure up a floor + fraction for the party, for this line
#       write the fraction overflow back to parties[party,]$frac

########################################################
# apportions percentages into integer ball counts:


thisRowMatrix <- function(shell,ballframe,partyframe){ 
  # create temporary matrix from the count of parties:  
  tempm <- matrix(data=0, ncol=3, nrow=nrow(partyframe))
  
  # partyframe ("parties" data frame) won't be dynamically updated within this function. 
  # As a result, we need to account for the changes locally.
  remaining <- as.vector(partyframe$remaining, mode="numeric")
  
  # how many balls in this row?
  ballcount <- ballframe[shell,]$balls
  
  # cycle through each row of the temporary matrix of parties:
  for (i in 1:nrow(tempm)) {
    # if this party has no more remaining slots, *before doing anything*, skip:    
    if (partyframe[i,]$remaining == 0) { next }
    
    # Compute the expected proportion of parties      
    #   tempm[i] <- as.vector(c(partyframe[i,]$frac * ballcount, floor(tempm[i,1]), (tempm[i,1]) %% 1))
    tempm[i,1] <- partyframe[i,]$frac * ballcount
    tempm[i,2] <- floor(tempm[i,1])
    tempm[i,3] <- (tempm[i,1]) %% 1
    
    # However, in order to avoid a non-contiguous grouping of small parties,
    # if there's fewer remaining slots than shells remaining to fill,
    # then print a ball this frame no matter what.
    if (partyframe[i,]$remaining < nrow(ballframe) - shell ) {
      tempm[i,2] <- 1
      partyframe[i,]$remaining <- partyframe[i,]$remaining - 1
      tempm[i,3] <- 0
      # pull the forced-print count from remaining as done for tempm:
      remaining[i] <- remaining[i] - 1
    }
  }
  
  print(tempm)
  flag=TRUE
  apportionedcount <- sum(tempm[,2])
  while(flag) {
    if (ballcount == apportionedcount) { flag=F }
    if (ballcount < apportionedcount) { # shouldn't happen
      print ("Row ball count is smaller than the amount allocated by the algorithm")
      pickme <- which.max(tempm[,2]) # pull one from the largest block for this arc
      tempm[pickme,3] <- (tempm[pickme,3] + 1)
      tempm[pickme,2] <- (tempm[pickme,2] - 1)
      apportionedcount <- (sum(tempm[,2]) )
      if (ballcount == apportionedcount) { flag=F }
    }
    
    # under-apportioned because of floor(), most likely outcome:
    while(ballcount > apportionedcount){
      print ("Row ball count is larger than the amount already allocated by the algorithm")
      pickme <- which.max(tempm[,3]) # pull one from the largest fraction for this arc
      print(pickme)
      tempm[pickme,2] <- tempm[pickme,2] + 1
      tempm[pickme,3] <- tempm[pickme,3] - 1
      stopifnot (tempm[pickme,3] > -2 )
      print(tempm)
      apportionedcount <- sum(tempm[,2])
      print(apportionedcount)
      if (ballcount == apportionedcount) { 
        print ("FIXED!")
        flag=F 
      }
    }
  }
  
  #  print(tempm)
  return(tempm)
}

apportionThisRow <- function(shell, ballframe, partyframe) {
  balltotal <- (ballframe[shell,]$balls) # redundant
  if (balltotal == 0 ) { return (0)  }
  
  # may have to adjust the actual ball count on each row for each party positively or negatively
  # (thus, the carry fraction might be negative, or positive and greater than one)
  thisrowm <- thisRowMatrix(shell, ballframe, partyframe)
  
  # now we know how to apportion them for this row.
  #  print(thisrowm)
  return (thisrowm)
}

# Populate shells:
populateShells <- function(r, ballcount, balldiameter, radiusmultiplier) {
  shells <- data.frame(shell=1:r, balls=0, frac=0, theta=0, arcsep=0)
  ballshellcount <- (0)  
  flag <- TRUE
  while (flag) {
    tempcount <- sum(shells$balls) 
    thisshell <- circlesPerShell(r, balldiameter, radiusmultiplier)
    cshell <- (r)
    ballshellcount <- thisshell[[1]]
    tempcount <- (tempcount + ballshellcount)
    frac <- thisshell[[2]]
    theta <- thisshell[[3]]
    arcsep <- getPhi(ballshellcount, theta)
    shells[r,1:5] <- as.vector(c(r, ballshellcount, frac, theta, arcsep))
    
    while(tempcount  > ballcount) {
      message("Will exceed the count with this shell; will pop one and recompute.")
      shells <- popOneAndRecompute(cshell, shells, balldiameter)
      #     print(shells)
      #    shells[cshell,1:5] <- as.vector(c(cshell,ballshellcount,frac,theta,arcsep))
      tempcount <- sum(shells$balls)
      message(sprintf('cshell count: %s; Temp count for this row: %s', cshell, tempcount))
      cshell <- cshell - 1
      if(cshell < 1) { cshell <- (r) }
      ballsum <- tempcount
      print(ballsum)  
    }
    tempcount <- (ballsum + ballshellcount)  
    ballsum <- ballsum + ballshellcount
    r <- r + 1
    #  print(r)
    #  cat(c("shell: ", r), sep="" ) 
    #  print(ballsum)
    #  flush.console()
    #  print(frac)
    #  print(theta)
    if(sum(shells$balls) == ballcount) { flag <- FALSE }
  }
  
  return(shells)
}

computePositions <- function(ballcount,shells,balldiameter) {
  
  pos <- data.frame(x=0,y=1:ballcount, linecolor="#000000", fillcolor="#ffffff", stringsAsFactors=FALSE )
  counter <- (1)
  
  for (i in 1:(nrow(shells))) {
    print(i)
    if (shells[i,]$balls==0) { next }
    for (j in 1:shells[i,]$balls) {
      listxy <- ballPosition(i, balldiameter, j, shells[i,]$theta, shells[i,]$arcsep)
      pos[counter,]$x <- listxy[[1]]
      pos[counter,]$y <- listxy[[2]]
      if (counter < nrow(pos) ) {
        counter <- ( counter + 1 )
      } else {
        break
      }
    } 
  } 
  return(pos)
}

plotParliamentDiagram <- function(pos,shells,labeltext, cexval, yheight) {
  if (missing(cexval)){ 
    cexval <- 1.5
  }
  
  if (missing(yheight)) { yheight <- (2)}
  
  par(xaxs="r", 
      yaxs="i", 
      mar=c(0,0,0,0), 
      oma=c(0,0,0,0)
      #     omd=c(1,1,1,1)
  )
  plot(pos$x, pos$y, 
       pch=21, 
       cex=cexval, 
       axes=F, 
       xlab="", 
       ylab="", 
       lwd=1.0, 
       bg=pos$fillcolor, 
       col=pos$linecolor, 
       xlim=c(-nrow(shells),nrow(shells)), 
       ylim=c(0,nrow(shells)),
       asp=1
  )
  text(0,yheight,labeltext, cex=4, font=2)
  return(1)
}


determineColors <- function(pos,
                            shells,
                            parties,
                            partycountvector) {
  partyorder <- orderPFP(partycountvector) # (largest, third largest, fourth largest, ... smallest, second largest)
  remaining <- as.vector(parties$remaining, mode = "numeric")
  counter <- 1
  # Now, by row, assign the proportion of each party to the count of cells in this matrix row.
  # For fractions, add and carry until they reach the next whole number.
  
  # # change the "remaining" values based on the returned matrix from apportionThisRow
  #       starting from counter(), color the next floor circles with party's color
  #       decrement parties[party,]$remaining by floor
  
  for (i in 1:(nrow(shells))) {
    print(i)
    
    if (shells[i,]$balls == 0) { 
      print("balls is zero")
      next 
    }
    
    tmpmt <- apportionThisRow(i, shells, parties)
    #    print(tmpmt)
    
    # need to make changes to parties first:
    # FIXME is this order guaranteed to be correct?
    parties$remaining <- parties$remaining - tmpmt[,2]
    
    print(shells[i,]$balls)
    
    # This HAS TO BE TRUE:
    stopifnot(shells[i,]$balls == sum(tmpmt[,2])) 
    # print ("AIEEEE you can't do that -- the computed balls on this shell 
    # is NOT the same as the count in the shells data frame.")
    
    for (j in partyorder) {
      thispartyballcount <- tmpmt[j, 2]
      print(paste(parties[j,]$color, parties[j,]$Shortname, sep=""))
      pos[c(counter:(counter + thispartyballcount)),]$fillcolor <- parties[j,]$color
      counter <- ( counter + thispartyballcount )
    }
    print(counter)
    
  }
  
  pos <- pos[-counter,] # this fixes an off-by-one error that is caused by starting the list at 1
  return(pos)
}


PNGparliamentdiagram <- function(pos,
                                 shells,
                                 ballcount,
                                 pngtitle,
                                 yheight,
                                 fontsize = 14,
                                 graphics = "quartz",
                                 outline = FALSE,
                                 boxsize = c(8,5),
                                 cexval = 1.5
                                 ) {

  png(filename=paste(pngtitle,"parliamentdiagram",".png", sep=""), 
      res=300, 
      bg="white", 
      type=, 
      pointsize=fontsize, 
      width=boxsize[1], 
      height=boxsize[2], 
      units="in"
  )
  
  plotParliamentDiagram(pos,shells,ballcount, cexval, yheight)
  if (outline==T){
    drawOutline(shells)
  }
  
  dev.off()
  return(1)
} 

# May need to handle these somewhere: FIXME
#plot.new()
#emptyplot()

drawOutline <- function(shells){
  require(shape)
  x1 <- -nrow(shells)-1
  x2 <- nrow(shells) +1
  y1=-0.5
  
  plotellipse(rx=(nrow(shells)+1), ry=(nrow(shells)+1), from=0, to=pi, mid=c(0,0))
  lines(x=c(x1,x2), y=c(y1,y1), lwd=2)
  lines(x=c(x1,x1),y=c(0,y1), lwd=2)
  lines(x=c(x2,x2),y=c(0,y1), lwd=2)
  return(1)
  
}

SVGparliamentdiagram <- function(pos,
                                 shells,
                                 ballcount,
                                 svgtitle,
                                 yheight,
                                 fontsize = 14,
                                 graphics = "quartz",
                                 outline = FALSE,
                                 boxsize = c(8,5),
                                 cexval = 1.5
                                 ) {

  svg(filename=paste(svgtitle,"parliamentdiagram",".svg", sep=""),
      width = boxsize[1], 
      height = boxsize[2], 
      pointsize = fontsize,
      bg="white",
      antialias = c("default"))
  
  plotParliamentDiagram(pos,shells,ballcount, cexval, yheight)
  
  if (outline==T){
    drawOutline(shells)
  }
  
  dev.off()
  
  return(1)
}


makeProportionalShells <- function(ballcount,
                                   r = 3,
                                   balldiameter = 0.95,
                                   ballspacing = 1,
                                   proportion = 0.381966) { #  (2 - phi)

  shells <- 0
  shells <- populateShells(r, ballcount, balldiameter, ballspacing)
  while(r < (nrow(shells) * proportion)) {
    r <- r + 1
    shells <- populateShells(r, ballcount, balldiameter, ballspacing)
  }
  return(shells)
}
