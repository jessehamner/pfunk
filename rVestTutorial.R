library(rvest)
require(shape)

source("parliament.R")

# rVestTutorial.R : get the data for the US Congress, 
# then make two parliament diagrams automagically.
# This code creates both SVG and PNG versions for each chamber.
# 
# Jesse Hamner
# jhamner@gmail.com
# V1.0, March 2015


################ Main #################

house <- html("http://en.wikipedia.org/wiki/Current_members_of_the_United_States_House_of_Representatives")

# Get the sixth table on the page:
replist <- house %>%
html_nodes("table") %>%
.[[6]] %>% html_table()

# The Minnesota Democrats are listed as "DFL"
replist$Party[which(replist$Party=="DFL")] <- "Democratic"
keeps <- which(variable.names(replist) != "")
cleaned <- replist[,keeps,drop=FALSE]
membercount <- nrow(cleaned)
partylist<-unique(replist$Party)

# Red, Blue, Yellow, White:
partycolors <- as.vector( c("#bb0000", "#0000de", "#f0f000", "#ffffff"), mode="character")

parties <- data.frame(party=partylist, 
                      count=(0),
                      mbrtotal=(membercount),
                      color=partycolors[1:length(partylist)],
                      frac=(0),
                      remaining=(0),
                      stringsAsFactors = FALSE
                      )

# parties$party <- factor(parties$party)

for (p in partylist){
  parties[which(parties$party==p),]$count <- length(which(cleaned$Party==p))
}
parties$frac=parties$count/parties$mbrtotal
parties$remaining=parties$count
pngtitle <- "GRAPHIC"

########################################################################################################
########              Parliament diagrams don't always look like "your" parliament.         ############
########      This type is a hollowed-out semicircle, more like the Wikipedia diagrams      ############
########                    of the United States Congress House and Senate                  ############
########################################################################################################

# basic parameters:
balldiameter <- 0.95 # aesthetics
ballspacing <- 1.0 # also aesthetics, but 1.0 is the best choice.
ballcount <- nrow(replist) # count of representatives or members
ballsum <-(0)
proportion <- 1/3 # aesthetics; maybe I should try phi? might make Tufte happy.
goldenratio <- (1 + sqrt(5))/2
proportion <- 2 - goldenratio
fontsize=14

#  shells <- data.frame(x=0,y=0, linecolor="#000000", fillcolor="#ffffff")
r <- (3)
shells <-(0)

############################################################
# Establish (via iteration) the relative size of the 
# diagram, in "shells" of width (ballspacing):
shells <- makeProportionalShells(r, ballcount, balldiameter, ballspacing, proportion)

############################################################
# Compute positions of each ball:
pos <- computePositions(ballcount, shells,balldiameter)

############################################################
# compute colors of each ball:

# Problem 1. Widows & orphans. If we use the fractional addition to 
# determine the next placement of a ball for a very small proportion of
# the set (say, vacant seats, the balls in that group will not be contiguous
# Problem 2. Counting down versus up. 
# Problem 3. The relative lack of elegance in a two-dimensional
# set of observations. 

# This isn't very elegant, but it works:
partyorder <- orderPartiesForPrinting(parties) # # (largest, third largest, fourth largest, ... smallest, second largest)
# counter <- (1) # reset
parties$remaining=parties$count # reset; did this above, but while testing, "belt & suspenders" applies.

############################################################
# Now determine the colors for each ball in each row according to the 
# proportions set out above.
pos <- determineColors(pos,shells,parties, parties$count)
stopifnot (nrow(pos) == sum(parties$count)) # watch those off-by-one errors


PNGparliamentdiagram(pos,
                     shells,
                     ballcount,
                     pngtitle="USHouse2015",
                     fontsize=14,
                     graphics="quartz",
                     outline=F, 
                     boxsize=c(8, 8/goldenratio),
                     cexval=2.0,
                     yheight=2
                     )

SVGparliamentdiagram(pos,
                     shells,
                     ballcount,
                     svgtitle="USHouse2015",
                     fontsize=14,
                     graphics="quartz",
                     outline=F, 
                     boxsize=c(8, 8/goldenratio),
                     cexval=2.0,
                     yheight=2
                     )


# OK, now for the Senate:

# senate <- html("http://en.wikipedia.org/wiki/United_States_Senate")
senate <- html("http://en.wikipedia.org/wiki/List_of_current_United_States_Senators")
senlist <- senate %>%
  html_nodes("table") %>%
  .[[6]] %>% html_table() # 2 is the short table, 6 is the long table

# The Minnesota Democrats are listed as "DFL"
senlist$Party[which(senlist$Party=="DFL")] <- "Democratic"
senlist$Party[which(senlist$Party=="Democratic-Farmer-Labor")] <- "Democratic"
keeps <- which(variable.names(senlist) != "")
cleaned <- senlist[,keeps,drop=FALSE]
membercount <- nrow(cleaned)
partylist<-unique(senlist$Party)


# Red, Blue, Yellow, Yellow, White:
partycolors <- as.vector( c("#bb0000", "#0000de", "#f0f000", "#f0f000", "#ffffff"), mode="character")

parties <- data.frame(party=partylist, 
                      count=(0),
                      mbrtotal=(membercount),
                      color=partycolors[1:length(partylist)],
                      frac=(0),
                      remaining=(0),
                      stringsAsFactors = FALSE
)

for (p in partylist){
  parties[which(parties$party==p),]$count <- length(which(cleaned$Party==p))
}
parties$frac=parties$count/parties$mbrtotal
parties$remaining=parties$count
pngtitle <- "GRAPHIC"

ballcount <- nrow(senlist) # count of representatives or members
r <- (3)
shells <-(0)

############################################################
# Establish (via iteration) the relative size of the 
# diagram, in "shells" of width (ballspacing):
shells <- makeProportionalShells(r, ballcount, balldiameter, ballspacing, proportion)

############################################################
# Compute positions of each ball:
pos <- computePositions(ballcount, shells,balldiameter)

############################################################

# This isn't very elegant, but it works:
partyorder <- orderPartiesForPrinting(parties) # # (largest, third largest, fourth largest, ... smallest, second largest)
# counter <- (1) # reset
parties$remaining=parties$count # reset; did this above, but while testing, "belt & suspenders" applies.

############################################################
# Now determine the colors for each ball in each row according to the 
# proportions set out above.
pos <- determineColors(pos,shells,parties, parties$count)
stopifnot (nrow(pos) == sum(parties$count)) # watch those off-by-one errors

PNGparliamentdiagram(pos,
                     shells,
                     ballcount,
                     pngtitle="USSenate2015",
                     fontsize=18,
                     graphics="quartz",
                     outline=F, 
                     boxsize=c(8, 8/goldenratio),
                     cexval=3.2,
                     yheight=1
)

SVGparliamentdiagram(pos,
                     shells,
                     ballcount,
                     svgtitle="USSenate2015",
                     fontsize=18,
                     graphics="quartz",
                     outline=F, 
                     boxsize=c(8, 8/goldenratio),
                     cexval=3.2,
                     yheight=1
)

# EOF
