library(rvest)

source("parliament.R")

# basic parameters:
balldiameter <- 0.95 # aesthetics
ballspacing <- 1.0 # also aesthetics, but 1.0 is the best choice.

ballsum <-(0)
proportion <- 1/3 # aesthetics; maybe I should try phi? might make Tufte happy.
goldenratio <- (1 + sqrt(5))/2
proportion <- 2 - goldenratio
fontsize=14

r <- 3
shell <- r
#  shells <- data.frame(x=0,y=0, linecolor="#000000", fillcolor="#ffffff")
shells <-(0)



plink <-"http://en.wikipedia.org/wiki/List_of_MPs_elected_in_the_United_Kingdom_general_election,_2010"
commons <- html(plink)
commonsmps <- commons %>% html_nodes("table") %>% .[[4]] %>% html_table()
keeps <- which(variable.names(commonsmps) != "")
cleanedcommons <- commonsmps[,keeps, drop=F]
britcolors1 <- html("http://en.wikipedia.org/wiki/Wikipedia:Index_of_United_Kingdom_political_parties_meta_attributes")
colortable <- britcolors1 %>% html_nodes("table") %>% .[[1]] %>% html_table()
partycounts <- commons %>% html_nodes("table") %>% .[[3]] %>% html_table(fill=T)
partycounts <- partycounts[-(which(is.na(partycounts[,2]))),] # pull out the NAs in the list of parties
names(partycounts) <- c("Delete", "Shortname","2010", "now")
keeps<- c(2,3,4)
mergeparliament <- merge(partycounts, colortable, by="Shortname")
mergeparliament <- mergeparliament[-c(2,8,6),]
mergeparliament <- mergeparliament[,-5]


ballcount <- sum(mergeparliament$now)

############################################################
# Establish (via iteration) the relative size of the 
# diagram, in "shells" of width (ballspacing):
shells <- populateShells(r, ballcount, balldiameter, ballspacing)
while(r < (nrow(shells) * proportion)) {
  r <- r + 1
  shells <- populateShells(r, ballcount, balldiameter, ballspacing)
}


mergeparliament$mbrtotal <- sum(mergeparliament$now)
mergeparliament$frac <- mergeparliament$now/mergeparliament$mbrtotal
mergeparliament$remaining <- mergeparliament$now

colnames <- names(mergeparliament)
colnames[which(names(mergeparliament)=="Colour")] <- "color"
names(mergeparliament) <- colnames
#mergeparliament$linecolor <- "#000000"

pos <- (0)
pos <- computePositions(ballcount, shells,balldiameter)

partyorder <- orderPFP(mergeparliament$now) # (largest, smallest, second smallest ... second largest)
#counter <- (1) # reset
mergeparliament$remaining=mergeparliament$now # reset; did this above, but while testing, "belt & suspenders" applies.

pos <- determineColors(pos,shells,mergeparliament, mergeparliament$now)
stopifnot (nrow(pos) == sum(mergeparliament$now)) # watch those off-by-one errors


cexvalue <- 1.7

PNGparliamentdiagram(pos,shells,ballcount,pngtitle="UKCommons2015",fontsize=14,graphics="quartz",outline=F, boxsize=c(8, 8/goldenratio), cexval=cexvalue, yheight=2)
SVGparliamentdiagram(pos,shells,ballcount,svgtitle="UKCommons2015",fontsize=14,graphics="quartz",outline=F, boxsize=c(8, 8/goldenratio), cexval=cexvalue, yheight=2)

