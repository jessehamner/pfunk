library(rvest)

source("parliament.R")
byear <- 2020
btag <- read_html("https://en.wikipedia.org/wiki/Bundestag")

# Get the whatever-th table on the page:
diet <- btag %>%  html_nodes("table") %>% .[[3]] %>% html_table(fill = TRUE,
                                                                header = TRUE,
                                                                trim = TRUE)

# Filter for parties with representatives:
diet <- diet[which(diet[,9] != "0"),]
diet <- diet[which(diet[,1] == ""),]
diet <- diet[which(diet[,2] != ""),]

# The color list comes from the first cell in the table, hidden inside a
# <td bgcolor="#000000"</td> type of tag
colortest <- btag %>%  html_nodes("table") %>% .[[3]] %>% html_nodes("td")
color <- unlist(colortest[grepl('bgcolor', colortest)] %>% html_attrs())

pinfo <- diet[,c(2,9)]
names(pinfo) <- c("party", "repcount")
pinfo$repcount <- as.numeric(pinfo$repcount)
#keeps <- which(variable.names(replist) != "")
#cleaned <- replist[,keeps,drop=FALSE]
color <- as.character(color)
color[length(color)+1 ] <- "#ffffff"
pinfo <- cbind(pinfo, color, stringsAsFactors = FALSE)
membercount <- sum(pinfo$repcount)
partylist <- unique(pinfo$party)

# Red, Blue, Yellow, White:
# partycolor <- as.vector( c("#bb0000", "#0000de", "#f0f000", "#ffffff"), mode="character")

pinfo$frac <- 0
pinfo$remaining <- 0
pinfo$mbrtotal <- membercount
pinfo$frac <- pinfo$repcount / pinfo$mbrtotal
pinfo$remaining <- pinfo$repcount
pinfo$count <- pinfo$repcount

# Super fragile, yes, but Wikipedia...
pinfo$Shortname <- gsub('\\[\\w\\]', '', gsub('.*.?\\((.*.?)\\)', '\\1', pinfo$party))

ballcount <- membercount # count of representatives or members
r <- 3
shells <- 0

balldiameter <- 0.85
shells <- makeProportionalShells(ballcount, r, balldiameter, ballspacing, proportion)

############################################################
# Compute positions of each ball:
pos <- computePositions(ballcount, shells, balldiameter)

partyorder <- orderPartiesForPrinting(pinfo) 
# (largest, third largest, fourth largest, ... smallest, second largest)
# pinfo$remaining = pinfo$repcount  
# a reset; did this above, but while testing, "belt & suspenders" applies.

pinfo2 <- pinfo[partyorder,]

pos <- determineColors(pos, shells, pinfo2, pinfo2$repcount)
stopifnot (nrow(pos) == sum(pinfo2$repcount)) # watch those off-by-one errors


PNGparliamentdiagram(pos,
                     shells,
                     ballcount,
                     pngtitle = sprintf("Bundestag%g", byear),
                     fontsize = 14,
                     graphics = "quartz",
                     outline = F, 
                     boxsize = c(8, 8/goldenratio),
                     cexval = 1.5,
                     yheight = 1
)
