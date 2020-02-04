#*************************************Required Libraries******************************************
require(plyr)
require(grid)
require(reshape2)
require(stringr)
require(ggplot2)
# require(logging)
# debug(VariableNumericalFormat)
#*************************************Options*****************************************************
options(error=recover)
options(warn=1)
# basicConfig()
# logdebug("not shown, basic is INFO")
# logwarn("shown and timestamped")

# system("defaults write org.R-project.R force.LANG en_US.UTF-8")
# debug("CreateCSV")

# debug(apply_lookups)
# debug(CreateDuration)
#*************************************Lookup Files*****************************************************
Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
# Path<-"~\\FPDS\\R scripts and data\\"
# Path<-"C:\\Users\\Greg Sanders\\SkyDrive\\Documents\\R Scripts and Data SkyDrive\\"



source(paste(Path,"helper.r",sep=""))
source(paste(Path,"lookups.r",sep=""))
source(paste(Path,"helper.r",sep=""))
source(paste(Path,"statistics_aggregators.r",sep=""))
source(paste(Path,"create_procedural_graphs.r",sep=""))

setwd("K:\\Development\\Finance")

# debug(create_procedural_graphs)

create_procedural_graphs("Finance","Overall")

