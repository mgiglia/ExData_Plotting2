## In project 2 folder run setupwd.R to set working directory.
## Check that "exdata-data-NEI_data" is a a folder in your working directory.
"exdata-data-NEI_data" %in% dir()

## If the above returns TRUE then the appropriate working directory has been established.

## The folder "exdata-data-NEI_data" contains two files:
## summarySCC_PM25.rds
## Source_Classification_Code.rds

## M2.5 Emissions Data (summarySCC_PM25.rds): This file contains a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year.

##fips:      A five-digit number (represented as a string) indicating the U.S. county
##SCC:       The name of the source as indicated by a digit string (see source code classification table)
##Pollutant: A string indicating the pollutant
##Emissions: Amount of PM2.5 emitted, in tons
##type:      The type of source (point, non-point, on-road, or non-road)
##year:      The year of emissions recorded

## Source Classification Code Table (Source_Classification_Code.rds): This table provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source. The sources are categorized in a few different ways from more general to more specific and you may choose to explore whatever categories you think are most useful. For example, source “10100101” is known as “Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal”.

library(grid)
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      library(grid)
      
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      
      numPlots = length(plots)
      
      # If layout is NULL, then use 'cols' to determine layout
      if (is.null(layout)) {
            # Make the panel
            # ncol: Number of columns of plots
            # nrow: Number of rows needed, calculated from # of cols
            layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                             ncol = cols, nrow = ceiling(numPlots/cols))
      }
      
      if (numPlots==1) {
            print(plots[[1]])
            
      } else {
            # Set up the page
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
            
            # Make each plot, in the correct location
            for (i in 1:numPlots) {
                  # Get the i,j matrix positions of the regions that contain this subplot
                  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                  
                  print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                  layout.pos.col = matchidx$col))
            }
      }
}

## Read in the data sets.
## Please note that it may take a few minutes to bring the data into R.
NEI <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds") ## 6,497,651 obs. of 6 variables
SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds") ##11,717 obs of 15 variables
?readRDS

str(NEI)
summary(NEI)
head(NEI)

str(SCC)
summary(SCC)
head(SCC)

## NEI and SCC join on the SCC attribute.  Set up keys using data.table.
library(data.table)
NEI <- data.table(NEI)
setkeyv(NEI, c("fips","SCC","Pollutant","Emissions","type","year"))
SCC <- data.table(SCC)
setkey(SCC, SCC)

## Check keys
tables()

## Merge the two sets together into one called "main"
main <- merge(NEI, SCC, by=c("SCC"), all.x=TRUE)  ## 6,497,651 obs of 20 variables
?merge
## Assignment

## The overall goal of this assignment is to explore the National Emissions Inventory database and see what it say about fine particulate matter pollution in the United states over the 10-year period 1999–2008. You may use any R package you want to support your analysis.

## Questions

## You must address the following questions and tasks in your exploratory analysis. For each question/task you will need to make a single plot. Unless specified, you can use any plotting system in R to make your plot.

#############################################

## plot3.R:

## 3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

## melt the data.table with categories as id variables, and Emissions as the measure variables using the reshape2 package.
library(reshape2)

## retrieve the names of the category variables (note, they are all of the variables except for "Emissions")
id.vars.for.melt <- names(main[,!colnames(main) %in% c("Emissions"), with=FALSE])

## melt the data.table, set "Emissions" as the measure.vars.
main.melt <- melt(main, id=id.vars.for.melt, measure.vars = c("Emissions"))
head(main.melt)

##note-- use same code above to get main.melt then:
?subset
main.melt.Baltimore <- subset(main.melt, fips == "24510")
## Note, use the same code to get to main.melt.Baltimore then:

##recast with year and type for sum of emissions.
emiss.by.type.year.Balt <- dcast(main.melt.Baltimore, year+type ~ variable, fun.aggregate = sum)
emiss.by.type.year.Balt

## load the ggplot2 package
library(ggplot2)
## set up the initial plot with year and Emissions as aesethics
g <- ggplot(emiss.by.type.year.Balt, aes(year, Emissions))
## add points, change to white background
g <- g + geom_point(aes(color=type, pch=type), size = 4) + theme_bw() + theme(legend.position="top")
## add smooth with linear model method
g <- g + geom_smooth(aes(color=type), linetype = 2, method = "lm") ##, se = FALSE
## break out by type (margins = FALSE surpresses a breakout by "all")
g <- g + facet_grid(~type, margins=FALSE)
## add a title and x, y, and legend labels
g <- g + labs(title="Total Emissions by Year and Source in Baltimore", x="Year", y="Total Emissions (PM25-PRI)", color="Source Type", pch="Source Type")
## fix x-axis so that it shows 1999, 2002, 2005, and 2008
g <- g + coord_cartesian(xlim=c(1998, 2009)) + scale_x_continuous(breaks=emiss.by.type.year.Balt$year) + theme(axis.text.x = element_text(color="dark gray"), axis.ticks.x = element_line(color="dark gray"))

## print the plot:
png(file = "plot3.png", width = 1400, height = 720, units = "px")
g
dev.off()