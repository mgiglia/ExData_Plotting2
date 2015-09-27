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

## plot1.R:

## 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

## Check distcint Polluant values.
table(main$Pollutant)
## Shows that all Pollutant only takes one value: "PM25-PRI"

## melt the data.table with categories as id variables, and Emissions as the measure variables using the reshape2 package.
library(reshape2)

## retrieve the names of the category variables (note, they are all of the variables except for "Emissions")
id.vars.for.melt <- names(main[,!colnames(main) %in% c("Emissions"), with=FALSE])

## melt the data.table, set "Emissions" as the measure.vars.
main.melt <- melt(main, id=id.vars.for.melt, measure.vars = c("Emissions"))
head(main.melt)

## recast with the sum of "Emissions" for the year.
tot.emissions.by.year <- dcast(main.melt, year ~ variable, fun.aggregate = sum)
tot.emissions.by.year

png(file = "plot1.png", width = 1400, height = 720, units = "px")
with(tot.emissions.by.year, 
     plot(year, Emissions
          ,xlab = "Year"
          ,ylab = "Total Emissions (PM25-PRI)"
          ,pch = 19
          ,ylim = c(0,8e+06)
          ,xlim = c(1999,2008)
          ,xaxt = "n"))
axis(side = 1, at=tot.emissions.by.year$year, labels = tot.emissions.by.year$year)
with(tot.emissions.by.year, abline(lm(Emissions ~ year), lty=2, col="red"))
legend("topright"
       ,col = c("red")
       ,lty = 2
       ##,xjust = .5
       ,legend = c("Trend Line"))
title(main="Total Emissions by Year", sub="Emissions from All Sources")
dev.off()
