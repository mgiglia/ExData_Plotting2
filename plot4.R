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

## plot4.R:

## 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

## melt the data.table with categories as id variables, and Emissions as the measure variables using the reshape2 package.
library(reshape2)

## retrieve the names of the category variables (note, they are all of the variables except for "Emissions")
id.vars.for.melt <- names(main[,!colnames(main) %in% c("Emissions"), with=FALSE])

## melt the data.table, set "Emissions" as the measure.vars.
main.melt <- melt(main, id=id.vars.for.melt, measure.vars = c("Emissions"))
head(main.melt)

names(main.melt)
comb <- main.melt[grepl("Combustion", main.melt$SCC.Level.One) | grepl("combustion", main.melt$SCC.Level.One),]
coal.comb <- comb[grepl("Coal", comb$SCC.Level.Three) | grepl("coal", comb$SCC.Level.Three) | grepl("Coal", comb$SCC.Level.Four) | grepl("coal", comb$SCC.Level.Four),]
coal.comb

coal.comb.tot.by.SCC.Level.One <- dcast(coal.comb, year+SCC.Level.One ~ variable, fun.aggregate = sum)
coal.comb.tot.by.SCC.Level.One

# log2 scaling of the y axis (with visually-equal spacing)
library(scales)     # Need the scales package
qplot(SCC.Level.One, value, data=coal.comb, geom=c("boxplot"), 
      fill=SCC.Level.One, main="Emissions from Coal Combustion by Year",
      xlab="", ylab="Emissions from Coal Combustion") + scale_y_continuous(trans=log2_trans()) + facet_grid(~year, margins=FALSE) + theme(legend.position="top") + labs(aes(fill="Year"))

##Set up initial aesthetics - comparison of SCC Level One Emissions
g <- ggplot(coal.comb, aes(as.factor(year), value))
## add boxplot, place legend at the top,
g <- g + geom_boxplot(aes(fill=SCC.Level.One)) + theme(legend.position="top")
## change y-asxis to log scale
g <- g + scale_y_continuous(trans=log1p_trans())
## titles and labels
g <- g + labs(fill="SCC Level One", title="Distribution of Coal Combustion Emissions \n By Source Classification Code Level One and Year", x="Year", y="Log(1 + Emissions (PM25-PRI))")

##Set up initial aesthetics - All Emissions
g2 <- ggplot(coal.comb, aes(value, fill=as.factor(year)))
## add density, place legend at the top, limit to values less than 0.05.
g2 <- g2 + geom_density(alpha=.3) + theme(legend.position="top") + xlim(-.005,.05)
## titles and labels
g2 <- g2 + labs(fill="Year", title="Density of Coal Combustion Emissions by Year", x="Emissions (PM25-PRI) Less Than 0.05")

## Set up initial aesthetics - scatter plot
g3 <- ggplot(coal.comb, aes(year, value)) + theme_bw()
## Scatter plot with jitter
g3 <- g3 + geom_jitter(aes(col=SCC.Level.Two), alpha=0.3)
## Add smoother
g3 <- g3 + geom_smooth(aes(color=SCC.Level.Two), linetype = 1, method = "lm", se = FALSE, size=2)
## fix x-axis so that it shows 1999, 2002, 2005, and 2008, drop y-axis down to highest density
g3 <- g3 + coord_cartesian(xlim=c(1996, 2011)) + scale_x_continuous(breaks=coal.comb$year) + theme(axis.text.x = element_text(color="dark gray"), axis.ticks.x = element_line(color="dark gray")) + ylim(0, 0.5)
## add labels
g3 <- g3 + labs(title="Jittered Scatter Plot and Annual Trend Lines for \n Coal Combustion Emissions \n by Source Classification Code Level Two", x="", y="Emissions (PM25-PRI)", color="SCC Level Two")

coal.comb.short.name <- dcast(coal.comb, Short.Name + SCC.Level.Two + year ~ variable, fun.aggregate = mean)
coal.comb.short.name

## set up initial aesthetics 
g4 <- ggplot(coal.comb.short.name, aes(x=year, y=Emissions, color=SCC.Level.Two, group=Short.Name))
## plot lines (one for each short.name) using log scale
g4 <- g4 + geom_line() + scale_y_continuous(trans=log2_trans())
## add labels
g4 <- g4 + labs(title="Mean Source Classification Emissions \n for Coal Combustion", x="", y="Natural Log of Emissions (PM25-PRI)", color="SCC Level Two")
## fix X axis
g4 <- g4 + coord_cartesian(xlim=c(1998, 2009)) + scale_x_continuous(breaks=coal.comb.short.name$year) + theme(axis.text.x = element_text(color="dark gray"), axis.ticks.x = element_line(color="dark gray"))

## print using multi-plot
png(file = "plot4.png", width = 1400, height = 720, units = "px")
multiplot(g, g2, g3, g4, cols=2)
dev.off()