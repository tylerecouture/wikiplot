library(reshape)
library(RCurl)
library(XML)
library(stringr)

# PARAMETERS FOR PLOT GENERATION:

# How many nearest data points to use for the local fit 
num_data_points = 30


theurl <- getURL("https://en.wikipedia.org/wiki/Opinion_polling_in_the_43rd_Canadian_federal_election", ssl.verifyPeer=FALSE)
tables <- readHTMLTable(theurl)

#get the nth tables on the page
df <- tables[[1]]

# Remove empty rows (wikipedia tables sometime use empty rows for spacing)
df <- df[!apply(df == "", 1, all),]
# Remove all non-standard rows, they will have <NA> values in them 
# (e.g Rows that are comments about leadership changes) 
df <- df[complete.cases(df),]

df #print resulting table to console for debugging

#get columns 1-2 and 4-9.  Col 3 is a link to poll source
df <- df[1:nrow(df), c(1:2, 4:10) ]

#add column headings
c.names <- c("Firm", "Date", "Liberal", "Conservative", "NDP", "BQ", "Green", "Error", "Sample_Size")
names(df) <- c.names

#print resulting table to console for debugging
df

# Calculate LOESS smoothing parameter [alpha] from number of polls [nrow(df)] 
# so that plot uses the same number of datapoints for the local fit even as new polls are added.
alpha <- num_data_points / nrow(df) 
print(alpha)

# DATE FORMATTING
# format Date column
df$Date = as.Date(substr(df[, 2], 9, 18))

#as.numeric gives date since 1970-01-01
# add days from 1900-01-01 to match spreadsheet data, +1 for 1970-01-01 day, and +1 for leapyear bug in 1900
df$Date = as.numeric(df$Date) - as.numeric(as.Date('1900-01-01')) +2

# SAMPLE SIZES
## EXTRACTING ROLLING POLL INFO
# Samples sizes are given as e.g.: 1000 (1/4), need to multiple sample size by fraction.

# Get number in parentheses
Rolling_Poll <- str_extract(df$Sample_Size, "(?<=\\().*(?=\\))")
# convert fraction text "1/4" to decimal number 0.25
Rolling_Poll = sapply(Rolling_Poll, function(x) eval(parse(text=x)))
# change NA to 1
Rolling_Poll[is.na(Rolling_Poll)] <- 1


# Extract sample size
df$Sample_Size = gsub(",", "", unlist(df$Sample_Size)) # remove commas in numbers
df$Sample_Size = sub("^$", "99999999", df$Sample_Size)
# Fancy regex to remove parenthetical notes, eg 1,000 (1/4) needs to remove comma and (1/4) to convert to number
df$Sample_Size = as.numeric(gsub("\\s*\\([^\\)]+\\)", "", unlist(df$Sample_Size)))

df$Sample_Size = df$Sample_Size * Rolling_Poll

# Print ot console when run individually
df

df$Error = 1/sqrt(df$Sample_Size)

# reorganize data
mdata <- melt(df, id=c("Date", "Firm", "Error", "Sample_Size"))

# sort data
mdata <- mdata[with(mdata, order(Date)), ]

# relabel data after reorganization
c.names <- c("Date", "Firm", "Error", "Sample_Size", "Party", "Popular_Support")
names(mdata) <- c.names
mdata
mdata$Popular_Support <- str_trim(mdata$Popular_Support)
mdata$Popular_Support <- as.numeric(mdata$Popular_Support)

#mdata$Sample_Size <- as.numeric(mdata$Sample_Size)

polls <- mdata

# Last election data
last_election_date_value <- 42296 # 2015/10/19
Date = rep.int(last_election_date_value, 5)
Party = c('Liberal','Conservative','NDP','BQ','Green')
Popular_Support = c(39.5,31.9,19.7,4.7,3.4)
Error = rep.int(0,5)
LastElection = data.frame(Date, Party, Popular_Support, Error)

# This election -- Add after election to get final election result points
next_election_date_value <- 43759 # 2019/10/21 
#Date = rep.int(42211, 5)
#Party = c('Conservative','Liberal','NDP','BQ','Green')
#Popular_Support = c(39.6,18.9,30.6,6,3.9)
#Error = rep.int(0,5)
#ThisElection = data.frame(Date, Party, Popular_Support, Error)

# Use this if including previous data as part of the smoothing, but don't want it displayed!
election_polls <- polls[polls$Date > (last_election_date_value + 10),]
#election_polls <- polls

colors <- c("red", "blue", "orange", "turquoise4", "green3")

library(ggplot2)

main_aes = aes(x = Date, y = Popular_Support, colour=Party, size=1/Error, weight=1/Error)

# Set plot file settings.  This need to be before we generate the plot.
svg(filename="PollsPlot.svg", 
    width=15, # inches, think 72px/inch: want 1080px / 72 ppi = 15 inches
    height=7, 
    pointsize=12
)


plot <- ggplot(election_polls) 
plot2 <- plot +  geom_point(main_aes)
plot2 <- plot2 + scale_colour_manual(values = colors)

# Add smooth trendline
plot_smooth <- plot2 + stat_smooth(data=polls, span = alpha, show_guide= F, main_aes) 

# Extract the data so we can work on it
smooth_data <- ggplot_build(plot_smooth)$data[[2]]

# Use this if including previous data as part of the smoothing, but don't want it displayed!
smooth_data <- smooth_data[smooth_data$x > last_election_date_value + 10, ]

# Format and add trendlines for each party/color
for(color in colors) {
  party_trend <- subset(smooth_data, colour == color)
  plot <- plot + geom_ribbon(data = party_trend, aes(x=x, ymin=ymin, ymax = ymax), alpha = .25)
  plot <- plot + geom_line(data = party_trend, colour=color, aes(x = x, y = y))
}

# Legend (Party)
plot <- plot + scale_colour_manual(values = colors)

# The scatterplot points
plot <- plot + geom_point(main_aes, alpha=0.8)

# Vertical line if seperation in dates between election and polls
#plot <- plot + geom_vline(xintercept = 42296, linetype = 5, color='darkgray',show_guide= F)

# Legend (Sample Size)
plot <- plot + scale_size_area(max_size=3, breaks=seq(20,60,10), labels=seq(20,60,10)^2, name="Sample Size") 
plot <- plot + guides(color = guide_legend(order=-1) )

# Last election data points and text                                
plot <- plot + geom_point(data=LastElection, size=3, shape=5, show_guide = F, main_aes) 
plot <- plot + geom_point(data=LastElection, size=2, show_guide=F, main_aes) 
plot <- plot + geom_text(data=LastElection, show_guide=F, 
            aes(x = Date, y = Popular_Support, label = Popular_Support), size=3, hjust=1.5, vjust=-0.4)

# This election -- Add after election to get final election result points
#plot <- plot + geom_point(data=ThisElection, size=3, shape=5, show_guide=F, main_aes) +
#  geom_point(data=ThisElection, size=2, show_guide=F, main_aes) +
#  geom_text(data=ThisElection, show_guide=F, 
#            aes(x = Date, y = Popular_Support, label = Popular_Support), size=3, hjust=-.2, vjust=-0.4)

last_election_date_value <- 42296 # 2015/10/19
next_election_date_value <- 43759 # 2019/10/21 
start_date_value <- last_election_date_value
end_date_value <- next_election_date_value

date_labels <- as.character(seq(as.Date("2015/10/19"), as.Date("2019/10/21"), by=77))
date_labels[1] <- "Election\n2015/10/19"
date_labels[length(date_labels)] <- "Election\n2019/10/21"

min_gridlines <- seq(last_election_date_value, next_election_date_value, by=11)
maj_gridlines <- seq(last_election_date_value, next_election_date_value, by=77)

# X-axis
plot <- plot + scale_x_continuous(name = "Date", limits=c(start_date_value, end_date_value), 
                                  minor_breaks = min_gridlines, 
                                  breaks = maj_gridlines,
                                  labels = date_labels
                                  )
                                  
plot <- plot + theme(axis.text.x = element_text(size = 11, vjust=0.5, hjust=0, angle = 90, colour="#333333"))
plot <- plot + theme(axis.title.x = element_blank())

# Y-axis
plot <- plot + scale_y_continuous(name = "% Popular Support", lim=c(0,56), expand=c(0,0)) 
plot <- plot + theme(axis.text.y = element_text(size = 11))
plot <- plot + theme(axis.title.y = element_text(size = 11, angle = 90, colour="#333333"))

# Legend location
#theme(legend.justification=c(1,1), legend.position=c(1,1))

# Run this command seperately (after running the entire script) to get the plot to appear in RStudio
print(plot) 

#dev.copy(svg,'PollsPlot.svg')
dev.off() # saves plot to R project's directory


