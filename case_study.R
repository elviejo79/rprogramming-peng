pm0 <- read.table("pm25_data/RD_501_88101_1999-0.txt",
		  comment.char="#", header=F, sep="|", na.strings="")

#attach column headers to the dataset and make sure
#they are properly formated for R data frames

cnames <- strsplit(readLines("pm25_data/RD_501_88101_1999-0.txt",1), split="|", fixed=T)
names(pm0) <- make.names(cnames[[1]])
head(pm0[,1:13])
x0 <- pm0$Sample.Value
summary(x0)
##Are missing values important here?
mean(is.na(x0))

#2012 data in the same manner in which we read the 1999 data (the data files are in the same format)

pm1<-read.table("pm25_data/RD_501_88101_2012-0.txt", comment.char="#", header=F, sep="|", na.strings="")

#set the column names (they are the same as the 1999 dataset)
names(pm1)<-make.names(cnames[[1]])
x1 <- pm1$Sample.Value

# we take the log of the PM values to adjust for the skew in the data.
## boxplot(log2(x0),log2(x1))

summary(x0)
summary(x1)
# from the summary of x1 there are some negative values which should not occur
negative <- x1 < 0
mean(negative, na.rm=T)

# perhaps negative dates occur more often in some parts of the year than other years.
dates <- as.Date(as.character(pm1$Date), "%Y%m%d")

# extract the month from each of the dates with negative values
missing.months <- month.name[as.POSIXlt(dates)$mon+1]
tab <-table(factor(missing.months, levels=month.name))

# First we subset the data frames to only include data from
# New York ( State.Code == 36 ) and only include the County.Code
# and the Site.ID (i.e. monitor number) variables.
site0 <-unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site1 <-unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))

#Then we create a new variable that combines the county code
# and the site ID into a single string.

site0 <- paste(site0[,1], site0[,2], sep=".")
site1 <- paste(site1[,1], site1[,2], sep=".")
both <- intersect(site0, site1)
print(both)

# Find how many observations available at each monitor
pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep="."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep="."))

cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)

# count the number of observations at each monitor to see which ones have the most observations
print("## 1999")
print(sapply ( split ( cnt0 , cnt0 $ county.site ), nrow ))
print("## 2012")
print(sapply ( split ( cnt1 , cnt1 $ county.site ), nrow ))

# A number of monitors seem suitable from the output,
# but we will focus here on County 63 and site ID 2008.
both.county <- 63
both.id <- 2008

print("## Choose county 63 and side ID 2008")
pm0sub <- subset ( pm0 , State.Code == 36 & County.Code == both.county & Site.ID == both.id )
pm1sub <- subset ( pm1 , State.Code == 36 & County.Code == both.county & Site.ID == both.id )

## Now we plot the time series data of PM for the monitor in both years.
dates1 <- as.Date(as.character(pm1sub$Date), "%Y%m%d")
x1sub <- pm1sub$Sample.Value
dates0 <- as.Date(as.character(pm0sub$Date), "%Y%m%d")
x0sub <- pm0sub$Sample.Value

print("> ## Find global range")
rng <- range(x0sub, x1sub, na.rm=T)

## Start with a graphic
par(mfrow = c(1,2), mar= c(4,5,2,1))
plot(dates0, x0sub, pch=20, ylim=rng, xlab="", ylab=expression(PM[2.5]* " (" * mu * g / m ^ 3 * ")" ))
abline(h=median(x0sub, na.rm=T))
plot(dates1, x1sub, pch=20, ylim=rng, xlab="", ylab=expression(PM[2.5]* " (" * mu * g / m ^ 3 * ")" ))
abline(h = median(x1sub, na.rm=T))

## it might be useful to examine changes in PM at the state level.
## 1999
mn0 <- with ( pm0 , tapply ( Sample.Value , State.Code , mean , na.rm = TRUE ))
## 2012
mn1 <- with ( pm1 , tapply ( Sample.Value , State.Code , mean , na.rm = TRUE ))

## Make separate data frames for states / years
d0 <- data.frame(state=names(mn0), mean=mn0)
d1 <- data.frame(state=names(mn1), mean=mn1)

mrg <- merge(d0,d1, by="state")
print(head(mrg))

## Now make a plot that shows the 1999 state-wide means in one “column” and the 2012 state-wide means in another columns. We
par(mfrow= c(1,1))
rng <- range(mrg[,2], mrg[,3])
with(mrg, plot(rep(1,52), mrg[,2], xlim=c(.5, 2.5), ylim=rng, xaxt="n", xlab="", ylab="State-wide Mean PM"))
with(mrg, points(rep(2,52), mrg[,3]))
segments(rep(1,52), mrg[,2], rep(2,52), mrg[,3])
axis(1, c(1,2), c("1999", "2012"))
