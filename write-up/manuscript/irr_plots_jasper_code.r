source(here::here("write-up", "manuscript", "setup.R"))

# load Jasper
require(Jasper)

# read in the raw data
rawdata <- read.csv(fs::path(output_dir_qmd, "irr_plots.csv"), as.is=TRUE)

# set up an object that stores the forest and associated metadata
forest1 <- FormatForest(rawdata=rawdata, forest.n=1, EstimateCol = "HR1", LCICol="LCI1", UCICol="UCI1", logData=FALSE, getBlanks=TRUE, getHets=FALSE, getTrends=FALSE, YLogNeededForSE=FALSE)

blank.rows <- forest1$blank.rows

# find labels to put in the left most column
left.labels <- convertUnicode(as.character(rawdata$Heading))
# what other columns should be plotted
other.cols <- -1
# find column headings
print.headings <- parseColHeadings(rawdata$ColHeadings, rd=rawdata)




##################################################################################

# set up the Jasper page
type <- "JPG"
SetPage(orient="PORTRAIT", perpage=1, type=type, filestem=path(output_dir_qmd, "irr_jasp_plots"),titlespace = 0.005,blank.bottom.percent = 0, append_datetime=FALSE, suppress.date=TRUE,page_height = 7)

# Set page margins: format is c(bottom, left, top, right)
par(mar=c(8.09424320667184, 2, 8.09424320667184, 2))

mainfont <- 1.0439
blankPlot(c(0,100),c(0,100), mainfont)
par(xpd=NA)

spacing <- 1.1445
plot.width <- 60

# draw the Forest plot(s)
xaxmin1 <- 23.5422
xaxmax1 <- xaxmin1 + plot.width
locs <- ForestBasic(forest1,
	LogScale=TRUE,
	ExponentiateDataOnPlot=FALSE,
	xaxmin=xaxmin1,
	xaxmax=xaxmax1,
	xlim=c(0.5, 8),
	xticks=c(0.6, 1, 3, 5, 7),
	ticklabs=c(0.6, 1, 3, 5, 7),
	xlab="<- favours Sanofi | favours Pfier BA.4-5 ->",
	NLabel=FALSE,
	mainfont=1,
	ValueLabels=TRUE,
	ValueLabelsHeader="",
	ValueLabelsEffectSE=FALSE,
	ValueDigits=NULL,
	ValueDigitsSE=NULL,
	lwd=1,
	CISecond=TRUE,
	spacing=spacing/2,
	pointGroupsFactor=0.9,
	verbose=FALSE,
	boxsizeoverride=TRUE,
	boxparm.stderr = 1.5,
	NoEffect.lty = "dotted")

# add titles above forest (may be left empty, change labels="" to what you will)
text(x=xaxmin1 + (xaxmax1 - xaxmin1 + (spacing/2) + strwidth("0.00 (0.00, 0.00)"))/2, y=100+1.5*strheight("A", cex=mainfont), labels="", adj=c(0.5,0), font=2)


# left hand column of labels
ylabels.offset <- 0 * strheight("A")
text(x=0, y=100, labels="", adj=c(0,0), font=2, cex=1)
text(x=0, y=locs$YLocs+ ylabels.offset, labels=left.labels[-blank.rows], adj=0, cex=1, font=ifelse(forest1$IsDiamond, 2,1))
text(x=0, y=locs$BlankLocs+ ylabels.offset, labels=left.labels[blank.rows], adj=0, font=4, cex=1)

# adding other columns
# add the main title
# mainTitle("Incidence Rate Ratios", mainfont=1)

# stop writing to file
closeFile(type)
