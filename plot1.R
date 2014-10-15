##################################################################
## plot1.R script
## includes:
###########################################
## read_n_clean() function:
##      1, check input file path and file name
##      2, read the csv file
##      3, filter data according to date range
##      4, transfer "Date" and "Time" column into Date and POSIXct data type
##      5, transfer number columns into numeric data type
##      6, save the tidy data in the external environment for later display use
###########################################
## plot1() function:
##      1, check if the display ready data is read-in
##      2, open PNG device
##      3, make plot and modify display elements
##      4, close PNG device
###########################################
## Important Notes: 
##      The data for display is saved in the external environment to
##      avoid repeating and longtime data read-in process
##
##################################################################
library(UsingR)

## function: to read-in csv file and perform data cleaning and transforming
## inputs: 
##      ppath: CSV folder name
##      filename: CSV file name
##      nrows: total number of rows to read. For testing purpose. -1: read all
##      dateRange: list of studying dates
read_n_clean <- function(ppath=".", filename="household_power_consumption.txt", nrows=-1, dateRange=c("1/2/2007", "2/2/2007")) {
    
    # to check inputs, target folder path and data file name
    cwd <- getwd()
    if(!file.exists(ppath)) stop("Target path is not right!")
    setwd(ppath)
    if(!file.exists(filename)) stop("E-power Usage data file does not exist !")
    
    # read-in data totally as charater type
    pdata <- read.csv2(filename, na.string="?", colClasses=c(rep("character", 9)), nrows=nrows)
    
    # save column names for naming consistence before and after data type transform
    pnames <- names(pdata)
    
    # date filtering
    pdata <- pdata[pdata$Date %in% dateRange,]
    
    # data type transform
    pdata[,10] <- as.Date(pdata[,1], "%d/%m/%Y")
    pdata <- cbind(pdata[,10], strptime(paste(pdata[,1], pdata[,2]), "%d/%m/%Y %H:%M:%S"), pdata[,3:9])
    pdata[,3]<-as.numeric(pdata[,3])
    pdata[,4]<-as.numeric(pdata[,4])
    pdata[,5]<-as.numeric(pdata[,5])
    pdata[,6]<-as.numeric(pdata[,6])
    pdata[,7]<-as.numeric(pdata[,7])    
    pdata[,8]<-as.numeric(pdata[,8])
    pdata[,9]<-as.numeric(pdata[,9])
    
    # restore column names after data type change
    names(pdata) <- pnames
    
    # data saved to external environment
    ppdata <<- pdata
    
    setwd(cwd)

}


# function: make plot based on the read-in data
# input: 
#   gpath: PNG file saving folder
#   gfname: PNG file saving name
#   ppath: csv file path
#   filename: csv file name
plot1 <- function(gpath=".", gfname="plot1.png", ppath=".", filename="household_power_consumption.txt"){
    
    cwd <- getwd()
    setwd(gpath)
    
    # check data existence, run data preparation code for 
    if(!exists("ppdata")) {ppdata <<- read_n_clean(ppath=dpath, filename=dfname)}
    
    # open PNG device with explicit dimensions
    png(filename=gfname, width=480, height=480)
    
    # make chart
    mtxt <- "Global Active Power"   # customize title
    xlabtxt <-"Global Active Power (kilowatts)"         # customize X axis label
    with(ppdata, hist(Global_active_power, col="red", main=mtxt, xlab=xlabtxt)) 

    dev.off()
    
    setwd(cwd)
}

