# Weather Impact on Life and Property
Buel Chandler  
`r format(Sys.time(), "%d %B, %Y")`  
#Weather Impact on Life and Property

> The end-goal is to determine which weather events have the biggest impact to deaths, injury, and property/crop damage. We are given a dataset that covers various weather events from 1950 through late 2011. The years 1950 through 1995 essentially accounted for only one weather type (Tornado), so we decide to look only from 1996 on. Then we have to do various transformations to the data (actually the bulk of this paper), due to non-conformity of manual input. Determining the impacts of the various weather events is then straightforward. The "Big 3" for each category are:

|Deaths|Injuries|Property|Crop|
|:----:|:------:|:------:|:---:|
|Heat|Tornado|Flood|Drought|
|Tornado|Flood|Hurricane|Hurricane|
|Flash Flood|Heat|Storm Surge|Flood|

## The Data

The U.S. National Oceanic and Atmospheric Administration's (NOAA) maintains a storm database which contains information of major weather events that have occurred over the years (since 1950) in and around the United States. The information includes when and where the events occurred, and impacts to life (fatalities, injuries), and impacts to property and crops. And those impacts are significant. In this paper we look at weather events that caused biggest impacts.

The [NOAA Storm Events](http://www.ncdc.noaa.gov/stormevents/details.jsp) website is a good starting point for exploring the resource. We note that right of we are told that only *Tornadoes* were covered from 1950 (the database starting point) through 1954. *Thunderstorm Wind, Hail* were added in 1955 as unique weather events. In 1996, 45 other unique weather events were added (e.g., *Hurricanes*, *Ice Storms*, et cetera)

The code to produce this report is available on [GitHub](https://github.com/buelchandler/datasciencecoursera/tree/master/reproducible-data/PeerAssesment2)

## Data Processing: Loading and cleaning the data


```r
knitr::opts_chunk$set(fig.path='figure-html/', echo=TRUE, warning=TRUE, message=TRUE)
```

### Load needed libraries, and find, then read-in our raw NOAA data


```r
## make sure any libraries we need are installed, then load them into workspace
pkgs = c("downloader", "dplyr", "car", "stringr", "lubridate", "lattice", "gridExtra")

if(length(new.pkgs <- setdiff(pkgs, rownames(installed.packages())))) 
  install.packages(new.pkgs, repos="http://cran.rstudio.com/")

suppressMessages(library(downloader)) ## getting files off the net
suppressMessages(library(dplyr)) ## data manipulation
suppressMessages(library(car)) ## to use "recode" function
suppressMessages(library(stringr)) ## some string handling functions
suppressMessages(library(lubridate)) ## some date/time handling functions
suppressMessages(library(lattice)) ## for our dotcharts
suppressMessages(library(gridExtra)) ## for our dotcharts

## setup file handles, and get the file and unzip if not in working directory
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zipfile <- "./data/repdata_data_StormData.csv.bz2"
if(!file.exists(zipfile)) {
  download(fileURL, dest=zipfile, mode="wb")
#  unzip(zipfile, exdir = "./data")
}

#print out the date the raw data was downloaded
tt <- file.info(zipfile) # get all file info
cat("File ", zipfile, " created on ", format(tt$ctime, "%Y-%m-%d %H:%M:%S %Z"))
```

```
## File  ./data/repdata_data_StormData.csv.bz2  created on  2016-05-20 10:04:33 EDT
```

```r
## read in raw file, put it to a data.table
all.storms <- read.csv(zipfile) ## read.csv will uncompress .bz2 automatically

str(all.storms) ## quick look at the dataset
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : Factor w/ 16335 levels "1/1/1966 0:00:00",..: 6523 6523 4242 11116 2224 2224 2260 383 3980 3980 ...
##  $ BGN_TIME  : Factor w/ 3608 levels "00:00:00 AM",..: 272 287 2705 1683 2584 3186 242 1683 3186 3186 ...
##  $ TIME_ZONE : Factor w/ 22 levels "ADT","AKS","AST",..: 7 7 7 7 7 7 7 7 7 7 ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: Factor w/ 29601 levels "","5NM E OF MACKINAC BRIDGE TO PRESQUE ISLE LT MI",..: 13513 1873 4598 10592 4372 10094 1973 23873 24418 4598 ...
##  $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : Factor w/ 35 levels "","  N"," NW",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_LOCATI: Factor w/ 54429 levels ""," Christiansburg",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_DATE  : Factor w/ 6663 levels "","1/1/1993 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_TIME  : Factor w/ 3647 levels ""," 0900CST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : Factor w/ 24 levels "","E","ENE","ESE",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_LOCATI: Factor w/ 34506 levels ""," CANTON"," TULIA",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ WFO       : Factor w/ 542 levels ""," CI","%SD",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ STATEOFFIC: Factor w/ 250 levels "","ALABAMA, Central",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ ZONENAMES : Factor w/ 25112 levels "","                                                                                                                               "| __truncated__,..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : Factor w/ 436781 levels "","\t","\t\t",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```

### Subset our data

As noted in our introduction above, prior to 1996 the database tracked *Tornado* related events only. So as to not skew our analysis, we will only examine data from 1996 on, when the use of 48 unique weather events was instituted (**EVTYPE**). We will use the variable **BGN_DATE**, which records the date an event started.

Impact to human life are covered in two variables: **FATALITIES**, **INJURIES**, which indicates number of each for any particular event.

Property damage is indicated by variable **PROPDMG**, which indicates some estimated monetary loss, and magnitude of that loss by **PROPDMGEXP**. Though not consistent throughout, we could see something like an overall $10,000,000 loss given as a **PROBDMG** of 10 and a **PROPDMGEXP** of "M" (for million). Details are broken out in an appropriate code segment below.

Crop damage is handled similar to property damage data, and is given in corresponding variables **CROPDMG** and **CROPDMGEXP**, and is detailed in an appropriate code segment below.


```r
## get good datetimes
all.storms$year <- year(as.POSIXct(strptime(all.storms$BGN_DATE, "%m/%d/%Y %H:%M:%S")))

storms <- all.storms %>%
  select(year, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>%
  filter(year >= 1996)
```

### Clean PROPDMGEXP and CROPDMGEXP

A preliminary look at **PROPDMGEXP** and **CROPDMGEXP** shows some pretty strange results:

```r
unique(storms$PROPDMGEXP)
```

```
## [1] K   M B 0
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
unique(storms$CROPDMGEXP)
```

```
## [1] K   M B
## Levels:  ? 0 2 B k K m M
```

As there appears to be "bad" data in these two fields, we'll clean them up by re-coding them using the *recode* function from the *car* (Companion to Applied Regression) library available on CRAN.


```r
storms$PROPDMGEXP <- recode(storms$PROPDMGEXP, ## look for magnitude adjustment
                           "c('h', 'H', '2') = 100.0;
                            c('k', 'K', '3') = 1000.0;
                            c('4') = 10000.0;
                            c('5') = 100000.0;
                            c('m', 'M', '6') = 1000000.0;
                            c('7') = 10000000.0;
                            c('8') = 100000000.0;
                            c('b', 'B') = 1000000000.0;
                            else = 1.0",
                            as.factor.result = FALSE, ## was a factor
                            as.numeric.result = TRUE) ## now its numeric

storms$prop <- storms$PROPDMG * storms$PROPDMGEXP ## total dollars property damage for this event
```


```r
storms$CROPDMGEXP <- recode(storms$CROPDMGEXP, ## look for magnitude adjustment
                           "c('h', 'H', '2') = 100.0;
                            c('k', 'K') = 1000.0;
                            c('m', 'M') = 1000000.0;
                            c('b', 'B') = 1000000000.0;
                            else = 1.0",
                            as.factor.result = FALSE, ## was a factor
                            as.numeric.result = TRUE) ## now its numeric

storms$crop <- storms$CROPDMG * storms$CROPDMGEXP ## total dollars crop damage for this event
```

### EVTYPE is Butt-Ugly

So NOAA has 48 unique standard events. Should be easy, but in the real world it's not. How many unique events in our data set?


```r
## Total number of unique EVTYPE in the dataset
num.EVTYPE <- length(unique(storms$EVTYPE))
num.EVTYPE
```

```
## [1] 516
```

```r
## show the first 25
head(sort(unique(storms$EVTYPE)), 25)
```

```
##  [1]    HIGH SURF ADVISORY           COASTAL FLOOD                
##  [3]  FLASH FLOOD                    LIGHTNING                    
##  [5]  TSTM WIND                      TSTM WIND (G45)              
##  [7]  WATERSPOUT                     WIND                         
##  [9] ABNORMAL WARMTH                ABNORMALLY DRY                
## [11] ABNORMALLY WET                 ACCUMULATED SNOWFALL          
## [13] AGRICULTURAL FREEZE            ASTRONOMICAL HIGH TIDE        
## [15] ASTRONOMICAL LOW TIDE          AVALANCHE                     
## [17] Beach Erosion                  BEACH EROSION                 
## [19] BITTER WIND CHILL              BITTER WIND CHILL TEMPERATURES
## [21] Black Ice                      BLACK ICE                     
## [23] BLIZZARD                       Blizzard Summary              
## [25] BLOW-OUT TIDE                 
## 985 Levels:    HIGH SURF ADVISORY  COASTAL FLOOD ... WND
```

Perusing the first 25 unique **EVTYPE** out of a total of 516 unique **EVTYPE** recorded in the subset data, we see liberties were taken in recording any given event. So we need to try and fit those 516 into our standard NOAA approved 48.

First order of business is to get a canonical list of the 48 standard events. We created a text file that listed the events one to a line using copy/paste from the NOAA document [here.](http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/Storm-Data-Export-Format.docx) 


```r
## Note I have made the valid NOAA Events available at:
## https://github.com/buelchandler/datasciencecoursera/blob/master/reproducible-data/PeerAssesment2/data/valid-events.txt

canonicalFile = "./data/valid-events.txt"
## might as well put into all lower, no extra spaces format used through rest of analysis
canonical <- str_to_lower(str_trim(readLines(canonicalFile)))
str(canonical)
```

```
##  chr [1:48] "astronomical low tide" "avalanche" "blizzard" ...
```

So let's first find which events are already valid in our working data-set:

```r
## convert EVTYPE to lower case, and remove extra spaces
storms$EVTYPE <- str_to_lower(str_trim(storms$EVTYPE))

## valid will contain the canonical label, else NA if EVTYPE is bad when last checked
## and we populate with all the observations that already have a "good" EVTYPE
storms$valid <- canonical[match(storms$EVTYPE, canonical)] 
```

The following code snippets are used interactively for the most part as we repeatedly analyze bad **EVTYPE** as we try and winnow these bad into good


```r
## show all "bad" EVTYPE not accounted for
## used heavily interactively as we work problem EVTYPE
bad <- subset(storms, is.na(valid))

## how many bad EVTYPE remain
start.bad.EVTYPE <- length(unique(bad$EVTYPE)) ## save the number for reporting results
## how many bad records remain
bad.recs <- nrow(bad) ## save the number for reporting results

## the following piece of code was used interactively to ferret out bad EVTTPE which contain some
## particular text. This is the workhorse
## unique(bad$EVTYPE[str_detect(bad$EVTYPE, "snow")]) ## substitue "snow" for whatever you want to look at
```

Note that we followed a repeating process of looking at partial strings to determine where to best fit a bad EVTYPE. The order we do the following remapping can be important, as some strings contain conflicting determinations, so sometimes we need precise matching before using a general matching.


```r
## at this point we have 148,899 bad EVTYPE (384 unique)
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "flash")] <- "flash flood"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "flood")] <- "flood"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "fld")] <- "flood"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "lakeshore flood")] <- "flood"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "dam b")] <- "flood"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "hail")] <- "hail"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "marine tstm")] <- "marine thunderstorm wind"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "non tstm wind")] <- "strong wind"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "non-tstm wind")] <- "strong wind"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "tstm wind")] <- "thunderstorm wind"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "thunderstorm wind")] <- "thunderstorm wind"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "tstm")] <- "thunderstorm"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "thunder")] <- "thunderstorm"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "wall")] <- "thunderstorm"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "metro storm")] <- "thunderstorm"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "ice")] <- "ice storm"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "icy")] <- "ice storm"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "freezing ")] <- "ice storm"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "glaze")] <- "ice storm"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "chill")] <- "cold/wind chill"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "cool")] <- "cold/wind chill"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "cold")] <- "cold/wind chill"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "freeze")] <- "cold/wind chill"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "hypo")] <- "cold/wind chill"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "hype")] <- "cold/wind chill"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "sleet")] <- "sleet"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "bli")] <- "blizzard"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "wind")] <- "high wind"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "wnd")] <- "high wind"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "dust")] <- "dust storm"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "lake-eff")] <- "lake-effect snow"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "lake eff")] <- "lake-effect snow"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "snow")] <- "heavy snowr"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "wint")] <- "winter weather"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "mixed pre")] <- "winter weather"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "heat")] <- "heat"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "warm")] <- "heat"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "hot")] <- "heat"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "hot")] <- "heat"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "record temp")] <- "heat"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "record high")] <- "heat"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "ture record")] <- "heat"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "monthly temp")] <- "heat"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "record low rainfall")] <- "drought"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "dry")] <- "drought"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "dri")] <- "drought"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "rain")] <- "heavy rain"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "prec")] <- "heavy rain"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "wet")] <- "heavy rain"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "typh")] <- "hurricane (typhoon)"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "hurr")] <- "hurricane (typhoon)"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "remnant")] <- "hurricane (typhoon)"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "tide")] <- "storm surge/tide"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "surf")] <- "storm surge/tide"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "surge")] <- "storm surge/tide"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "flag")] <- "storm surge/tide"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "swell")] <- "storm surge/tide"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "seas")] <- "storm surge/tide"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "wave")] <- "storm surge/tide"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "water")] <- "storm surge/tide"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "marine")] <- "storm surge/tide"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "coast")] <- "high surf"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "beach")] <- "high surf"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "rip")] <- "rip current"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "drown")] <- "rip current"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "fire")] <- "wildfire"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "smoke")] <- "wildfire"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "slid")] <- "avalanche"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "lands")] <- "avalanche"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "volc")] <- "volcanic ash"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "vog")] <- "dense fog"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "fog")] <- "dense fog"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "micro")] <- "tornado"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "torn")] <- "tornado"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "funnel")] <- "tornado"
storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "burst")] <- "tornado"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "frost")] <- "frost"

storms$valid[is.na(storms$valid) & str_detect(storms$EVTYPE, "spout")] <- "waterspout"
```

We repeat some code to tell us how well we did reducing the bad **EVTYPE**.


```r
## show all "bad" EVTYPE not accounted for
## used heavily interactively as we work problem EVTYPE
bad <- subset(storms, is.na(valid))
bad.EVTYPE <- sort(unique(bad$EVTYPE))
final.recs <- nrow(bad) ## how many bad records remain
final.EVTYPE <- length(unique(bad$EVTYPE)) ## how many unique bad EVTYPE remain
```

So how well did we do?
Our original raw data had 902297 rows. We filtered the raw by taking only records from 1996 on when NOAA expanded to 48 event categories, giving us 653530. However we saw that the initial filtered data-set had 516 unique EVTYPE, requiring lots of cleaning.

After we did the initial pass to account for properly named EVTYPE, we had 384 **EVTYPE** not in canonical format spread over 148899 rows. Analyzing the bad data repeatedly we came up with a sequence of remapping of bad to canonical which left 130 rows we could not easily account for representing 70 bad **EVTYPE**. Overall we reduced the bad data to 0.019892 percent of the initial filtered data-set.

What were these last 130 we left out? They consisted of the following **EVTYPE**:

```r
## print out all current bad EVTYPE
sort(unique(bad$EVTYPE))
```

```
##  [1] "no severe weather"      "none"                  
##  [3] "northern lights"        "other"                 
##  [5] "summary august 10"      "summary august 11"     
##  [7] "summary august 17"      "summary august 2-3"    
##  [9] "summary august 21"      "summary august 28"     
## [11] "summary august 4"       "summary august 7"      
## [13] "summary august 9"       "summary jan 17"        
## [15] "summary july 23-24"     "summary june 18-19"    
## [17] "summary june 5-6"       "summary june 6"        
## [19] "summary of april 12"    "summary of april 13"   
## [21] "summary of april 21"    "summary of april 27"   
## [23] "summary of april 3rd"   "summary of august 1"   
## [25] "summary of july 11"     "summary of july 2"     
## [27] "summary of july 22"     "summary of july 26"    
## [29] "summary of july 29"     "summary of july 3"     
## [31] "summary of june 10"     "summary of june 11"    
## [33] "summary of june 12"     "summary of june 13"    
## [35] "summary of june 15"     "summary of june 16"    
## [37] "summary of june 18"     "summary of june 23"    
## [39] "summary of june 24"     "summary of june 3"     
## [41] "summary of june 30"     "summary of june 4"     
## [43] "summary of june 6"      "summary of march 14"   
## [45] "summary of march 23"    "summary of march 24"   
## [47] "summary of march 24-25" "summary of march 27"   
## [49] "summary of march 29"    "summary of may 10"     
## [51] "summary of may 13"      "summary of may 14"     
## [53] "summary of may 22"      "summary of may 22 am"  
## [55] "summary of may 22 pm"   "summary of may 26 am"  
## [57] "summary of may 26 pm"   "summary of may 31 am"  
## [59] "summary of may 31 pm"   "summary of may 9-10"   
## [61] "summary sept. 25-26"    "summary september 20"  
## [63] "summary september 23"   "summary september 3"   
## [65] "summary september 4"    "summary: nov. 16"      
## [67] "summary: nov. 6-7"      "summary: oct. 20-21"   
## [69] "summary: october 31"    "summary: sept. 18"
```

## Results: Impact to life, limb and property

First we create a summary data-set that holds all the data we need to produce our exploratory graphs:

```r
costs <- storms %>%
  group_by(valid) %>%
  summarise(crop.dmg = sum(crop)/1000000, ## Normalize to Millions of dollars
            prop.dmg = sum(prop)/1000000, ## Normalize to Millions of dollars
            death = sum(FATALITIES),
            injury = sum(INJURIES))
```

### Property/Crop damage by Weather Event


```r
prop <- head(costs[order(-costs$prop.dmg), ], 10) ## use prop damage to sort order
crop <- head(costs[order(-costs$crop.dmg), ], 10) ## use crop damage to sort order

## PROPERTY/CROP DAMAGE Totals
prop.plot <- dotplot(reorder(valid, prop.dmg) ~ prop.dmg, data = prop,
        aspect = 1.5,
        scales = list(cex = .65),
        main = "Property Damage",
        xlab = "1996--2011 ($Millions)",
        panel = function (x, y) {
          panel.segments(rep(0, length(x)), as.numeric(y),
                         x, as.numeric(y), lty = 2, col = "gray")
          panel.xyplot(x, as.numeric(y), pch = 16, col = "black")} )

crop.plot <- dotplot(reorder(valid, crop.dmg) ~ crop.dmg, data = crop,
        aspect = 1.5,
        scales = list(cex = .65),
        main = "Crop Damage",
        xlab = "1996--2011 ($Millions)",
        panel = function (x, y) {
          panel.segments(rep(0, length(x)), as.numeric(y),
                         x, as.numeric(y), lty = 2, col = "gray")
          panel.xyplot(x, as.numeric(y), pch = 16, col = "black")} )

 grid.arrange(prop.plot, crop.plot, ncol=2)
```

![](figure-html/unnamed-chunk-14-1.png)<!-- -->

The dot-charts shows that the top 3 events for causing property damage are, in order: flood, hurricane, and storm surge. For crop damage, the top 3 are: drought, hurricane. flood.

### Human Cost -- Fatalities and Injuries


```r
death <- head(costs[order(-costs$death), ], 10) ## use death to sort order
injury <- head(costs[order(-costs$injury), ], 10) ## use injury to sort order

## Death/Injury Totals
death.plot <- dotplot(reorder(valid, death) ~ death, data = death,
        aspect = 1.5,
        scales = list(cex = .65),
        main = "Deaths",
        xlab = "1996--2011",
        panel = function (x, y) {
          panel.segments(rep(0, length(x)), as.numeric(y),
                         x, as.numeric(y), lty = 2, col = "gray")
          panel.xyplot(x, as.numeric(y), pch = 16, col = "black")} )

injury.plot <- dotplot(reorder(valid, injury) ~ injury, data = injury,
        aspect = 1.5,
        scales = list(cex = .65),
        main = "Injuries",
        xlab = "1996--2011",
        panel = function (x, y) {
          panel.segments(rep(0, length(x)), as.numeric(y),
                         x, as.numeric(y), lty = 2, col = "gray")
          panel.xyplot(x, as.numeric(y), pch = 16, col = "black")} )

 grid.arrange(death.plot, injury.plot, ncol=2)
```

![](figure-html/unnamed-chunk-15-1.png)<!-- -->

Heat, followed by tornado and flash floods are the most significant cause of death of the various events.

Tornadoes are by far the overwhelming cause of bodily, non-fatal injury. The next four events (flood, heat, wind and lightning) also cause significant injury.
