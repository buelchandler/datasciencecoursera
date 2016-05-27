# Impact of Weather on Life and Property
Buel Chandler  
#Impact of Weather on Life and Property

The U.S. National Oceanic and Atmospheric Administration's (NOAA) maintains a storm database which contains information of major weather events that have occured over the years (since 1950) in and around the United States. The information includes when and where the events occured, and impacts to life (fatalities, injuries), and impacts to property and crops. And those impacts are significant. In this paper we look at weather events that caused biggest impacts.

The [NOAA Storm Events](http://www.ncdc.noaa.gov/stormevents/details.jsp) website is a good starting point for exploring the resource. We note that right of we are told that only *Tornadoes* were covered from 1950 (the database starting point) through 1954. *Thunderstorm Wind, Hail* were added in 1955 as unique weather events. In 1996, 45 other unique weather events were added (e.g., *Hurricanes*, *Ice Storms*, et cetera)

## Data Processing: Loading and cleaning the data


```r
knitr::opts_chunk$set(fig.width=12, fig.height=8, fig.path='figure-html/',
                      echo=TRUE, warning=TRUE, message=TRUE)
```

### Load needed libraries, and find, then read-in our raw NOAA data


```r
## make sure any libraries we need are installed, then load them into workspace
pkgs = c("downloader", "dplyr", "car", "stringr", "lubridate")

if(length(new.pkgs <- setdiff(pkgs, rownames(installed.packages())))) 
  install.packages(new.pkgs, repos="http://cran.rstudio.com/")

suppressMessages(library(downloader)) ## getting files off the net
suppressMessages(library(dplyr)) ## data manipulation
suppressMessages(library(car)) ## to use "recode" function
suppressMessages(library(stringr)) ## some string handling functions
suppressMessages(library(lubridate)) ## some date/time handling functions

## setup file handles, and get the file and unzip if not in working directory
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zipfile <- "./data/repdata_data_StormData.csv.bz2"
if(!file.exists(zipfile)) {
  download(fileURL, dest=zipfile, mode="wb")
#  unzip(zipfile, exdir = "./")
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
all.storms <- read.csv(zipfile) 

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

As noted in our introduction above, prior to 1996 the database tracked *Tornado* related events only. So as to not skew our analysis, we will only examine data from 1996 on, when the use of 48 unique weather events was instituted (**EVTYPE**). We will use the variable **BGN_DATE**, which records the date event started.

Impact to human life are covered in two variables: **FATALITIES**, **INJURIES**, which indicates number of each for any particular event.

Property damage is indicated by variable **PROPDMG**, which indicates some estimated monetary loss, and magnitude of that loss by **PROPDMGEXP**. Though not consistant throughout, we could see something like an overall $10,000,000 loss given as a **PROBDMG** of 10 and a **PROPDMGEXP** of "M" (for million). Details are broken out in an appropriate code segment below.

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

As there appears to be "bad" data in these two fields, we'll clean them up by recoding them using the *recode* function from the *car* (Companion to Applied Regression) library available on CRAN.


```r
storms$PROPDMGEXP <- recode(storms$PROPDMGEXP, ## look for magnitude adjustment
                           "c('h', 'H') = 100.0;
                            c('k', 'K') = 1000.0;
                            c('m', 'M') = 1000000.0;
                            c('b', 'B') = 1000000000.0;
                            else = 1.0",
                            as.factor.result = FALSE, ## was a factor
                            as.numeric.result = TRUE) ## now its numeric

storms$prop <- storms$PROPDMG * storms$PROPDMGEXP ## total dollars property damage for this event
```


```r
storms$CROPDMGEXP <- recode(storms$CROPDMGEXP, ## look for magnitude adjustment
                           "c('h', 'H') = 100.0;
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

Perusing the first 25 unique EVTYPE we see liberties were taken in recording any given event. So we need to try and fit WINTER STORM, TORNADO, TSTM WIND, HAIL, HIGH WIND, HEAVY RAIN, FLASH FLOOD, FREEZING RAIN, EXTREME COLD, EXCESSIVE HEAT, LIGHTNING, FUNNEL CLOUD, EXTREME WINDCHILL, BLIZZARD, URBAN/SML STREAM FLD, FLOOD, TSTM WIND/HAIL, WATERSPOUT, RIP CURRENTS, HEAVY SNOW, Other, Record dry month, Temperature record, WILD/FOREST FIRE, Minor Flooding, ICE STORM, STORM SURGE, Ice jam flood (minor, High Wind, DUST STORM, STRONG WIND, DUST DEVIL, Tstm Wind, DROUGHT, DRY MICROBURST, FOG, ROUGH SURF, Wind, THUNDERSTORMS, Heavy Surf, HEAVY SURF, Dust Devil, Wind Damage, Marine Accident, Snow, AVALANCHE, Freeze, TROPICAL STORM, Snow Squalls, Coastal Flooding, Heavy Rain, Strong Wind, WINDS, WIND, COASTAL FLOOD, COASTAL STORM, COASTALFLOOD, Erosion/Cstl Flood, Heavy Rain and Wind, Light Snow/Flurries, Wet Month, Wet Year, Tidal Flooding, River Flooding, SNOW, DAMAGING FREEZE, Damaging Freeze, HURRICANE, Beach Erosion, Hot and Dry, Flood/Flash Flood, Icy Roads, High Surf, Heavy Rain/High Surf, HIGH SURF, Thunderstorm Wind, Rain Damage, ICE JAM, Unseasonable Cold, Early Frost, Wintry Mix, blowing snow, STREET FLOODING, Record Cold, Extreme Cold, Ice Fog, Excessive Cold, Torrential Rainfall, Freezing Rain, Landslump, Late-season Snowfall, Hurricane Edouard, Coastal Storm, Flood, HEAVY RAIN/WIND, TIDAL FLOODING, Winter Weather, Snow squalls, Strong Winds, Strong winds, RECORD WARM TEMPS., Ice/Snow, Mudslide, Glaze, Extended Cold, Snow Accumulation, Freezing Fog, Drifting Snow, Whirlwind, Heavy snow shower, Heavy rain, COASTAL FLOODING, LATE SNOW, Record May Snow, Record Winter Snow, Heavy Precipitation,  COASTAL FLOOD, Record temperature, Light snow, Late Season Snowfall, Gusty Wind, small hail, Light Snow, MIXED PRECIP, Black Ice, Mudslides, Gradient wind, Snow and Ice, COLD, Freezing Spray, DOWNBURST, Summary Jan 17, Summary of March 14, Summary of March 23, Summary of March 24, Summary of April 3rd, Summary of April 12, Summary of April 13, Summary of April 21, Summary August 11, Summary of April 27, Summary of May 9-10, Summary of May 10, Summary of May 13, Summary of May 14, Summary of May 22 am, Summary of May 22 pm, Heatburst, Summary of May 26 am, Summary of May 26 pm, Metro Storm, May 26, Summary of May 31 am, Summary of May 31 pm, Summary of June 3, Summary of June 4, Summary June 5-6, Summary June 6, Summary of June 11, Summary of June 12, Summary of June 13, Summary of June 15, Summary of June 16, Summary June 18-19, Summary of June 23, Summary of June 24, Summary of June 30, Summary of July 2, Summary of July 3, Summary of July 11, Summary of July 22, Summary July 23-24, Summary of July 26, Summary of July 29, Summary of August 1, Summary August 2-3, Summary August 7, Summary August 9, Summary August 10, Summary August 17, Summary August 21, Summary August 28, Summary September 4, Summary September 20, Summary September 23, Summary Sept. 25-26, Summary: Oct. 20-21, Summary: October 31, Summary: Nov. 6-7, Summary: Nov. 16, Microburst, wet micoburst, HAIL/WIND, Hail(0.75), Funnel Cloud, Urban Flooding, No Severe Weather, Urban flood, Urban Flood, Cold, WINTER WEATHER, Summary of May 22, Summary of June 6, Summary August 4, Summary of June 10, Summary of June 18, Summary September 3, Summary: Sept. 18, Coastal Flood, coastal flooding, Small Hail, Record Temperatures, Light Snowfall, Freezing Drizzle, Gusty wind/rain, GUSTY WIND/HVY RAIN, Blowing Snow, Early snowfall, Monthly Snowfall, Record Heat, Seasonal Snowfall, Monthly Rainfall, Cold Temperature, Sml Stream Fld, Heat Wave, MUDSLIDE/LANDSLIDE, Saharan Dust, Volcanic Ash, Volcanic Ash Plume, Thundersnow shower, NONE, COLD AND SNOW, DAM BREAK, RAIN, RAIN/SNOW, OTHER, FREEZE, TSTM WIND (G45), RECORD WARMTH, STRONG WINDS, FREEZING DRIZZLE, UNSEASONABLY WARM, SLEET/FREEZING RAIN, BLACK ICE, WINTRY MIX, BLOW-OUT TIDES, UNSEASONABLY COLD, UNSEASONABLY COOL, TSTM HEAVY RAIN, UNSEASONABLY DRY, Gusty Winds, GUSTY WIND, TSTM WIND 40, TSTM WIND 45, HARD FREEZE, TSTM WIND (41), HEAT, RIVER FLOOD, TSTM WIND (G40), RIP CURRENT, TSTM WND, DENSE FOG, Wintry mix,  TSTM WIND, MUD SLIDE, MUDSLIDES, MUDSLIDE, Frost, Frost/Freeze, SNOW AND ICE, WIND DAMAGE, RAIN (HEAVY), Record Warmth, Prolong Cold, Cold and Frost, RECORD COLD, PROLONG COLD, AGRICULTURAL FREEZE, URBAN/SML STREAM FLDG, SNOW SQUALL, HEAVY SNOW SQUALLS, SNOW/ICE, GUSTY WINDS, SMALL HAIL, SNOW SQUALLS, LAKE EFFECT SNOW, STRONG WIND GUST, LATE FREEZE, RECORD TEMPERATURES, ICY ROADS, RECORD SNOWFALL, BLOW-OUT TIDE, THUNDERSTORM, Hypothermia/Exposure, HYPOTHERMIA/EXPOSURE, Lake Effect Snow, Mixed Precipitation, Record High, COASTALSTORM, LIGHT SNOW, Snow and sleet, Freezing rain, Gusty winds, FUNNEL CLOUDS, WATERSPOUTS, Blizzard Summary, FROST, ICE, SUMMARY OF MARCH 24-25, SUMMARY OF MARCH 27, SUMMARY OF MARCH 29, GRADIENT WIND, Icestorm/Blizzard, Flood/Strong Wind, TSTM WIND AND LIGHTNING, gradient wind, SEVERE THUNDERSTORMS, EXCESSIVE RAIN, Freezing drizzle, Mountain Snows, URBAN/SMALL STRM FLDG, WET MICROBURST, Heavy surf and wind, Mild and Dry Pattern, COLD AND FROST, RECORD HEAT, TYPHOON, LANDSLIDES, HIGH SWELLS, HIGH  SWELLS, VOLCANIC ASH, HIGH WINDS, DRY SPELL,  LIGHTNING, BEACH EROSION, UNSEASONAL RAIN, EARLY RAIN, PROLONGED RAIN, WINTERY MIX, COASTAL FLOODING/EROSION, UNSEASONABLY WET, HOT SPELL, HEAT WAVE, UNSEASONABLY HOT, UNSEASONABLY WARM AND DRY,  TSTM WIND (G45), TSTM WIND  (G45), HIGH WIND (G40), TSTM WIND (G35), DRY WEATHER, TSTM WINDS, FREEZING RAIN/SLEET, ABNORMAL WARMTH, UNUSUAL WARMTH, GLAZE, WAKE LOW WIND, MONTHLY RAINFALL, COLD TEMPERATURES, COLD WIND CHILL TEMPERATURES, MODERATE SNOW, MODERATE SNOWFALL, URBAN/STREET FLOODING, COASTAL EROSION, UNUSUAL/RECORD WARMTH, BITTER WIND CHILL, BITTER WIND CHILL TEMPERATURES, SEICHE, TSTM, COASTAL  FLOODING/EROSION, SNOW DROUGHT, UNSEASONABLY WARM YEAR, HYPERTHERMIA/EXPOSURE, SNOW/SLEET, ROCK SLIDE, ICE PELLETS, URBAN FLOOD, PATCHY DENSE FOG, RECORD COOL, RECORD WARM, HOT WEATHER, RIVER FLOODING, RECORD TEMPERATURE, SAHARAN DUST, TROPICAL DEPRESSION, VOLCANIC ERUPTION, COOL SPELL, WIND ADVISORY, GUSTY WIND/HAIL, RED FLAG FIRE WX, FIRST FROST, EXCESSIVELY DRY, HEAVY SEAS, FLASH FLOOD/FLOOD, SNOW AND SLEET, LIGHT SNOW/FREEZING PRECIP, VOG, EXCESSIVE RAINFALL, FLASH FLOODING, MONTHLY PRECIPITATION, MONTHLY TEMPERATURE, RECORD DRYNESS, EXTREME WINDCHILL TEMPERATURES, MIXED PRECIPITATION, EXTREME WIND CHILL, DRY CONDITIONS, HEAVY RAINFALL, REMNANTS OF FLOYD, EARLY SNOWFALL, FREEZING FOG, LANDSPOUT, DRIEST MONTH, RECORD  COLD, LATE SEASON HAIL, EXCESSIVE SNOW, WINTER MIX, DRYNESS, FLOOD/FLASH/FLOOD, WIND AND WAVE, SEVERE THUNDERSTORM, LIGHT FREEZING RAIN,  WIND, MONTHLY SNOWFALL, DRY, RECORD RAINFALL, RECORD PRECIPITATION, ICE ROADS, HIGH SEAS, SLEET, ROUGH SEAS, UNSEASONABLY WARM/WET, UNSEASONABLY COOL & WET, UNUSUALLY WARM, TSTM WIND G45, NON SEVERE HAIL, RECORD SNOW, SNOW/FREEZING RAIN, SNOW/BLOWING SNOW, NON-SEVERE WIND DAMAGE, UNUSUALLY COLD, WARM WEATHER, LANDSLUMP, THUNDERSTORM WIND (G40), LANDSLIDE, WALL CLOUD, HIGH WATER, UNSEASONABLY WARM & WET,  FLASH FLOOD, LOCALLY HEAVY RAIN, WIND GUSTS, UNSEASONAL LOW TEMP, HIGH SURF ADVISORY, LATE SEASON SNOW, GUSTY LAKE WIND, ABNORMALLY DRY, WINTER WEATHER MIX, RED FLAG CRITERIA, WND, CSTL FLOODING/EROSION, SMOKE,  WATERSPOUT, SNOW ADVISORY, EXTREMELY WET, UNUSUALLY LATE SNOW, VERY DRY, RECORD LOW RAINFALL, ROGUE WAVE, SNOWMELT FLOODING, PROLONG WARMTH, ACCUMULATED SNOWFALL, FALLING SNOW/ICE, DUST DEVEL, NON-TSTM WIND, NON TSTM WIND, BRUSH FIRE, GUSTY THUNDERSTORM WINDS, PATCHY ICE, SNOW SHOWERS, HEAVY RAIN EFFECTS, BLOWING DUST, EXCESSIVE HEAT/DROUGHT, NORTHERN LIGHTS, MARINE TSTM WIND,    HIGH SURF ADVISORY, WIND CHILL, HAZARDOUS SURF, WILDFIRE, FROST/FREEZE, WINTER WEATHER/MIX, ASTRONOMICAL HIGH TIDE, COLD WEATHER, WHIRLWIND, VERY WARM, ABNORMALLY WET, TORNADO DEBRIS, EXTREME COLD/WIND CHILL, ICE ON ROAD, FIRST SNOW, ICE/SNOW, DROWNING, GUSTY THUNDERSTORM WIND, MARINE HAIL, HIGH SURF ADVISORIES, HURRICANE/TYPHOON, HEAVY SURF/HIGH SURF, SLEET STORM, STORM SURGE/TIDE, COLD/WIND CHILL, LAKE-EFFECT SNOW, MARINE HIGH WIND, THUNDERSTORM WIND, TSUNAMI, DENSE SMOKE, LAKESHORE FLOOD, MARINE THUNDERSTORM WIND, MARINE STRONG WIND, ASTRONOMICAL LOW TIDE, VOLCANIC ASHFALL into our standard NOAA approved 48.

First order of business is to get a canonical list of the 48 standard events. We created a text file that listed the events one to a line using copy/paste from the NOAA document [here.](http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/Storm-Data-Export-Format.docx) 


```r
canonicalFile = "./data/valid-events.txt"
## might as well put into all lower, no extra spaces format used through rest of analysis
canonical <- str_to_lower(str_trim(readLines(canonicalFile)))
str(canonical)
```

```
##  chr [1:48] "astronomical low tide" "avalanche" "blizzard" ...
```

So let's first find which events are already valid in our working dataset:

```r
## valid will contain the canonical label, else NA if EVTYPE is bad when last checked
## and we populate with all the observations that already have a "good" EVTYPE
storms$valid <- canonical[match(str_to_lower(str_trim(storms$EVTYPE)), canonical)] 

## show all "bad" EVTYPE not accounted for
## used heavily interactively as we work problem EVTYPE
bad <- subset(storms, is.na(valid))
sort(unique(bad$EVTYPE))
```

```
##   [1]    HIGH SURF ADVISORY           TSTM WIND                    
##   [3]  TSTM WIND (G45)                WIND                         
##   [5] ABNORMAL WARMTH                ABNORMALLY DRY                
##   [7] ABNORMALLY WET                 ACCUMULATED SNOWFALL          
##   [9] AGRICULTURAL FREEZE            ASTRONOMICAL HIGH TIDE        
##  [11] Beach Erosion                  BEACH EROSION                 
##  [13] BITTER WIND CHILL              BITTER WIND CHILL TEMPERATURES
##  [15] Black Ice                      BLACK ICE                     
##  [17] Blizzard Summary               BLOW-OUT TIDE                 
##  [19] BLOW-OUT TIDES                 BLOWING DUST                  
##  [21] blowing snow                   Blowing Snow                  
##  [23] BRUSH FIRE                     COASTAL  FLOODING/EROSION     
##  [25] COASTAL EROSION                coastal flooding              
##  [27] Coastal Flooding               COASTAL FLOODING              
##  [29] COASTAL FLOODING/EROSION       Coastal Storm                 
##  [31] COASTAL STORM                  COASTALFLOOD                  
##  [33] COASTALSTORM                   Cold                          
##  [35] COLD                           Cold and Frost                
##  [37] COLD AND FROST                 COLD AND SNOW                 
##  [39] Cold Temperature               COLD TEMPERATURES             
##  [41] COLD WEATHER                   COLD WIND CHILL TEMPERATURES  
##  [43] COOL SPELL                     CSTL FLOODING/EROSION         
##  [45] DAM BREAK                      Damaging Freeze               
##  [47] DAMAGING FREEZE                DOWNBURST                     
##  [49] DRIEST MONTH                   Drifting Snow                 
##  [51] DROWNING                       DRY                           
##  [53] DRY CONDITIONS                 DRY MICROBURST                
##  [55] DRY SPELL                      DRY WEATHER                   
##  [57] DRYNESS                        DUST DEVEL                    
##  [59] Early Frost                    EARLY RAIN                    
##  [61] Early snowfall                 EARLY SNOWFALL                
##  [63] Erosion/Cstl Flood             Excessive Cold                
##  [65] EXCESSIVE HEAT/DROUGHT         EXCESSIVE RAIN                
##  [67] EXCESSIVE RAINFALL             EXCESSIVE SNOW                
##  [69] EXCESSIVELY DRY                Extended Cold                 
##  [71] Extreme Cold                   EXTREME COLD                  
##  [73] EXTREME WIND CHILL             EXTREME WINDCHILL             
##  [75] EXTREME WINDCHILL TEMPERATURES EXTREMELY WET                 
##  [77] FALLING SNOW/ICE               FIRST FROST                   
##  [79] FIRST SNOW                     FLASH FLOOD/FLOOD             
##  [81] FLASH FLOODING                 Flood/Flash Flood             
##  [83] FLOOD/FLASH/FLOOD              Flood/Strong Wind             
##  [85] FOG                            Freeze                        
##  [87] FREEZE                         Freezing drizzle              
##  [89] Freezing Drizzle               FREEZING DRIZZLE              
##  [91] Freezing rain                  Freezing Rain                 
##  [93] FREEZING RAIN                  FREEZING RAIN/SLEET           
##  [95] Freezing Spray                 Frost                         
##  [97] FROST                          FUNNEL CLOUDS                 
##  [99] Glaze                          GLAZE                         
## [101] gradient wind                  Gradient wind                 
## [103] GRADIENT WIND                  GUSTY LAKE WIND               
## [105] GUSTY THUNDERSTORM WIND        GUSTY THUNDERSTORM WINDS      
## [107] Gusty Wind                     GUSTY WIND                    
## [109] GUSTY WIND/HAIL                GUSTY WIND/HVY RAIN           
## [111] Gusty wind/rain                Gusty winds                   
## [113] Gusty Winds                    GUSTY WINDS                   
## [115] Hail(0.75)                     HAIL/WIND                     
## [117] HARD FREEZE                    HAZARDOUS SURF                
## [119] Heat Wave                      HEAT WAVE                     
## [121] Heatburst                      Heavy Precipitation           
## [123] Heavy Rain and Wind            HEAVY RAIN EFFECTS            
## [125] Heavy Rain/High Surf           HEAVY RAIN/WIND               
## [127] HEAVY RAINFALL                 HEAVY SEAS                    
## [129] Heavy snow shower              HEAVY SNOW SQUALLS            
## [131] Heavy Surf                     HEAVY SURF                    
## [133] Heavy surf and wind            HEAVY SURF/HIGH SURF          
## [135] HIGH  SWELLS                   HIGH SEAS                     
## [137] HIGH SURF ADVISORIES           HIGH SURF ADVISORY            
## [139] HIGH SWELLS                    HIGH WATER                    
## [141] HIGH WIND (G40)                HIGH WINDS                    
## [143] Hot and Dry                    HOT SPELL                     
## [145] HOT WEATHER                    HURRICANE                     
## [147] Hurricane Edouard              HURRICANE/TYPHOON             
## [149] HYPERTHERMIA/EXPOSURE          Hypothermia/Exposure          
## [151] HYPOTHERMIA/EXPOSURE           ICE                           
## [153] Ice Fog                        ICE JAM                       
## [155] Ice jam flood (minor           ICE ON ROAD                   
## [157] ICE PELLETS                    ICE ROADS                     
## [159] Ice/Snow                       ICE/SNOW                      
## [161] Icestorm/Blizzard              Icy Roads                     
## [163] ICY ROADS                      Lake Effect Snow              
## [165] LAKE EFFECT SNOW               LANDSLIDE                     
## [167] LANDSLIDES                     Landslump                     
## [169] LANDSLUMP                      LANDSPOUT                     
## [171] LATE FREEZE                    LATE SEASON HAIL              
## [173] LATE SEASON SNOW               Late Season Snowfall          
## [175] LATE SNOW                      Late-season Snowfall          
## [177] LIGHT FREEZING RAIN            Light snow                    
## [179] Light Snow                     LIGHT SNOW                    
## [181] Light Snow/Flurries            LIGHT SNOW/FREEZING PRECIP    
## [183] Light Snowfall                 LOCALLY HEAVY RAIN            
## [185] Marine Accident                MARINE TSTM WIND              
## [187] Metro Storm, May 26            Microburst                    
## [189] Mild and Dry Pattern           Minor Flooding                
## [191] MIXED PRECIP                   Mixed Precipitation           
## [193] MIXED PRECIPITATION            MODERATE SNOW                 
## [195] MODERATE SNOWFALL              MONTHLY PRECIPITATION         
## [197] Monthly Rainfall               MONTHLY RAINFALL              
## [199] Monthly Snowfall               MONTHLY SNOWFALL              
## [201] MONTHLY TEMPERATURE            Mountain Snows                
## [203] MUD SLIDE                      Mudslide                      
## [205] MUDSLIDE                       MUDSLIDE/LANDSLIDE            
## [207] Mudslides                      MUDSLIDES                     
## [209] No Severe Weather              NON SEVERE HAIL               
## [211] NON TSTM WIND                  NON-SEVERE WIND DAMAGE        
## [213] NON-TSTM WIND                  NONE                          
## [215] NORTHERN LIGHTS                Other                         
## [217] OTHER                          PATCHY DENSE FOG              
## [219] PATCHY ICE                     Prolong Cold                  
## [221] PROLONG COLD                   PROLONG WARMTH                
## [223] PROLONGED RAIN                 RAIN                          
## [225] RAIN (HEAVY)                   Rain Damage                   
## [227] RAIN/SNOW                      RECORD  COLD                  
## [229] Record Cold                    RECORD COLD                   
## [231] RECORD COOL                    Record dry month              
## [233] RECORD DRYNESS                 Record Heat                   
## [235] RECORD HEAT                    Record High                   
## [237] RECORD LOW RAINFALL            Record May Snow               
## [239] RECORD PRECIPITATION           RECORD RAINFALL               
## [241] RECORD SNOW                    RECORD SNOWFALL               
## [243] Record temperature             RECORD TEMPERATURE            
## [245] Record Temperatures            RECORD TEMPERATURES           
## [247] RECORD WARM                    RECORD WARM TEMPS.            
## [249] Record Warmth                  RECORD WARMTH                 
## [251] Record Winter Snow             RED FLAG CRITERIA             
## [253] RED FLAG FIRE WX               REMNANTS OF FLOYD             
## [255] RIP CURRENTS                   RIVER FLOOD                   
## [257] River Flooding                 RIVER FLOODING                
## [259] ROCK SLIDE                     ROGUE WAVE                    
## [261] ROUGH SEAS                     ROUGH SURF                    
## [263] Saharan Dust                   SAHARAN DUST                  
## [265] Seasonal Snowfall              SEVERE THUNDERSTORM           
## [267] SEVERE THUNDERSTORMS           SLEET STORM                   
## [269] SLEET/FREEZING RAIN            small hail                    
## [271] Small Hail                     SMALL HAIL                    
## [273] Sml Stream Fld                 SMOKE                         
## [275] Snow                           SNOW                          
## [277] Snow Accumulation              SNOW ADVISORY                 
## [279] Snow and Ice                   SNOW AND ICE                  
## [281] Snow and sleet                 SNOW AND SLEET                
## [283] SNOW DROUGHT                   SNOW SHOWERS                  
## [285] SNOW SQUALL                    Snow squalls                  
## [287] Snow Squalls                   SNOW SQUALLS                  
## [289] SNOW/BLOWING SNOW              SNOW/FREEZING RAIN            
## [291] SNOW/ICE                       SNOW/SLEET                    
## [293] SNOWMELT FLOODING              STORM SURGE                   
## [295] STREET FLOODING                STRONG WIND GUST              
## [297] Strong winds                   Strong Winds                  
## [299] STRONG WINDS                   Summary August 10             
## [301] Summary August 11              Summary August 17             
## [303] Summary August 2-3             Summary August 21             
## [305] Summary August 28              Summary August 4              
## [307] Summary August 7               Summary August 9              
## [309] Summary Jan 17                 Summary July 23-24            
## [311] Summary June 18-19             Summary June 5-6              
## [313] Summary June 6                 Summary of April 12           
## [315] Summary of April 13            Summary of April 21           
## [317] Summary of April 27            Summary of April 3rd          
## [319] Summary of August 1            Summary of July 11            
## [321] Summary of July 2              Summary of July 22            
## [323] Summary of July 26             Summary of July 29            
## [325] Summary of July 3              Summary of June 10            
## [327] Summary of June 11             Summary of June 12            
## [329] Summary of June 13             Summary of June 15            
## [331] Summary of June 16             Summary of June 18            
## [333] Summary of June 23             Summary of June 24            
## [335] Summary of June 3              Summary of June 30            
## [337] Summary of June 4              Summary of June 6             
## [339] Summary of March 14            Summary of March 23           
## [341] Summary of March 24            SUMMARY OF MARCH 24-25        
## [343] SUMMARY OF MARCH 27            SUMMARY OF MARCH 29           
## [345] Summary of May 10              Summary of May 13             
## [347] Summary of May 14              Summary of May 22             
## [349] Summary of May 22 am           Summary of May 22 pm          
## [351] Summary of May 26 am           Summary of May 26 pm          
## [353] Summary of May 31 am           Summary of May 31 pm          
## [355] Summary of May 9-10            Summary Sept. 25-26           
## [357] Summary September 20           Summary September 23          
## [359] Summary September 3            Summary September 4           
## [361] Summary: Nov. 16               Summary: Nov. 6-7             
## [363] Summary: Oct. 20-21            Summary: October 31           
## [365] Summary: Sept. 18              Temperature record            
## [367] Thundersnow shower             THUNDERSTORM                  
## [369] THUNDERSTORM WIND (G40)        THUNDERSTORMS                 
## [371] Tidal Flooding                 TIDAL FLOODING                
## [373] TORNADO DEBRIS                 Torrential Rainfall           
## [375] TSTM                           TSTM HEAVY RAIN               
## [377] Tstm Wind                      TSTM WIND                     
## [379] TSTM WIND  (G45)               TSTM WIND (41)                
## [381] TSTM WIND (G35)                TSTM WIND (G40)               
## [383] TSTM WIND (G45)                TSTM WIND 40                  
## [385] TSTM WIND 45                   TSTM WIND AND LIGHTNING       
## [387] TSTM WIND G45                  TSTM WIND/HAIL                
## [389] TSTM WINDS                     TSTM WND                      
## [391] TYPHOON                        Unseasonable Cold             
## [393] UNSEASONABLY COLD              UNSEASONABLY COOL             
## [395] UNSEASONABLY COOL & WET        UNSEASONABLY DRY              
## [397] UNSEASONABLY HOT               UNSEASONABLY WARM             
## [399] UNSEASONABLY WARM & WET        UNSEASONABLY WARM AND DRY     
## [401] UNSEASONABLY WARM YEAR         UNSEASONABLY WARM/WET         
## [403] UNSEASONABLY WET               UNSEASONAL LOW TEMP           
## [405] UNSEASONAL RAIN                UNUSUAL WARMTH                
## [407] UNUSUAL/RECORD WARMTH          UNUSUALLY COLD                
## [409] UNUSUALLY LATE SNOW            UNUSUALLY WARM                
## [411] Urban flood                    Urban Flood                   
## [413] URBAN FLOOD                    Urban Flooding                
## [415] URBAN/SMALL STRM FLDG          URBAN/SML STREAM FLD          
## [417] URBAN/SML STREAM FLDG          URBAN/STREET FLOODING         
## [419] VERY DRY                       VERY WARM                     
## [421] VOG                            Volcanic Ash Plume            
## [423] VOLCANIC ASHFALL               VOLCANIC ERUPTION             
## [425] WAKE LOW WIND                  WALL CLOUD                    
## [427] WARM WEATHER                   WATERSPOUTS                   
## [429] wet micoburst                  WET MICROBURST                
## [431] Wet Month                      Wet Year                      
## [433] Whirlwind                      WHIRLWIND                     
## [435] WILD/FOREST FIRE               Wind                          
## [437] WIND                           WIND ADVISORY                 
## [439] WIND AND WAVE                  WIND CHILL                    
## [441] Wind Damage                    WIND DAMAGE                   
## [443] WIND GUSTS                     WINDS                         
## [445] WINTER MIX                     WINTER WEATHER MIX            
## [447] WINTER WEATHER/MIX             WINTERY MIX                   
## [449] Wintry mix                     Wintry Mix                    
## [451] WINTRY MIX                     WND                           
## 985 Levels:    HIGH SURF ADVISORY  COASTAL FLOOD ... WND
```

```r
costs <- storms %>%
  group_by(EVTYPE) %>%
  summarise(damage = sum(prop, crop), death = sum(FATALITIES), injury = sum(INJURIES))
```

## Results: Impact to life, limb and property

