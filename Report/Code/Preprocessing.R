# Cleaning and summarizing data from the NTSB Incident database

# Step 0:  Load the NTSB output file:
# The NTSB text output has a header with the variable names, and uses the "|" 
# character as the field separator character.

# The following commands removes the field separator character and transforms
# the variable names into ones appropriate for a data frame (replacing any spaces in the variable name with a period). 

filename <- "AviationData.txt" 
ntsb.data.raw = read.delim(filename, header=TRUE, sep="|",  quote = "",dec = ".",fill = TRUE, comment.char = "")

## Fixing the Location Column
city.locs.ndx = union(which(!is.na(ntsb.data.raw$Location)), which(!is.na(ntsb.data.raw$Country)) )
ntsb.data.raw = ntsb.data.raw[city.locs.ndx,]
extra.abb = c("AS", "DC", "FM","GU","MH", "MP", "PW", "PR", "VI", "GM", "AO","PO")
extra.name = c("American Samoa", "District of Columbia", "Federated States of Micronesia",
               "Guam", "Marshall Islands", "Northern Mariana Islands", "Palau",
               "Puerto Rico", "Virgin Islands", "Gulf of Mexico", "Atlantic Ocean",
               "Pacific Ocean")

# Now append them to the R-provided list of 50 state names and abbreviations
usps.abb = c(state.abb,extra.abb)
usps.state = c(state.name,extra.name)

# These three vectors each initialized with a number of NA values equal to the
# number of rows to ensure that the final vector will be compatible with ntsb.data
comma.pos = rep(NA,nrow(ntsb.data.raw)) # start with NA for number of commas for all Locations
city.vec = rep(NA,nrow(ntsb.data.raw))
state.vec = rep(NA,nrow(ntsb.data.raw))

for(x in 1:length(city.locs.ndx)){
  # Create a list that contains vector of comma positions
  comma.pos.list = gregexpr(",", ntsb.data.raw$Location[city.locs.ndx[x]], fixed=TRUE)
  comma.pos.vec = comma.pos.list[[1]] # Vector of comma positions
  comma.pos[x] = comma.pos.vec[length(comma.pos.vec)]
  
  # Get the length of the Location field character string
  num.chars = nchar(ntsb.data.raw$Location[city.locs.ndx[x]]) 
  
  # Use last comma position to determine where split character string and find state code
  city.vec[city.locs.ndx[x]] =  substr(ntsb.data.raw$Location[city.locs.ndx[x]], 1,(comma.pos[x]-1)) 
  state.vec[city.locs.ndx[x]] =  substr(ntsb.data.raw$Location[city.locs.ndx[x]], (comma.pos[x]+2),num.chars)
} 

# Can now add city and state abbreviations to data frame
library(tools)
library(dplyr)
ntsb.data.raw$City=city.vec
ntsb.data.raw$City = toTitleCase(tolower(ntsb.data.raw$City)) # Ensure city names are in title case
ntsb.data.raw$State.code=state.vec


ntsb.data = ntsb.data.raw

# DATA CLEANING

# Step 1: Eliminate any columns (variables) added as a consequence of Step 0,
# columns which have no data (all are NA) 
elim.var = which(apply(ntsb.data, 2, function(x) all(is.na(x))))
elim.var = as.data.frame(elim.var) # Column name and column number of this data frame are the columns to be cut

# Eliminates all columns with only NA values
if (nrow(elim.var)>0) ntsb.data = ntsb.data[,-elim.var[,1]] 
# Eliminates rows by Investigation Types based on certain parameters
ntsb.data = ntsb.data[!is.na(ntsb.data$Investigation.Type),]
ntsb.data = ntsb.data[!(ntsb.data$Investigation.Type=="Incident"),]

# Step 2: Ensure character variables are of type character

# As part of the process, need to ensure that there are not extra spaces
# at the beginning or end of each character value

# ================================
# Use function 'stripper', which works like the str_trim() function in the strigr package  
# Using this function to use only base R package.

stripper <- function(x){
  # This function removes leading and trailing spaces from a vector.
  # Space characters include tab, newline, vertical tab, form feed, 
  # carriage return, space.
  # Equivalent to the str_trim() function in the strigr package   
  x = as.character(x)
  x = sub("[[:space:]]+$", "", x) # Remove leading space characters
  x = sub("^[[:space:]]+", "", x) # Remove trailing space characters
  return(x)
}
# ================================

# Find which columns match the following known character vectors
char.vecs = c("Event.Id","Investigation.Type","Accident.Number",       
              "Location", "Country", "Airport.Code", "Airport.Name", "Injury.Severity",
              "Aircraft.Damage", "Aircraft.Category", "Registration.Number", "Make",
              "Model", "Amateur.Built", "Engine.Type", "FAR.Description","Schedule",              
              "Purpose.of.Flight", "Air.Carrier", "Weather.Condition",
              "Broad.Phase.of.Flight", "Report.Status")

char.vecs.ndx = which(colnames(ntsb.data) %in% char.vecs)

# Ensure that the character variable is of type character, then remove extra spaces
for (i in 1:length(char.vecs.ndx)) {
  ntsb.data[,char.vecs.ndx[i]] = as.character(ntsb.data[,char.vecs.ndx[i]])
  ntsb.data[,char.vecs.ndx[i]] = stripper(ntsb.data[,char.vecs.ndx[i]])
}

# Step 3: Ensure numerical variables are of type numeric

# Find which columns match the following known numerical vectors
num.vecs = c("Latitude", "Longitude", "Number.of.Engines", "Total.Fatal.Injuries",  
             "Total.Serious.Injuries", "Total.Minor.Injuries", "Total.Uninjured")

# Creates a vector for the column numbers for numeric variables
num.vecs.ndx = which((colnames(ntsb.data)) %in% num.vecs)

# Note: This step appears to replace missing numeric values with NA
for (i in 1:length(num.vecs.ndx)) {
  ntsb.data[,num.vecs.ndx[i]] = as.numeric(as.character(ntsb.data[,num.vecs.ndx[i]]))
}

# Step 4: Change date variables into a format suitable for R

# Dates are in form mm/dd/YYYY, must convert to a date format of YYYY-mm-dd
# Two date variables, Event.Date and Pulication Date
ntsb.data$Event.Date = as.Date(ntsb.data$Event.Date, "%m/%d/%Y")
ntsb.data$Publication.Date = as.Date(ntsb.data$Publication.Date, "%m/%d/%Y")
# Note: This step appears to replace missing date values with NA

# Now will have separate columns for Year, Month, Day, and Weekday for Event.Date
ntsb.data$Year = as.numeric(format(ntsb.data$Event.Date, "%Y")) # Ensure it is a numeric variable
ntsb.data$Month = months(ntsb.data$Event.Date, abbreviate = TRUE)
ntsb.data$Day = format(ntsb.data$Event.Date, "%d")
ntsb.data$Weekday = weekdays(ntsb.data$Event.Date, abbreviate = TRUE)

# DATA CONVERSION: Ordering days of the week and months of the year 

# Make months and days of the week factors and order them as they are in a calendar
ntsb.data$Month = factor(ntsb.data$Month,levels=c("Jan", "Feb","Mar", "Apr","May",
                                                  "Jun","Jul","Aug","Sep","Oct", "Nov","Dec"), ordered=TRUE)

ntsb.data$Weekday = factor(ntsb.data$Weekday,levels=c("Sun","Mon","Tue",
                                                      "Wed","Thu","Fri","Sat"), ordered=TRUE)

# Step 5 Eliminate any rows (records) which has no date (Event.Date is NA)
elim.row = which(is.na(ntsb.data$Event.Date))
if (length(elim.row)>0) ntsb.data = ntsb.data[-elim.row,] # Eliminates all rows with only NA value for Event.Date

# Step 6: Change blank, "N/A","Unknown", and similar responses to NA

# Must first put airport names and make in title case, first by using tolower() followed by toTitleCase()
# This has to be done before any effort to replace words like "UNK", and "NA" with NA

# Will install {tools} package for title case function
# The first step is to install new packages that will be needed for the analysis.
# In this case, the function needed is toTitleCase(text)
options(repos = c(CRAN = "http://cran.rstudio.com"))
if("tools" %in% rownames(installed.packages()) == FALSE) 
{install.packages("tools")}
library(tools)

ntsb.data$Airport.Name = toTitleCase(tolower(ntsb.data$Airport.Name))
ntsb.data$Air.Carrier = toTitleCase(tolower(ntsb.data$Air.Carrier))
ntsb.data$Make = toTitleCase(tolower(ntsb.data$Make))
# NOTE: Not a perfect solution, does not take into account names like McFarland, will end up as Mcfarland

# First, define  a vector of words or phrases that are to be replaced with NA
repl.words = c("", ".", "N/A", "n/a", "N/a","NA","na","Na", "none", "None", "UNK","Unknown","Unavailable", "UNKNOWN", "unknown", "Unk", "unk")
# Note that the following checks all columns, even non-character one, but in the end,
# only character columns will have any non-zero values for replacement words
repl.list = apply(ntsb.data,2, function(x) which(x %in% repl.words))

# The number of times replacement words occur for each database variable (column), 
# is placed in a one-column data frame where the row names correspond to the 
# variable names the column has the number of times the replacement words occur
with.missing = as.data.frame(sapply(repl.list,length)) 
colnames(with.missing) = "Replacement.Words"

# Identify columns corresponding with character-based
# variables with at least one replacement word
with.missing.ndx = which(with.missing[,1]>0) 

# Replace replacement words with NAs in those columns containing
# one or more replacement words

for(i in 1:length(with.missing.ndx)){
  repl.vector=ntsb.data[,with.missing.ndx[i]]%in%repl.words 
  ntsb.data[repl.vector,with.missing.ndx[i]]=NA
} 


# Step 7: Add a new variable that has the maximum injury outcome (Fatal, Injury, or None) for
#       every record with at least one non-NA value for the number of fatalities,
#       injuries (serious, minor, or none). Total number of fatalities already given by 
#       variable Total.Fatal.Injuries, so just need the non-NA  values greater than zero for fatal Max.Injury value

# First, the Max.Injury fatals
ntsb.data$Max.Injury = rep(NA,nrow(ntsb.data)) # start with NA for maximum injury
fatal.ndx = which(ntsb.data$Total.Fatal.Injuries > 0)
ntsb.data$Max.Injury[fatal.ndx] = "Fatal"

# Second, the Max.Injury injuries. For Max.Injury to be equal to Injury, there must be at 
#       least one minor or serious injury, and no fatalities
#       Get the index of fatals and the index of injuries,
#       find the intersection, then take away the intersection from the
#       index of injuries.

injury.ndx = which(ntsb.data$Total.Serious.Injuries > 0 | ntsb.data$Total.Minor.Injuries>0)
injury.only.ndx = setdiff(injury.ndx,intersect(injury.ndx,fatal.ndx)) # Injuries, but not fatalitiesf
ntsb.data$Max.Injury[injury.only.ndx] = "Injury"

# Lastly, the no injury events, which would be the no injury events, 
#       excluding the Max.Injury injury events previously marked as "Fatal" or "Injury"
no.injury.ndx = which(ntsb.data$Total.Uninjured > 0)
no.injury.ndx = setdiff(no.injury.ndx,union(fatal.ndx,injury.only.ndx))
ntsb.data$Max.Injury[no.injury.ndx] = "None"

# Also adding a variable for total total injuries, and total number of people involved
#       for records with non-NA data

ntsb.data$Total.Injured = rep(NA,nrow(ntsb.data))
ntsb.data$Total.Injured[injury.ndx] = apply(ntsb.data[injury.ndx, c("Total.Serious.Injuries","Total.Minor.Injuries")],1,sum,na.rm=TRUE)

# For total number of people involved, need index of every row with at least one non-zero value for
#       fatals, minor injuries, serious injuries, or no injuries
occupied.ndx = unique(sort(c(fatal.ndx,injury.ndx,no.injury.ndx))) # non-duplicated

ntsb.data$Total.Involved = rep(NA,nrow(ntsb.data))
ntsb.data$Total.Involved[occupied.ndx] = apply(ntsb.data[occupied.ndx, c("Total.Fatal.Injuries","Total.Serious.Injuries","Total.Minor.Injuries","Total.Uninjured")],1,sum,na.rm=TRUE)


# Step 8: Arrange the new columns in logical groupings

new.cols = c("Event.Id", "Investigation.Type", "Accident.Number",
             "Event.Date", "Year", "Month", "Day", "Weekday",
             "Location", "City", "State.code", "Country", 
             "Airport.Code", "Airport.Name", "Latitude", "Longitude",
             "Injury.Severity", "Aircraft.Damage", "Aircraft.Category",
             "Registration.Number", "Make", "Model", "Amateur.Built",
             "Number.of.Engines", "Engine.Type", "FAR.Description", "Schedule",
             "Purpose.of.Flight", "Air.Carrier", "Total.Fatal.Injuries",
             "Total.Serious.Injuries", "Total.Minor.Injuries", "Total.Injured", "Total.Uninjured", "Total.Involved",
             "Max.Injury",
             "Weather.Condition", "Broad.Phase.of.Flight", "Report.Status",
             "Publication.Date")

ntsb.data = ntsb.data[,new.cols]

# Further Steps
ntsb.data = ntsb.data %>% filter(ntsb.data$Aircraft.Category == "Airplane")
ntsb.data = ntsb.data %>% filter(ntsb.data$Country == "United States")
ntsb.data$Injury.Severity = gsub('[[:digit:]]+',"",as.character(ntsb.data$Injury.Severity))
ntsb.data$Injury.Severity = gsub("[\\*\\(\\)]","",as.character(ntsb.data$Injury.Severity))
ntsb.data = ntsb.data[!(ntsb.data$Injury.Severity=="Incident"),]

# Step 9 (Final step): Save the processed data frame as a CSV
write.csv(ntsb.data, file = "ntsb_data.csv", row.names = FALSE)

####################################################################################
# SUMMARY STATISTICS

paste("Total number of records - ", format(nrow(ntsb.data.raw), big.mark=","), sep="") 

paste("Number of records excluded - ", nrow(ntsb.data.raw)-nrow(ntsb.data)) 

paste("Number of records processed - ", format(nrow(ntsb.data), big.mark=","), sep="") 

paste("Number of events with an indentifiable number of occupants - ", format(length(occupied.ndx), big.mark=","), sep="") 

paste("Number of events with either fatalities or injuries - ", format(length(union(fatal.ndx,injury.only.ndx)), big.mark=","), sep="") 

paste("Number of records with a US location - ", format(nrow(ntsb.data[which(ntsb.data$Country=="United States"),]), big.mark=","),".", sep="")

paste("Total number of records involving fatalities - ", format(sum(ntsb.data$Total.Fatal.Injuries>=1, na.rm=TRUE), big.mark=","),".", sep="") 

paste("Total fatalities - ", format(sum(ntsb.data$Total.Fatal.Injuries, na.rm=TRUE), big.mark=","),".", sep="") 

print("Table of reports by state")
table(ntsb.data[which(ntsb.data$Year>=2005),]$State.code)

print("Table of top 15 states by number of events")
sort(table(ntsb.data[ntsb.data$Year>=2005,]$State.code), decreasing = TRUE)[1:15]


# Vector of fatal events
fatal.vec = which(ntsb.data$Total.Fatal.Injuries>=1 & ntsb.data$Year>=2005)

# Data frame of all fatals from 2005 and beyond
fatal.df = ntsb.data[fatal.vec,]

# Histogram of top 15 states by number of events
barplot(sort(table(ntsb.data[ntsb.data$Year>=2005,]$State.code),
             decreasing = TRUE)[1:15], col="dodgerblue",xlab="State", ylab="Events", 
        cex.names = 0.7, main="Top 15 states by number of events")


print("Table of top 15 states by number of fatal events")
# Vector of all fatals for events with a US state code
sort(table(ntsb.data[fatal.vec,]$State.code), decreasing = TRUE)[1:15]

# Top 15 states by total fatalities
print("Table of top 15 states by number of fatalities")
sort(as.table(tapply(fatal.df$Total.Fatal.Injuries, fatal.df$State.code, sum)), decreasing = TRUE)[1:15]

# Histogram of top 15 states by number of fatal events 
barplot(sort(table(ntsb.data[fatal.vec,]$State.code), decreasing = TRUE)[1:15],
        col="dodgerblue",xlab="State", ylab="Fatal events",
        cex.names = 0.7, main="Top 15 states by number of fatal events")


# Table of events by day of the week 
print("Events by day of the week")
table(ntsb.data[ntsb.data$Year>=2005,]$Weekday)

# Histogram events by day of the week
barplot(table(ntsb.data[ntsb.data$Year>=2005,]$Weekday), col="dodgerblue",
        xlab="Day", ylab="Events", main="Events by day of week")

# Table of fatal events by day of the week 
print("Fatal events by day of the week")
table(fatal.df$Weekday)

# Histogram of fatal events by day of the week
barplot(table(fatal.df$Weekday), col="dodgerblue", cex.names = 1.2,
        xlab="Day", ylab="Events", main="Fatal events by day of week")

# Table of events by month of the year
print("Events by month of the year")
table(ntsb.data[ntsb.data$Year>=2005,]$Month)

# Histogram of events by month of the year
barplot(table(ntsb.data[which(ntsb.data$Year>=2005),]$Month), col="dodgerblue", cex.names = 0.8,
        xlab="Month", ylab="Events", main="Events by month")

# Table of fatal events by month of the year
print("Fatal events by month of the year")
table(fatal.df$Month)

# Histogram of fatal events by month of the year
barplot(table(fatal.df$Month), col="dodgerblue", cex.names = 0.8,
        xlab="Day", ylab="Fatal events", main="Fatal events by month")



# Plot of events by year
barplot(table(ntsb.data[ntsb.data$Year>=2005,]$Year), col="dodgerblue",
        xlab="Year", ylab="Events", main="Events by year")


# Histogram of fatal events by year
barplot(table(ntsb.data[fatal.vec,]$Year), col="dodgerblue",
        xlab="Year", ylab="Fatal events", main="Fatal events by year")

# Fatalites by year

# Do a tapply for sums by category then ensure it is table
death.table = as.table(tapply(fatal.df$Total.Fatal.Injuries, fatal.df$Year, sum))

barplot(death.table, col="dodgerblue",
        xlab="Year", ylab="Fatalities", main="Fatalities by year")

# Total fatalities by US state and territory
as.table(tapply(fatal.df$Total.Fatal.Injuries, fatal.df$State.code, sum))

# Total fatal events  by US state and territory
table(fatal.df$State.code)

# Total fatal events by US state and territory sorted
sort(table(fatal.df$State.code), decreasing=TRUE)

################################################################################
ntsb.data = ntsb.data %>% filter(ntsb.data$FAR.Description == "Part 91: General Aviation")
