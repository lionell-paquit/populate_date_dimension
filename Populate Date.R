#install.packages("lubridate")

library(lubridate)

#function to identify weekday names in different languages
WeekNames <- function(x, lang) {
  if (lang == "spanish") {
    switch(x, "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
    
  } else if (lang == "french") {
    switch(x, "Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    
  } else if (lang == "german")
    switch(x, "Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")
}

#function to identify month names in different languages
MonthNames <- function(x, lang) {
  if (lang == "spanish") {
    switch(x, "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubure", "Noviembre", "Diciembre")
    
  } else if (lang == "french") {
    switch(x, "Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "November", "Décembre")
    
  } else if (lang == "german")
    switch(x, "Januar", "Februar", "Marz", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember")
}

#function that populate the date dimension
PopDateInfo <- function(currentDate, endDate) {
  i <- 1
  dateInfo <- data.frame(NULL)
  while (currentDate < endDate) {
    dateInfo[i,1] <- format(currentDate, format="%Y%m%d") #DateID
    dateInfo[i,2] <- as.character(currentDate) #FullDate
    dateInfo[i,3] <- wday(currentDate, week_start = getOption("lubridate.week.start", 1)) #DayNumberOfWeek 1 Starts on Monday
    dateInfo[i,4] <- weekdays(currentDate) #EnglishDayNameofWeek
    dateInfo[i,5] <- WeekNames(dateInfo[i,3], "spanish") #SpanishDayNameOfWeek
    dateInfo[i,6] <- WeekNames(dateInfo[i,3], "french") #FrenchDayNameOfWeek
    dateInfo[i,7] <- WeekNames(dateInfo[i,3], "german") #GermanDayNameOfWeek
    dateInfo[i,8] <- wday(currentDate, label = TRUE, abbr =TRUE) #DayNameOfWeekAbbr
    dateInfo[i,9] <- month(currentDate) #MonthNumberOfYear
    dateInfo[i,10] <- months(currentDate) #EnglishMonthName
    dateInfo[i,11] <- MonthNames(dateInfo[i,9], "spanish") #SpanishMonthname
    dateInfo[i,12] <- MonthNames(dateInfo[i,9], "french") #FrenchMonthName
    dateInfo[i,13] <- MonthNames(dateInfo[i,9], "german") #GermanMonthName
    dateInfo[i,14] <- month(currentDate, label = TRUE, abbr =TRUE) #MonthNameAbbr
    dateInfo[i,15] <- year(currentDate) #Calendar Year
    dateInfo[i,16] <- quarter(currentDate) #Calendar Quarter
    dateInfo[i,17] <- semester(currentDate) #Calendar Semester
  
    
    currentDate <- currentDate + 1
    i <- i+1
  }
  return(dateInfo)
}

#clear environment
#rm(list = ls())

# initialise the range
startDate <- as.Date("2001-01-01") #beginning date
endDate <- as.Date("2018-01-01") #ending date

#call populate date function
dateInfo <- PopDateInfo(startDate, endDate)

#label column names
colnames(dateInfo) <- c("DateID","FullDate", "DayNumberOfWeek",
                        "EnglishDayNameOfWeek", "SpanishDayNameOfWeek", "FrenchDayNameOfWeek", "GermanDayNameOfWeek",
                        "DayNameofWeekAbbr", "MonthNumberOfYear",
                        "EnglishMonthName", "SpanishMonthName", "FrenchMonthName", "GermanMonthName",
                        "MonthNameAbbr", "CalendarYear", "CalendarQuarter", "CalendarSemester")

# write data frame into csv files for bulk insert in etl later
setwd("C://DAT701")
write.table(dateInfo, "DimDate_Lookup.csv", append = FALSE, quote = FALSE, sep = ",", col.names = TRUE, row.names = FALSE, eol = "\n")

