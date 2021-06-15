read_daily_discharge_data <- function(){
  
  # Function from Hubbard Brook Ecosystem Study website
  
  # Package ID: knb-lter-hbr.2.11 Cataloging System:https://pasta.edirepository.org.
  # Data set title: Hubbard Brook Experimental Forest: Daily Streamflow by Watershed, 1956 - present.
  # Data set creator:    - USDA Forest Service, Northern Research Station 
  # Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
  # Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 
  
  inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/2/11/1254d17cbd381556c05afa740d380e78" 
  infile1 <- tempfile()
  try(download.file(inUrl1,infile1,method="curl"))
  if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
  
  
  Q_daily <-read.csv(infile1,header=F,
                 skip=1,
                 sep=",",
                 quot='"',
                 col.names=c(
                   "DATE",     
                   "WS",     
                   "Streamflow",     
                   "Flag"),
                 check.names=TRUE)
  
  unlink(infile1)
  
  # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
  
  # attempting to convert Q_daily$DATE dateTime string to R date structure (date or POSIXct)                                
  tmpDateFormat<-"%Y-%m-%d"
  tmp1DATE<-as.Date(Q_daily$DATE,format=tmpDateFormat)
  # Keep the new dates only if they all converted correctly
  if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){Q_daily$DATE <- tmp1DATE } else {print("Date conversion failed for Q_daily$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
  rm(tmpDateFormat,tmp1DATE) 
  if (class(Q_daily$WS)!="factor") Q_daily$WS<- as.factor(Q_daily$WS)
  if (class(Q_daily$Streamflow)=="factor") Q_daily$Streamflow <-as.numeric(levels(Q_daily$Streamflow))[as.integer(Q_daily$Streamflow) ]               
  if (class(Q_daily$Streamflow)=="character") Q_daily$Streamflow <-as.numeric(Q_daily$Streamflow)
  if (class(Q_daily$Flag)=="factor") Q_daily$Flag <-as.numeric(levels(Q_daily$Flag))[as.integer(Q_daily$Flag) ]               
  if (class(Q_daily$Flag)=="character") Q_daily$Flag <-as.numeric(Q_daily$Flag)
  
  # Convert Missing Values to NA for non-dates
  
  Q_daily$Flag <- ifelse((trimws(as.character(Q_daily$Flag))==trimws("NA")),NA,Q_daily$Flag)               
  suppressWarnings(Q_daily$Flag <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(Q_daily$Flag))==as.character(as.numeric("NA"))),NA,Q_daily$Flag))
  
  
  # Here is the structure of the input data frame:
  str(Q_daily)                            
  attach(Q_daily)                            
  # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
  
  summary(DATE)
  summary(WS)
  summary(Streamflow)
  summary(Flag) 
  # Get more details on character variables
  
  summary(as.factor(Q_daily$WS))
  detach(Q_daily)               
   
}