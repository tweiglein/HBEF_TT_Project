read_stream_conc_data <- function(){
  
  # Function from Hubbard Brook Ecosystem Study website
  
  # Package ID: knb-lter-hbr.208.4 Cataloging System:https://pasta.edirepository.org.
  # Data set title: Continuous precipitation and stream chemistry data, Hubbard Brook Ecosystem Study, 1963 - present
  # Data set creator:    - Hubbard Brook Watershed Ecosystem Record (HBWatER) 
  # Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
  # Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 
  
  inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/208/4/86b854394a3acde2b4958787b7b5f5aa" 
  infile2 <- tempfile()
  try(download.file(inUrl2,infile2,method="curl"))
  if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")
  
  stream_conc_data <-read.csv(infile2,
                              header=F,
                              skip=1,
                              sep=",",
                              quot='"',
                              col.names=c(
                                "site",
                                "date",
                                "timeEST",
                                "pH",
                                "DIC",
                                "spCond",
                                "temp",
                                "ANC960",
                                "ANCMet",
                                "gageHt",
                                "hydroGraph",
                                "flowGageHt",
                                "fieldCode",
                                "notes",
                                "uniqueID",
                                "waterYr",
                                "datetime",
                                "Ca",
                                "Mg",
                                "K",
                                "Na",
                                "TMAl",
                                "OMAl",
                                "Al_ICP",
                                "NH4",
                                "SO4",
                                "NO3",
                                "Cl",
                                "PO4",
                                "DOC",
                                "TDN",
                                "DON",
                                "SiO2",
                                "Mn",
                                "Fe",
                                "F",
                                "cationCharge",
                                "anionCharge",
                                "theoryCond",
                                "ionError",
                                "duplicate",
                                "sampleType",
                                "ionBalance",
                                "canonical",
                                "pHmetrohm",
                                "archived"),
                              check.names=TRUE)
  
  unlink(infile2)
  
  # Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
  
  if (class(stream_conc_data$site)!="factor") stream_conc_data$site<- as.factor(stream_conc_data$site)                                   
  # attempting to convert stream_conc_data$date dateTime string to R date structure (date or POSIXct)                                
  tmpDateFormat<-"%Y-%m-%d"
  tmp2date<-as.Date(stream_conc_data$date,format=tmpDateFormat)
  # Keep the new dates only if they all converted correctly
  if(length(tmp2date) == length(tmp2date[!is.na(tmp2date)])){stream_conc_data$date <- tmp2date } else {print("Date conversion failed for stream_conc_data$date. Please inspect the data and do the date conversion yourself.")}                                                                    
  rm(tmpDateFormat,tmp2date) 
  if (class(stream_conc_data$pH)=="factor") stream_conc_data$pH <-as.numeric(levels(stream_conc_data$pH))[as.integer(stream_conc_data$pH) ]               
  if (class(stream_conc_data$pH)=="character") stream_conc_data$pH <-as.numeric(stream_conc_data$pH)
  if (class(stream_conc_data$DIC)=="factor") stream_conc_data$DIC <-as.numeric(levels(stream_conc_data$DIC))[as.integer(stream_conc_data$DIC) ]               
  if (class(stream_conc_data$DIC)=="character") stream_conc_data$DIC <-as.numeric(stream_conc_data$DIC)
  if (class(stream_conc_data$spCond)=="factor") stream_conc_data$spCond <-as.numeric(levels(stream_conc_data$spCond))[as.integer(stream_conc_data$spCond) ]               
  if (class(stream_conc_data$spCond)=="character") stream_conc_data$spCond <-as.numeric(stream_conc_data$spCond)
  if (class(stream_conc_data$temp)=="factor") stream_conc_data$temp <-as.numeric(levels(stream_conc_data$temp))[as.integer(stream_conc_data$temp) ]               
  if (class(stream_conc_data$temp)=="character") stream_conc_data$temp <-as.numeric(stream_conc_data$temp)
  if (class(stream_conc_data$ANC960)=="factor") stream_conc_data$ANC960 <-as.numeric(levels(stream_conc_data$ANC960))[as.integer(stream_conc_data$ANC960) ]               
  if (class(stream_conc_data$ANC960)=="character") stream_conc_data$ANC960 <-as.numeric(stream_conc_data$ANC960)
  if (class(stream_conc_data$ANCMet)=="factor") stream_conc_data$ANCMet <-as.numeric(levels(stream_conc_data$ANCMet))[as.integer(stream_conc_data$ANCMet) ]               
  if (class(stream_conc_data$ANCMet)=="character") stream_conc_data$ANCMet <-as.numeric(stream_conc_data$ANCMet)
  if (class(stream_conc_data$gageHt)=="factor") stream_conc_data$gageHt <-as.numeric(levels(stream_conc_data$gageHt))[as.integer(stream_conc_data$gageHt) ]               
  if (class(stream_conc_data$gageHt)=="character") stream_conc_data$gageHt <-as.numeric(stream_conc_data$gageHt)
  if (class(stream_conc_data$hydroGraph)!="factor") stream_conc_data$hydroGraph<- as.factor(stream_conc_data$hydroGraph)
  if (class(stream_conc_data$flowGageHt)=="factor") stream_conc_data$flowGageHt <-as.numeric(levels(stream_conc_data$flowGageHt))[as.integer(stream_conc_data$flowGageHt) ]               
  if (class(stream_conc_data$flowGageHt)=="character") stream_conc_data$flowGageHt <-as.numeric(stream_conc_data$flowGageHt)
  if (class(stream_conc_data$fieldCode)!="factor") stream_conc_data$fieldCode<- as.factor(stream_conc_data$fieldCode)
  if (class(stream_conc_data$notes)!="factor") stream_conc_data$notes<- as.factor(stream_conc_data$notes)
  if (class(stream_conc_data$uniqueID)!="factor") stream_conc_data$uniqueID<- as.factor(stream_conc_data$uniqueID)                                   
  # attempting to convert stream_conc_data$datetime dateTime string to R date structure (date or POSIXct)                                
  tmpDateFormat<-"%Y-%m-%d %H:%M" 
  tmp2datetime<-as.POSIXct(stream_conc_data$datetime,format=tmpDateFormat)
  # Keep the new dates only if they all converted correctly
  if(length(tmp2datetime) == length(tmp2datetime[!is.na(tmp2datetime)])){stream_conc_data$datetime <- tmp2datetime } else {print("Date conversion failed for stream_conc_data$datetime. Please inspect the data and do the date conversion yourself.")}                                                                    
  rm(tmpDateFormat,tmp2datetime) 
  if (class(stream_conc_data$Ca)=="factor") stream_conc_data$Ca <-as.numeric(levels(stream_conc_data$Ca))[as.integer(stream_conc_data$Ca) ]               
  if (class(stream_conc_data$Ca)=="character") stream_conc_data$Ca <-as.numeric(stream_conc_data$Ca)
  if (class(stream_conc_data$Mg)=="factor") stream_conc_data$Mg <-as.numeric(levels(stream_conc_data$Mg))[as.integer(stream_conc_data$Mg) ]               
  if (class(stream_conc_data$Mg)=="character") stream_conc_data$Mg <-as.numeric(stream_conc_data$Mg)
  if (class(stream_conc_data$K)=="factor") stream_conc_data$K <-as.numeric(levels(stream_conc_data$K))[as.integer(stream_conc_data$K) ]               
  if (class(stream_conc_data$K)=="character") stream_conc_data$K <-as.numeric(stream_conc_data$K)
  if (class(stream_conc_data$Na)=="factor") stream_conc_data$Na <-as.numeric(levels(stream_conc_data$Na))[as.integer(stream_conc_data$Na) ]               
  if (class(stream_conc_data$Na)=="character") stream_conc_data$Na <-as.numeric(stream_conc_data$Na)
  if (class(stream_conc_data$TMAl)=="factor") stream_conc_data$TMAl <-as.numeric(levels(stream_conc_data$TMAl))[as.integer(stream_conc_data$TMAl) ]               
  if (class(stream_conc_data$TMAl)=="character") stream_conc_data$TMAl <-as.numeric(stream_conc_data$TMAl)
  if (class(stream_conc_data$OMAl)=="factor") stream_conc_data$OMAl <-as.numeric(levels(stream_conc_data$OMAl))[as.integer(stream_conc_data$OMAl) ]               
  if (class(stream_conc_data$OMAl)=="character") stream_conc_data$OMAl <-as.numeric(stream_conc_data$OMAl)
  if (class(stream_conc_data$Al_ICP)=="factor") stream_conc_data$Al_ICP <-as.numeric(levels(stream_conc_data$Al_ICP))[as.integer(stream_conc_data$Al_ICP) ]               
  if (class(stream_conc_data$Al_ICP)=="character") stream_conc_data$Al_ICP <-as.numeric(stream_conc_data$Al_ICP)
  if (class(stream_conc_data$NH4)=="factor") stream_conc_data$NH4 <-as.numeric(levels(stream_conc_data$NH4))[as.integer(stream_conc_data$NH4) ]               
  if (class(stream_conc_data$NH4)=="character") stream_conc_data$NH4 <-as.numeric(stream_conc_data$NH4)
  if (class(stream_conc_data$SO4)=="factor") stream_conc_data$SO4 <-as.numeric(levels(stream_conc_data$SO4))[as.integer(stream_conc_data$SO4) ]               
  if (class(stream_conc_data$SO4)=="character") stream_conc_data$SO4 <-as.numeric(stream_conc_data$SO4)
  if (class(stream_conc_data$NO3)=="factor") stream_conc_data$NO3 <-as.numeric(levels(stream_conc_data$NO3))[as.integer(stream_conc_data$NO3) ]               
  if (class(stream_conc_data$NO3)=="character") stream_conc_data$NO3 <-as.numeric(stream_conc_data$NO3)
  if (class(stream_conc_data$Cl)=="factor") stream_conc_data$Cl <-as.numeric(levels(stream_conc_data$Cl))[as.integer(stream_conc_data$Cl) ]               
  if (class(stream_conc_data$Cl)=="character") stream_conc_data$Cl <-as.numeric(stream_conc_data$Cl)
  if (class(stream_conc_data$PO4)=="factor") stream_conc_data$PO4 <-as.numeric(levels(stream_conc_data$PO4))[as.integer(stream_conc_data$PO4) ]               
  if (class(stream_conc_data$PO4)=="character") stream_conc_data$PO4 <-as.numeric(stream_conc_data$PO4)
  if (class(stream_conc_data$DOC)=="factor") stream_conc_data$DOC <-as.numeric(levels(stream_conc_data$DOC))[as.integer(stream_conc_data$DOC) ]               
  if (class(stream_conc_data$DOC)=="character") stream_conc_data$DOC <-as.numeric(stream_conc_data$DOC)
  if (class(stream_conc_data$TDN)=="factor") stream_conc_data$TDN <-as.numeric(levels(stream_conc_data$TDN))[as.integer(stream_conc_data$TDN) ]               
  if (class(stream_conc_data$TDN)=="character") stream_conc_data$TDN <-as.numeric(stream_conc_data$TDN)
  if (class(stream_conc_data$DON)=="factor") stream_conc_data$DON <-as.numeric(levels(stream_conc_data$DON))[as.integer(stream_conc_data$DON) ]               
  if (class(stream_conc_data$DON)=="character") stream_conc_data$DON <-as.numeric(stream_conc_data$DON)
  if (class(stream_conc_data$SiO2)=="factor") stream_conc_data$SiO2 <-as.numeric(levels(stream_conc_data$SiO2))[as.integer(stream_conc_data$SiO2) ]               
  if (class(stream_conc_data$SiO2)=="character") stream_conc_data$SiO2 <-as.numeric(stream_conc_data$SiO2)
  if (class(stream_conc_data$Mn)=="factor") stream_conc_data$Mn <-as.numeric(levels(stream_conc_data$Mn))[as.integer(stream_conc_data$Mn) ]               
  if (class(stream_conc_data$Mn)=="character") stream_conc_data$Mn <-as.numeric(stream_conc_data$Mn)
  if (class(stream_conc_data$Fe)=="factor") stream_conc_data$Fe <-as.numeric(levels(stream_conc_data$Fe))[as.integer(stream_conc_data$Fe) ]               
  if (class(stream_conc_data$Fe)=="character") stream_conc_data$Fe <-as.numeric(stream_conc_data$Fe)
  if (class(stream_conc_data$F)=="factor") stream_conc_data$F <-as.numeric(levels(stream_conc_data$F))[as.integer(stream_conc_data$F) ]               
  if (class(stream_conc_data$F)=="character") stream_conc_data$F <-as.numeric(stream_conc_data$F)
  if (class(stream_conc_data$cationCharge)=="factor") stream_conc_data$cationCharge <-as.numeric(levels(stream_conc_data$cationCharge))[as.integer(stream_conc_data$cationCharge) ]               
  if (class(stream_conc_data$cationCharge)=="character") stream_conc_data$cationCharge <-as.numeric(stream_conc_data$cationCharge)
  if (class(stream_conc_data$anionCharge)=="factor") stream_conc_data$anionCharge <-as.numeric(levels(stream_conc_data$anionCharge))[as.integer(stream_conc_data$anionCharge) ]               
  if (class(stream_conc_data$anionCharge)=="character") stream_conc_data$anionCharge <-as.numeric(stream_conc_data$anionCharge)
  if (class(stream_conc_data$theoryCond)=="factor") stream_conc_data$theoryCond <-as.numeric(levels(stream_conc_data$theoryCond))[as.integer(stream_conc_data$theoryCond) ]               
  if (class(stream_conc_data$theoryCond)=="character") stream_conc_data$theoryCond <-as.numeric(stream_conc_data$theoryCond)
  if (class(stream_conc_data$ionError)=="factor") stream_conc_data$ionError <-as.numeric(levels(stream_conc_data$ionError))[as.integer(stream_conc_data$ionError) ]               
  if (class(stream_conc_data$ionError)=="character") stream_conc_data$ionError <-as.numeric(stream_conc_data$ionError)
  if (class(stream_conc_data$duplicate)!="factor") stream_conc_data$duplicate<- as.factor(stream_conc_data$duplicate)
  if (class(stream_conc_data$sampleType)!="factor") stream_conc_data$sampleType<- as.factor(stream_conc_data$sampleType)
  if (class(stream_conc_data$ionBalance)=="factor") stream_conc_data$ionBalance <-as.numeric(levels(stream_conc_data$ionBalance))[as.integer(stream_conc_data$ionBalance) ]               
  if (class(stream_conc_data$ionBalance)=="character") stream_conc_data$ionBalance <-as.numeric(stream_conc_data$ionBalance)
  if (class(stream_conc_data$canonical)!="factor") stream_conc_data$canonical<- as.factor(stream_conc_data$canonical)
  if (class(stream_conc_data$pHmetrohm)!="factor") stream_conc_data$pHmetrohm<- as.factor(stream_conc_data$pHmetrohm)
  if (class(stream_conc_data$archived)!="factor") stream_conc_data$archived<- as.factor(stream_conc_data$archived)
  
  # Convert Missing Values to NA for non-dates
  
  stream_conc_data$site <- as.factor(ifelse((trimws(as.character(stream_conc_data$site))==trimws("NA")),NA,as.character(stream_conc_data$site)))
  stream_conc_data$pH <- ifelse((trimws(as.character(stream_conc_data$pH))==trimws("NA")),NA,stream_conc_data$pH)               
  suppressWarnings(stream_conc_data$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$pH))==as.character(as.numeric("NA"))),NA,stream_conc_data$pH))
  stream_conc_data$DIC <- ifelse((trimws(as.character(stream_conc_data$DIC))==trimws("NA")),NA,stream_conc_data$DIC)               
  suppressWarnings(stream_conc_data$DIC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$DIC))==as.character(as.numeric("NA"))),NA,stream_conc_data$DIC))
  stream_conc_data$spCond <- ifelse((trimws(as.character(stream_conc_data$spCond))==trimws("NA")),NA,stream_conc_data$spCond)               
  suppressWarnings(stream_conc_data$spCond <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$spCond))==as.character(as.numeric("NA"))),NA,stream_conc_data$spCond))
  stream_conc_data$temp <- ifelse((trimws(as.character(stream_conc_data$temp))==trimws("NA")),NA,stream_conc_data$temp)               
  suppressWarnings(stream_conc_data$temp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$temp))==as.character(as.numeric("NA"))),NA,stream_conc_data$temp))
  stream_conc_data$ANC960 <- ifelse((trimws(as.character(stream_conc_data$ANC960))==trimws("NA")),NA,stream_conc_data$ANC960)               
  suppressWarnings(stream_conc_data$ANC960 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$ANC960))==as.character(as.numeric("NA"))),NA,stream_conc_data$ANC960))
  stream_conc_data$ANCMet <- ifelse((trimws(as.character(stream_conc_data$ANCMet))==trimws("NA")),NA,stream_conc_data$ANCMet)               
  suppressWarnings(stream_conc_data$ANCMet <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$ANCMet))==as.character(as.numeric("NA"))),NA,stream_conc_data$ANCMet))
  stream_conc_data$gageHt <- ifelse((trimws(as.character(stream_conc_data$gageHt))==trimws("NA")),NA,stream_conc_data$gageHt)               
  suppressWarnings(stream_conc_data$gageHt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$gageHt))==as.character(as.numeric("NA"))),NA,stream_conc_data$gageHt))
  stream_conc_data$hydroGraph <- as.factor(ifelse((trimws(as.character(stream_conc_data$hydroGraph))==trimws("NA")),NA,as.character(stream_conc_data$hydroGraph)))
  stream_conc_data$flowGageHt <- ifelse((trimws(as.character(stream_conc_data$flowGageHt))==trimws("NA")),NA,stream_conc_data$flowGageHt)               
  suppressWarnings(stream_conc_data$flowGageHt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$flowGageHt))==as.character(as.numeric("NA"))),NA,stream_conc_data$flowGageHt))
  stream_conc_data$fieldCode <- as.factor(ifelse((trimws(as.character(stream_conc_data$fieldCode))==trimws("NA")),NA,as.character(stream_conc_data$fieldCode)))
  stream_conc_data$notes <- as.factor(ifelse((trimws(as.character(stream_conc_data$notes))==trimws("NA")),NA,as.character(stream_conc_data$notes)))
  stream_conc_data$uniqueID <- as.factor(ifelse((trimws(as.character(stream_conc_data$uniqueID))==trimws("NA")),NA,as.character(stream_conc_data$uniqueID)))
  stream_conc_data$Ca <- ifelse((trimws(as.character(stream_conc_data$Ca))==trimws("NA")),NA,stream_conc_data$Ca)               
  suppressWarnings(stream_conc_data$Ca <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$Ca))==as.character(as.numeric("NA"))),NA,stream_conc_data$Ca))
  stream_conc_data$Mg <- ifelse((trimws(as.character(stream_conc_data$Mg))==trimws("NA")),NA,stream_conc_data$Mg)               
  suppressWarnings(stream_conc_data$Mg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$Mg))==as.character(as.numeric("NA"))),NA,stream_conc_data$Mg))
  stream_conc_data$K <- ifelse((trimws(as.character(stream_conc_data$K))==trimws("NA")),NA,stream_conc_data$K)               
  suppressWarnings(stream_conc_data$K <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$K))==as.character(as.numeric("NA"))),NA,stream_conc_data$K))
  stream_conc_data$Na <- ifelse((trimws(as.character(stream_conc_data$Na))==trimws("NA")),NA,stream_conc_data$Na)               
  suppressWarnings(stream_conc_data$Na <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$Na))==as.character(as.numeric("NA"))),NA,stream_conc_data$Na))
  stream_conc_data$TMAl <- ifelse((trimws(as.character(stream_conc_data$TMAl))==trimws("NA")),NA,stream_conc_data$TMAl)               
  suppressWarnings(stream_conc_data$TMAl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$TMAl))==as.character(as.numeric("NA"))),NA,stream_conc_data$TMAl))
  stream_conc_data$OMAl <- ifelse((trimws(as.character(stream_conc_data$OMAl))==trimws("NA")),NA,stream_conc_data$OMAl)               
  suppressWarnings(stream_conc_data$OMAl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$OMAl))==as.character(as.numeric("NA"))),NA,stream_conc_data$OMAl))
  stream_conc_data$Al_ICP <- ifelse((trimws(as.character(stream_conc_data$Al_ICP))==trimws("NA")),NA,stream_conc_data$Al_ICP)               
  suppressWarnings(stream_conc_data$Al_ICP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$Al_ICP))==as.character(as.numeric("NA"))),NA,stream_conc_data$Al_ICP))
  stream_conc_data$NH4 <- ifelse((trimws(as.character(stream_conc_data$NH4))==trimws("NA")),NA,stream_conc_data$NH4)               
  suppressWarnings(stream_conc_data$NH4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$NH4))==as.character(as.numeric("NA"))),NA,stream_conc_data$NH4))
  stream_conc_data$SO4 <- ifelse((trimws(as.character(stream_conc_data$SO4))==trimws("NA")),NA,stream_conc_data$SO4)               
  suppressWarnings(stream_conc_data$SO4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$SO4))==as.character(as.numeric("NA"))),NA,stream_conc_data$SO4))
  stream_conc_data$NO3 <- ifelse((trimws(as.character(stream_conc_data$NO3))==trimws("NA")),NA,stream_conc_data$NO3)               
  suppressWarnings(stream_conc_data$NO3 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$NO3))==as.character(as.numeric("NA"))),NA,stream_conc_data$NO3))
  stream_conc_data$Cl <- ifelse((trimws(as.character(stream_conc_data$Cl))==trimws("NA")),NA,stream_conc_data$Cl)               
  suppressWarnings(stream_conc_data$Cl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$Cl))==as.character(as.numeric("NA"))),NA,stream_conc_data$Cl))
  stream_conc_data$PO4 <- ifelse((trimws(as.character(stream_conc_data$PO4))==trimws("NA")),NA,stream_conc_data$PO4)               
  suppressWarnings(stream_conc_data$PO4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$PO4))==as.character(as.numeric("NA"))),NA,stream_conc_data$PO4))
  stream_conc_data$DOC <- ifelse((trimws(as.character(stream_conc_data$DOC))==trimws("NA")),NA,stream_conc_data$DOC)               
  suppressWarnings(stream_conc_data$DOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$DOC))==as.character(as.numeric("NA"))),NA,stream_conc_data$DOC))
  stream_conc_data$TDN <- ifelse((trimws(as.character(stream_conc_data$TDN))==trimws("NA")),NA,stream_conc_data$TDN)               
  suppressWarnings(stream_conc_data$TDN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$TDN))==as.character(as.numeric("NA"))),NA,stream_conc_data$TDN))
  stream_conc_data$DON <- ifelse((trimws(as.character(stream_conc_data$DON))==trimws("NA")),NA,stream_conc_data$DON)               
  suppressWarnings(stream_conc_data$DON <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$DON))==as.character(as.numeric("NA"))),NA,stream_conc_data$DON))
  stream_conc_data$SiO2 <- ifelse((trimws(as.character(stream_conc_data$SiO2))==trimws("NA")),NA,stream_conc_data$SiO2)               
  suppressWarnings(stream_conc_data$SiO2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$SiO2))==as.character(as.numeric("NA"))),NA,stream_conc_data$SiO2))
  stream_conc_data$Mn <- ifelse((trimws(as.character(stream_conc_data$Mn))==trimws("NA")),NA,stream_conc_data$Mn)               
  suppressWarnings(stream_conc_data$Mn <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$Mn))==as.character(as.numeric("NA"))),NA,stream_conc_data$Mn))
  stream_conc_data$Fe <- ifelse((trimws(as.character(stream_conc_data$Fe))==trimws("NA")),NA,stream_conc_data$Fe)               
  suppressWarnings(stream_conc_data$Fe <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$Fe))==as.character(as.numeric("NA"))),NA,stream_conc_data$Fe))
  stream_conc_data$F <- ifelse((trimws(as.character(stream_conc_data$F))==trimws("NA")),NA,stream_conc_data$F)               
  suppressWarnings(stream_conc_data$F <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$F))==as.character(as.numeric("NA"))),NA,stream_conc_data$F))
  stream_conc_data$cationCharge <- ifelse((trimws(as.character(stream_conc_data$cationCharge))==trimws("NA")),NA,stream_conc_data$cationCharge)               
  suppressWarnings(stream_conc_data$cationCharge <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$cationCharge))==as.character(as.numeric("NA"))),NA,stream_conc_data$cationCharge))
  stream_conc_data$anionCharge <- ifelse((trimws(as.character(stream_conc_data$anionCharge))==trimws("NA")),NA,stream_conc_data$anionCharge)               
  suppressWarnings(stream_conc_data$anionCharge <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$anionCharge))==as.character(as.numeric("NA"))),NA,stream_conc_data$anionCharge))
  stream_conc_data$theoryCond <- ifelse((trimws(as.character(stream_conc_data$theoryCond))==trimws("NA")),NA,stream_conc_data$theoryCond)               
  suppressWarnings(stream_conc_data$theoryCond <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$theoryCond))==as.character(as.numeric("NA"))),NA,stream_conc_data$theoryCond))
  stream_conc_data$ionError <- ifelse((trimws(as.character(stream_conc_data$ionError))==trimws("NA")),NA,stream_conc_data$ionError)               
  suppressWarnings(stream_conc_data$ionError <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$ionError))==as.character(as.numeric("NA"))),NA,stream_conc_data$ionError))
  stream_conc_data$duplicate <- as.factor(ifelse((trimws(as.character(stream_conc_data$duplicate))==trimws("NA")),NA,as.character(stream_conc_data$duplicate)))
  stream_conc_data$sampleType <- as.factor(ifelse((trimws(as.character(stream_conc_data$sampleType))==trimws("NA")),NA,as.character(stream_conc_data$sampleType)))
  stream_conc_data$ionBalance <- ifelse((trimws(as.character(stream_conc_data$ionBalance))==trimws("NA")),NA,stream_conc_data$ionBalance)               
  suppressWarnings(stream_conc_data$ionBalance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(stream_conc_data$ionBalance))==as.character(as.numeric("NA"))),NA,stream_conc_data$ionBalance))
  stream_conc_data$canonical <- as.factor(ifelse((trimws(as.character(stream_conc_data$canonical))==trimws("NA")),NA,as.character(stream_conc_data$canonical)))
  stream_conc_data$pHmetrohm <- as.factor(ifelse((trimws(as.character(stream_conc_data$pHmetrohm))==trimws("NA")),NA,as.character(stream_conc_data$pHmetrohm)))
  stream_conc_data$archived <- as.factor(ifelse((trimws(as.character(stream_conc_data$archived))==trimws("NA")),NA,as.character(stream_conc_data$archived)))
  
  # Here is the structure of the input data frame:
  
  str(stream_conc_data)                            
  attach(stream_conc_data)                            
  
  # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
  
  summary(site)
  summary(date)
  summary(timeEST)
  summary(pH)
  summary(DIC)
  summary(spCond)
  summary(temp)
  summary(ANC960)
  summary(ANCMet)
  summary(gageHt)
  summary(hydroGraph)
  summary(flowGageHt)
  summary(fieldCode)
  summary(notes)
  summary(uniqueID)
  summary(waterYr)
  summary(datetime)
  summary(Ca)
  summary(Mg)
  summary(K)
  summary(Na)
  summary(TMAl)
  summary(OMAl)
  summary(Al_ICP)
  summary(NH4)
  summary(SO4)
  summary(NO3)
  summary(Cl)
  summary(PO4)
  summary(DOC)
  summary(TDN)
  summary(DON)
  summary(SiO2)
  summary(Mn)
  summary(Fe)
  summary(F)
  summary(cationCharge)
  summary(anionCharge)
  summary(theoryCond)
  summary(ionError)
  summary(duplicate)
  summary(sampleType)
  summary(ionBalance)
  summary(canonical)
  summary(pHmetrohm)
  summary(archived) 
  
  # Get more details on character variables
  
  summary(as.factor(stream_conc_data$site)) 
  summary(as.factor(stream_conc_data$hydroGraph)) 
  summary(as.factor(stream_conc_data$fieldCode)) 
  summary(as.factor(stream_conc_data$notes)) 
  summary(as.factor(stream_conc_data$uniqueID)) 
  summary(as.factor(stream_conc_data$duplicate)) 
  summary(as.factor(stream_conc_data$sampleType)) 
  summary(as.factor(stream_conc_data$canonical)) 
  summary(as.factor(stream_conc_data$pHmetrohm)) 
  summary(as.factor(stream_conc_data$archived))
  detach(stream_conc_data)               
  
}