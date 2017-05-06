###FINAL CODE FOR ANALYSIS 
rm(list=ls())
options("scipen"=999)
library(dplyr)

#sum(is.na(nispuf03$state))
###########################################################################################
###########################################################################################
nispuf03<-read_csv("~/Desktop/R/BF ALL/Nis2003.csv")
nispuf03 <- select(nispuf03, YEAR, STATE, MOBIL, 
                   CBF_01, BF_ENDR, BF_EXCLR, 
                   EDUC1, MARITAL, M_AGEGRP,RACEETHK, INCPOV1R)

names(nispuf03) <- c("year", "state", "moved", "bfEver", "bfDurration", "bfExclusive", 
                     "matEd", "married", "matAge", "raceEthnicity", "pov")

nispuf03$year <- as.factor(nispuf03$year)

nispuf03$state <- as.factor(nispuf03$state)
nispuf03$state <- recode(nispuf03$state,  "1  ALABAMA"="ALABAMA",
                                          "10  DELAWARE" ="DELAWARE",          
                                          "11  DIST. OF COLUMBIA" = "DISTRICT OF COLUMBIA",
                                          "12  FLORIDA"="FLORIDA",           
                                          "13  GEORGIA"="GEORGIA",        
                                          "15  HAWAII"="HAWAII",
                                          "16  IDAHO"="IDAHO",
                                          "17  ILLINOIS"="ILLINOIS",
                                          "18  INDIANA"="INDIANA",
                                          "19  IOWA"="IOWA",
                                          "2  ALASKA"="ALASKA",
                                          "20  KANSAS"="KANSAS",
                                          "21  KENTUCKY"="KENTUCKY",
                                          "22  LOUISIANA" = "LOUISIANA",
                                          "23  MAINE"="MAINE",
                                          "24  MARYLAND"="MARYLAND",
                                          "25  MASSACHUSETTS"="MASSACHUSETTS",
                                          "26  MICHIGAN"="MICHIGAN",
                                          "27  MINNESOTA"="MINNESOTA",
                                          "28  MISSISSIPPI"="MISSISSIPPI",
                                          "29  MISSOURI"="MISSOURI",  
                                          "30  MONTANA" ="MONTANA",
                                          "31  NEBRASKA"="NEBRASKA",          
                                          "32  NEVADA"="NEVADA",
                                          "33  NEW HAMPSHIRE"  ="NEW HAMPSHIRE",   
                                          "34  NEW JERSEY" ="NEW JERSEY",       
                                          "35  NEW MEXICO"="NEW MEXICO",
                                          "36  NEW YORK" ="NEW YORK",         
                                          "37  NORTH CAROLINA"="NORTH CAROLINA",    
                                          "38  NORTH DAKOTA" ="NORTH DAKOTA",   
                                          "39  OHIO"="OHIO",              
                                          "4  ARIZONA" ="ARIZONA",           
                                          "40  OKLAHOMA"="OKLAHOMA",
                                          "41  OREGON"="OREGON",          
                                          "42  PENNSYLVANIA" ="PENNSYLVANIA",     
                                          "44  RHODE ISLAND"="RHODE ISLAND",     
                                          "45  SOUTH CAROLINA"="SOUTH CAROLINA",    
                                          "46  SOUTH DAKOTA"="SOUTH DAKOTA",      
                                          "47  TENNESSEE" ="TENNESSEE",       
                                          "48  TEXAS"="TEXAS",             
                                          "49  UTAH"="UTAH",              
                                          "5  ARKANSAS"="ARKANSAS",          
                                          "50  VERMONT"="VERMONT",           
                                          "51  VIRGINIA"="VIRGINIA",         
                                          "53  WASHINGTON"="WASHINGTON",  
                                          "54  WEST VIRGINIA"="WEST VIRGINIA",     
                                          "55  WISCONSIN" ="WISCONSIN",        
                                          "56  WYOMING"="WYOMING",          
                                          "6  CALIFORNIA"="CALIFORNIA",         
                                          "8  COLORADO"="COLORADO",           
                                          "9  CONNECTICUT"="CONNECTICUT")
nispuf03$moved <- as.factor(nispuf03$moved)
nispuf03$moved <- recode(nispuf03$moved,  
                         "1  MOVED FROM DIFFERENT STATE"="Moved",
                         "2  DID NOT MOVE FROM DIFFERENT STATE" = "No Move",          
                         "3  UNKNOWN" = NULL)

nispuf03$bfEver <- as.factor(nispuf03$bfEver)
nispuf03$bfEver <- with(nispuf03, ifelse(bfEver %in% "1  YES" , 1,
                                 ifelse(bfEver %in% "2  NO", 0, NA)))

nispuf03$matEd <- as.factor(nispuf03$matEd)
nispuf03$matEd <- recode(nispuf03$matEd,  
                         "1  <12 YEARS"="<12 years",
                         "2  12 YEARS" = "12 years",          
                         "3  >12, NON COLLEGE GRADUATE" = ">12, non-college graduate",
                         "4  COLLEGE GRADUATE" = "College graduate")

nispuf03$married <- as.factor(nispuf03$married)
nispuf03$married <- recode(nispuf03$married,  
                         "1  WIDOWED/DIVORCED/SEPARATED/DECEASED"="Not married",
                         "2  NEVER MARRIED" = "Not married",          
                         "3  MARRIED" = "Currently married")

nispuf03$matAge <- as.factor(nispuf03$matAge)
nispuf03$matAge <- recode(nispuf03$matAge,  
                           "1  <=19"="<=19",
                           "2  20 - 29" = "20 - 29",          
                           "3  30+" = ">=30")

nispuf03$raceEthnicity <- as.factor(nispuf03$raceEthnicity)
nispuf03$raceEthnicity <- recode(nispuf03$raceEthnicity,  
                          "1  HISPANIC"="Hispanic",
                          "2  NON-HISPANIC WHITE ONLY" = "Non-Hispanic White",          
                          "3  NON-HISPANIC BLACK ONLY" = "Non-Hispanic Black",
                          "4  NON-HISPANIC OTHER + MULTIPLE" = "Other or multiple race/ethnicities")

nispuf03$pov <- as.factor(nispuf03$pov)
nispuf03$pov <- recode(nispuf03$pov,  
                            "1  >= 1.0 (ABOVE)"="Not below FPL",
                            "2  < 1.0 (BELOW)" = "Below FPL",          
                            "3  UNKNOWN" = NULL)

###########################################################################################
###########################################################################################
nispuf04<-read_csv("~/Desktop/R/BF ALL/nispuf04.csv")
nispuf04 <- select(nispuf04, YEAR, STATE, MOBIL, 
                   CBF_01, BF_ENDR, BF_EXCLR, 
                   EDUC1, MARITAL, M_AGEGRP,RACEETHK, INCPOV1R)
names(nispuf04) <- c("year", "state", "moved", "bfEver", "bfDurration", "bfExclusive", 
                     "matEd", "married", "matAge", "raceEthnicity", "pov")

nispuf04$year <- as.factor(nispuf04$year)

nispuf04$state <- as.factor(nispuf04$state)
nispuf04$state <- recode(nispuf04$state,  "1  ALABAMA"="ALABAMA",
                         "10  DELAWARE" ="DELAWARE",          
                         "11  DIST. OF COLUMBIA" = "DISTRICT OF COLUMBIA",
                         "12  FLORIDA"="FLORIDA",           
                         "13  GEORGIA"="GEORGIA",        
                         "15  HAWAII"="HAWAII",
                         "16  IDAHO"="IDAHO",
                         "17  ILLINOIS"="ILLINOIS",
                         "18  INDIANA"="INDIANA",
                         "19  IOWA"="IOWA",
                         "2  ALASKA"="ALASKA",
                         "20  KANSAS"="KANSAS",
                         "21  KENTUCKY"="KENTUCKY",
                         "22  LOUISIANA" = "LOUISIANA",
                         "23  MAINE"="MAINE",
                         "24  MARYLAND"="MARYLAND",
                         "25  MASSACHUSETTS"="MASSACHUSETTS",
                         "26  MICHIGAN"="MICHIGAN",
                         "27  MINNESOTA"="MINNESOTA",
                         "28  MISSISSIPPI"="MISSISSIPPI",
                         "29  MISSOURI"="MISSOURI",  
                         "30  MONTANA" ="MONTANA",
                         "31  NEBRASKA"="NEBRASKA",          
                         "32  NEVADA"="NEVADA",
                         "33  NEW HAMPSHIRE"  ="NEW HAMPSHIRE",   
                         "34  NEW JERSEY" ="NEW JERSEY",       
                         "35  NEW MEXICO"="NEW MEXICO",
                         "36  NEW YORK" ="NEW YORK",         
                         "37  NORTH CAROLINA"="NORTH CAROLINA",    
                         "38  NORTH DAKOTA" ="NORTH DAKOTA",   
                         "39  OHIO"="OHIO",              
                         "4  ARIZONA" ="ARIZONA",           
                         "40  OKLAHOMA"="OKLAHOMA",
                         "41  OREGON"="OREGON",          
                         "42  PENNSYLVANIA" ="PENNSYLVANIA",     
                         "44  RHODE ISLAND"="RHODE ISLAND",     
                         "45  SOUTH CAROLINA"="SOUTH CAROLINA",    
                         "46  SOUTH DAKOTA"="SOUTH DAKOTA",      
                         "47  TENNESSEE" ="TENNESSEE",       
                         "48  TEXAS"="TEXAS",             
                         "49  UTAH"="UTAH",              
                         "5  ARKANSAS"="ARKANSAS",          
                         "50  VERMONT"="VERMONT",           
                         "51  VIRGINIA"="VIRGINIA",         
                         "53  WASHINGTON"="WASHINGTON",  
                         "54  WEST VIRGINIA"="WEST VIRGINIA",     
                         "55  WISCONSIN" ="WISCONSIN",        
                         "56  WYOMING"="WYOMING",          
                         "6  CALIFORNIA"="CALIFORNIA",         
                         "8  COLORADO"="COLORADO",           
                         "9  CONNECTICUT"="CONNECTICUT")

nispuf04$moved <- as.factor(nispuf04$moved)
nispuf04$moved <- recode(nispuf04$moved,  
                         "1  MOVED FROM DIFFERENT STATE"="Moved",
                         "2  DID NOT MOVE FROM DIFFERENT STATE" = "No Move",          
                         "3  UNKNOWN" = NULL)

nispuf04$bfEver <- as.factor(nispuf04$bfEver)
nispuf04$bfEver <- with(nispuf04, ifelse(bfEver %in% "1  YES" , 1,
                                         ifelse(bfEver %in% "2  NO", 0, NA)))

nispuf04$matEd <- as.factor(nispuf04$matEd)
nispuf04$matEd <- recode(nispuf04$matEd,  
                         "1  <12 YEARS"="<12 years",
                         "2  12 YEARS" = "12 years",          
                         "3  >12, NON COLLEGE GRADUATE" = ">12, non-college graduate",
                         "4  COLLEGE GRADUATE" = "College graduate")

nispuf04$married <- as.factor(nispuf04$married)
nispuf04$married <- recode(nispuf04$married,  
                           "1  WIDOWED/DIVORCED/SEPARATED/DECEASED"="Not married",
                           "2  NEVER MARRIED" = "Not married",          
                           "3  MARRIED" = "Currently married")

nispuf04$matAge <- as.factor(nispuf04$matAge)
nispuf04$matAge <- recode(nispuf04$matAge,  
                          "1  <=19"="<=19",
                          "2  20 - 29" = "20 - 29",          
                          "3  30+" = ">=30")

nispuf04$raceEthnicity <- as.factor(nispuf04$raceEthnicity)
nispuf04$raceEthnicity <- recode(nispuf04$raceEthnicity,  
                                 "1  HISPANIC"="Hispanic",
                                 "2  NON-HISPANIC WHITE ONLY" = "Non-Hispanic White",          
                                 "3  NON-HISPANIC BLACK ONLY" = "Non-Hispanic Black",
                                 "4  NON-HISPANIC OTHER + MULTIPLE" = "Other or multiple race/ethnicities")

nispuf04$pov <- as.factor(nispuf04$pov)
nispuf04$pov <- recode(nispuf04$pov,  
                       "1  >= 1.0 (ABOVE)"="Not below FPL",
                       "2  < 1.0 (BELOW)" = "Below FPL",          
                       "3  UNKNOWN" = NULL)

###########################################################################################
###########################################################################################
nispuf05<-read_csv("~/Desktop/R/BF ALL/nispuf05.csv")
nispuf05 <- select(nispuf05, YEAR, STATE, MOBIL_I, 
                   CBF_01, BF_ENDR, BF_EXCLR, 
                   EDUC1, MARITAL, M_AGEGRP,RACEETHK, INCPOV1)
names(nispuf05) <- c("year", "state", "moved", "bfEver", "bfDurration", "bfExclusive", 
                     "matEd", "married", "matAge", "raceEthnicity", "pov")

nispuf05$year <- as.factor(nispuf05$year)

nispuf05$state <- as.factor(nispuf05$state)

nispuf05$moved <- as.factor(nispuf05$moved)
nispuf05$moved <- recode(nispuf05$moved,  
                         "MOVED FROM DIFFERENT STATE"="Moved",
                         "DID NOT MOVE FROM DIFFERENT STATE" = "No Move",          
                         "UNKNOWN" = NULL)

nispuf05$bfEver <- as.factor(nispuf05$bfEver)
nispuf05$bfEver <- recode(nispuf05$bfEver, 
                         "DON'T KNOW" = NULL,  
                         "MISSING" = NULL,
                         "REFUSED" = NULL)

nispuf05$bfEver <- with(nispuf05, ifelse(bfEver %in% "YES" , 1,
                                         ifelse(bfEver %in% "NO", 0, NA)))

nispuf05$bfDurration <- as.numeric(nispuf05$bfDurration) 

nispuf05$bfExclusive <- as.numeric(nispuf05$bfExclusive) 

nispuf05$matEd <- as.factor(nispuf05$matEd)
nispuf05$matEd <- recode(nispuf05$matEd,  
                         "< 12 YEARS" = "<12 years",  
                         "12 YEARS" = "12 years",
                         "> 12 YEARS, NON-COLLEGE GRAD" = ">12, non-college graduate",
                         "COLLEGE GRAD" = "College graduate")

nispuf05$married <- as.factor(nispuf05$married)
nispuf05$married <- recode(nispuf05$married,  
                           "WIDOWED/DIVORCED/SEPARATED/DECEASED"="Not married",
                           "NEVER MARRIED" = "Not married",          
                           "MARRIED" = "Currently married")

nispuf05$matAge <- as.factor(nispuf05$matAge)
nispuf05$matAge <- recode(nispuf05$matAge,  
                           "WIDOWED/DIVORCED/SEPARATED/DECEASED"="Not married",
                           "NEVER MARRIED" = "Not married",          
                           "MARRIED" = "Currently married")

nispuf05$matAge <- as.factor(nispuf05$matAge)
nispuf05$matAge <- recode(nispuf05$matAge,  
                          "<= 19 YEARS"="<=19",
                          "20 - 29 YEARS" = "20 - 29",          
                          ">= 30 YEARS" = ">=30")

nispuf05$raceEthnicity <- as.factor(nispuf05$raceEthnicity)
nispuf05$raceEthnicity <- recode(nispuf05$raceEthnicity,  
                                 "HISPANIC"="Hispanic",
                                 "NON-HISPANIC WHITE ONLY" = "Non-Hispanic White",          
                                 "NON-HISPANIC BLACK ONLY" = "Non-Hispanic Black",
                                 "NON-HISPANIC OTHER + MULTIPLE RACE" = "Other or multiple race/ethnicities")

nispuf05$pov <- as.factor(nispuf05$pov)
nispuf05$pov <- recode(nispuf05$pov,  
                       "ABOVE POVERTY, <= $75K"="Not below FPL",
                       "ABOVE POVERTY, > $75K" = "Not below FPL",          
                       "BELOW POVERTY" = "Below FPL",
                       "UNKNOWN"=NULL)
str(nispuf05)
###########################################################################################
###########################################################################################
nispuf06<-read_csv("~/Desktop/R/BF ALL/nispuf06.csv")
nispuf06 <- select(nispuf06, YEAR, STATE, MOBIL_I, 
                   CBF_01, BF_ENDR06, BF_EXCLR06, 
                   EDUC1, MARITAL, M_AGEGRP,RACEETHK, INCPOV1)
names(nispuf06) <- c("year", "state", "moved", "bfEver", "bfDurration", "bfExclusive", 
                     "matEd", "married", "matAge", "raceEthnicity", "pov")

nispuf06$year <- as.factor(nispuf06$year)

nispuf06$state <- as.factor(nispuf06$state)

nispuf06$moved <- as.factor(nispuf06$moved)
nispuf06$moved <- recode(nispuf06$moved,  
                         "MOVED FROM DIFFERENT STATE"="Moved",
                         "DID NOT MOVE FROM DIFFERENT STATE" = "No Move",          
                         "UNKNOWN" = NULL)

nispuf06$bfEver <- as.factor(nispuf06$bfEver)
nispuf06$bfEver <- recode(nispuf06$bfEver, 
                          "DON'T KNOW" = NULL,  
                          "MISSING" = NULL,
                          "REFUSED" = NULL)

nispuf06$bfEver <- with(nispuf06, ifelse(bfEver %in% "YES" , 1,
                                         ifelse(bfEver %in% "NO", 0, NA)))

nispuf06$bfDurration <- as.numeric(nispuf06$bfDurration) 

nispuf06$bfExclusive <- as.numeric(nispuf06$bfExclusive) 

nispuf06$matEd <- as.factor(nispuf06$matEd)
nispuf06$matEd <- recode(nispuf06$matEd,  
                         "< 12 YEARS" = "<12 years",  
                         "12 YEARS" = "12 years",
                         "> 12 YEARS, NON-COLLEGE GRAD" = ">12, non-college graduate",
                         "COLLEGE GRAD" = "College graduate")

nispuf06$married <- as.factor(nispuf06$married)
nispuf06$married <- recode(nispuf06$married,  
                           "WIDOWED/DIVORCED/SEPARATED/DECEASED"="Not married",
                           "NEVER MARRIED" = "Not married",          
                           "MARRIED" = "Currently married")

nispuf06$matAge <- as.factor(nispuf06$matAge)
nispuf06$matAge <- recode(nispuf06$matAge,  
                          "<= 19 YEARS"="<=19",
                          "20 - 29 YEARS" = "20 - 29",          
                          ">= 30 YEARS" = ">=30")

nispuf06$raceEthnicity <- as.factor(nispuf06$raceEthnicity)
nispuf06$raceEthnicity <- recode(nispuf06$raceEthnicity,  
                                 "HISPANIC"="Hispanic",
                                 "NON-HISPANIC WHITE ONLY" = "Non-Hispanic White",          
                                 "NON-HISPANIC BLACK ONLY" = "Non-Hispanic Black",
                                 "NON-HISPANIC OTHER + MULTIPLE RACE" = "Other or multiple race/ethnicities")

nispuf06$pov <- as.factor(nispuf06$pov)
nispuf06$pov <- recode(nispuf06$pov,  
                       "ABOVE POVERTY, <= $75K"="Not below FPL",
                       "ABOVE POVERTY, > $75K" = "Not below FPL",          
                       "BELOW POVERTY" = "Below FPL",
                       "UNKNOWN"=NULL)
str(nispuf06)
###########################################################################################
###########################################################################################
nispuf07<-read_csv("~/Desktop/R/BF ALL/nispuf07.csv")
nispuf07 <- select(nispuf07, YEAR, STATE, MOBIL_I, 
                   CBF_01, BF_ENDR06, BF_EXCLR06, 
                   EDUC1, MARITAL, M_AGEGRP,RACEETHK, INCPOV1)

names(nispuf07) <- c("year", "state", "moved", "bfEver", "bfDurration", "bfExclusive", 
                     "matEd", "married", "matAge", "raceEthnicity", "pov")

nispuf07$year <- as.factor(nispuf07$year)

nispuf07$state <- as.factor(nispuf07$state)

nispuf07$moved <- as.factor(nispuf07$moved)
nispuf07$moved <- recode(nispuf07$moved,  
                         "MOVED FROM DIFFERENT STATE"="Moved",
                         "DID NOT MOVE FROM DIFFERENT STATE" = "No Move",          
                         "UNKNOWN" = NULL)

nispuf07$bfEver <- as.factor(nispuf07$bfEver)
nispuf07$bfEver <- recode(nispuf07$bfEver, 
                          "DON'T KNOW" = NULL,  
                          "MISSING" = NULL,
                          "REFUSED" = NULL)

nispuf07$bfEver <- with(nispuf07, ifelse(bfEver %in% "YES" , 1,
                                         ifelse(bfEver %in% "NO", 0, NA)))

nispuf07$bfDurration <- as.numeric(nispuf07$bfDurration) 

nispuf07$bfExclusive <- as.numeric(nispuf07$bfExclusive) 

nispuf07$matEd <- as.factor(nispuf07$matEd)
nispuf07$matEd <- recode(nispuf07$matEd,  
                         "< 12 YEARS" = "<12 years",  
                         "12 YEARS" = "12 years",
                         "> 12 YEARS, NON-COLLEGE GRAD" = ">12, non-college graduate",
                         "COLLEGE GRAD" = "College graduate")

nispuf07$married <- as.factor(nispuf07$married)
nispuf07$married <- recode(nispuf07$married,  
                           "WIDOWED/DIVORCED/SEPARATED/DECEASED"="Not married",
                           "NEVER MARRIED" = "Not married",          
                           "MARRIED" = "Currently married")

nispuf07$matAge <- as.factor(nispuf07$matAge)
nispuf07$matAge <- recode(nispuf07$matAge,  
                          "<= 19 YEARS"="<=19",
                          "20 - 29 YEARS" = "20 - 29",          
                          ">= 30 YEARS" = ">=30")

nispuf07$raceEthnicity <- as.factor(nispuf07$raceEthnicity)
nispuf07$raceEthnicity <- recode(nispuf07$raceEthnicity,  
                                 "HISPANIC"="Hispanic",
                                 "NON-HISPANIC WHITE ONLY" = "Non-Hispanic White",          
                                 "NON-HISPANIC BLACK ONLY" = "Non-Hispanic Black",
                                 "NON-HISPANIC OTHER + MULTIPLE RACE" = "Other or multiple race/ethnicities")

nispuf07$pov <- as.factor(nispuf07$pov)
nispuf07$pov <- recode(nispuf07$pov,  
                       "ABOVE POVERTY, <= $75K"="Not below FPL",
                       "ABOVE POVERTY, > $75K" = "Not below FPL",          
                       "BELOW POVERTY" = "Below FPL",
                       "UNKNOWN"=NULL)
str(nispuf07)

###########################################################################################
###########################################################################################
nispuf08<-read_csv("~/Desktop/R/BF ALL/nispuf08.csv")
nispuf08 <- select(nispuf08, YEAR, STATE, MOBIL_I, 
                   CBF_01, BF_ENDR06, BF_EXCLR06, 
                   EDUC1, MARITAL, M_AGEGRP,RACEETHK, INCPOV1)

names(nispuf08) <- c("year", "state", "moved", "bfEver", "bfDurration", "bfExclusive", 
                     "matEd", "married", "matAge", "raceEthnicity", "pov")

nispuf08$year <- as.factor(nispuf08$year)

nispuf08$state <- as.factor(nispuf08$state)

nispuf08$moved <- as.factor(nispuf08$moved)
nispuf08$moved <- recode(nispuf08$moved,  
                         "MOVED FROM DIFFERENT STATE"="Moved",
                         "DID NOT MOVE FROM DIFFERENT STATE" = "No Move")

nispuf08$bfEver <- as.factor(nispuf08$bfEver)
nispuf08$bfEver <- recode(nispuf08$bfEver, 
                          "DON'T KNOW" = NULL,  
                          "MISSING" = NULL,
                          "REFUSED" = NULL)

nispuf08$bfEver <- with(nispuf08, ifelse(bfEver %in% "YES" , 1,
                                         ifelse(bfEver %in% "NO", 0, NA)))

nispuf08$bfDurration <- as.numeric(nispuf08$bfDurration) 

nispuf08$bfExclusive <- as.numeric(nispuf08$bfExclusive) 

nispuf08$matEd <- as.factor(nispuf08$matEd)
nispuf08$matEd <- recode(nispuf08$matEd,  
                         "< 12 YEARS" = "<12 years",  
                         "12 YEARS" = "12 years",
                         "> 12 YEARS, NON-COLLEGE GRAD" = ">12, non-college graduate",
                         "COLLEGE GRAD" = "College graduate")

nispuf08$married <- as.factor(nispuf08$married)
nispuf08$married <- recode(nispuf08$married,  
                           "WIDOWED/DIVORCED/SEPARATED/DECEASED"="Not married",
                           "NEVER MARRIED" = "Not married",          
                           "MARRIED" = "Currently married")

nispuf08$matAge <- as.factor(nispuf08$matAge)
nispuf08$matAge <- recode(nispuf08$matAge,  
                          "<= 19 YEARS"="<=19",
                          "20 - 29 YEARS" = "20 - 29",          
                          ">= 30 YEARS" = ">=30")

nispuf08$raceEthnicity <- as.factor(nispuf08$raceEthnicity)
nispuf08$raceEthnicity <- recode(nispuf08$raceEthnicity,  
                                 "HISPANIC"="Hispanic",
                                 "NON-HISPANIC WHITE ONLY" = "Non-Hispanic White",          
                                 "NON-HISPANIC BLACK ONLY" = "Non-Hispanic Black",
                                 "NON-HISPANIC OTHER + MULTIPLE RACE" = "Other or multiple race/ethnicities")

nispuf08$pov <- as.factor(nispuf08$pov)
nispuf08$pov <- recode(nispuf08$pov,  
                       "ABOVE POVERTY, <= $75K"="Not below FPL",
                       "ABOVE POVERTY, > $75K" = "Not below FPL",          
                       "BELOW POVERTY" = "Below FPL",
                       "UNKNOWN"=NULL)
str(nispuf08)
###########################################################################################
###########################################################################################
nispuf09 <-read_csv("~/Desktop/R/BF ALL/nispuf09.csv")
nispuf09 <- select(nispuf09, YEAR, STATE, MOBIL_I, 
                   CBF_01, BF_ENDR06, BF_EXCLR06, 
                   EDUC1, MARITAL2, M_AGEGRP,RACEETHK, INCPOV1)

names(nispuf09) <- c("year", "state", "moved", "bfEver", "bfDurration", "bfExclusive", 
                     "matEd", "married", "matAge", "raceEthnicity", "pov")

nispuf09$year <- as.factor(nispuf09$year)

nispuf09$state <- as.factor(nispuf09$state)

nispuf09$moved <- as.factor(nispuf09$moved)
nispuf09$moved <- recode(nispuf09$moved,  
                         "MOVED FROM DIFFERENT STATE"="Moved",
                         "DID NOT MOVE FROM DIFFERENT STATE" = "No Move")

nispuf09$bfEver <- as.factor(nispuf09$bfEver)
nispuf09$bfEver <- recode(nispuf09$bfEver, 
                          "DON'T KNOW" = NULL,  
                          "MISSING" = NULL,
                          "REFUSED" = NULL)

nispuf09$bfEver <- with(nispuf09, ifelse(bfEver %in% "YES" , 1,
                                         ifelse(bfEver %in% "NO", 0, NA)))

nispuf09$bfDurration <- as.numeric(nispuf09$bfDurration) 

nispuf09$bfExclusive <- as.numeric(nispuf09$bfExclusive) 

nispuf09$matEd <- as.factor(nispuf09$matEd)
nispuf09$matEd <- recode(nispuf09$matEd,  
                         "< 12 YEARS" = "<12 years",  
                         "12 YEARS" = "12 years",
                         "> 12 YEARS, NON-COLLEGE GRAD" = ">12, non-college graduate",
                         "COLLEGE GRAD" = "College graduate")

nispuf09$married <- as.factor(nispuf09$married)
nispuf09$married <- recode(nispuf09$married,  
                           "NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED" = "Not married",          
                           "MARRIED" = "Currently married")

nispuf09$matAge <- as.factor(nispuf09$matAge)
nispuf09$matAge <- recode(nispuf09$matAge,  
                          "<= 19 YEARS"="<=19",
                          "20 - 29 YEARS" = "20 - 29",          
                          ">= 30 YEARS" = ">=30")

nispuf09$raceEthnicity <- as.factor(nispuf09$raceEthnicity)
nispuf09$raceEthnicity <- recode(nispuf09$raceEthnicity,  
                                 "HISPANIC"="Hispanic",
                                 "NON-HISPANIC WHITE ONLY" = "Non-Hispanic White",          
                                 "NON-HISPANIC BLACK ONLY" = "Non-Hispanic Black",
                                 "NON-HISPANIC OTHER + MULTIPLE RACE" = "Other or multiple race/ethnicities")

nispuf09$pov <- as.factor(nispuf09$pov)
nispuf09$pov <- recode(nispuf09$pov,  
                       "ABOVE POVERTY, <= $75K"="Not below FPL",
                       "ABOVE POVERTY, > $75K" = "Not below FPL",          
                       "BELOW POVERTY" = "Below FPL",
                       "UNKNOWN"=NULL)
str(nispuf09)
###########################################################################################
###########################################################################################
nispuf10 <-read_csv("~/Desktop/R/BF ALL/nispuf10.csv")
nispuf10 <- select(nispuf10, YEAR, STATE, MOBIL_I, 
                   CBF_01, BF_ENDR06, BF_EXCLR06, 
                   EDUC1, MARITAL2, M_AGEGRP,RACEETHK, INCPOV1)

names(nispuf10) <- c("year", "state", "moved", "bfEver", "bfDurration", "bfExclusive", 
                     "matEd", "married", "matAge", "raceEthnicity", "pov")

nispuf10$year <- as.factor(nispuf10$year)

nispuf10$state <- as.factor(nispuf10$state)

nispuf10$moved <- as.factor(nispuf10$moved)
nispuf10$moved <- recode(nispuf10$moved,  
                         "MOVED FROM DIFFERENT STATE"="Moved",
                         "DID NOT MOVE FROM DIFFERENT STATE" = "No Move")

nispuf10$bfEver <- as.factor(nispuf10$bfEver)
nispuf10$bfEver <- recode(nispuf10$bfEver, 
                          "DON'T KNOW" = NULL,  
                          "MISSING" = NULL,
                          "REFUSED" = NULL)

nispuf10$bfEver <- with(nispuf10, ifelse(bfEver %in% "YES" , 1,
                                         ifelse(bfEver %in% "NO", 0, NA)))

nispuf10$bfDurration <- as.numeric(nispuf10$bfDurration) 

nispuf10$bfExclusive <- as.numeric(nispuf10$bfExclusive) 

nispuf10$matEd <- as.factor(nispuf10$matEd)
nispuf10$matEd <- recode(nispuf10$matEd,  
                         "< 12 YEARS" = "<12 years",  
                         "12 YEARS" = "12 years",
                         "> 12 YEARS, NON-COLLEGE GRAD" = ">12, non-college graduate",
                         "COLLEGE GRAD" = "College graduate")

nispuf10$married <- as.factor(nispuf10$married)
nispuf10$married <- recode(nispuf10$married,  
                           "NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED" = "Not married",          
                           "MARRIED" = "Currently married")

nispuf10$matAge <- as.factor(nispuf10$matAge)
nispuf10$matAge <- recode(nispuf10$matAge,  
                          "<= 19 YEARS"="<=19",
                          "20 - 29 YEARS" = "20 - 29",          
                          ">= 30 YEARS" = ">=30")

nispuf10$raceEthnicity <- as.factor(nispuf10$raceEthnicity)
nispuf10$raceEthnicity <- recode(nispuf10$raceEthnicity,  
                                 "HISPANIC"="Hispanic",
                                 "NON-HISPANIC WHITE ONLY" = "Non-Hispanic White",          
                                 "NON-HISPANIC BLACK ONLY" = "Non-Hispanic Black",
                                 "NON-HISPANIC OTHER + MULTIPLE RACE" = "Other or multiple race/ethnicities")

nispuf10$pov <- as.factor(nispuf10$pov)
nispuf10$pov <- recode(nispuf10$pov,  
                       "ABOVE POVERTY, <= $75K"="Not below FPL",
                       "ABOVE POVERTY, > $75K" = "Not below FPL",          
                       "BELOW POVERTY" = "Below FPL",
                       "UNKNOWN"=NULL)
str(nispuf10)
###########################################################################################
###########################################################################################
nispuf11 <-read_csv("~/Desktop/R/BF ALL/nispuf11.csv")
nispuf11 <- select(nispuf11, YEAR, STATE, MOBIL_I, 
                   CBF_01, BF_ENDR06, BF_EXCLR06, 
                   EDUC1, MARITAL2, M_AGEGRP,RACEETHK, INCPOV1)
names(nispuf11) <- c("year", "state", "moved", "bfEver", "bfDurration", "bfExclusive", 
                     "matEd", "married", "matAge", "raceEthnicity", "pov")

nispuf11$year <- as.factor(nispuf11$year)

nispuf11$state <- as.factor(nispuf11$state)

nispuf11$moved <- as.factor(nispuf11$moved)
nispuf11$moved <- recode(nispuf11$moved,  
                         "MOVED FROM DIFFERENT STATE"="Moved",
                         "DID NOT MOVE FROM DIFFERENT STATE" = "No Move")

nispuf11$bfEver <- as.factor(nispuf11$bfEver)
nispuf11$bfEver <- recode(nispuf11$bfEver, 
                          "DON'T KNOW" = NULL,  
                          "MISSING" = NULL,
                          "REFUSED" = NULL)

nispuf11$bfEver <- with(nispuf11, ifelse(bfEver %in% "YES" , 1,
                                         ifelse(bfEver %in% "NO", 0, NA)))

nispuf11$bfDurration <- as.numeric(nispuf11$bfDurration) 

nispuf11$bfExclusive <- as.numeric(nispuf11$bfExclusive) 

nispuf11$matEd <- as.factor(nispuf11$matEd)
nispuf11$matEd <- recode(nispuf11$matEd,  
                         "< 12 YEARS" = "<12 years",  
                         "12 YEARS" = "12 years",
                         "> 12 YEARS, NON-COLLEGE GRAD" = ">12, non-college graduate",
                         "COLLEGE GRAD" = "College graduate")

nispuf11$married <- as.factor(nispuf11$married)
nispuf11$married <- recode(nispuf11$married,  
                           "NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED" = "Not married",          
                           "MARRIED" = "Currently married")

nispuf11$matAge <- as.factor(nispuf11$matAge)
nispuf11$matAge <- recode(nispuf11$matAge,  
                          "<= 19 YEARS"="<=19",
                          "20 - 29 YEARS" = "20 - 29",          
                          ">= 30 YEARS" = ">=30")

nispuf11$raceEthnicity <- as.factor(nispuf11$raceEthnicity)
nispuf11$raceEthnicity <- recode(nispuf11$raceEthnicity,  
                                 "HISPANIC"="Hispanic",
                                 "NON-HISPANIC WHITE ONLY" = "Non-Hispanic White",          
                                 "NON-HISPANIC BLACK ONLY" = "Non-Hispanic Black",
                                 "NON-HISPANIC OTHER + MULTIPLE RACE" = "Other or multiple race/ethnicities")

nispuf11$pov <- as.factor(nispuf11$pov)
nispuf11$pov <- recode(nispuf11$pov,  
                       "ABOVE POVERTY, <= $75K"="Not below FPL",
                       "ABOVE POVERTY, > $75K" = "Not below FPL",          
                       "BELOW POVERTY" = "Below FPL",
                       "UNKNOWN"=NULL)
str(nispuf11)
###########################################################################################
###########################################################################################
nispuf12 <-read_csv("~/Desktop/R/BF ALL/nispuf12.csv")
nispuf12 <- select(nispuf12, YEAR, STATE, MOBIL_I, 
                   CBF_01, BF_ENDR06, BF_EXCLR06, 
                   EDUC1, MARITAL2, M_AGEGRP,RACEETHK, INCPOV1)
names(nispuf12) <- c("year", "state", "moved", "bfEver", "bfDurration", "bfExclusive", 
                     "matEd", "married", "matAge", "raceEthnicity", "pov")

nispuf12$year <- as.factor(nispuf12$year)

nispuf12$state <- as.factor(nispuf12$state)

nispuf12$moved <- as.factor(nispuf12$moved)
nispuf12$moved <- recode(nispuf12$moved,  
                         "MOVED FROM DIFFERENT STATE"="Moved",
                         "DID NOT MOVE FROM DIFFERENT STATE" = "No Move")

nispuf12$bfEver <- as.factor(nispuf12$bfEver)
nispuf12$bfEver <- recode(nispuf12$bfEver, 
                          "DON'T KNOW" = NULL,  
                          "MISSING" = NULL,
                          "REFUSED" = NULL)

nispuf12$bfEver <- with(nispuf12, ifelse(bfEver %in% "YES" , 1,
                                         ifelse(bfEver %in% "NO", 0, NA)))

nispuf12$bfDurration <- as.numeric(nispuf12$bfDurration) 

nispuf12$bfExclusive <- as.numeric(nispuf12$bfExclusive) 

nispuf12$matEd <- as.factor(nispuf12$matEd)
nispuf12$matEd <- recode(nispuf12$matEd,  
                         "< 12 YEARS" = "<12 years",  
                         "12 YEARS" = "12 years",
                         "> 12 YEARS, NON-COLLEGE GRAD" = ">12, non-college graduate",
                         "COLLEGE GRAD" = "College graduate")

nispuf12$married <- as.factor(nispuf12$married)
nispuf12$married <- recode(nispuf12$married,  
                           "NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED" = "Not married",          
                           "MARRIED" = "Currently married")

nispuf12$matAge <- as.factor(nispuf12$matAge)
nispuf12$matAge <- recode(nispuf12$matAge,  
                          "<= 19 YEARS"="<=19",
                          "20 - 29 YEARS" = "20 - 29",          
                          ">= 30 YEARS" = ">=30")

nispuf12$raceEthnicity <- as.factor(nispuf12$raceEthnicity)
nispuf12$raceEthnicity <- recode(nispuf12$raceEthnicity,  
                                 "HISPANIC"="Hispanic",
                                 "NON-HISPANIC WHITE ONLY" = "Non-Hispanic White",          
                                 "NON-HISPANIC BLACK ONLY" = "Non-Hispanic Black",
                                 "NON-HISPANIC OTHER + MULTIPLE RACE" = "Other or multiple race/ethnicities")

nispuf12$pov <- as.factor(nispuf12$pov)
nispuf12$pov <- recode(nispuf12$pov,  
                       "ABOVE POVERTY, <= $75K"="Not below FPL",
                       "ABOVE POVERTY, > $75K" = "Not below FPL",          
                       "BELOW POVERTY" = "Below FPL",
                       "UNKNOWN"=NULL)
str(nispuf12)
###########################################################################################
###########################################################################################
nispuf13 <-read_csv("~/Desktop/R/BF ALL/nispuf13.csv")
nispuf13 <- select(nispuf13, YEAR, STATE, MOBIL_I, 
                   CBF_01, BF_ENDR06, BF_EXCLR06, 
                   EDUC1, MARITAL2, M_AGEGRP,RACEETHK, INCPOV1)
names(nispuf13) <- c("year", "state", "moved", "bfEver", "bfDurration", "bfExclusive", 
                     "matEd", "married", "matAge", "raceEthnicity", "pov")

nispuf13$year <- as.factor(nispuf13$year)

nispuf13$state <- as.factor(nispuf13$state)

nispuf13$moved <- as.factor(nispuf13$moved)
nispuf13$moved <- recode(nispuf13$moved,  
                         "MOVED FROM DIFFERENT STATE"="Moved",
                         "DID NOT MOVE FROM DIFFERENT STATE" = "No Move")

nispuf13$bfEver <- as.factor(nispuf13$bfEver)
nispuf13$bfEver <- recode(nispuf13$bfEver, 
                          "DON'T KNOW" = NULL,  
                          "MISSING" = NULL,
                          "REFUSED" = NULL)

nispuf13$bfEver <- with(nispuf13, ifelse(bfEver %in% "YES" , 1,
                                         ifelse(bfEver %in% "NO", 0, NA)))

nispuf13$bfDurration <- as.numeric(nispuf13$bfDurration) 

nispuf13$bfExclusive <- as.numeric(nispuf13$bfExclusive) 

nispuf13$matEd <- as.factor(nispuf13$matEd)
nispuf13$matEd <- recode(nispuf13$matEd,  
                         "< 12 YEARS" = "<12 years",  
                         "12 YEARS" = "12 years",
                         "> 12 YEARS, NON-COLLEGE GRAD" = ">12, non-college graduate",
                         "COLLEGE GRAD" = "College graduate")

nispuf13$married <- as.factor(nispuf13$married)
nispuf13$married <- recode(nispuf13$married,  
                           "NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED" = "Not married",          
                           "MARRIED" = "Currently married")

nispuf13$matAge <- as.factor(nispuf13$matAge)
nispuf13$matAge <- recode(nispuf13$matAge,  
                          "<= 19 YEARS"="<=19",
                          "20 - 29 YEARS" = "20 - 29",          
                          ">= 30 YEARS" = ">=30")

nispuf13$raceEthnicity <- as.factor(nispuf13$raceEthnicity)
nispuf13$raceEthnicity <- recode(nispuf13$raceEthnicity,  
                                 "HISPANIC"="Hispanic",
                                 "NON-HISPANIC WHITE ONLY" = "Non-Hispanic White",          
                                 "NON-HISPANIC BLACK ONLY" = "Non-Hispanic Black",
                                 "NON-HISPANIC OTHER + MULTIPLE RACE" = "Other or multiple race/ethnicities")

nispuf13$pov <- as.factor(nispuf13$pov)
nispuf13$pov <- recode(nispuf13$pov,  
                       "ABOVE POVERTY, <= $75K"="Not below FPL",
                       "ABOVE POVERTY, > $75K" = "Not below FPL",          
                       "BELOW POVERTY" = "Below FPL",
                       "UNKNOWN"=NULL)
str(nispuf13)
###########################################################################################
###########################################################################################
nispuf14 <-read_csv("~/Desktop/R/BF ALL/nispuf14.csv")
nispuf14 <- select(nispuf14, YEAR, STATE, MOBIL_I, 
                   CBF_01, BF_ENDR06, BF_EXCLR06, 
                   EDUC1, MARITAL2, M_AGEGRP,RACEETHK, INCPOV1)
names(nispuf14) <- c("year", "state", "moved", "bfEver", "bfDurration", "bfExclusive", 
                     "matEd", "married", "matAge", "raceEthnicity", "pov")
nispuf14$year <- as.factor(nispuf14$year)

nispuf14$state <- as.factor(nispuf14$state)

nispuf14$moved <- as.factor(nispuf14$moved)
nispuf14$moved <- recode(nispuf14$moved,  
                         "MOVED FROM DIFFERENT STATE"="Moved",
                         "DID NOT MOVE FROM DIFFERENT STATE" = "No Move")

nispuf14$bfEver <- as.factor(nispuf14$bfEver)
nispuf14$bfEver <- recode(nispuf14$bfEver, 
                          "DON'T KNOW" = NULL,  
                          "MISSING" = NULL,
                          "REFUSED" = NULL)

nispuf14$bfEver <- with(nispuf14, ifelse(bfEver %in% "YES" , 1,
                                         ifelse(bfEver %in% "NO", 0, NA)))

nispuf14$bfDurration <- as.numeric(nispuf14$bfDurration) 

nispuf14$bfExclusive <- as.numeric(nispuf14$bfExclusive) 

nispuf14$matEd <- as.factor(nispuf14$matEd)
nispuf14$matEd <- recode(nispuf14$matEd,  
                         "< 12 YEARS" = "<12 years",  
                         "12 YEARS" = "12 years",
                         "> 12 YEARS, NON-COLLEGE GRAD" = ">12, non-college graduate",
                         "COLLEGE GRAD" = "College graduate")

nispuf14$married <- as.factor(nispuf14$married)
nispuf14$married <- recode(nispuf14$married,  
                           "NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED" = "Not married",          
                           "MARRIED" = "Currently married")

nispuf14$matAge <- as.factor(nispuf14$matAge)
nispuf14$matAge <- recode(nispuf14$matAge,  
                          "<= 19 YEARS"="<=19",
                          "20 - 29 YEARS" = "20 - 29",          
                          ">= 30 YEARS" = ">=30")

nispuf14$raceEthnicity <- as.factor(nispuf14$raceEthnicity)
nispuf14$raceEthnicity <- recode(nispuf14$raceEthnicity,  
                                 "HISPANIC"="Hispanic",
                                 "NON-HISPANIC WHITE ONLY" = "Non-Hispanic White",          
                                 "NON-HISPANIC BLACK ONLY" = "Non-Hispanic Black",
                                 "NON-HISPANIC OTHER + MULTIPLE RACE" = "Other or multiple race/ethnicities")

nispuf14$pov <- as.factor(nispuf14$pov)
nispuf14$pov <- recode(nispuf14$pov,  
                       "ABOVE POVERTY, <= $75K"="Not below FPL",
                       "ABOVE POVERTY, > $75K" = "Not below FPL",          
                       "BELOW POVERTY" = "Below FPL",
                       "UNKNOWN"=NULL)
str(nispuf14)
###########################################################################################
###########################################################################################
nispuf15 <-read_csv("~/Desktop/R/BF ALL/nispuf15.csv")
nispuf15 <- select(nispuf15, YEAR, STATE, MOBIL_I, 
                   CBF_01, BF_ENDR06, BF_EXCLR06, 
                   EDUC1, MARITAL2, M_AGEGRP,RACEETHK, INCPOV1)

names(nispuf15) <- c("year", "state", "moved", "bfEver", "bfDurration", "bfExclusive", 
                     "matEd", "married", "matAge", "raceEthnicity", "pov")

nispuf15$year <- as.factor(nispuf15$year)

nispuf15$state <- as.factor(nispuf15$state)

nispuf15$moved <- as.factor(nispuf15$moved)
nispuf15$moved <- recode(nispuf15$moved,  
                         "MOVED FROM DIFFERENT STATE"="Moved",
                         "DID NOT MOVE FROM DIFFERENT STATE" = "No Move")

nispuf15$bfEver <- as.factor(nispuf15$bfEver)
nispuf15$bfEver <- recode(nispuf15$bfEver, 
                          "DON'T KNOW" = NULL,  
                          "MISSING" = NULL,
                          "REFUSED" = NULL)

nispuf15$bfEver <- with(nispuf15, ifelse(bfEver %in% "YES" , 1,
                                         ifelse(bfEver %in% "NO", 0, NA)))

nispuf15$bfDurration <- as.numeric(nispuf15$bfDurration) 

nispuf15$bfExclusive <- as.numeric(nispuf15$bfExclusive) 

nispuf15$matEd <- as.factor(nispuf15$matEd)
nispuf15$matEd <- recode(nispuf15$matEd,  
                         "< 12 YEARS" = "<12 years",  
                         "12 YEARS" = "12 years",
                         "> 12 YEARS, NON-COLLEGE GRAD" = ">12, non-college graduate", 
                         "COLLEGE GRAD" = "College graduate")

nispuf15$married <- as.factor(nispuf15$married)
nispuf15$married <- recode(nispuf15$married,  
                           "NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED/LIVING WITH PARTNER" = "Not married",          
                           "MARRIED" = "Currently married")

nispuf15$matAge <- as.factor(nispuf15$matAge)
nispuf15$matAge <- recode(nispuf15$matAge,  
                          "<= 19 YEARS"="<=19",
                          "20 - 29 YEARS" = "20 - 29",          
                          ">= 30 YEARS" = ">=30")

nispuf15$raceEthnicity <- as.factor(nispuf15$raceEthnicity)
nispuf15$raceEthnicity <- recode(nispuf15$raceEthnicity,  
                                 "HISPANIC"="Hispanic",
                                 "NON-HISPANIC WHITE ONLY" = "Non-Hispanic White",          
                                 "NON-HISPANIC BLACK ONLY" = "Non-Hispanic Black",
                                 "NON-HISPANIC OTHER + MULTIPLE RACE" = "Other or multiple race/ethnicities")

nispuf15$pov <- as.factor(nispuf15$pov)
nispuf15$pov <- recode(nispuf15$pov,  
                       "ABOVE POVERTY, <= $75K"="Not below FPL",
                       "ABOVE POVERTY, > $75K" = "Not below FPL",          
                       "BELOW POVERTY" = "Below FPL",
                       "UNKNOWN"=NULL)
str(nispuf15)

###################################################################################
###################################################################################
pflDF <- bind_rows(nispuf03, nispuf04)
pflDF <- bind_rows(pflDF, nispuf05)
pflDF <- bind_rows(pflDF, nispuf06)
pflDF <- bind_rows(pflDF, nispuf07)
pflDF <- bind_rows(pflDF, nispuf08)
pflDF <- bind_rows(pflDF, nispuf09)
pflDF <- bind_rows(pflDF, nispuf10)
pflDF <- bind_rows(pflDF, nispuf11)
pflDF <- bind_rows(pflDF, nispuf12)
pflDF <- bind_rows(pflDF, nispuf13)
pflDF <- bind_rows(pflDF, nispuf14)
pflDF <- bind_rows(pflDF, nispuf15)
write.csv(pflDF, "pflDF.csv")

###################################################################################
###################################################################################
pflDF <- read_csv("~/Desktop/R/BF ALL/pflDF.csv")
str(pflDF)
pflDF$year <- as.numeric(pflDF$year)
pflDF$state <- as.factor(pflDF$state)
pflDF$moved <- as.factor(pflDF$moved)
pflDF$bfEver <- as.numeric(pflDF$bfEver)
pflDF$matEd <- as.factor(pflDF$matEd)
pflDF$married <- as.factor(pflDF$married)
pflDF$matAge<- as.factor(pflDF$matAge)
pflDF$raceEthnicity<- as.factor(pflDF$raceEthnicity)
pflDF$pov<- as.factor(pflDF$pov)
str(pflDF)

library(tableone)
pflDF$year <- as.factor(pflDF$year)
vars <- c("state","moved", "matEd","married","matAge","raceEthnicity","pov")
tableOne <- CreateTableOne(vars = vars, strata = c("bfEver"), data = pflDF)
print(tableOne)
#by year
vars <- c("bfEver","bfDurration","bfExclusive")
tableOne <- CreateTableOne(vars = vars, strata = c("year"), data = pflDF)
print(tableOne)
###################################################################################
###################################################################################
pflDF$txState = ifelse(pflDF$state == "CALIFORNIA" | pflDF$state == "NEW JERSEY",  1, 0)

pflDF$treated = ifelse(pflDF$txState == 1 & pflDF$year >= 2006 |
                         pflDF$txState == 1 & pflDF$year >= 2011,  1, 0)

summary(lm(bfExclusive ~ treated + year + state, data=pflDF))

summary(lm(bfDurration ~ treated + year + state, data=pflDF))

summary(glm(bfEver ~ treated + year + state, family = binomial(), data=pflDF))
