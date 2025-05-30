

#-----------------------------------------------------------------------------------
# Now set up the screen 3 columns
#     screening the social code 
#-----------------------------------------------------------------------------------

Workingfile <- "./data/04_Data-ready-2nd-screen/MainScreeningData_Screen1_complete.RData"
load(Workingfile)
bib_all       <- data_bib; rm(data_bib)

#-----------------------------------------------------------------------------------
# FILTER TO THE ROWS YOU WANT
# 
# we just wanted the papers where it was the human impacts of flash flood
# If we ticked screen2_social as TRUE then we wanted to keep the paper
# 
# FOR YOU. you want screen1_rejectnotentreP to be false (spelt correctly)
data_bib      <- bib_all[which(bib_all$Screen1_RejectNOTEntreP == FALSE),]

#-----------------------------------------------------------------------------------
# set up new columns. One for each thing you want to store.
# these are your output columns for the shiny app
#  keep these three the same
#  If you want a TRUE/FALSE, set to NA. 
#  If you want it to store numbers or text,set to "
#-----------------------------------------------------------------------------------

data_bib$Screen2_Assessed <- FALSE
data_bib$Screen2_Reject   <- NA
data_bib$Screen2_Methods     <- ""

data_bib$Screen2_DFITheme    <- ""   
data_bib$Screen2_GAD         <- ""
data_bib$Screen2_TechType    <- ""
data_bib$Screen2_IncDev      <- ""
data_bib$Screen2_MMType      <- ""
data_bib$Screen2_EntrePType  <- ""
data_bib$Screen2_EntrePSec   <- ""
data_bib$Screen2_Geography   <- ""


#  keep this the same
data_bib$Screen2_Notes      <- ""


#----------------------------
# and save to the data file
#----------------------------
#Workingfile <- "./data/04_Data-ready-2nd-screen/MainScreeningData_Screen1_complete.RData"

save(data_bib, file = "./data/04_Data-ready-2nd-screen/MainScreeningData_Screen2_PreProcessed.RData")
save(data_bib, file = "./data/MainScreeningData_Screen2.rData")

#----------------------------
# Split into groups of 50
#----------------------------
grouped <- split(1:nrow(data_bib), ceiling(seq_along(1:nrow(data_bib)) / 50))

AllDataBib <- data_bib

for (n in 1:length(grouped)){
   data_bib <- AllDataBib[grouped[[n]],]
   

   fname1 <-  paste0("./data/MainScreeningData_Screen2_SUBSET_",
                     sprintf("%02d",min(grouped[[n]])), "-" ,
                     max(grouped[[n]]),".RData")
   
   fname2 <-  paste0("./data/04_Data-ready-2nd-screen/MainScreeningData_Screen2_SUBSET_",
                     sprintf("%02d",min(grouped[[n]])), "-" ,
                     max(grouped[[n]]),".RData")
   
   save(data_bib, file = fname1)
   save(data_bib, file = fname2)
   
}

sample_CrossVal <- sample(1:nrow(AllDataBib),20,replace=FALSE)
data_bib <- AllDataBib[sample_CrossVal,]

fname1 <-  paste0("./data/MainScreeningData_Screen2_CROSSVAL_Sample20.RData")
fname2 <-  paste0("./data/04_Data-ready-2nd-screen/MainScreeningData_Screen2_CROSSVAL_Sample20.RData")

save(data_bib, file = fname1)
save(data_bib, file = fname2)

rm(list=ls())


#---------------------------------------------------------------
# OPTIONAL.  You can automatically tag stuff before starting
#---------------------------------------------------------------
# search_risk_assessment <- grep(toupper("risk assessment"), toupper(data_bib$TI))
# data_bib$Screen3_assessment[search_risk_assessment] <- "1"
# 
# search_vulnerability <- grep(toupper("vulnerability"), toupper(data_bib$TI))
# data_bib$Screen3_assessment[search_vulnerability] <- "2"
# 
# search_risk_perception <- grep(toupper("risk perception"), toupper(data_bib$TI))
# data_bib$Screen3_assessment[search_risk_perception] <- "3"
# 
# #----------------------------
# # Geography type search
# search_urban <- grep(toupper("urban"), toupper(data_bib$AB))
# data_bib$Screen3_geo[search_urban] <- "1"
# search_city <- grep(toupper("city"), toupper(data_bib$AB))
# data_bib$Screen3_geo[search_city] <- "1"
# search_rural <- grep(toupper("rural"), toupper(data_bib$AB))
# data_bib$Screen3_geo[search_rural] <- "2"
# 
# 
# #----------------------------
# # Flood type search
# search_runoff <- grep(toupper("runoff"), toupper(data_bib$AB))
# search_rainfall <- grep(toupper("rainfall"), toupper(data_bib$AB))
# search_dam <- grep(toupper("dam "), toupper(data_bib$AB))
# search_levee <- grep(toupper("levee"), toupper(data_bib$AB))
# search_landslide <- grep(toupper("landslide"), toupper(data_bib$AB))
# search_mudslide <- grep(toupper("mudslide"), toupper(data_bib$AB))
# search_snow <- grep(toupper("snow"), toupper(data_bib$AB))
# 
# 
# data_bib$Screen3_flood[search_runoff] <- "2"
# data_bib$Screen3_flood[search_rainfall] <- "2"
# data_bib$Screen3_flood[search_dam] <- "3"
# data_bib$Screen3_flood[search_levee] <- "3"
# data_bib$Screen3_flood[search_landslide] <- "5"
# data_bib$Screen3_flood[search_mudslide] <- "5"
# data_bib$Screen3_flood[search_snow] <- "6"
# 
# 
# save(data_bib, file = "data/screeningSocialData.rData")



######################### Not Used

#----------------------------
# Meta-analysis search

# search_climate <- grep(toupper("climate change"),toupper(data_bib$AB))
# data_bib$Screen3_meta[search_climate] <- "1"
# search_policy <- grep(toupper("policy"),toupper(data_bib$AB))
# data_bib$Screen3_meta[search_policy] <- "4"


#----------------------------
# Impact type search -- THIS NEEDS TO APPEND NOT OVERWRITE
# DOESN"T WORK IN MAIN CODE ANYWAY 
# search_impact_fatal <- grep(toupper("death"), toupper(data_bib$AB))
# data_bib$Screen3_impact[search_impact_fatal] <- "1"
# search_impact_comm <- grep(toupper("community"), toupper(data_bib$AB))
# data_bib$Screen3_impact[search_impact_comm] <- "5"
# search_impact_infra <- grep(toupper("infrastructure"), toupper(data_bib$AB))
# data_bib$Screen3_impact[search_impact_infra] <- "6"


######################### SAMPLE 
# search_climate <- grep(toupper("climate change"),toupper(data_bib$TI))
# search_vulnerability <- grep(toupper("vulnerability") ,toupper(data_bib$TI))
# data_bib$TEST <- NA
# # This finds the titles with "climate change" OR "vulnerability" then puts "OR" into those rows of the TEST column
# # You are playing with the same output column so it will overwrite.
# data_bib$TEST [unique(c(search_climate,search_vulnerability))] <- "OR"
# 
# # This finds the titles with "climate change" AND "vulnerability" then puts "AND" into those rows of the TEST column
# data_bib$TEST [intersect(search_climate,search_vulnerability)]  <- "AND" ### AND
# # But maybe you want both tags in your output file, so this will do both separated by ;
# data_bib$TEST2 <- NA
# # This finds the titles with "climate change" AND "vulnerability" then puts "AND" into those rows of the TEST column
# data_bib$TEST2 [intersect(search_climate,search_vulnerability)]  <- "AND" ### AND
# data_bib$TEST2 [unique(c(search_climate,search_vulnerability))] <-
#   paste(data_bib$TEST2[unique(c(search_climate,search_vulnerability))],"OR",sep=";")
# #print out the results
# data_bib[,c("TI","TEST","TEST2")]