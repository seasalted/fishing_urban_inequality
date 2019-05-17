#===============================================================================
# Connecting MRIP metropolitan site landings with socioeconomic datasets
#  
# Name: 
# Created: J. Zachary (Zach) Koehn
# Modified: Benoit Parmentier
# Email: zkoehn@uw.edu
# For: SESYNC Graduate Pursuit
# Date started: 12/17/2018
# Revised: 05/16/2019
#===============================================================================


#libraries and data
library(tidyverse) #for data cleaning/plotting
library(readxl) # for working with excel workbooks
library(RColorBrewer) #for data viz
library(ppcor)
library(corrplot)
library(mctest)
library(pastecs)
library(MASS)
library(mctest)
library(pastecs)
#library(MASS) #for ordinal logit
library(nnet) #for multinomial logit
library(effects) # for visualizing effects of logits
library(brant) # for testing parallel regression assumption in ordinal logits
library(car) #for analyzing results
library(stargazer) # for viewing tables

###### Functions used in this script and sourced from other files

create_dir_fun <- function(outDir,out_suffix=NULL){
  #if out_suffix is not null then append out_suffix string
  if(!is.null(out_suffix)){
    out_name <- paste("output_",out_suffix,sep="")
    outDir <- file.path(outDir,out_name)
  }
  #create if does not exists
  if(!file.exists(outDir)){
    dir.create(outDir)
  }
  return(outDir)
}

# #Benoit setup and filepaths
# script_path <- "/nfs/bparmentier-data/Data/projects/FishingandUrbanInequality-data/scripts"
# #ARGS 1
# in_dir <- "/nfs/bparmentier-data/Data/projects/FishingandUrbanInequality-data/data"
# #ARGS 2
# out_dir <- "/nfs/bparmentier-data/Data/projects/FishingandUrbanInequality-data/outputs"

# crop_data_processing_functions <- "link_metrofish_ses_functions_05142019.R"
# source(file.path(script_path,crop_data_processing_functions))



#Zach setup and filepaths
script_path <- "/Users/zachkoehn/UW/FoodFishHappy/SESYNC.Grad/scriptdata/scripts"
#ARGS 1
in_dir_metro_zips <- "/Users/zachkoehn/UW/FoodFishHappy/SESYNC.Grad/scriptdata/data/metro_data"
in_dir_metro_landings <- "/Users/zachkoehn/UW/FoodFishHappy/SESYNC.Grad/scriptdata/data/metro_ZipSiteLanding"
in_dir_metro <- "/Users/zachkoehn/UW/FoodFishHappy/SESYNC.Grad/scriptdata/data/"
#ARGS 2
out_dir <- "/Users/zachkoehn/UW/FoodFishHappy/SESYNC.Grad/scriptdata/outputs"


############################################################################
#####  Parameters and argument set up ###########

# set working directory
#setwd("/Users/zachkoehn/UW/FoodFishHappy/SESYNC.Grad/ScriptData/ForBenoit")
# i hate it does this... 
#options(stringsAsFactors=FALSE)
# Personal R is set to french/spanish depending on the day and I have no idea how to fix it other than run this
#Sys.setenv(LANG = "en")



#ARGS 3
create_out_dir_param=TRUE #create a new ouput dir if TRUE
#ARGS 7
out_suffix <-"example_analyses_metrofish_05152019" #output suffix for the files and ouptut folder #param 12
#ARGS 8
num_cores <- 2 # number of cores

# all metropolitan zips for NOLA & TBSP
metro_zips_filename <- "metropolitan_LA_FL_ZCTAs.csv"
# fish landing zip data
LA_MRIP_filename <- "metro_ZipSiteLanding_LA.csv"
FL_MRIP_filename <- "metro_ZipSiteLanding_FL.csv"
fish_dat_filename <- "mrip_species_zip_site_2004_2017_012019.csv"
#SheetNames <- "All_variables.xls"
ses_dat_full_filename <- "All_variables.xls" 

var_names <- c(
  "zcta","state","landings","landings_quantile",
  "racial_minority_percent_pop",
  "foreign_born_percent_pop",
  "poverty_percent_famil",
  "median_income_dollars_hhlds",
  "education_HS_GED_percent_pop",
  "no_vehicles_percent_hhlds",
  "one_vehicle_percent_hhlds",
  "food_stamp_percent_hhlds"
)

################# START SCRIPT ###############################

######### PARTt6 0: Set up the output dir ################

options(scipen=999)

if(is.null(out_dir)){
  out_dir <- in_dir #output will be created in the input dir
  
}
#out_dir <- in_dir #output will be created in the input dir

out_suffix_s <- out_suffix #xcan modify name of output suffix
if(create_out_dir_param==TRUE){
  out_dir <- create_dir_fun(out_dir,out_suffix)
  setwd(out_dir)
}else{
  setwd(out_dir) #use previoulsy defined directory
}

#######################################
### PART I READ AND PREPARE DATA #######
#set up the working directory
#Create output directory

##################################################################
###
###        1
### ___________________
### Clean data and add landings quantiles for analyses
###
##################################################################

## directory set earlier

# all metropolitan zips for NOLA & TBSP
metro_zips <- read.csv(file.path(in_dir_metro_zips,metro_zips_filename),header=TRUE)
# metro_zips <- data.frame(ZCTA5=metro_zips[,1])
# fish landing zip data
LA_MRIP <- read.csv(file.path(in_dir_metro_landings,LA_MRIP_filename),header = TRUE)
FL_MRIP <- read.csv(file.path(in_dir_metro_landings,FL_MRIP_filename),header = TRUE)
fish_dat <- read.csv(file.path(in_dir,fish_dat_filename),header = TRUE)

table(metro_zips$STATE) #two states
#12  22 
#136  80 

#create landings quantiles. right now we're just breaking them into bottom , middle third 
#and top third of landings for each metropolitan area
LA_MRIP <- LA_MRIP %>%
	filter(YEAR %in% 2007:2011) %>% #includes only years for which we have socioeconomic info
	group_by(ZIP) %>% #groups everything by zip
	summarise(LANDINGS_sum_2007to2011 = sum(landing,na.rm=TRUE) )  #sums all landing info across 2007 to 2011 

#Before separating into quantitle

plot(LA_MRIP$LANDINGS_sum_2007to2011,type="h")

# separate into thirds
LA_MRIP <- within(LA_MRIP, 
                  quantile <- as.integer(cut(LANDINGS_sum_2007to2011, 
                                             quantile(LANDINGS_sum_2007to2011, probs=0:3/3), 
                                             include.lowest=TRUE)))
#quantiles_LA <- quantile(LA_MRIP$LANDINGS_sum_2007to2011, probs=0:3/3)

### Florida:
FL_MRIP <- FL_MRIP %>%
	filter(YEAR %in% 2007:2011) %>% #includes only years for which we have socioeconomic info
	group_by(ZIP) %>% #groups everything by zip
	summarise(LANDINGS_sum_2007to2011 = sum(landing,na.rm=TRUE) )  #sums all landing info across 2007 to 2011 

plot(FL_MRIP$LANDINGS_sum_2007to2011,type="h")

FL_MRIP <- within(FL_MRIP, 
                  quantile <- as.integer(cut(LANDINGS_sum_2007to2011, 
                                             quantile(LANDINGS_sum_2007to2011, probs=0:3/3), 
                                             include.lowest=TRUE)))
#quantiles_FL <- quantile(FL_MRIP$LANDINGS_sum_2007to2011, probs=0:3/3)

# collapse the landings quantiles back into the same dataset
ZIP_MRIP <- rbind(LA_MRIP,FL_MRIP)
head(ZIP_MRIP)

# socioeconomic data extract from excel and bind into single data frame

# Make first column of the data frame to bind all other csv tables into
#SheetNames <- excel_sheets("data/All_variables.xls")
#ses_dat_full <- read_excel(file.path(in_dir,ses_dat_full_filename) , 
#                           sheet=SheetNames[1])

#SheetNames <- excel_sheets(file.path(in_dir,ses_dat_full_filename))
#test <- read_excel(file.path(in_dir,ses_dat_full_filename) , sheet=SheetNames[1])

#test$Id == ses_dat_full$Id

SheetNames <- excel_sheets(file.path(in_dir_metro_zips,ses_dat_full_filename))
ses_dat_full <- read_excel(file.path(in_dir_metro_zips,ses_dat_full_filename),sheet=SheetNames[1])#default reads the first shee
ses_dat <- cbind(ses_dat_full[,2],ses_dat_full[,4:dim(ses_dat_full)[2]])

# set names 
names(ses_dat)[1] <- "Id2"
ses_dat_uncleaned <- ses_dat

# loop through remainders


# loop through remainders to fill in the dataframe with contents extracted from each sheet
for(i in 2:length(SheetNames)) {
  
  sheet_for_rbind <- read_excel( file.path(in_dir_metro_zips,ses_dat_full_filename) , sheet=SheetNames[i])
  sheet_for_rbind <- cbind(sheet_for_rbind[,2],sheet_for_rbind[,4:dim(sheet_for_rbind)[2]])
  names(sheet_for_rbind)[1] <- "Id2"

  ses_dat <- merge(ses_dat,sheet_for_rbind, by="Id2")

}


#remove margin of error columns, as we currently do not have need for it
ses_dat <- ses_dat[, -grep("Margin of Error", colnames(ses_dat))] 
#remove duplicate columns for total household estimates, only need one
ses_dat <- ses_dat[,-c(9,11,16)] #more general way of doing this
colnames(ses_dat)[5] <- "Total_households"


#cleaner as function:


# commenting out this lapply version of processing excel workbook into useable dataframe b/c I can't get it to work correctly and using the for loop above 
# test <- lapply(SheetNames,
#                FUN= process_variables,
#                data_variables=file.path(in_dir_metro_zips,ses_dat_full_filename))

# View(test[[1]])

# for(i in 2:length(SheetNames)) {
	
# 	sheet_for_rbind <- read_excel(file.path(in_dir,ses_dat_full_filename), 
# 	                              sheet=SheetNames[i])
# 	sheet_for_rbind <- cbind(sheet_for_rbind[,2],sheet_for_rbind[,4:dim(sheet_for_rbind)[2]])
# 	names(sheet_for_rbind)[1] <- "Id2"
# 	ses_dat <- merge(ses_dat,sheet_for_rbind, by="Id2")
# }




#check to see if ZCTA5 and ZIPs line up
# intersect(metro_zips$ZCTA5,ZIP_MRIP$ZIP)
# intersect(metro_zips$ZCTA5,ses_dat$Id2)

# Merge the socioeconic information I pulled from socioeconomic datasets Sarita gave me (the loop) 
#with only metro zips for the two areas
metro_ses <- merge(metro_zips, ses_dat,all.x = TRUE, by.x = "ZCTA5",by.y = "Id2") 
#this merges dataframe with landings quantiles calculated above
metro_landings <- merge(metro_zips,ZIP_MRIP,all.x=TRUE,by.x="ZCTA5",by.y="ZIP") 
# metro_landings$quantile <- as.factor(metro_landings$quantile) 

intersect(metro_landings$ZCTA5,ses_dat$Id2) #check to see overlap

#### Select only zips, ctza with relevant socio-economic information?
# brings together zipcode landings quantiles and socioeconomic information for statistical analysis
ses_metro_landings <- merge(metro_landings,metro_ses,all.y=TRUE,by=c("ZCTA5","STATE"))

dim(ses_metro_landings)

# Change from integer values to character values: use recode from dplyr!!
#https://dplyr.tidyverse.org/reference/recode.html

for(i in 1:nrow(ses_metro_landings)) {
  if( is.na(ses_metro_landings$quantile[i])==TRUE) {ses_metro_landings$quantile[i] <- "NotMRIP"}
  if(ses_metro_landings$quantile[i]==1) {ses_metro_landings$quantile[i] <- "Low"}
  if(ses_metro_landings$quantile[i]==2) {ses_metro_landings$quantile[i] <- "Mod"}
  if(ses_metro_landings$quantile[i]==3) {ses_metro_landings$quantile[i] <- "High"}
  
}

#and realize that the state classification used to split datasets below isn't in the SES data
# fortunately, zips for Louisiana are in the 7000 range while Florida is 3000, so can use a
#  conditional to separate ---> keep the label. I suggest adding a variable!

ses_metro_landings$STATE <- ifelse(ses_metro_landings$ZCTA5<60000,12,22) # but this doesn't work

# bind together only relevant vectors of info: use this instead
selected_variables <- c("ZCTA5", "STATE") #, "LANDINGS", "QUANTILE")
fish_metro_dat <- ses_metro_landings[,selected_variables]

fish_metro_dat <- cbind(
  ses_metro_landings[,1:4], # ZCTA5, STATE, LANDINGS, QUANTILE :use name!!!
  ses_metro_landings[,7], # pct racial minority
  ses_metro_landings[,9], # pct foreign born
  ses_metro_landings[,11], #pct families below poverty level
  ses_metro_landings[,12], #median income
  ses_metro_landings[,13], # pct with GED highest degree
  ses_metro_landings[,15:16], # pct no/1 vehicle
  ses_metro_landings[,18] # pct hhlds receiving food stamps
)

head(fish_metro_dat)

# and name them somethign reasonably informative but short

names(fish_metro_dat) <- c(
  "zcta","state","landings","landings_quantile",
  "racial_minority_percent_pop",
  "foreign_born_percent_pop",
  "poverty_percent_famil",
  "median_income_dollars_hhlds",
  "education_HS_GED_percent_pop",
  "no_vehicles_percent_hhlds",
  "one_vehicle_percent_hhlds",
  "food_stamp_percent_hhlds"
)

# convert to numeric wherever needed
fish_metro_dat$poverty_percent_famil <- as.numeric(fish_metro_dat$poverty_percent_famil)
fish_metro_dat$median_income_dollars_hhlds <- as.numeric(fish_metro_dat$median_income_dollars_hhlds)

### write out processed data
out_file_processed_data <- paste0("fish_metro_dat","_",out_suffix,".csv")
write.table(fish_metro_dat,
            file.path(out_dir,out_file_processed_data))

##################################################################
###
###        2
### ___________________
### Is multicollinearity a concern?
### - Answer: at first glance, yes, but when each variable is analyzed not so much
###
##################################################################

# so before running models, do we have to worry about collinearity between variables? 
# uses https://datascienceplus.com/multicollinearity-in-r/ as a template
cor_fl = cor(fish_metro_dat[fish_metro_dat$state==12,5:12],use="na.or.complete")

corrplot.mixed(cor_fl, lower.col = "black", number.cex = .7) #cute correlation plot to visualize correlation matrix

cor_la = cor(fish_metro_dat[fish_metro_dat$state==22,5:12],use="na.or.complete")

corrplot.mixed(cor_la, lower.col = "black", number.cex = .7) #cute correlation plot to visualize correlation matrix

# income has higher correlation with a lot of variables, as does food stamp, 
#library(mctest)
# first figure out whether whole model has multicollinearity issues by implementing brant test (from mctest)
omcdiag(fish_metro_dat[fish_metro_dat$state==12,5:12],fish_metro_dat[fish_metro_dat$state==12,]$median_income_dollars_hhlds) #column selected for y doesn't change outcome, just selected median income
omcdiag(fish_metro_dat[fish_metro_dat$state==22,5:12],fish_metro_dat[fish_metro_dat$state==22,]$median_income_dollars_hhlds) #column selected for y doesn't change outcome, just selected median income
# answer is yes, so second can we locate which variable contributes most to multicollinearity using this package
#library(ppcor)

# TAMPA-ST. PETES metro area
fl_data_test <- fish_metro_dat[fish_metro_dat$state==12,5:12]

fl_pcor_pvals <- pcor(fl_data_test[complete.cases(fl_data_test==TRUE),],method="pearson")$p.value
fl_pcor_coefs <- round(pcor(fl_data_test[complete.cases(fl_data_test==TRUE),],method="pearson")$estimate,digits = 2)
fl_pcor_coefs
fl_pcor_pvals

# calcluates variable inflation factor for each dataset
imcdiag(fish_metro_dat[fish_metro_dat$state==12,5:12],
        as.numeric(as.factor(fish_metro_dat[fish_metro_dat$state==12,]$landings_quantile)))

# fl_pcor_coefs>0.599
# Tampa-St.Petes coefs >0.599 include 
# foreign_born_percent_pop & poverty_percent_famil (pval*** = 2.152549e-21); 
# foreign_born_percent_pop & no_vehicles_percent_hhlds (pval*** = 3.745262e-34);
# almost:
# poverty_percent_famil & one_vehicle_percent_hhlds is -0.59  (pval*** = 5.927526e-13)

fl_cor_coefs <-round(cor(fish_metro_dat[fish_metro_dat$state==12,5:12],use="na.or.complete"),digits=2)
fl_cor_coefs

#library(pastecs)
write.csv(t(round(stat.desc(fish_metro_dat[fish_metro_dat$state==12,5:12]),digits=2)),
          file.path(out_dir,"fl_summary_stats.csv"))
write.csv(t(round(stat.desc(fish_metro_dat[fish_metro_dat$state==22,5:12]),digits=2)),
          file.path(out_dir,"la_summary_stats.csv"))
names(fish_metro_dat)

# NOLA metro area
la_data_test <- fish_metro_dat[fish_metro_dat$state==22,5:12]
la_pcor_pvals <- pcor(la_data_test[complete.cases(la_data_test==TRUE),],method="pearson")$p.value
la_pcor_coefs <- round(pcor(la_data_test[complete.cases(la_data_test==TRUE),],method="pearson")$estimate,digits = 2)
la_pcor_coefs

imcdiag(fish_metro_dat[fish_metro_dat$state==22,5:12],as.numeric(as.factor(fish_metro_dat[fish_metro_dat$state==22,]$landings_quantile)))
# la_pcor_coefs>0.599

# NOlacoefs >0.599 include 
# foreign_born_percent_pop & poverty_percent_famil(pval*** = 2.152549e-21); 
# foreign_born_percent_pop & no_vehicles_percent_hhlds; (pval*** = 3.745262e-34)

# almost:
# foreign_born_percent_pop & racial_minority_percent_pop is 0.58 (pval*** = 1.840686e-07)

##################################################################
###
###        3
### ___________________
### Run a number of models, and export performance to html summary tables
### 
###
##################################################################


#library(MASS) #for ordinal logit
#library(nnet) #for multinomial logit
#library(effects) # for visualizing effects of logits
#ibrary(brant) # for testing parallel regression assumption in ordinal logits
#library(car) #for analyzing results


# first remove zip rows with NA

fl_dat <- fish_metro_dat[fish_metro_dat$state==12,]
la_dat <- fish_metro_dat[fish_metro_dat$state==22,]

names(fl_dat)

fl_dat <- fl_dat[complete.cases(fl_dat[,5:12])==TRUE,]
la_dat <- la_dat[complete.cases(la_dat[,5:12])==TRUE,]

fl_dat$median_income_dollars_hhlds_percent_scaled <- fl_dat$median_income_dollars_hhlds/max(fl_dat$median_income_dollars_hhlds)
la_dat$median_income_dollars_hhlds_percent_scaled <- la_dat$median_income_dollars_hhlds/max(la_dat$median_income_dollars_hhlds)

out_file_fl_data <- paste0("fl_updated_income","_",out_suffix,".csv")
write.csv(fl_dat,
          "fl_updated_income.csv",row.names=FALSE)
out_file_la_data <- paste0("la_dat","_",out_suffix,".csv")
write.csv(la_dat,
          "outputs/la_updated_income.csv",row.names=FALSE)
file.path(out_dir,out_file_processed_data)
### write out processed data
out_file_processed_data <- paste0("fish_metro_dat","_",out_suffix,".csv")
write.table(fish_metro_dat,
            file.path(out_dir,out_file_processed_data))

#and order MRIP landings categories from 
fl_dat$landings_quantile <- factor(
	fl_dat$landings_quantile, 
	levels = c("NotMRIP","Low","Mod","High")
	)
la_dat$landings_quantile <- factor(
	la_dat$landings_quantile, 
	levels = c("NotMRIP","Low","Mod","High")
	)


model_call <- c(
  "landings_quantile ~ racial_minority_percent_pop",
  "landings_quantile ~ foreign_born_percent_pop",
  "landings_quantile ~ median_income_dollars_hhlds_percent_scaled",
  "landings_quantile ~ education_HS_GED_percent_pop",
  "landings_quantile ~ no_vehicles_percent_hhlds",
  "landings_quantile ~ one_vehicle_percent_hhlds",
  "landings_quantile ~ food_stamp_percent_hhlds",
  "landings_quantile ~
      racial_minority_percent_pop +
      foreign_born_percent_pop +
      median_income_dollars_hhlds_percent_scaled +  
      education_HS_GED_percent_pop +
      no_vehicles_percent_hhlds +
      one_vehicle_percent_hhlds +
      food_stamp_percent_hhlds"

)

polr_model_objs_function <- function(model_call,data) {
  model_object <- polr(model_call,data=data, Hess = TRUE)
  return(model_object)
}

# Tampa Bay St. Pete's models
fl_ordinal_logit_model_objects <- lapply(model_call, function(x) polr_model_objs_function(x,data=fl_dat) )

as.vector(lapply(fl_ordinal_logit_model_objects,function(x) summary(x)))
as.vector(lapply(fl_ordinal_logit_model_objects,function(x) AIC(x)))

# Now New Orleans models
la_ordinal_logit_model_objects <- lapply(model_call, function(x) polr_model_objs_function(x,data=la_dat) )

as.vector(lapply(la_ordinal_logit_model_objects,function(x) summary(x)))
as.vector(lapply(la_ordinal_logit_model_objects,function(x) AIC(x)))


# testing for the proportional odds assumption using a Brant test
# a nonsignificant omnibus (aka across the whole model) would show that the effect
# of the response (landings) is constant across separate model fits 
# to each category - and coefficients should be the same for each logistic regression
# this significes that the influence of 
# socioeconomic predictor varialbes are proportional across each category of landings

# For Florida, null hypothesis that parallel regression assumption 
 # is shown to be false for the multivariate model
as.vector(lapply(fl_ordinal_logit_model_objects,function(x) brant(x)))

# For Louisiana, the null hypothesis DOES hold for the multivariate model
as.vector(lapply(la_ordinal_logit_model_objects,function(x) brant(x)))

# Given this, and that the NotMRIP category is definitely not "rank ordered", we are
# using a multinomial logit over the ordinal 
#library(nnet)

# create function that passes model calls with {nnet} multinom
multinom_model_objs_function <- function(model_call,data) {
  require(nnet)
  model_object <- multinom(model_call,data=data, Hess = TRUE)
  return(model_object)
}


# Tampa Bay St. Pete's models
fl_multinom_logit_model_objects <- lapply(model_call, function(x) multinom_model_objs_function(x,data=fl_dat) )

lapply(fl_multinom_logit_model_objects,function(x) summary(x))
unlist(lapply(fl_multinom_logit_model_objects,function(x) AIC(x)))

# Now New Orleans models
la_multinom_logit_model_objects <- lapply(model_call, function(x) multinom_model_objs_function(x,data=la_dat) )

lapply(la_multinom_logit_model_objects,function(x) summary(x))
lapply(la_multinom_logit_model_objects,function(x) x$edf)

# create nice model selection summary tables in r
# useful guide from Hao Zhu here https://haozhu233.github.io/kableExtra/awesome_table_in_html.html
# and on formattable here: https://www.displayr.com/formattable/

place <- c(
    rep("TSP",8),
    rep("NOLA",8)
  )

model_AIC <- round(
  c(  
    unlist(lapply(fl_multinom_logit_model_objects,function(x) AIC(x))),
    unlist(lapply(la_multinom_logit_model_objects,function(x) AIC(x)))
  ),
  digits=3
)

model_AIC

model_df <- round(
  c(
    unlist(lapply(fl_multinom_logit_model_objects,function(x) x$edf)),
    unlist(lapply(la_multinom_logit_model_objects,function(x) x$edf))
  )
)

model_deviance <- round(
  c(
    unlist(lapply(fl_multinom_logit_model_objects,function(x) x$deviance)),
    unlist(lapply(la_multinom_logit_model_objects,function(x) x$deviance))
  ),
  digits=3
)
model_convergence <- round(
  c(
    unlist(lapply(fl_multinom_logit_model_objects,function(x) x$convergence)),
    unlist(lapply(la_multinom_logit_model_objects,function(x) x$convergence))
  ),
  digits=3
)
model_call <- 
  as.character(
    c( 
  	  unlist(lapply(fl_multinom_logit_model_objects, function(x) eval( x$call[[2]] ) ) ),
      unlist(lapply(la_multinom_logit_model_objects, function(x) eval( x$call[[2]] ) ) )

  )
)




model_outputs <- data.frame(
  model = 1:16,
  formula = model_call,
  df = model_df,
  AIC = model_AIC,
  Deviance = model_deviance
)


options(knitr.table.format = "html") 
out_file <- file.path("model_performance.html")

model_outputs %>%
  mutate(
    # model = row.names(.),
    
    AIC = round(AIC,digits = 2),
    Deviance = round(Deviance,digits = 1),
    convergence = ifelse(convergence == 1,
                         "No",
                         "Yes"
                         ),
    convergence = ifelse(convergence == "Yes",
                         cell_spec(convergence,color = "black"),
                         cell_spec(convergence,color="red",bold = TRUE)
                         ),
    brant = ifelse(brant != "No",
                         cell_spec(brant,color = "black"),
                         cell_spec(brant,color="red")
                         ),
    # AIC = ifelse(7 >= abs(AIC-min(AIC)),
    #              cell_spec(AIC,color="blue"),
    #              cell_spec(AIC,color="black")
    # ),
    Deviance = color_bar("darksalmon")(Deviance)
    
  ) %>%
  kable(
    col.names = c(
      "Model #",
      "Formula",
      "Model Type",
      "df",
      "Converged?",
      "Brant Test Passed?",
      "AIC",
      "Residual Deviance"
    ),
    escape = F
    ) %>%
  kable_styling("hover", full_width = T) %>%
  group_rows("Tampa St. Pete's Metro Area Models", 1, 3, label_row_css = "background-color: #666; color: #fff;") %>%
  group_rows("New Orleans Metro Area Models", 4,5, label_row_css = "background-color: #666; color: #fff;") %>%
  group_rows("Tampa St. Pete's Metro Area Model without category for nonMRIP zips", 6,6, label_row_css = "background-color: #666; color: #fff;") %>%
  group_rows("New Orleans Metro Area Models without category for nonMRIP zips", 7,7, label_row_css = "background-color: #666; color: #fff;") %>%
  save_kable(file = out_file, self_contained = T)



#library(car)

Anova(fl_ordered_logit_noPov)
Anova(la_ordered_logit_noPov)

Anova(fl_multinom_logit_noPov)
Anova(fl_multinom_logit_noPovNoInc,type="III")

Anova(la_multinom_logit_noPov,type="III")

broom::tidy(fl_multinom_logit_noPov)
broom::tidy(fl_multinom_logit_noPovNoInc)

Anova(fl_multinom_logit_noPov,type="III")
Anova(fl_multinom_logit_noPovNoInc,type="III")

summary(fl_multinom_logit_noPov)$coefficients

#library(effects)

plot(allEffects(fl_multinom_logit_noPov))
plot(allEffects(la_multinom_logit_noPov))


Anova(la_ordered_logit_noPov,type="III")

pdf("plots/diagnosticPlots/NOLA_ordinal_logit_effects.pdf")
plot(effect("racial_minority_percent_pop",la_ordered_logit_noPov))
plot(effect("foreign_born_percent_pop",la_ordered_logit_noPov))
plot(effect("median_income_dollars_hhlds_percent_scaled",la_ordered_logit_noPov))
plot(effect("education_HS_GED_percent_pop",la_ordered_logit_noPov))
plot(effect("no_vehicles_percent_hhlds",la_ordered_logit_noPov))
plot(effect("one_vehicle_percent_hhlds",la_ordered_logit_noPov))
plot(effect("food_stamp_percent_hhlds",la_ordered_logit_noPov))

dev.off()

plot(effect("foreign_born_percent_pop",la_ordered_logit_noPov))

Anova(fl_ordered_logit_NoPovNoInc,type="III")
pdf("plots/diagnosticPlots/TB_ordinal_logit_effects.pdf")

plot(effect("racial_minority_percent_pop",fl_ordered_logit_NoPovNoInc))
plot(effect("foreign_born_percent_pop",fl_ordered_logit_NoPovNoInc))
plot(effect("education_HS_GED_percent_pop",fl_ordered_logit_NoPovNoInc))
plot(effect("no_vehicles_percent_hhlds",fl_ordered_logit_NoPovNoInc))
plot(effect("one_vehicle_percent_hhlds",fl_ordered_logit_NoPovNoInc))
plot(effect("food_stamp_percent_hhlds",fl_ordered_logit_NoPovNoInc))
dev.off()

################## Figures generation, I suggest using function for clarity

pdf("plots/diagnosticPlots/SES_diagnostics_plots.pdf")
	i=3
	dat <- ses_metro_landings[,i]
	layout <- layout(
		matrix(
		  c(
				1,2,3,4
			),
		nrow=2,
		ncol=2
		)
	)
	# layout.show(layout)

	par(mar=c(2,2,2,2),
		oma=c(0,0,3,0)
	)
	boxplot(dat,frame=FALSE)

	plot.new()
	legend(
		"center",
		bty = "n",
		legend = c(
				paste("Sample Size:",length(dat)),
				paste("Median:",format(median(dat,na.rm=TRUE),digits=0) ),
				paste("Mean:",format(mean(dat,na.rm=TRUE),digits=0) ),
				paste("St Dev:",format(sd(dat,na.rm=TRUE),digits=0) ),
				paste("3 Min Vals::",
					format(sort(dat,decreasing=FALSE)[1],digits=0),",",
					format(sort(dat,decreasing=FALSE)[2],digits=0),",",
					format(sort(dat,decreasing=FALSE)[3],digits=0)
					),
				paste("3 Max Vals:",
					format(sort(dat,decreasing=TRUE)[1],digits=0),",",
					format(sort(dat,decreasing=TRUE)[2],digits=0),",",
					format(sort(dat,decreasing=TRUE)[3],digits=0)
					)
			),
		cex=1.2
	)

	qqnorm(y=dat,bty="n")
	qqline(y=dat)

	hist(dat,main=NULL)

	title(
			paste(names(ses_metro_landings)[i]),
			outer=TRUE
	)


	for(i in 5:18) {
		# i=11
		dat <- as.numeric(ses_metro_landings[,i])
		layout <- layout(
			matrix(c(
					1,2,3,4
				),
			nrow=2,
			ncol=2
			)
		)
		# layout.show(layout)

		par(mar=c(2,2,2,2),
			oma=c(0,0,3,0)
		)
		boxplot(dat,frame=FALSE)

		plot.new()
		legend(
			"center",
			bty = "n",
			legend = c(
					paste("Sample Size:",length(dat)),
					paste("Median:",format(median(dat,na.rm=TRUE),digits=0) ),
					paste("Mean:",format(mean(dat,na.rm=TRUE),digits=0) ),
					paste("St Dev:",format(sd(dat,na.rm=TRUE),digits=0) ),
					paste("3 Min Vals::",
						format(sort(dat,decreasing=FALSE)[1],digits=0),",",
						format(sort(dat,decreasing=FALSE)[2],digits=0),",",
						format(sort(dat,decreasing=FALSE)[3],digits=0)
						),
					paste("3 Max Vals:",
						format(sort(dat,decreasing=TRUE)[1],digits=0),",",
						format(sort(dat,decreasing=TRUE)[2],digits=0),",",
						format(sort(dat,decreasing=TRUE)[3],digits=0)
						)
				),
			cex=1.2
		)

		qqnorm(y=dat,bty="n")
		qqline(y=dat)

		hist(dat,main=NULL)

		title(
				paste(names(ses_metro_landings)[i]),
				outer=TRUE
		)
	}

dev.off()

pdf("plots/diagnosticPlots/SES_diagnostics_plots_florida.pdf")
	florida <- ses_metro_landings[ses_metro_landings$STATE==12,]
	i=3
	dat <- florida[,i]
	layout <- layout(
		matrix(c(
				1,2,3,4
			),
		nrow=2,
		ncol=2
		),
	)
	# layout.show(layout)

	par(mar=c(2,2,2,2),
		oma=c(0,0,3,0)
	)
	boxplot(dat,frame=FALSE)

	plot.new()
	legend(
		"center",
		bty = "n",
		legend = c(
				paste("Sample Size:",length(dat)),
				paste("Median:",format(median(dat,na.rm=TRUE),digits=0) ),
				paste("Mean:",format(mean(dat,na.rm=TRUE),digits=0) ),
				paste("St Dev:",format(sd(dat,na.rm=TRUE),digits=0) ),
				paste("3 Min Vals::",
					format(sort(dat,decreasing=FALSE)[1],digits=0),",",
					format(sort(dat,decreasing=FALSE)[2],digits=0),",",
					format(sort(dat,decreasing=FALSE)[3],digits=0)
					),
				paste("3 Max Vals:",
					format(sort(dat,decreasing=TRUE)[1],digits=0),",",
					format(sort(dat,decreasing=TRUE)[2],digits=0),",",
					format(sort(dat,decreasing=TRUE)[3],digits=0)
					)
			),
		cex=1.2
	)

	qqnorm(y=dat,bty="n")
	qqline(y=dat)

	hist(dat,main=NULL)

	title(
			paste(names(florida)[i]),
			outer=TRUE
	)


	for(i in 5:18) {
		# i=11
		dat <- as.numeric(florida[,i])
		layout <- layout(
			matrix(c(
					1,2,3,4
				),
			nrow=2,
			ncol=2
			),
		)
		# layout.show(layout)

		par(mar=c(2,2,2,2),
			oma=c(0,0,3,0)
		)
		boxplot(dat,frame=FALSE)

		plot.new()
		legend(
			"center",
			bty = "n",
			legend = c(
					paste("Sample Size:",length(dat)),
					paste("Median:",format(median(dat,na.rm=TRUE),digits=0) ),
					paste("Mean:",format(mean(dat,na.rm=TRUE),digits=0) ),
					paste("St Dev:",format(sd(dat,na.rm=TRUE),digits=0) ),
					paste("3 Min Vals::",
						format(sort(dat,decreasing=FALSE)[1],digits=0),",",
						format(sort(dat,decreasing=FALSE)[2],digits=0),",",
						format(sort(dat,decreasing=FALSE)[3],digits=0)
						),
					paste("3 Max Vals:",
						format(sort(dat,decreasing=TRUE)[1],digits=0),",",
						format(sort(dat,decreasing=TRUE)[2],digits=0),",",
						format(sort(dat,decreasing=TRUE)[3],digits=0)
						)
				),
			cex=1.2
		)

		qqnorm(y=dat,bty="n")
		qqline(y=dat)

		hist(dat,main=NULL)

		title(
				paste(names(florida)[i]),
				outer=TRUE
		)
	}

dev.off()

pdf("plots/diagnosticPlots/SES_diagnostics_plots_louisiana.pdf")
	louisiana <- ses_metro_landings[ses_metro_landings$STATE==22,]
	i=3
	dat <- louisiana[,i]
	layout <- layout(
		matrix(c(
				1,2,3,4
			),
		nrow=2,
		ncol=2
		),
	)
	# layout.show(layout)

	par(mar=c(2,2,2,2),
		oma=c(0,0,3,0)
	)
	boxplot(dat,frame=FALSE)

	plot.new()
	legend(
		"center",
		bty = "n",
		legend = c(
				paste("Sample Size:",length(dat)),
				paste("Median:",format(median(dat,na.rm=TRUE),digits=0) ),
				paste("Mean:",format(mean(dat,na.rm=TRUE),digits=0) ),
				paste("St Dev:",format(sd(dat,na.rm=TRUE),digits=0) ),
				paste("3 Min Vals::",
					format(sort(dat,decreasing=FALSE)[1],digits=0),",",
					format(sort(dat,decreasing=FALSE)[2],digits=0),",",
					format(sort(dat,decreasing=FALSE)[3],digits=0)
					),
				paste("3 Max Vals:",
					format(sort(dat,decreasing=TRUE)[1],digits=0),",",
					format(sort(dat,decreasing=TRUE)[2],digits=0),",",
					format(sort(dat,decreasing=TRUE)[3],digits=0)
					)
			),
		cex=1.2
	)

	qqnorm(y=dat,bty="n")
	qqline(y=dat)

	hist(dat,main=NULL)

	title(
			paste(names(louisiana)[i]),
			outer=TRUE
	)


	for(i in 5:18) {
		# i=11
		dat <- as.numeric(louisiana[,i])
		layout <- layout(
			matrix(c(
					1,2,3,4
				),
			nrow=2,
			ncol=2
			),
		)
		# layout.show(layout)

		par(mar=c(2,2,2,2),
			oma=c(0,0,3,0)
		)
		boxplot(dat,frame=FALSE)

		plot.new()
		legend(
			"center",
			bty = "n",
			legend = c(
					paste("Sample Size:",length(dat)),
					paste("Median:",format(median(dat,na.rm=TRUE),digits=0) ),
					paste("Mean:",format(mean(dat,na.rm=TRUE),digits=0) ),
					paste("St Dev:",format(sd(dat,na.rm=TRUE),digits=0) ),
					paste("3 Min Vals::",
						format(sort(dat,decreasing=FALSE)[1],digits=0),",",
						format(sort(dat,decreasing=FALSE)[2],digits=0),",",
						format(sort(dat,decreasing=FALSE)[3],digits=0)
						),
					paste("3 Max Vals:",
						format(sort(dat,decreasing=TRUE)[1],digits=0),",",
						format(sort(dat,decreasing=TRUE)[2],digits=0),",",
						format(sort(dat,decreasing=TRUE)[3],digits=0)
						)
				),
			cex=1.2
		)

		qqnorm(y=dat,bty="n")
		qqline(y=dat)

		hist(dat,main=NULL)

		title(
				paste(names(louisiana)[i]),
				outer=TRUE
		)
	}

dev.off()


pdf("plots/diagnosticPlots/SES_diagnostics_plots_florida_LOG.pdf")
	florida <- ses_metro_landings[ses_metro_landings$STATE==12,]
	i=3
	dat <- log(florida[,i])
	dat <- dat[is.finite(dat)==TRUE]

	layout <- layout(
		matrix(c(
				1,2,3,4
			),
		nrow=2,
		ncol=2
		),
	)
	# layout.show(layout)

	par(mar=c(2,2,2,2),
		oma=c(0,0,3,0)
	)
	boxplot(dat,frame=FALSE)

	plot.new()
	legend(
		"center",
		bty = "n",
		legend = c(
				paste("Sample Size:",length(dat)),
				paste("Median:",format(median(dat,na.rm=TRUE),digits=0) ),
				paste("Mean:",format(mean(dat,na.rm=TRUE),digits=0) ),
				paste("St Dev:",format(sd(dat,na.rm=TRUE),digits=0) ),
				paste("3 Min Vals::",
					format(sort(dat,decreasing=FALSE)[1],digits=0),",",
					format(sort(dat,decreasing=FALSE)[2],digits=0),",",
					format(sort(dat,decreasing=FALSE)[3],digits=0)
					),
				paste("3 Max Vals:",
					format(sort(dat,decreasing=TRUE)[1],digits=0),",",
					format(sort(dat,decreasing=TRUE)[2],digits=0),",",
					format(sort(dat,decreasing=TRUE)[3],digits=0)
					)
			),
		cex=1.2
	)

	qqnorm(y=dat,bty="n")
	qqline(y=dat)

	hist(dat,main=NULL)

	title(
			paste(names(florida)[i]),
			outer=TRUE
	)


	for(i in 5:18) {
		# i=11
		dat <- log(as.numeric(florida[,i]))
		dat <- dat[is.finite(dat)==TRUE]
		layout <- layout(
			matrix(c(
					1,2,3,4
				),
			nrow=2,
			ncol=2
			),
		)
		# layout.show(layout)

		par(mar=c(2,2,2,2),
			oma=c(0,0,3,0)
		)
		boxplot(dat,frame=FALSE)

		plot.new()
		legend(
			"center",
			bty = "n",
			legend = c(
					paste("Sample Size:",length(dat)),
					paste("Median:",format(median(dat,na.rm=TRUE),digits=0) ),
					paste("Mean:",format(mean(dat,na.rm=TRUE),digits=0) ),
					paste("St Dev:",format(sd(dat,na.rm=TRUE),digits=0) ),
					paste("3 Min Vals::",
						format(sort(dat,decreasing=FALSE)[1],digits=0),",",
						format(sort(dat,decreasing=FALSE)[2],digits=0),",",
						format(sort(dat,decreasing=FALSE)[3],digits=0)
						),
					paste("3 Max Vals:",
						format(sort(dat,decreasing=TRUE)[1],digits=0),",",
						format(sort(dat,decreasing=TRUE)[2],digits=0),",",
						format(sort(dat,decreasing=TRUE)[3],digits=0)
						)
				),
			cex=1.2
		)

		qqnorm(y=dat,bty="n")
		qqline(y=dat)

		hist(dat,main=NULL)

		title(
				paste(names(florida)[i]),
				outer=TRUE
		)
	}

dev.off()

pdf("plots/diagnosticPlots/SES_diagnostics_plots_louisiana_LOG.pdf")
	louisiana <- ses_metro_landings[ses_metro_landings$STATE==22,]
	i=3
	dat <- log(louisiana[,i])
	dat <- dat[is.finite(dat)==TRUE]

	layout <- layout(
		matrix(c(
				1,2,3,4
			),
		nrow=2,
		ncol=2
		),
	)
	# layout.show(layout)

	par(mar=c(2,2,2,2),
		oma=c(0,0,3,0)
	)
	boxplot(dat,frame=FALSE)

	plot.new()
	legend(
		"center",
		bty = "n",
		legend = c(
				paste("Sample Size:",length(dat)),
				paste("Median:",format(median(dat,na.rm=TRUE),digits=0) ),
				paste("Mean:",format(mean(dat,na.rm=TRUE),digits=0) ),
				paste("St Dev:",format(sd(dat,na.rm=TRUE),digits=0) ),
				paste("3 Min Vals::",
					format(sort(dat,decreasing=FALSE)[1],digits=0),",",
					format(sort(dat,decreasing=FALSE)[2],digits=0),",",
					format(sort(dat,decreasing=FALSE)[3],digits=0)
					),
				paste("3 Max Vals:",
					format(sort(dat,decreasing=TRUE)[1],digits=0),",",
					format(sort(dat,decreasing=TRUE)[2],digits=0),",",
					format(sort(dat,decreasing=TRUE)[3],digits=0)
					)
			),
		cex=1.2
	)

	qqnorm(y=dat,bty="n")
	qqline(y=dat)

	hist(dat,main=NULL)

	title(
			paste(names(louisiana)[i]),
			outer=TRUE
	)


	for(i in 5:18) {
		# i=11
		dat <- log(as.numeric(louisiana[,i]))
		dat <- dat[is.finite(dat)==TRUE]

		layout <- layout(
			matrix(c(
					1,2,3,4
				),
			nrow=2,
			ncol=2
			),
		)
		# layout.show(layout)

		par(mar=c(2,2,2,2),
			oma=c(0,0,3,0)
		)
		boxplot(dat,frame=FALSE)

		plot.new()
		legend(
			"center",
			bty = "n",
			legend = c(
					paste("Sample Size:",length(dat)),
					paste("Median:",format(median(dat,na.rm=TRUE),digits=0) ),
					paste("Mean:",format(mean(dat,na.rm=TRUE),digits=0) ),
					paste("St Dev:",format(sd(dat,na.rm=TRUE),digits=0) ),
					paste("3 Min Vals::",
						format(sort(dat,decreasing=FALSE)[1],digits=0),",",
						format(sort(dat,decreasing=FALSE)[2],digits=0),",",
						format(sort(dat,decreasing=FALSE)[3],digits=0)
						),
					paste("3 Max Vals:",
						format(sort(dat,decreasing=TRUE)[1],digits=0),",",
						format(sort(dat,decreasing=TRUE)[2],digits=0),",",
						format(sort(dat,decreasing=TRUE)[3],digits=0)
						)
				),
			cex=1.2
		)

		qqnorm(y=dat,bty="n")
		qqline(y=dat)

		hist(dat,main=NULL)

		title(
				paste(names(louisiana)[i]),
				outer=TRUE
		)
	}

dev.off()

fl_dat <- ses_metro_landings[ses_metro_landings$STATE==12,]
la_dat <- ses_metro_landings[ses_metro_landings$STATE==22,]

pdf("plots/diagnosticPlots/Landings_Density_Plots.pdf")
par(mfrow=c(2,1))
plot(density(fl_dat[,"LANDINGS_sum_2007to2011"],na.rm=T),col="navyblue",main="Tampa/St Petes Metro Landings")
abline(v=quantiles_FL[2:3],col="indianred3")
legend(
	"topright",
	legend=c(
		"low/high landings separator",
		paste0("Median: ",format(median(fl_dat[,"LANDINGS_sum_2007to2011"],na.rm = T),digits=0)),
		paste("no data n =",dim(fl_dat[fl_dat$quantile=="NotMRIP",])[1]),
		paste("low (<",round(quantiles_FL[2],1), ") n =",dim(fl_dat[fl_dat$quantile=="Low",])[1]),
		paste("moderate n =",dim(fl_dat[fl_dat$quantile=="Mod",])[1]),
		paste("high (>",round(quantiles_FL[3],1), ") n =",dim(fl_dat[fl_dat$quantile=="High",])[1])
	),
	lty=c(
		1,
		NA,
		NA,
		NA,
		NA,
		NA
	),
	col=c(
		"indianred3",
		NA,
		NA,
		NA,
		NA,
		NA	
	),
	cex=0.8,bty = "n"
)

plot(density(la_dat[,"LANDINGS_sum_2007to2011"],na.rm=T),col="navyblue",main="NOLA Metro Landings")
abline(v=quantiles_LA[2:3],col="indianred3")
legend(
	"topright",
	legend=c(
		"low/high landings separator",
		paste0("Median: ",format(median(la_dat[,"LANDINGS_sum_2007to2011"],na.rm = T),digits=0)),
		paste("no data n =",dim(la_dat[la_dat$quantile=="NotMRIP",])[1]),
		paste("low (<",round(quantiles_LA[2],1), ") n =",dim(la_dat[la_dat$quantile=="Low",])[1]),
		paste("moderate n =",dim(la_dat[la_dat$quantile=="Mod",])[1]),
		paste("high (>",round(quantiles_LA[3],1), ") n =",dim(la_dat[la_dat$quantile=="High",])[1])
	),
	lty=c(
		1,
		NA,
		NA,
		NA,
		NA,
		NA
	),
	col=c(
		"indianred3",
		NA,
		NA,
		NA,
		NA,
		NA	
	),
	cex=0.8,bty = "n"
)
dev.off()

######################################### End of script ##############################################