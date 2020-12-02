require(dplyr)

setwd(choose.dir())
health_insurance <- read.csv("original_datasets/Health_insurance.csv", header = T, sep = ",")
heart_disease <- read.csv("original_datasets/Heart_disease_mortality_rate.csv", header = T, sep = ",")
hospital_beds <- read.csv("original_datasets/Hospital_beds.csv", header = T, sep = ",")
state_codes <- read.csv("original_datasets/state_codes.csv", header = F, sep = ",")
county_pops <- read.csv("original_datasets/county_population_estimates.csv", header=T, sep = ",")
names(state_codes) <- c("code", "Full")


# Pivoting heart disease data set to match other formats
# Also dropping metadata columns from dataset to move them to independent file
# Dropped columns:
# col 1 - Year - all data is from 2017
# col 5 - DataSource - NVSS <- this is provenance
# col 6 - Class - all data is Cardiovascular Diseases
# col 7 - Topic - all data is Heart Disease Mortality 
# col 9 - Data_value_unit - all data is per 100,000 population
# col 10 - Data_value_type - data is an age-adjusted 3 year average
# col 11/12 - Data_value_footnote - data that is insufficient will be indicated with an NA
# col 13 - StratificationCategory1 - col 14 is being renamed to gender
# col 15 - StratificationCategory2 - col 16 is being renamed to Race/Ethnicity
# col 17 - TopicID - I don't even know what this is
# col 18 - LocationID - don't need it
# col 19/20 - Removing latitude and longitude data

heart_disease[,c(1,5,6,7,9,10,11,12,13,15,17,18,19,20)] <- NULL
names(heart_disease)[c(5,6)] <- c("Gender", "Race/Ethnicity")

# getting rid of non-county data
heart_disease <- heart_disease[-which(heart_disease$GeographicLevel != "County"),]
#and deleting county tag
heart_disease$GeographicLevel <- NULL
names(heart_disease) <- c("State", "County", "Heart Disease Mortality","Gender", "Race_Ethnicity")

for(i in 1:nrow(heart_disease)){
  if(grepl("Baltimore", heart_disease[i,2])){
    heart_disease[i,2] <- paste(strsplit(heart_disease[i,2], split = " City", fixed = TRUE)[[1]][1], "city")
    heart_disease[i,2] <- strsplit(heart_disease[i,2], split = " County", fixed = TRUE)[[1]][1]
  } else if(grepl("St. Louis", heart_disease[i,2]) & heart_disease[i,1] == "MO"){
    heart_disease[i,2] <- paste(strsplit(heart_disease[i,2], split = " City", fixed = TRUE)[[1]][1], "city")
    heart_disease[i,2] <- strsplit(heart_disease[i,2], split = " County", fixed = TRUE)[[1]][1]
  } else if(grepl("Fairfax", heart_disease[i,2])){
    heart_disease[i,2] <- paste(strsplit(heart_disease[i,2], split = " City", fixed = TRUE)[[1]][1], "city")
    heart_disease[i,2] <- strsplit(heart_disease[i,2], split = " County", fixed = TRUE)[[1]][1]
  } else if(grepl("Franklin", heart_disease[i,2])& heart_disease[i,1] == "VA"){
    heart_disease[i,2] <- paste(strsplit(heart_disease[i,2], split = " City", fixed = TRUE)[[1]][1], "city")
    heart_disease[i,2] <- strsplit(heart_disease[i,2], split = " County", fixed = TRUE)[[1]][1]
  } else if(grepl("Richmond", heart_disease[i,2]) & heart_disease[i,1] == "VA"){
    heart_disease[i,2] <- paste(strsplit(heart_disease[i,2], split = " City", fixed = TRUE)[[1]][1], "city")
    heart_disease[i,2] <- strsplit(heart_disease[i,2], split = " County", fixed = TRUE)[[1]][1]
  } else if(grepl("Roanoke", heart_disease[i,2]) & heart_disease[i,1] == "VA"){
    heart_disease[i,2] <- paste(strsplit(heart_disease[i,2], split = " City", fixed = TRUE)[[1]][1], "city")
    heart_disease[i,2] <- strsplit(heart_disease[i,2], split = " County", fixed = TRUE)[[1]][1]
  } else{
    heart_disease[i,2] <- strsplit(heart_disease[i,2], split = " County", fixed = TRUE)[[1]][1]
    heart_disease[i,2] <- strsplit(heart_disease[i,2], split = " Municipio", fixed = TRUE)[[1]][1]
    heart_disease[i,2] <- strsplit(heart_disease[i,2], split = " Parish", fixed = TRUE)[[1]][1]
    heart_disease[i,2] <- strsplit(heart_disease[i,2], split = " City", fixed = TRUE)[[1]][1]
  }
}

heart_disease[which(grepl(" Ana",heart_disease$County)),2] <- "Doña Ana"


heart_disease_restricted <- filter(heart_disease, Gender == "Overall" & Race_Ethnicity == "Overall")
# setting NA values to -1 so that I know which ones were insufficient data and just not there
heart_disease_restricted[is.na(heart_disease_restricted)] <- -1



# Health Insurance data set
# Dropping all columns except location data and percent of population with No Health Insurance Coverage
# Keeping:
# col 3 - County Name
# col 4 - State Name
# col 75 - Total Civilian Noninstitutionalize Population - with public coverage
# col 83 - % of population with no health insurance coverage
# col 84 - margin of error for col 83
health_insurance[,c(-3,-4,-75,-83,-84)] <- NULL
state_codes[which(state_codes[,1] == "DC"),2] <- "District of Columbia"

# converting state identifiers to the two letter code
for(i in 1:nrow(health_insurance)){
  new_code = state_codes[which(state_codes[,2] == health_insurance[i,2]),1]
  health_insurance[i,2] = new_code
}

names(health_insurance) <- c("County", "State", "Population with public coverage", "Percent of pop w/no health insurance", "margin of error")


for(i in 1:nrow(health_insurance)){
  if(grepl("Baltimore", health_insurance[i,1])){
    health_insurance[i,1] <- strsplit(health_insurance[i,1], split = " County", fixed = TRUE)[[1]]
  } else if(grepl("St. Louis", health_insurance[i,1]) & health_insurance[i,2] == "MO"){
    health_insurance[i,1] <- strsplit(health_insurance[i,1], split = " County", fixed = TRUE)[[1]]
  } else if(grepl("Fairfax", health_insurance[i,1])){
    health_insurance[i,1] <- strsplit(health_insurance[i,1], split = " County", fixed = TRUE)[[1]]
  } else if(grepl("Franklin", health_insurance[i,1])& health_insurance[i,2] == "VA"){
    health_insurance[i,1] <- strsplit(health_insurance[i,1], split = " County", fixed = TRUE)[[1]]
  } else if(grepl("Richmond", health_insurance[i,1]) & health_insurance[i,2] == "VA"){
    hospital_beds[i,4] <- strsplit(hospital_beds[i,4], split = " County", fixed = TRUE)[[1]]
    hospital_beds[i,4] <- paste(strsplit(hospital_beds[i,4], split = " City", fixed = TRUE)[[1]][1], "city")
  } else if(grepl("Roanoke", health_insurance[i,1]) & health_insurance[i,2] == "VA"){
    hospital_beds[i,4] <- strsplit(hospital_beds[i,4], split = " County", fixed = TRUE)[[1]]
    hospital_beds[i,4] <- paste(strsplit(hospital_beds[i,4], split = " City", fixed = TRUE)[[1]][1], "city")
  } else {
    health_insurance[i,1] <- strsplit(health_insurance[i,1], split = " County", fixed = TRUE)[[1]]
    health_insurance[i,1] <- strsplit(health_insurance[i,1], split = " Census", fixed = TRUE)[[1]][1]
    health_insurance[i,1] <- strsplit(health_insurance[i,1], split = " Borough", fixed = TRUE)[[1]][1]
    health_insurance[i,1] <- strsplit(health_insurance[i,1], split = " City", fixed = TRUE)[[1]][1]
    health_insurance[i,1] <- strsplit(health_insurance[i,1], split = " Municipality", fixed = TRUE)[[1]][1]
    health_insurance[i,1] <- strsplit(health_insurance[i,1], split = " Parish", fixed = TRUE)[[1]][1]
    if(health_insurance[i,2] == "VA"){
      health_insurance[i,1] <- strsplit(health_insurance[i,1], split = " city", fixed = TRUE)[[1]][1]
    }
  }
}

health_insurance[which(grepl(" Ana",health_insurance$County)),1] <- "Doña Ana"


# Hospital Beds Data Set
# Getting rid of 
# col 1/2 - don't need data coordinates
# col 3 OBJECTID - just an ID
# col 6/7 Address - don't need the hospitals address
# col 8 HQ_CITY
# col 10 HQ_ZIP
# col 12 STATE_NAME - already have state code in another column
# col 13 STATE_FIPS
# col 14 CNTY_FIPS
# col 15 FIPS
# col 19 ADULT_ICU_BEDS
# col 20 PEDI_ICU_BEDS
# col 21 BED_UTILIZATION
# col 22 POTENTIAL_INCREASE_IN_BED_CAPAC
# col 23 AVG_VENTILATION_USAGE

hospital_beds[,c(-4,-5,-9,-11,-16,-17,-18)] <- NULL
hospital_beds[is.na(hospital_beds)] <- 0

for(i in 1:nrow(hospital_beds)){
  if(grepl("Baltimore", hospital_beds[i,4])){
    hospital_beds[i,4] <- strsplit(hospital_beds[i,4], split = " County", fixed = TRUE)[[1]]
    hospital_beds[i,4] <- paste(strsplit(hospital_beds[i,4], split = " City", fixed = TRUE)[[1]][1], "city")
  } else if(grepl("St. Louis", hospital_beds[i,4]) & hospital_beds[i,3] == "MO"){
    hospital_beds[i,4] <- strsplit(hospital_beds[i,4], split = " County", fixed = TRUE)[[1]]
    hospital_beds[i,4] <- paste(strsplit(hospital_beds[i,4], split = " City", fixed = TRUE)[[1]][1], "city")
  } else if(grepl("Carson", hospital_beds[i,4])){
    hospital_beds[i,4] <- strsplit(hospital_beds[i,4], split = " City", fixed = TRUE)[[1]][1]
  } else if(grepl("Fairfax", hospital_beds[i,4]) & hospital_beds[i,3] == "VA"){
    hospital_beds[i,4] <- strsplit(hospital_beds[i,4], split = " County", fixed = TRUE)[[1]]
    hospital_beds[i,4] <- paste(strsplit(hospital_beds[i,4], split = " City", fixed = TRUE)[[1]][1], "city")
  } else if(grepl("Franklin", hospital_beds[i,4])& hospital_beds[i,3] == "VA"){
    hospital_beds[i,4] <- strsplit(hospital_beds[i,4], split = " County", fixed = TRUE)[[1]]
    hospital_beds[i,4] <- paste(strsplit(hospital_beds[i,4], split = " City", fixed = TRUE)[[1]][1], "city")
  } else if(grepl("James", hospital_beds[i,4]) & hospital_beds[i,3] == "VA"){
    hospital_beds[i,4] <- strsplit(hospital_beds[i,4], split = " City", fixed = TRUE)[[1]][1]
  } else if(grepl("Richmond", hospital_beds[i,4]) & hospital_beds[i,3] == "VA"){
    hospital_beds[i,4] <- strsplit(hospital_beds[i,4], split = " County", fixed = TRUE)[[1]]
    hospital_beds[i,4] <- paste(strsplit(hospital_beds[i,4], split = " City", fixed = TRUE)[[1]][1], "city")
  } else if(grepl("Roanoke", hospital_beds[i,4]) & hospital_beds[i,3] == "VA"){
    hospital_beds[i,4] <- strsplit(hospital_beds[i,4], split = " County", fixed = TRUE)[[1]]
    hospital_beds[i,4] <- paste(strsplit(hospital_beds[i,4], split = " City", fixed = TRUE)[[1]][1], "city")
  } else {
    hospital_beds[i,4] <- strsplit(hospital_beds[i,4], split = " County", fixed = TRUE)[[1]]
    hospital_beds[i,4] <- strsplit(hospital_beds[i,4], split = " city", fixed = TRUE)[[1]][1]
  }
}
hospital_beds[which(grepl(" Ana",hospital_beds$COUNTY_NAME)),4] <- "Doña Ana"

# creating sums of hospital beds by county
hospital_beds_aggregate <- aggregate(hospital_beds[,c(5,6,7)], by = list(State = hospital_beds$HQ_STATE, County = hospital_beds$COUNTY_NAME), FUN = sum)



# Using a county population dataset so that way hospital beds can be regularized per 100,000 population
# dropping all columns except state, county, and 2018 population estimate
county_pops[,c(-6,-7,-18)] <- NULL
county_pops <- county_pops[-which(county_pops$STNAME == county_pops$CTYNAME),] # removing entire state data

#removing "county" from each instance of a county
for(i in 1:nrow(county_pops)){
  if(grepl("Baltimore", county_pops[i,2])){
    county_pops[i,2] <- strsplit(county_pops[i,2], split = " County", fixed = TRUE)[[1]][1]
  } else if(grepl("St. Louis", county_pops[i,2]) & county_pops[i,1] == "MO"){
    county_pops[i,2] <- strsplit(county_pops[i,2], split = " County", fixed = TRUE)[[1]][1]
  } else if(grepl("Fairfax", county_pops[i,2])){
    county_pops[i,2] <- strsplit(county_pops[i,2], split = " County", fixed = TRUE)[[1]][1]
  } else if(grepl("Franklin", county_pops[i,2]) & county_pops[i,1] == "VA"){
    if(county_pops[i,3] == 8013){
      county_pops[i,2] = "Franklin city"
    } else if(county_pops[i,3] == 56195){
      county_pops[i,2] = "Franklin"
    }
  } else if(grepl("Richmond", county_pops[i,2]) & county_pops[i,1] == "VA"){
    if(county_pops[i,3] == 228783){
      county_pops[i,2] = "Richmond city"
    } else if(county_pops[i,3] == 9038){
      county_pops[i,2] = "Richmond"
    }
  } else if(grepl("Roanoke", county_pops[i,2]) & county_pops[i,1] == "VA"){
    
    if(county_pops[i,3] == 99920){
      county_pops[i,2] = "Roanoke city"
    } else if(county_pops[i,3] == 94073){
      county_pops[i,2] = "Roanoke"
    }
  } else {
    county_pops[i,2] <- strsplit(county_pops[i,2], split = " County", fixed = TRUE)[[1]][1]
    county_pops[i,2] <- strsplit(county_pops[i,2], split = " Census", fixed = TRUE)[[1]][1]
    county_pops[i,2] <- strsplit(county_pops[i,2], split = " Borough", fixed = TRUE)[[1]][1]
    county_pops[i,2] <- strsplit(county_pops[i,2], split = " city", fixed = TRUE)[[1]][1]
    county_pops[i,2] <- strsplit(county_pops[i,2], split = " City", fixed = TRUE)[[1]][1]
    county_pops[i,2] <- strsplit(county_pops[i,2], split = " Municipality", fixed = TRUE)[[1]][1]
    county_pops[i,2] <- strsplit(county_pops[i,2], split = " Parish", fixed = TRUE)[[1]][1]
  }
}

for(i in 1:nrow(county_pops)){
  new_code = state_codes[which(state_codes[,2] == county_pops[i,1]),1]
  county_pops[i,1] = new_code
}
names(county_pops) <- c("State", "County", "PopEstimate2018")



# merging data set
final_dataset <- merge(hospital_beds_aggregate,health_insurance, all = TRUE)
final_dataset <- merge(final_dataset, county_pops, all = TRUE)
final_dataset <- merge(final_dataset, heart_disease_restricted, all = TRUE)
final_dataset$NUM_STAFFED_BEDS <- NULL
final_dataset$NUM_ICU_BEDS <- NULL
final_dataset$Gender <- NULL # only considering overall for both gender and race/ethnicity
final_dataset$Race_Ethnicity <- NULL


# by hand cleaning
final_dataset <- final_dataset[-which(final_dataset$State == "AS"),] # removing American Samoa
final_dataset <- final_dataset[-which(final_dataset$State == "MP"),] # removing Mariana
final_dataset <- final_dataset[-which(final_dataset$State == "PR"),] # removing Puerto Rico
final_dataset <- final_dataset[-which(final_dataset$State == "GU"),] # removing Guam
final_dataset <- final_dataset[-which(final_dataset$State == "VI"),] # removing Virgin Islands


# adjusted beds is # of beds per 100000 people
final_dataset$adjusted_beds <- 100000 * (final_dataset$NUM_LICENSED_BEDS / final_dataset$PopEstimate2018)
model1 <- lm(`Heart Disease Mortality` ~ `Percent of pop w/no health insurance`, final_dataset)
plot(`Heart Disease Mortality` ~ `Percent of pop w/no health insurance`, data = final_dataset)
abline(model1)

model2 <- lm(`Heart Disease Mortality` ~ `adjusted_beds`, data = final_dataset)
plot(`Heart Disease Mortality` ~ `adjusted_beds`, data = final_dataset)
abline(model2)


final_dataset$inquiry1 <- final_dataset$`Heart Disease Mortality` / final_dataset$adjusted_beds
final_dataset$inquiry2 <- final_dataset$`Heart Disease Mortality` / final_dataset$`Percent of pop w/no health insurance`
write.csv(final_dataset,"final_analysis.csv", row.names = FALSE)
