library(dplyr)


################################# Version Separation #######################################################

P_values_v4 = read.csv("New data/Raw/prem_backup_8Nov2019.csv")
P_labels = read.csv("New data/Raw/prem_entries_lables.csv")




# P_values_v4

##Replacing coloumns for V4
names(P_values_v4)[4] = "createdOn"
names(P_values_v4)[5] = "Patient ID"
names(P_values_v4)[6] = "Patient name"
names(P_values_v4)[7] = "Gender"
names(P_values_v4)[8] = "Age (in Months)"
names(P_values_v4)[9] = "Weight (Kg)"
names(P_values_v4)[10] = "Temperature (Degrees)"
names(P_values_v4)[11] = "Development"
names(P_values_v4)[12] = "Airway"
names(P_values_v4)[13] = "Breathing"
names(P_values_v4)[14] = "Circulation"
names(P_values_v4)[15] = "Disability"
names(P_values_v4)[16] = "Blood Glucose mg/dL"
names(P_values_v4)[17] = "WBC (x10^3 /microL)"
names(P_values_v4)[18] = "RBC (x10^6 /microL)"
names(P_values_v4)[19] = "Hematocrit Count (%)"
names(P_values_v4)[20] = "Platelet Count (x10^3 /microL)"
names(P_values_v4)[21] = "Haemoglobin Count (g/dL)"
names(P_values_v4)[22] = "Etiology"
names(P_values_v4)[23] = "Cardiac"
names(P_values_v4)[24] = "CNS"
names(P_values_v4)[25] = "RS"
names(P_values_v4)[26] = "Renal Disease"
names(P_values_v4)[27] = "Liver Disease"
names(P_values_v4)[28] = "Blood"
names(P_values_v4)[29] = "Metabolic"
names(P_values_v4)[30] = "Post Surgical"
names(P_values_v4)[31] = "Bottle Fed"
names(P_values_v4)[32] = "Poor child rearing practices"
names(P_values_v4)[33] = "PLHA"
names(P_values_v4)[34] = "Medications"
names(P_values_v4)[35] = "Home available solutions"
names(P_values_v4)[36] = "Oral Rehydration Solution (ORS)"
names(P_values_v4)[37] = "Head Tilt Chin Lift"
names(P_values_v4)[38] = "Bag-valve Mask Ventilation"
names(P_values_v4)[39] = "CPAP"
names(P_values_v4)[40] = "Intubation"
names(P_values_v4)[41] = "Bronchodialators"
names(P_values_v4)[42] = "Cardiac Massage"
names(P_values_v4)[43] = "Normal Saline Bolus"
names(P_values_v4)[44] = "Inotrope"
names(P_values_v4)[45] = "25% Dextrose"
names(P_values_v4)[46] = "CSstabilization"
names(P_values_v4)[47] = "Anti-Fit Medication"
names(P_values_v4)[48] = "Stomach Wash"
names(P_values_v4)[49] = "De-Contamination"
names(P_values_v4)[50] = "Thoracocentesis"
names(P_values_v4)[51] = "Blood Products"
names(P_values_v4)[52] = "Antibiotic"
names(P_values_v4)[53] = "Prazosin"
names(P_values_v4)[54] = "Anti Snake Venom"
names(P_values_v4)[55] = "Antidote for Poisons"
names(P_values_v4)[56] = "Outcome"
names(P_values_v4)[57] = "Referred Out Institute Name"
names(P_values_v4)[58] = "Reason For Referal"
names(P_values_v4)[59] = "Time of Arrival"
names(P_values_v4)[60] = "Time of first treatment"
names(P_values_v4)[61] = "Time of transfer"
names(P_values_v4)[62] = "Cause Of Death"


P_entries_v4 = P_values_v4

#Removing entries from IITM from Version 4
v4_iitm = subset(P_entries_v4 , P_entries_v4$owner == "5b30e12f8296ab4dd2994ac6")
v4_iitm = setdiff(P_entries_v4, v4_iitm)
v4_iitm2 = subset(v4_iitm , v4_iitm$owner == "5b74263fa930b40257a82d5a")
v4_iitm2 = setdiff(v4_iitm, v4_iitm2)

write.csv(v4_iitm2, "New data/1. Version Separated/Version4.csv", row.names = FALSE)


#Substituting NA for the missing values
v4_iitm2 = read.csv("New data/1. Version Separated/Version4.csv", header=T, na.strings=c("","[]","NA"))

#Adding Hospital names

hospital_details = read.csv("Data/hospitals.csv")

v4_iitm2$Hospital.Name = hospital_details$hospName[match(v4_iitm2$owner,hospital_details$X_id)]
v4_iitm2$District = hospital_details$district[match(v4_iitm2$owner,hospital_details$X_id)]
v4_iitm2$Type = hospital_details$type[match(v4_iitm2$owner,hospital_details$X_id)]

write.csv(v4_iitm2,"New data/1. Version Separated/Added hospital values & NA/Version4.csv", row.names = FALSE)
















data = read.csv("New data/1. Version Separated/Added hospital values & NA/Version4.csv")
data$X = NULL
data$date = NULL
#data[58:64] = NULL

#Finding the Missing ABCD values
ABCD_na = subset(data , is.na(data$Airway) == "TRUE" | is.na(data$Breathing) == "TRUE" | is.na(data$Circulation) == "TRUE" | is.na(data$Disability) == "TRUE")
#ABCD_na$X = NULL
#ABCD_na$data..58..value = NULL
write.csv(ABCD_na,"New data/Missing/Missing ABCD_v4.csv", row.names = FALSE)

#Finding the Missing Etiology
data = setdiff(data, ABCD_na)
et_na =subset(data, is.na(data$Etiology) == "TRUE")
#et_na$X = NULL
write.csv(et_na,"New data/Missing/Missing Etiology_v4.csv", row.names = FALSE)

#Finding the Missing Treatment
data = setdiff(data, et_na)

onlyNArows_idx <- data[23:55] %>%
  is.na() %>%
  apply(MARGIN = 1, FUN = all)


treat_na = data[onlyNArows_idx,]
#treat_na$X =NULL
write.csv(treat_na,"New data/Missing/Missing Treatment_v4.csv", row.names = FALSE)


#Finding the Missing Outcome
data = setdiff(data, treat_na)

outcome_na = subset(data, is.na(data$Outcome) == "TRUE" )
#outcome_na$X = NULL
write.csv(outcome_na,"New data/Missing/Missing Outcome_v4.csv", row.names = FALSE)




