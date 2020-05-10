library(dplyr)


################################# Version Separation #######################################################

P_values = read.csv("New data/Raw/prem_backup_8Nov2019.csv")
P_labels = read.csv("New data/Raw/prem_entries_lables.csv")


#Extracting Version 2
check_labels_v2 = as.factor(P_labels$data..51..label == "Reason For Referal")
P_values_v2 = subset(P_values, check_labels_v2==TRUE)

#Extracting Version 3 & 4
check_labels_v3_v4 = as.factor(P_labels$data..53..label == "Reason For Referal")
P_values_v3_v4 = subset(P_values, check_labels_v3_v4 ==TRUE)

#Extracting Version 4
check_label_v4 = as.factor(P_labels_v3_v4$data..57..label == "Cause Of Death")
P_values_v4 = subset(P_values_v3_v4, check_label_v4 == TRUE)


#Extracting Version 3
P_values_v3 = subset(P_values_v3_v4, check_label_v4 == FALSE)

#Extracting Version 1
library(dplyr)
v = setdiff(P_values,P_values_v2)
P_values_v1 = setdiff(v,P_values_v3_v4)

# Different Versions of Data are : 
# P_values_v1
# P_values_v2
# P_values_v3
# P_values_v4

#Replacing coloumns for V1
names(P_values_v1)[5] = "Patient ID"
names(P_values_v1)[6] = "Patient name"
names(P_values_v1)[7] = "Gender"
names(P_values_v1)[8] = "Age (in Months)"
names(P_values_v1)[9] = "Weight (Kg)"
names(P_values_v1)[10] = "Temperature (Degrees)"
names(P_values_v1)[11] = "Development"
names(P_values_v1)[12] = "Airway"
names(P_values_v1)[13] = "Breathing"
names(P_values_v1)[14] = "Circulation"
names(P_values_v1)[15] = "Disability"
names(P_values_v1)[16] = "Blood Glucose mg/dL"
names(P_values_v1)[17] = "WBC (x10^3 /microL)"
names(P_values_v1)[18] = "RBC (x10^6 /microL)"
names(P_values_v1)[19] = "Hematocrit Count (%)"
names(P_values_v1)[20] = "Platelet Count (x10^3 /microL)"
names(P_values_v1)[21] = "Haemoglobin Count (g/dL)"
names(P_values_v1)[22] = "Etiology"
names(P_values_v1)[23] = "Cardiac"
names(P_values_v1)[24] = "CNS"
names(P_values_v1)[25] = "RS"
names(P_values_v1)[26] = "Renal Disease"
names(P_values_v1)[27] = "Liver Disease"
names(P_values_v1)[28] = "Blood"
names(P_values_v1)[29] = "Metabolic"
names(P_values_v1)[30] = "Post Surgical"
names(P_values_v1)[31] = "Bottle Fed"
names(P_values_v1)[32] = "Poor child rearing practices"
names(P_values_v1)[33] = "PLHA"
names(P_values_v1)[34] = "Medications"
names(P_values_v1)[35] = "Head Tilt Chin Lift"
names(P_values_v1)[36] = "Bag-valve Mask Ventilation"
names(P_values_v1)[37] = "CPAP"
names(P_values_v1)[38] = "Intubation"
names(P_values_v1)[39] = "Bronchodialators"
names(P_values_v1)[40] = "Cardiac Massage"
names(P_values_v1)[41] = "Normal Saline Bolus"
names(P_values_v1)[42] = "Inotrope"
names(P_values_v1)[43] = "25% Dextrose"
names(P_values_v1)[44] = "CSstabilization"
names(P_values_v1)[45] = "Anti-Fit Medication"
names(P_values_v1)[46] = "Stomach Wash"
names(P_values_v1)[47] = "De-Contamination"
names(P_values_v1)[48] = "Thoracocentesis"
names(P_values_v1)[49] = "Blood Products"
names(P_values_v1)[50] = "Antibiotic"
names(P_values_v1)[51] = "Prazosin"
names(P_values_v1)[52] = "Anti Snake Venom"
names(P_values_v1)[53] = "Antidote for Poisons"
names(P_values_v1)[54] = "Outcome"


##Replacing coloumns for V2
names(P_values_v2)[5] = "Patient ID"
names(P_values_v2)[6] = "Patient name"
names(P_values_v2)[7] = "Gender"
names(P_values_v2)[8] = "Age (in Months)"
names(P_values_v2)[9] = "Weight (Kg)"
names(P_values_v2)[10] = "Temperature (Degrees)"
names(P_values_v2)[11] = "Development"
names(P_values_v2)[12] = "Airway"
names(P_values_v2)[13] = "Breathing"
names(P_values_v2)[14] = "Circulation"
names(P_values_v2)[15] = "Disability"
names(P_values_v2)[16] = "Blood Glucose mg/dL"
names(P_values_v2)[17] = "WBC (x10^3 /microL)"
names(P_values_v2)[18] = "RBC (x10^6 /microL)"
names(P_values_v2)[19] = "Hematocrit Count (%)"
names(P_values_v2)[20] = "Platelet Count (x10^3 /microL)"
names(P_values_v2)[21] = "Haemoglobin Count (g/dL)"
names(P_values_v2)[22] = "Etiology"
names(P_values_v2)[23] = "Cardiac"
names(P_values_v2)[24] = "CNS"
names(P_values_v2)[25] = "RS"
names(P_values_v2)[26] = "Renal Disease"
names(P_values_v2)[27] = "Liver Disease"
names(P_values_v2)[28] = "Blood"
names(P_values_v2)[29] = "Metabolic"
names(P_values_v2)[30] = "Post Surgical"
names(P_values_v2)[31] = "Bottle Fed"
names(P_values_v2)[32] = "Poor child rearing practices"
names(P_values_v2)[33] = "PLHA"
names(P_values_v2)[34] = "Medications"
names(P_values_v2)[35] = "Head Tilt Chin Lift"
names(P_values_v2)[36] = "Bag-valve Mask Ventilation"
names(P_values_v2)[37] = "CPAP"
names(P_values_v2)[38] = "Intubation"
names(P_values_v2)[39] = "Bronchodialators"
names(P_values_v2)[40] = "Cardiac Massage"
names(P_values_v2)[41] = "Normal Saline Bolus"
names(P_values_v2)[42] = "Inotrope"
names(P_values_v2)[43] = "25% Dextrose"
names(P_values_v2)[44] = "CSstabilization"
names(P_values_v2)[45] = "Anti-Fit Medication"
names(P_values_v2)[46] = "Stomach Wash"
names(P_values_v2)[47] = "De-Contamination"
names(P_values_v2)[48] = "Thoracocentesis"
names(P_values_v2)[49] = "Blood Products"
names(P_values_v2)[50] = "Antibiotic"
names(P_values_v2)[51] = "Prazosin"
names(P_values_v2)[52] = "Anti Snake Venom"
names(P_values_v2)[53] = "Antidote for Poisons"
names(P_values_v2)[54] = "Outcome"
names(P_values_v2)[55] = "Referred Out Institute Name"
names(P_values_v2)[56] = "Reason For Referal"

##Replacing coloumns for V3
names(P_values_v3)[5] = "Patient ID"
names(P_values_v3)[6] = "Patient name"
names(P_values_v3)[7] = "Gender"
names(P_values_v3)[8] = "Age (in Months)"
names(P_values_v3)[9] = "Weight (Kg)"
names(P_values_v3)[10] = "Temperature (Degrees)"
names(P_values_v3)[11] = "Development"
names(P_values_v3)[12] = "Airway"
names(P_values_v3)[13] = "Breathing"
names(P_values_v3)[14] = "Circulation"
names(P_values_v3)[15] = "Disability"
names(P_values_v3)[16] = "Blood Glucose mg/dL"
names(P_values_v3)[17] = "WBC (x10^3 /microL)"
names(P_values_v3)[18] = "RBC (x10^6 /microL)"
names(P_values_v3)[19] = "Hematocrit Count (%)"
names(P_values_v3)[20] = "Platelet Count (x10^3 /microL)"
names(P_values_v3)[21] = "Haemoglobin Count (g/dL)"
names(P_values_v3)[22] = "Etiology"
names(P_values_v3)[23] = "Cardiac"
names(P_values_v3)[24] = "CNS"
names(P_values_v3)[25] = "RS"
names(P_values_v3)[26] = "Renal Disease"
names(P_values_v3)[27] = "Liver Disease"
names(P_values_v3)[28] = "Blood"
names(P_values_v3)[29] = "Metabolic"
names(P_values_v3)[30] = "Post Surgical"
names(P_values_v3)[31] = "Bottle Fed"
names(P_values_v3)[32] = "Poor child rearing practices"
names(P_values_v3)[33] = "PLHA"
names(P_values_v3)[34] = "Medications"
names(P_values_v3)[35] = "Home available solutions"
names(P_values_v3)[36] = "Oral Rehydration Solution (ORS)"
names(P_values_v3)[37] = "Head Tilt Chin Lift"
names(P_values_v3)[38] = "Bag-valve Mask Ventilation"
names(P_values_v3)[39] = "CPAP"
names(P_values_v3)[40] = "Intubation"
names(P_values_v3)[41] = "Bronchodialators"
names(P_values_v3)[42] = "Cardiac Massage"
names(P_values_v3)[43] = "Normal Saline Bolus"
names(P_values_v3)[44] = "Inotrope"
names(P_values_v3)[45] = "25% Dextrose"
names(P_values_v3)[46] = "CSstabilization"
names(P_values_v3)[47] = "Anti-Fit Medication"
names(P_values_v3)[48] = "Stomach Wash"
names(P_values_v3)[49] = "De-Contamination"
names(P_values_v3)[50] = "Thoracocentesis"
names(P_values_v3)[51] = "Blood Products"
names(P_values_v3)[52] = "Antibiotic"
names(P_values_v3)[53] = "Prazosin"
names(P_values_v3)[54] = "Anti Snake Venom"
names(P_values_v3)[55] = "Antidote for Poisons"
names(P_values_v3)[56] = "Outcome"
names(P_values_v3)[57] = "Referred Out Institute Name"
names(P_values_v3)[58] = "Reason For Referal"

##Replacing coloumns for V4
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


#Removing entries from IITM from Version 1 
v1_iitm = subset(P_entries_v1 , P_entries_v1$owner == "5b30e12f8296ab4dd2994ac6")
v1_iitm = setdiff(P_entries_v1, v1_iitm)
v1_iitm2 = subset(v1_iitm , v1_iitm$owner == "5b74263fa930b40257a82d5a")
v1_iitm2 = setdiff(v1_iitm, v1_iitm2)

write.csv(v1_iitm2, "1. Version Separated/Version1.csv" , row.names = FALSE)

#Removing entries from IITM from Version 2
v2_iitm = subset(P_entries_v2 , P_entries_v2$owner == "5b30e12f8296ab4dd2994ac6")
v2_iitm = setdiff(P_entries_v2, v2_iitm)
v2_iitm2 = subset(v2_iitm , v2_iitm$owner == "5b74263fa930b40257a82d5a")
v2_iitm2 = setdiff(v2_iitm, v2_iitm2)

write.csv(v2_iitm2, "Data/1. Version Separated/Version2.csv", row.names = FALSE)


#Removing entries from IITM from Version 3
v3_iitm = subset(P_entries_v3 , P_entries_v3$owner == "5b30e12f8296ab4dd2994ac6")
v3_iitm = setdiff(P_entries_v3, v3_iitm)
v3_iitm2 = subset(v3_iitm , v3_iitm$owner == "5b74263fa930b40257a82d5a")
v3_iitm2 = setdiff(v3_iitm, v3_iitm2)

write.csv(v3_iitm2, "1. Version Separated/Version3.csv", row.names = FALSE)

#Removing entries from IITM from Version 4
v4_iitm = subset(P_entries_v4 , P_entries_v4$owner == "5b30e12f8296ab4dd2994ac6")
v4_iitm = setdiff(P_entries_v4, v4_iitm)
v4_iitm2 = subset(v4_iitm , v4_iitm$owner == "5b74263fa930b40257a82d5a")
v4_iitm2 = setdiff(v4_iitm, v4_iitm2)

write.csv(v4_iitm2, "Data/1. Version Separated/Version4.csv", row.names = FALSE)


#Substituting NA for the missing values
v2_iitm2 = read.csv("Data/1. Version Separated/Version2.csv", header=T, na.strings=c("","[]","NA"))
v4_iitm2 = read.csv("Data/1. Version Separated/Version4.csv", header=T, na.strings=c("","[]","NA"))

#Adding Hospital names

v2_iitm2$Hospital.Name = hospital_details$hospName[match(v2_iitm2$owner,hospital_details$X_id)]
v2_iitm2$District = hospital_details$district[match(v2_iitm2$owner,hospital_details$X_id)]
v2_iitm2$Type = hospital_details$type[match(v2_iitm2$owner,hospital_details$X_id)]

write.csv(v2_iitm2,"Data/1. Version Separated/Added hospital values & NA/Version2.csv", row.names = FALSE)

hospital_details = read.csv("Raw/hospitals.csv")

v4_iitm2$Hospital.Name = hospital_details$hospName[match(v4_iitm2$owner,hospital_details$X_id)]
v4_iitm2$District = hospital_details$district[match(v4_iitm2$owner,hospital_details$X_id)]
v4_iitm2$Type = hospital_details$type[match(v4_iitm2$owner,hospital_details$X_id)]

write.csv(v4_iitm2,"Data/1. Version Separated/Added hospital values & NA/Version4.csv", row.names = FALSE)
















data = read.csv("Data/1. Version Separated/Added hospital values & NA/Version2.csv")
data$X = NULL
data$date = NULL
#data[58:64] = NULL

#Finding the Missing ABCD values
ABCD_na = subset(data , is.na(data$Airway) == "TRUE" | is.na(data$Breathing) == "TRUE" | is.na(data$Circulation) == "TRUE" | is.na(data$Disability) == "TRUE")
#ABCD_na$X = NULL
#ABCD_na$data..58..value = NULL
write.csv(ABCD_na,"Data/Missing/Missing ABCD_v2.csv", row.names = FALSE)

#Finding the Missing Etiology
data = setdiff(data, ABCD_na)
et_na =subset(data, is.na(data$Etiology) == "TRUE")
#et_na$X = NULL
write.csv(et_na,"Data/Missing/Missing Etiology_v2.csv", row.names = FALSE)

#Finding the Missing Treatment
data = setdiff(data, et_na)

onlyNArows_idx <- data[23:55] %>%
  is.na() %>%
  apply(MARGIN = 1, FUN = all)


treat_na = data[onlyNArows_idx,]
#treat_na$X =NULL
write.csv(treat_na,"Data/Missing/Missing Treatment_v2.csv", row.names = FALSE)


#Finding the Missing Outcome
data = setdiff(data, treat_na)

outcome_na = subset(data, is.na(data$Outcome) == "TRUE" )
#outcome_na$X = NULL
write.csv(outcome_na,"Data/Missing/Missing Outcome_v2.csv", row.names = FALSE)




