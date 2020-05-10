library(openxlsx)
library(stringr)
P_values_v4 = read.xlsx("Missing_Master Nov9.xlsx")

names(P_values_v4)[4] = "createdOn"
names(P_values_v4)[6] = "Patient ID"
names(P_values_v4)[6] = "Patient name"
names(P_values_v4)[7] = "Gender"
names(P_values_v4)[8] = "Age (in Months)"
names(P_values_v4)[9] = "Weight (Kg)"
names(P_values_v4)[10] = "Temperature (Degrees)"
names(P_values_v4)[11] = "Development"
names(P_values_v4)[12] = "Airway"
names(P_values_v4)[14] = "Breathing"
names(P_values_v4)[14] = "Circulation"
names(P_values_v4)[15] = "Disability"
names(P_values_v4)[16] = "Blood Glucose mg/dL"
names(P_values_v4)[17] = "WBC (x10^4 /microL)"
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

write.csv(P_values_v4 , "Missing_Master Nov9.csv", row.names=FALSE)

P = read.csv("Missing_Master Nov9.csv")

hosp = as.factor(P$Hospital.Name)
hlevel = levels(hosp)

h1 = subset(P, P$Hospital.Name == hlevel[1])
h1n = as.character(hlevel[1])
tmp = cbind(h1n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h1, tmp , row.names=FALSE)

h2 = subset(P, P$Hospital.Name == hlevel[2])
h2n = as.character(hlevel[2])
tmp = cbind(h2n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h2, tmp , row.names=FALSE)

h3 = subset(P, P$Hospital.Name == hlevel[3])
h3n = as.character(hlevel[3])
tmp = cbind(h3n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h3, tmp , row.names=FALSE)

h4 = subset(P, P$Hospital.Name == hlevel[4])
h4n = as.character(hlevel[4])
tmp = cbind(h4n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h4, tmp , row.names=FALSE)

h5 = subset(P, P$Hospital.Name == hlevel[5])
h5n = as.character(hlevel[5])
tmp = cbind(h5n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h5, tmp , row.names=FALSE)

h6 = subset(P, P$Hospital.Name == hlevel[6])
h6n = as.character(hlevel[6])
tmp = cbind(h6n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h6, tmp , row.names=FALSE)


h7 = subset(P, P$Hospital.Name == hlevel[7])
h7n = as.character(hlevel[7])
tmp = cbind(h7n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h7, tmp , row.names=FALSE)

h8 = subset(P, P$Hospital.Name == hlevel[8])
h8n = as.character(hlevel[8])
tmp = cbind(h8n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h8, tmp , row.names=FALSE)


h9 = subset(P, P$Hospital.Name == hlevel[9])
h9n = as.character(hlevel[9])
tmp = cbind(h9n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h9, tmp , row.names=FALSE)


h10 = subset(P, P$Hospital.Name == hlevel[10])
h10n = as.character(hlevel[10])
tmp = cbind(h10n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h10, tmp , row.names=FALSE)


h11 = subset(P, P$Hospital.Name == hlevel[11])
h11n = as.character(hlevel[11])
tmp = cbind(h11n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h11, tmp , row.names=FALSE)


h12 = subset(P, P$Hospital.Name == hlevel[12])
h12n = as.character(hlevel[12])
tmp = cbind(h12n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h12, tmp , row.names=FALSE)


h13 = subset(P, P$Hospital.Name == hlevel[13])
h13n = as.character(hlevel[13])
tmp = cbind(h13n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h13, tmp , row.names=FALSE)


h14 = subset(P, P$Hospital.Name == hlevel[14])
h14n = as.character(hlevel[14])
tmp = cbind(h14n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h14, tmp , row.names=FALSE)


h15 = subset(P, P$Hospital.Name == hlevel[15])
h15n = as.character(hlevel[15])
tmp = cbind(h15n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h6, tmp , row.names=FALSE)


h16 = subset(P, P$Hospital.Name == hlevel[16])
h16n = as.character(hlevel[16])
tmp = cbind(h16n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h16, tmp , row.names=FALSE)

h17 = subset(P, P$Hospital.Name == hlevel[17])
h17n = as.character(hlevel[17])
tmp = cbind(h17n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h17, tmp , row.names=FALSE)

h18 = subset(P, P$Hospital.Name == hlevel[18])
h18n = as.character(hlevel[18])
tmp = cbind(h18n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h18, tmp , row.names=FALSE)

h21 = subset(P, P$Hospital.Name == hlevel[21])
h21n = as.character(hlevel[21])
tmp = cbind(h21n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h21, tmp , row.names=FALSE)

h19 = subset(P, P$Hospital.Name == hlevel[19])
h19n = as.character(hlevel[19])
tmp = cbind(h19n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h19, tmp , row.names=FALSE)

h20 = subset(P, P$Hospital.Name == hlevel[20])
h20n = as.character(hlevel[20])
tmp = cbind(h20n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h20, tmp , row.names=FALSE)

h22 = subset(P, P$Hospital.Name == hlevel[22])
h22n = as.character(hlevel[22])
tmp = cbind(h22n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h22, tmp , row.names=FALSE)

h23 = subset(P, P$Hospital.Name == hlevel[23])
h23n = as.character(hlevel[23])
tmp = cbind(h23n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h23, tmp , row.names=FALSE)

h24 = subset(P, P$Hospital.Name == hlevel[24])
h24n = as.character(hlevel[24])
tmp = cbind(h24n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h24, tmp , row.names=FALSE)

h25 = subset(P, P$Hospital.Name == hlevel[25])
h25n = as.character(hlevel[25])
tmp = cbind(h25n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h25, tmp , row.names=FALSE)

h26 = subset(P, P$Hospital.Name == hlevel[26])
h26n = as.character(hlevel[26])
tmp = cbind(h26n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h26, tmp , row.names=FALSE)

h27 = subset(P, P$Hospital.Name == hlevel[27])
h27n = as.character(hlevel[27])
tmp = cbind(h27n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h27, tmp , row.names=FALSE)

h28 = subset(P, P$Hospital.Name == hlevel[28])
h28n = as.character(hlevel[28])
tmp = cbind(h28n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h28, tmp , row.names=FALSE)

h29 = subset(P, P$Hospital.Name == hlevel[29])
h29n = as.character(hlevel[29])
tmp = cbind(h29n , "xlsx")
tmp = str_c(tmp, collapse = ".")
write.xlsx(h29, tmp , row.names=FALSE)

h16n
