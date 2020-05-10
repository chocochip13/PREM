#Percentage of empty ABCD entries from each district
MissingABCD_percent_v2_Dist = table(P_v2_ABCD_na$District)/table(P_v2$District)*100
MissingABCD_percent_v4_Dist = table(P_v4_ABCD_na$District)/table(P_v4$District)*100


#Percentage of empty Etiology entries from each district
MissingEtiology_percent_v2_Dist = table(P_v2_etiology_na$District)/table(P_v2$District)*100
MissingEtiology_percent_v4_Dist = table(P_v4_etiology_na$District)/table(P_v4$District)*100

#Percentage of empty Outcome entries from each district
MissingOutcome_percent_v2_Dist = table(P_v2_outcome_na$District)/table(P_v2$District)*100
MissingOutcome_percent_v4_Dist = table(P_v4_outcome_na$District)/table(P_v4$District)*100


#Percentage of empty ABCD entries from each Hospital
MissingABCD_percent_v2_Hosp = table(P_v2_ABCD_na$Hospital.Name)/table(P_v2$Hospital.Name)*100
MissingABCD_percent_v4_Hosp = table(P_v4_ABCD_na$Hospital.Name)/table(P_v4$Hospital.Name)*100

#Percentage of empty Etiology entries from each Hospital
MissingEtiology_percent_v2_Hosp = table(P_v2_etiology_na$Hospital.Name)/table(P_v2$Hospital.Name)*100
MissingEtiology_percent_v4_Hosp = table(P_v4_etiology_na$Hospital.Name)/table(P_v4$Hospital.Name)*100

#Percentage of empty Outcome entries from each district
MissingOutcome_percent_v2_Hosp = table(P_v2_outcome_na$Hospital.Name)/table(P_v2$Hospital.Name)*100
MissingOutcome_percent_v4_Hosp = table(P_v4_outcome_na$Hoapital.Name)/table(P_v4$Hospital.Name)*100





############################################Hospitals & District list##################################################
#Missing ABCD files v2
a = table(P_v2_ABCD_na$Hospital.Name)
write.csv(a , file = "crap/a.csv" , sep = ",")
a = read.csv("crap/a.csv")
a$District = hospital_details$district[match(a$Var1,hospital_details$hospName)]
b = table(P_v2_ABCD_na$Hospital.Name)/table(P_v2$Hospital.Name)*100
write.csv(b , file = "crap/b.csv" , sep = ",")
b = read.csv("crap/b.csv")
a$Missing.Percent = b$Freq[match(a$Var1,b$Var1)]

write.csv(a , file = "Missing Entries Data/Analysis/Missing ABCD Percent v2.csv" , row.names = FALSE)

#Missing ABCD files v4
a = table(P_v4_ABCD_na$Hospital.Name)
write.csv(a , file = "crap/a.csv" , sep = ",")
a = read.csv("crap/a.csv")
a$District = hospital_details$district[match(a$Var1,hospital_details$hospName)]
b = table(P_v4_ABCD_na$Hospital.Name)/table(P_v4$Hospital.Name)*100
write.csv(b , file = "crap/b.csv" , sep = ",")
b = read.csv("crap/b.csv")
a$Missing.Percent = b$Freq[match(a$Var1,b$Var1)]

write.csv(a , file = "Missing Entries Data/Analysis/Missing ABCD Percent v4.csv" , row.names = FALSE)



#Missing Etiology files v2
a = table(P_v2_etiology_na$Hospital.Name)
write.csv(a , file = "crap/a.csv" , sep = ",")
a = read.csv("crap/a.csv")
a$District = hospital_details$district[match(a$Var1,hospital_details$hospName)]
b = table(P_v2_etiology_na$Hospital.Name)/table(P_v2$Hospital.Name)*100
write.csv(b , file = "crap/b.csv" , sep = ",")
b = read.csv("crap/b.csv")
a$Missing.Percent = b$Freq[match(a$Var1,b$Var1)]

write.csv(a , file = "Missing Entries Data/Analysis/Missing etiology Percent v2.csv" , row.names = FALSE)


#Missing Etiology files v4
a = table(P_v4_etiology_na$Hospital.Name)
write.csv(a , file = "crap/a.csv" , sep = ",")
a = read.csv("crap/a.csv")
a$District = hospital_details$district[match(a$Var1,hospital_details$hospName)]
b = table(P_v4_etiology_na$Hospital.Name)/table(P_v4$Hospital.Name)*100
write.csv(b , file = "crap/b.csv" , sep = ",")
b = read.csv("crap/b.csv")
a$Missing.Percent = b$Freq[match(a$Var1,b$Var1)]

write.csv(a , file = "Missing Entries Data/Analysis/Missing etiology Percent v4.csv" , row.names = FALSE)


#Missing Outcome files v2
a = table(P_v2_outcome_na$Hospital.Name)
write.csv(a , file = "crap/a.csv" , sep = ",")
a = read.csv("crap/a.csv")
a$District = hospital_details$district[match(a$Var1,hospital_details$hospName)]
b = table(P_v2_outcome_na$Hospital.Name)/table(P_v2$Hospital.Name)*100
write.csv(b , file = "crap/b.csv" , sep = ",")
b = read.csv("crap/b.csv")
a$Missing.Percent = b$Freq[match(a$Var1,b$Var1)]

write.csv(a , file = "Missing Entries Data/Analysis/Missing Outcome Percent v2.csv" , row.names = FALSE)

#Missing Outcome files v4
a = table(P_v4_outcome_na$Hospital.Name)
write.csv(a , file = "crap/a.csv" , sep = ",")
a = read.csv("crap/a.csv")
a$District = hospital_details$district[match(a$Var1,hospital_details$hospName)]
b = table(P_v4_outcome_na$Hospital.Name)/table(P_v4$Hospital.Name)*100
write.csv(b , file = "crap/b.csv" , sep = ",")
b = read.csv("crap/b.csv")
a$Missing.Percent = b$Freq[match(a$Var1,b$Var1)]

write.csv(a , file = "Missing Entries Data/Analysis/Missing outcome Percent v4.csv" , row.names = FALSE)





############################# Frequency of etiology in each district with Valid data ####################################
library(plyr)
P_valid_data_v2 = read.csv("Valid data/P_valid_data_v2.csv")
Etiology_District_v2 = count(P_valid_data_v2 , c("Etiology" , "District"))

write.csv(Etiology_District_v2 , file = "Analysis/valid data/freq_etiology_district_v2.csv" , row.names = FALSE)




P_valid_data_v4 = read.csv("Valid data/P_valid_data_v4.csv")
Etiology_District_v4 = count(P_valid_data_v4 , c("Etiology" , "District"))

write.csv(Etiology_District_v4 , file = "Analysis/valid data/freq_etiology_district_v4.csv" , row.names = FALSE)




