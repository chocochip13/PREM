setwd("data")
dat = read.csv("Valid data/P_valid_data_v444.csv")

#Checking for duplicate rows - No two rows can be exactly same even if the data is entered twice as the 
#timestamp cannot be same.
colnames(dat)
d= dat[,5:65]

dups = subset(dat, duplicated(d))
 
VD = subset(dat, duplicated(d) == FALSE)

write.csv(VD, "Valid data/P_valid_data_v4.csv", row.names = FALSE)
write.csv(dups, "Valid data/dups_v4.csv", row.names = FALSE)



#Preparing the data with with all binary coloumns
df = read.csv("Valid data/P_valid_data_v2_age.csv")


#Removing kg from weight
df$Weight..Kg. = gsub("[a-zA-Z ]", "", df$Weight..Kg.)
#write.csv(df, "Valid data/P_valid_data_v2.csv", row.names = FALSE)

df = read.csv("Valid data/P_valid_data_v4.csv")


df$Airway = gsub("Clear airway",0, df$Airway)
df$Airway = gsub("Obstructed",1, df$Airway)
df$Airway = gsub("Unmaintainable",2, df$Airway)

#Replacing Codes for Breathing
df$Breathing = gsub("Normal",0, df$Breathing)
df$Breathing = gsub("Distress",1, df$Breathing)
df$Breathing = gsub("Apnea",2, df$Breathing)

#Replacing Codes for Circulation
df$Circulation = gsub("Shock BP Normal",2,df$Circulation)
df$Circulation = gsub("Normal",0,df$Circulation)
df$Circulation = gsub("Brady",1,df$Circulation)
df$Circulation = gsub("Shocklow BP",3,df$Circulation)

#Replacing Codes for Disability
df$Disability = gsub("Normal",0, df$Disability)
df$Disability = gsub("ALC", 1,df$Disability)
df$Disability = gsub("Status Epilepticus",2, df$Disability)
df$Disability = gsub("Raised Intra Cranial Pressure",3, df$Disability)


df$Etiology = gsub("Edema", "Et.Edema", df$Etiology)
df$Etiology = gsub("Trauma", "Et.Trauma", df$Etiology)
df$Etiology = gsub("Toxin", "Et.Toxin", df$Etiology)
df$Etiology = gsub("Submersion", "Et.Submersion", df$Etiology)
df$Etiology = gsub("Distension", "Et.Distension", df$Etiology)
df$Etiology = gsub("Abdomial Pain", "Et.Abdomial Pain", df$Etiology)
df$Etiology = gsub("GI Bleed", "Et.GI Bleed", df$Etiology)
df$Etiology = gsub("Vomiting", "Et.Vomiting", df$Etiology)
df$Etiology = gsub("AWD", "Et.AWD", df$Etiology)
df$Etiology = gsub("Jaundice", "Et.Jaundice", df$Etiology)
df$Etiology = gsub("Snake/ Scorpion", "Et.Snake/ Scorpion", df$Etiology)
df$Etiology = gsub( "Rash", "Et.Rash", df$Etiology)
df$Etiology = gsub("Bleed", "Et.Bleed", df$Etiology)
df$Etiology = gsub("Foreign Body", "Et.Foreign Body", df$Etiology)
df$Etiology = gsub("Noisy Breathing", "Et.Noisy Breathing", df$Etiology)
df$Etiology = gsub("Focus of Infection", "Et.Focus of Infection", df$Etiology)
df$Etiology = gsub("Fever", "Et.Fever", df$Etiology)
df$Etiology = gsub("Posturing", "Et.Posturing", df$Etiology)
df$Etiology = gsub("Convulsions", "Et.Convulsions", df$Etiology)
df$Etiology = gsub("Unresponsive", "Et.Unresponsive", df$Etiology)
df$Etiology = gsub("Breathlessness", "Et.Breathlessness", df$Etiology)


#Replacing codes for Cardiac
df$Cardiac = gsub(0, "Card.CHD" , df$Cardiac)
df$Cardiac = gsub(1, "Card.RHD" , df$Cardiac)
df$Cardiac = gsub(2, "Card.Cardiomypathy" , df$Cardiac)

#Replacing  Codes CNS
df$CNS = gsub(0, "CNS.Seizure" ,df$CNS)
df$CNS = gsub(1, "CNS.VP shunt" ,df$CNS)
df$CNS = gsub(2, "CNS.neurodegenerative" ,df$CNS)
df$CNS = gsub(3, "CNS.CP" ,df$CNS)

#Replacing Codes for RS
df$RS = gsub(0, "RS.Laryngomalacia" ,df$RS)
df$RS = gsub(1, "RS.Asthma" ,df$RS)
df$RS = gsub(2, "RS.Lung malformations" ,df$RS)

#Replacing Codes for Bronchodialators
df$Bronchodialators = gsub(0,"Bron.Salbutamol",df$Bronchodialators)
df$Bronchodialators = gsub(1,"Bron.Ipravent",df$Bronchodialators)
df$Bronchodialators = gsub(2,"Bron.3% Saline",df$Bronchodialators)
df$Bronchodialators = gsub(3,"Bron.SQ Epinephrine",df$Bronchodialators)
df$Bronchodialators = gsub(4,"Bron.Inj Magnesium Sulphate",df$Bronchodialators)
df$Bronchodialators = gsub(5,"Bron.nj Aminophylline",df$Bronchodialators)

#Replacing Codes for Inotrope
df$Inotrope = gsub(0,"Inot.Epinephrine", df$Inotrope)
df$Inotrope = gsub(1,"Inot.Nor-Epinephrine", df$Inotrope)
df$Inotrope = gsub(2,"Inot.Dopamine", df$Inotrope)
df$Inotrope = gsub(3,"Inot.Dobutamine", df$Inotrope)

#Replacing Codes for Anti.Fit.Medication
df$Anti.Fit.Medication = gsub(0,"AFM.Benzodiazepine 1st", df$Anti.Fit.Medication)
df$Anti.Fit.Medication = gsub(1,"AFM.Benzodiazepine 2nd", df$Anti.Fit.Medication)
df$Anti.Fit.Medication = gsub(2,"AFM.Phenytoin", df$Anti.Fit.Medication)
df$Anti.Fit.Medication = gsub(3,"AFM.Leviteracetam", df$Anti.Fit.Medication)
df$Anti.Fit.Medication = gsub(4,"AFM.Sodium Valproate", df$Anti.Fit.Medication)
df$Anti.Fit.Medication = gsub(5,"AFM.Phenobarbitone", df$Anti.Fit.Medication)
df$Anti.Fit.Medication = gsub(6,"AFM.3%saline", df$Anti.Fit.Medication)

#Replacing Codes for Antibiotic
df$Antibiotic = gsub(0,"Abio.Ceftriaxzone",df$Antibiotic)
df$Antibiotic = gsub(1,"Abio.Cefotoxine",df$Antibiotic)
df$Antibiotic = gsub(2,"Abio.Azithral",df$Antibiotic)
df$Antibiotic = gsub(3,"Abio.Doxycycline",df$Antibiotic)

#Replacing Codes for Antidote.for.Poisons
df$Antidote.for.Poisons = gsub(0,"Adot.Atropine",df$Antidote.for.Poisons)
df$Antidote.for.Poisons = gsub(1,"Adot.PAM",df$Antidote.for.Poisons)
df$Antidote.for.Poisons = gsub(3,"Adot.N-acetyl cysteine",df$Antidote.for.Poisons)


#Replacing Codes for Outcome for Version 2
df$Outcome = gsub(0,"Out.Referred Out", df$Outcome)
df$Outcome = gsub(1,"Out.Discharge", df$Outcome)
df$Outcome = gsub(2,"Out.Died", df$Outcome)

#Replacing Codes for Outcome for Version 4
df$Outcome = gsub(0,"Out.Admitted", df$Outcome)
df$Outcome = gsub(1,"Out.Referred Out", df$Outcome)
df$Outcome = gsub(2,"Out.Discharge", df$Outcome)
df$Outcome = gsub(3,"Out.Died", df$Outcome)

write.csv(df,"Valid data/Replaced Codes/P_valid_data_v4.csv" )

#Splitting into several binary coloumns coloumns
library(qdapTools)

#Splitting Etiology into binary coloumns
df$Etiology = gsub("\\[|\\]", "", df$Etiology)
df = cbind(df,mtabulate(strsplit(as.character(df$Etiology), ',')))
df$Etiology = NULL

#Splitting Cardiac into binary coloumns
df$Cardiac = gsub("\\[|\\]", "", df$Cardiac)
df = cbind(df,mtabulate(strsplit(as.character(df$Cardiac), ',')))
df$Cardiac = NULL

#Splitting CNS into binary coloumns
df$CNS = gsub("\\[|\\]", "", df$CNS)
df = cbind(df,mtabulate(strsplit(as.character(df$CNS), ',')))
df$CNS = NULL

#Splitting RS into binary coloumns
df$RS = gsub("\\[|\\]", "", df$RS)
df = cbind(df,mtabulate(strsplit(as.character(df$RS), ',')))
df$RS = NULL

#Splitting Bronchodialators into binary coloumns
df$Bronchodialators = gsub("\\[|\\]", "", df$Bronchodialators)
df = cbind(df,mtabulate(strsplit(as.character(df$Bronchodialators), ',')))
df$Bronchodialators = NULL

#Splitting Inotrope into binary coloumns
df$Inotrope = gsub("\\[|\\]", "", df$Inotrope)
df = cbind(df,mtabulate(strsplit(as.character(df$Inotrope), ',')))
df$Inotrope = NULL

#Splitting Anti.Fit.Medication into binary coloumns
df$Anti.Fit.Medication = gsub("\\[|\\]", "", df$Anti.Fit.Medication)
df = cbind(df,mtabulate(strsplit(as.character(df$Anti.Fit.Medication), ',')))
df$Anti.Fit.Medication = NULL

#Splitting Antibiotic into binary coloumns
df$Antibiotic = gsub("\\[|\\]", "", df$Antibiotic)
df = cbind(df,mtabulate(strsplit(as.character(df$Antibiotic), ',')))
df$Antibiotic = NULL

#Splitting Antidote.for.Poisons into binary coloumns
df$Antidote.for.Poisons = gsub("\\[|\\]", "", df$Antidote.for.Poisons)
df = cbind(df,mtabulate(strsplit(as.character(df$Antidote.for.Poisons), ',')))
df$Antidote.for.Poisons = NULL

#Removing all the unnecessary variables like name, ID but it still has hospital name and other details
df[1:6] = NULL

##Removing multiple entries in Airway, Breathing, Circulation, Disability
df = subset(df, (df$Airway == "[0]" | df$Airway == "[1]" | df$Airway == "[2]") & (df$Breathing == "[0]" | df$Breathing == "[1]" | df$Breathing == "[2]") & (df$Circulation == "[0]" | df$Circulation == "[1]" | df$Circulation == "[2]") & (df$Disability == "[0]" | df$Disability == "[1]" | df$Disability == "[2]" | df$Disability == "[3]"))


#write.csv(df, "Working data/P_working_data_v4.csv", row.names = FALSE)

#Checking the dimension of new data
 
red = subset(df, ((df$Airway == "[0]" & df$Breathing == "[1]" & df$Circulation == "[1]" & df$Disability == "[1]") | (df$Airway == "[0]" & df$Breathing == "[0]" & df$Circulation == "[1]" & df$Disability == "[1]") | (df$Airway == "[0]" & df$Breathing == "[0]" & df$Circulation == "[3]" & df$Disability == "[1]") | (df$Airway == "[2]" & df$Breathing == "[2]" & df$Circulation == "[2]")))
green = subset(df, (df$Airway == "[0]" & df$Breathing == "[1]" & df$Circulation == "[0]" & df$Disability == "[0]") | (df$Airway == "[1]" & df$Breathing == "[1]" & df$Circulation == "[0]" & df$Disability == "[0]") )
orange = subset(df, (df$Airway == "[1]" & df$Breathing == "[1]" & df$Circulation == "[3]") | (df$Airway == "[1]" & df$Breathing == "[1]" & df$Circulation == "[1]") | (df$Airway == "[1]" & df$Breathing == "[1]" & df$Circulation == "[2]" & df$Disability == "[1]"))

wd = read.csv("Working data/P_working_data_v4.csv")


