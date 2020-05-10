library(qdap)
#Replacing Codes for Gender
ABCD$Gender = gsub(0,"Male",ABCD$Gender)
ABCD$Gender = gsub(1,"Female",ABCD$Gender)

#Replacing Codes for Development
ABCD$Development = gsub(0,"Normal", ABCD$Development)
ABCD$Development = gsub(1,"Not Normal", ABCD$Development)

#Replacing Codes for Airway
ABCD$Airway = gsub(0,"Clear airway" , ABCD$Airway)
ABCD$Airway = gsub(1,"Obstructed" , ABCD$Airway)
ABCD$Airway = gsub(2,"Unmaintainable" , ABCD$Airway)

#Replacing Codes for Breathing
ABCD$Breathing = gsub(0,"Normal", ABCD$Breathing)
ABCD$Breathing = gsub(1,"Distress", ABCD$Breathing)
ABCD$Breathing = gsub(2,"Apnea", ABCD$Breathing)

#Replacing Codes for Circulation
ABCD$Circulation = gsub(0,"Normal",ABCD$Circulation)
ABCD$Circulation = gsub(1,"Brady",ABCD$Circulation)
ABCD$Circulation = gsub(2,"Shock BP Normal",ABCD$Circulation)
ABCD$Circulation = gsub(3,"Shocklow BP",ABCD$Circulation)

#Replacing Codes for Disability
ABCD$Disability = gsub(0,"Normal", ABCD$Disability)
ABCD$Disability = gsub(1,"ALC", ABCD$Disability)
ABCD$Disability = gsub(2,"Status Epilepticus", ABCD$Disability)
ABCD$Disability = gsub(3,"Raised Intra Cranial Pressure", ABCD$Disability)

#Replacing Codes Cardiac
ABCD$Cardiac = gsub(0, "CHD" , ABCD$Cardiac)
ABCD$Cardiac = gsub(1, "RHD" , ABCD$Cardiac)
ABCD$Cardiac = gsub(2, "Cardiomypathy" , ABCD$Cardiac)

#Replacing  Codes CNS
ABCD$CNS = gsub(0, "Seizure" ,ABCD$CNS)
ABCD$CNS = gsub(1, "VP shunt" ,ABCD$CNS)
ABCD$CNS = gsub(2, "neurodegenerative" ,ABCD$CNS)
ABCD$CNS = gsub(3, "CP" ,ABCD$CNS)

#Replacing Codes for RS
ABCD$RS = gsub(0, "Laryngomalacia" ,ABCD$RS)
ABCD$RS = gsub(1, "Asthma" ,ABCD$RS)
ABCD$RS = gsub(2, "Lung malformations" ,ABCD$RS)

#Replacing Codes for Bronchodialators
ABCD$Bronchodialators = gsub(0,"Salbutamol",ABCD$Bronchodialators)
ABCD$Bronchodialators = gsub(1,"Ipravent",ABCD$Bronchodialators)
ABCD$Bronchodialators = gsub(2,"3% Saline",ABCD$Bronchodialators)
ABCD$Bronchodialators = gsub(3,"SQ Epinephrine",ABCD$Bronchodialators)
ABCD$Bronchodialators = gsub(4,"Inj Magnesium Sulphate",ABCD$Bronchodialators)
ABCD$Bronchodialators = gsub(5,"nj Aminophylline",ABCD$Bronchodialators)

#Replacing Codes for Inotrope
ABCD$Inotrope = gsub(0,"Epinephrine", ABCD$Inotrope)
ABCD$Inotrope = gsub(1,"Nor-Epinephrine", ABCD$Inotrope)
ABCD$Inotrope = gsub(2,"Dopamine", ABCD$Inotrope)
ABCD$Inotrope = gsub(3,"Dobutamine", ABCD$Inotrope)

#Replacing Codes for Anti.Fit.Medication
ABCD$Anti.Fit.Medication = gsub(0,"Benzodiazepine 1st", ABCD$Anti.Fit.Medication)
ABCD$Anti.Fit.Medication = gsub(1,"Benzodiazepine 2nd", ABCD$Anti.Fit.Medication)
ABCD$Anti.Fit.Medication = gsub(2,"Phenytoin", ABCD$Anti.Fit.Medication)
ABCD$Anti.Fit.Medication = gsub(3,"Leviteracetam", ABCD$Anti.Fit.Medication)
ABCD$Anti.Fit.Medication = gsub(4,"Sodium Valproate", ABCD$Anti.Fit.Medication)
ABCD$Anti.Fit.Medication = gsub(5,"Phenobarbitone", ABCD$Anti.Fit.Medication)
ABCD$Anti.Fit.Medication = gsub(6,"3%saline", ABCD$Anti.Fit.Medication)

#Replacing Codes for Antibiotic
ABCD$Antibiotic = gsub(0,"Ceftriaxzone",ABCD$Antibiotic)
ABCD$Antibiotic = gsub(1,"Cefotoxine",ABCD$Antibiotic)
ABCD$Antibiotic = gsub(2,"Azithral",ABCD$Antibiotic)
ABCD$Antibiotic = gsub(3,"Doxycycline",ABCD$Antibiotic)

#Replacing Codes for Antidote.for.Poisons
ABCD$Antidote.for.Poisons = gsub(0,"Atropine",ABCD$Antidote.for.Poisons)
ABCD$Antidote.for.Poisons = gsub(1,"PAM",ABCD$Antidote.for.Poisons)
ABCD$Antidote.for.Poisons = gsub(3,"N-acetyl cysteine",ABCD$Antidote.for.Poisons)

#Replacing Codes for Outcome for Version 2
ABCD$Outcome = gsub(0,"Referred Out", ABCD$Outcome)
ABCD$Outcome = gsub(1,"Discharge", ABCD$Outcome)
ABCD$Outcome = gsub(2,"Died", ABCD$Outcome)

#Replacing Codes for Outcome for Version 4
ABCD$Outcome = gsub(0,"Admitted", ABCD$Outcome)
ABCD$Outcome = gsub(1,"Referred Out", ABCD$Outcome)
ABCD$Outcome = gsub(2,"Discharge", ABCD$Outcome)
ABCD$Outcome = gsub(3,"Died", ABCD$Outcome)


#Replacing Codes for Etiology
ABCD$Etiology = gsub(20, "Edema", ABCD$Etiology)
ABCD$Etiology = gsub(19, "Trauma", ABCD$Etiology)
ABCD$Etiology = gsub(18, "Toxin", ABCD$Etiology)
ABCD$Etiology = gsub(17, "Submersion", ABCD$Etiology)
ABCD$Etiology = gsub(16, "Distension", ABCD$Etiology)
ABCD$Etiology = gsub(15, "Abdomial Pain", ABCD$Etiology)
ABCD$Etiology = gsub(14, "GI Bleed", ABCD$Etiology)
ABCD$Etiology = gsub(13, "Vomiting", ABCD$Etiology)
ABCD$Etiology = gsub(12, "AWD", ABCD$Etiology)
ABCD$Etiology = gsub(11, "Jaundice", ABCD$Etiology)
ABCD$Etiology = gsub(10, "Snake/ Scorpion", ABCD$Etiology)
ABCD$Etiology = gsub(9, "Rash", ABCD$Etiology)
ABCD$Etiology = gsub(8, "Bleed", ABCD$Etiology)
ABCD$Etiology = gsub(7, "Foreign Body", ABCD$Etiology)
ABCD$Etiology = gsub(6, "Noisy Breathing", ABCD$Etiology)
ABCD$Etiology = gsub(5, "Focus of Infection", ABCD$Etiology)
ABCD$Etiology = gsub(4, "Fever", ABCD$Etiology)
ABCD$Etiology = gsub(3, "Posturing", ABCD$Etiology)
ABCD$Etiology = gsub(2, "Convulsions", ABCD$Etiology)
ABCD$Etiology = gsub(1, "Unresponsive", ABCD$Etiology)
ABCD$Etiology = gsub(0, "Breathlessness", ABCD$Etiology)




