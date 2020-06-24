basic_preproc <- function(df, red_cols) 
{ 
  # Removing duplicates
  df <- subset(df, duplicated(data[,15:col_length]) == FALSE)
  
  #Remove Redundant columns and Removing Empty ABCD and multiple, Etiology
  df <- df %>% 
    select(-all_of(red_cols)) %>%
    filter(!is.na(Airway) & !is.na(Breathing) & !is.na(Circulation) & !is.na(Disability)) %>%
    filter(!grepl(",", Airway) & !grepl(",", Breathing) & !grepl(",", Circulation) & !grepl(",", Disability)) %>%
    filter(!is.na(Etiology)) 
  #Making the ABCD as factors
  df <- df %>% 
    mutate(Version = replace(Version, Version == "v4", "V4")) %>%
    mutate(Airway = gsub("\\[|\\]", "", Airway)) %>%
    mutate(Airway = replace(Airway, Airway == "Normal", "Clear airway")) %>%
    mutate(Airway = as.factor(Airway)) %>% 
    mutate(Breathing = gsub("\\[|\\]", "", Breathing)) %>%
    mutate(Breathing = replace(Breathing, Breathing == "distress", "Distress")) %>%
    mutate(Breathing = as.factor(Breathing)) %>% 
    mutate(Circulation = gsub("\\[|\\]", "", Circulation)) %>%
    mutate(Circulation = replace(Circulation, Circulation == "normal", "Normal")) %>%
    mutate(Circulation = as.factor(Circulation)) %>% 
    mutate(Disability = gsub("\\[|\\]", "", Disability)) %>%
    mutate(Disability = as.factor(Disability)) 
  
  #Making gender binary
  df <- df %>%
    mutate(Gender = as.character(Gender)) %>%
    mutate(Gender = replace(Gender, Gender == "Female", "0")) %>%
    mutate(Gender = replace(Gender, Gender == "Male", "1")) %>%
    mutate(Gender = as.factor(Gender))

  df<- df %>% 
    mutate(RS = as.character(RS))  %>%
    mutate(RS = replace(RS, RS == "ASTHMA", "[Asthma]")) %>%
    mutate(RS = as.factor(RS))
  
  df<- df %>% 
    mutate(Liver.Disease = as.character(Liver.Disease))  %>%
    mutate(Liver.Disease = replace(Liver.Disease, Liver.Disease == "NORMAL", "0")) %>%
    mutate(Liver.Disease = replace(Liver.Disease, Liver.Disease == "Not Normal", "1")) %>%
    mutate(Liver.Disease = as.factor(Liver.Disease))
  #Making Liver Disease to either TRUE or FALSE or NA
  df<- df %>% 
    mutate(Liver.Disease = as.character(Liver.Disease)) %>%
    mutate(Liver.Disease = replace(Liver.Disease, Liver.Disease == "normal", "0")) %>%
    mutate(Liver.Disease = replace(Liver.Disease, Liver.Disease == "NORMAL", "0")) %>%
    mutate(Liver.Disease = replace(Liver.Disease, Liver.Disease == "-", NA)) %>%
    mutate(Liver.Disease = replace(Liver.Disease, Liver.Disease == "FALSE", "0")) %>%   
    mutate(Liver.Disease = replace(Liver.Disease, Liver.Disease == "TRUE", "1")) %>%
    mutate(Liver.Disease = as.factor(Liver.Disease))
  
  #Making Renal Disease to either TRUE or FALSE or NA
  df<- df %>% 
    mutate(Renal.Disease = as.character(Renal.Disease)) %>%
    mutate(Renal.Disease = replace(Renal.Disease, Renal.Disease == "normal", "0")) %>%
    mutate(Renal.Disease = replace(Renal.Disease, Renal.Disease == "NORMAL", "0")) %>%
    mutate(Renal.Disease = replace(Renal.Disease, Renal.Disease == "-", NA)) %>%
    mutate(Renal.Disease = replace(Renal.Disease, Renal.Disease == "FALSE", "0")) %>%   
    mutate(Renal.Disease = replace(Renal.Disease, Renal.Disease == "TRUE", "1")) %>%
    mutate(Renal.Disease = as.factor(Renal.Disease))
  
  df<- df %>% 
    mutate(Blood = as.character(Blood)) %>%
    mutate(Blood = replace(Blood, Blood == "normal", "0")) %>%
    mutate(Blood = replace(Blood, Blood == "NORMAL", "0")) %>%
    mutate(Blood = replace(Blood, Blood == "-", NA)) %>%
    mutate(Blood = replace(Blood, Blood == "FALSE", "0")) %>%   
    mutate(Blood = replace(Blood, Blood == "TRUE", "1")) %>%
    mutate(Blood = as.factor(Blood))
  
  df<- df %>% 
    mutate(Metabolic = as.character(Metabolic)) %>%
    mutate(Metabolic = replace(Metabolic, Metabolic == "normal", "0")) %>%
    mutate(Metabolic = replace(Metabolic, Metabolic == "NORMAL", "0")) %>%
    mutate(Metabolic = replace(Metabolic, Metabolic == "-", NA)) %>%
    mutate(Metabolic = replace(Metabolic, Metabolic == "FALSE", "0")) %>%   
    mutate(Metabolic = replace(Metabolic, Metabolic == "TRUE", "1")) %>%
    mutate(Metabolic = as.factor(Metabolic))
  
  df<- df %>% 
    mutate(Post.Surgical = as.character(Post.Surgical)) %>%
    mutate(Post.Surgical = replace(Post.Surgical, Post.Surgical == "normal", "0")) %>%
    mutate(Post.Surgical = replace(Post.Surgical, Post.Surgical == "NORMAL", "0")) %>%
    mutate(Post.Surgical = replace(Post.Surgical, Post.Surgical == "no", "0")) %>%
    mutate(Post.Surgical = replace(Post.Surgical, Post.Surgical == "-", NA)) %>%
    mutate(Post.Surgical = replace(Post.Surgical, Post.Surgical == "FALSE", "0")) %>%   
    mutate(Post.Surgical = replace(Post.Surgical, Post.Surgical == "TRUE", "1")) %>%
    mutate(Post.Surgical = as.factor(Post.Surgical))
  
  df<- df %>% 
    mutate(Bottle.Fed = as.character(Bottle.Fed)) %>%
    mutate(Bottle.Fed = replace(Bottle.Fed, Bottle.Fed == "normal", "0")) %>%
    mutate(Bottle.Fed = replace(Bottle.Fed, Bottle.Fed == "NORMAL", "0")) %>%
    mutate(Bottle.Fed = replace(Bottle.Fed, Bottle.Fed == "no", "0")) %>%
    mutate(Bottle.Fed = replace(Bottle.Fed, Bottle.Fed == "-", NA)) %>%
    mutate(Bottle.Fed = replace(Bottle.Fed, Bottle.Fed == "FALSE", "0")) %>%   
    mutate(Bottle.Fed = replace(Bottle.Fed, Bottle.Fed == "TRUE", "1")) %>%
    mutate(Bottle.Fed = as.factor(Bottle.Fed))
  
  df<- df %>% 
    mutate(Poor.child.rearing.practices = as.character(Poor.child.rearing.practices)) %>%
    mutate(Poor.child.rearing.practices = replace(Poor.child.rearing.practices, Poor.child.rearing.practices == "normal", "0")) %>%
    mutate(Poor.child.rearing.practices = replace(Poor.child.rearing.practices, Poor.child.rearing.practices == "NORMAL", "0")) %>%
    mutate(Poor.child.rearing.practices = replace(Poor.child.rearing.practices, Poor.child.rearing.practices == "no", "0")) %>%
    mutate(Poor.child.rearing.practices = replace(Poor.child.rearing.practices, Poor.child.rearing.practices == "NO", "0")) %>%
    mutate(Poor.child.rearing.practices = replace(Poor.child.rearing.practices, Poor.child.rearing.practices == "-", NA)) %>%
    mutate(Poor.child.rearing.practices = replace(Poor.child.rearing.practices, Poor.child.rearing.practices == "FALSE", "0")) %>%   
    mutate(Poor.child.rearing.practices = replace(Poor.child.rearing.practices, Poor.child.rearing.practices == "TRUE", "1")) %>%
    mutate(Poor.child.rearing.practices = as.factor(Poor.child.rearing.practices))
  
  df<- df %>% 
    mutate(PLHA = as.character(PLHA)) %>%
    mutate(PLHA = replace(PLHA, PLHA == "normal", "0")) %>%
    mutate(PLHA = replace(PLHA, PLHA == "NORMAL", "0")) %>%
    mutate(PLHA = replace(PLHA, PLHA == "-", NA)) %>%
    mutate(PLHA = replace(PLHA, PLHA == "FALSE", "0")) %>%   
    mutate(PLHA = replace(PLHA, PLHA == "TRUE", "1")) %>%
    mutate(PLHA = as.factor(PLHA))  
  
  df<- df %>% 
    mutate(Medications = as.character(Medications)) %>%
    mutate(Medications = replace(Medications, Medications == "normal", "0")) %>%
    mutate(Medications = replace(Medications, Medications == "NORMAL", "0")) %>%
    mutate(Medications = replace(Medications, Medications == "-", NA)) %>%
    mutate(Medications = replace(Medications, Medications == "FALSE", "0")) %>%   
    mutate(Medications = replace(Medications, Medications == "TRUE", "1")) %>%
    mutate(Medications = as.factor(Medications))
  
  df<- df %>% 
    mutate(Home.available.solutions = as.character(Home.available.solutions)) %>%
    mutate(Home.available.solutions = replace(Home.available.solutions, Home.available.solutions == "normal", "0")) %>%
    mutate(Home.available.solutions = replace(Home.available.solutions, Home.available.solutions == "NORMAL", "0")) %>%
    mutate(Home.available.solutions = replace(Home.available.solutions, Home.available.solutions == "-", NA)) %>%
    mutate(Home.available.solutions = replace(Home.available.solutions, Home.available.solutions == "FALSE", "0")) %>%   
    mutate(Home.available.solutions = replace(Home.available.solutions, Home.available.solutions == "TRUE", "1")) %>%
    mutate(Home.available.solutions = as.factor(Home.available.solutions))
  
  df<- df %>% 
    mutate(Oral.Rehydration.Solution..ORS. = as.character(Oral.Rehydration.Solution..ORS.)) %>%
    mutate(Oral.Rehydration.Solution..ORS. = replace(Oral.Rehydration.Solution..ORS., Oral.Rehydration.Solution..ORS. == "normal", "0")) %>%
    mutate(Oral.Rehydration.Solution..ORS. = replace(Oral.Rehydration.Solution..ORS., Oral.Rehydration.Solution..ORS. == "NORMAL", "0")) %>%
    mutate(Oral.Rehydration.Solution..ORS. = replace(Oral.Rehydration.Solution..ORS., Oral.Rehydration.Solution..ORS. == "-", NA)) %>%
    mutate(Oral.Rehydration.Solution..ORS. = replace(Oral.Rehydration.Solution..ORS., Oral.Rehydration.Solution..ORS. == "FALSE", "0")) %>%   
    mutate(Oral.Rehydration.Solution..ORS. = replace(Oral.Rehydration.Solution..ORS., Oral.Rehydration.Solution..ORS. == "TRUE", "1")) %>%
    mutate(Oral.Rehydration.Solution..ORS. = as.factor(Oral.Rehydration.Solution..ORS.))
  
  df<- df %>% 
    mutate(Head.Tilt.Chin.Lift = as.character(Head.Tilt.Chin.Lift)) %>%
    mutate(Head.Tilt.Chin.Lift = replace(Head.Tilt.Chin.Lift, Head.Tilt.Chin.Lift == "normal", "0")) %>%
    mutate(Head.Tilt.Chin.Lift = replace(Head.Tilt.Chin.Lift, Head.Tilt.Chin.Lift == "NORMAL", "0")) %>%
    mutate(Head.Tilt.Chin.Lift = replace(Head.Tilt.Chin.Lift, Head.Tilt.Chin.Lift == "-", NA)) %>%
    mutate(Head.Tilt.Chin.Lift = replace(Head.Tilt.Chin.Lift, Head.Tilt.Chin.Lift == "FALSE", "0")) %>%   
    mutate(Head.Tilt.Chin.Lift = replace(Head.Tilt.Chin.Lift, Head.Tilt.Chin.Lift == "TRUE", "1")) %>%
    mutate(Head.Tilt.Chin.Lift = as.factor(Head.Tilt.Chin.Lift))
  
  
  df<- df %>% 
    mutate(Bag.valve.Mask.Ventilation = as.character(Bag.valve.Mask.Ventilation)) %>%
    mutate(Bag.valve.Mask.Ventilation = replace(Bag.valve.Mask.Ventilation, Bag.valve.Mask.Ventilation == "no", "0")) %>%
    mutate(Bag.valve.Mask.Ventilation = replace(Bag.valve.Mask.Ventilation, Bag.valve.Mask.Ventilation == "NORMAL", "0")) %>%
    mutate(Bag.valve.Mask.Ventilation = replace(Bag.valve.Mask.Ventilation, Bag.valve.Mask.Ventilation == "-", NA)) %>%
    mutate(Bag.valve.Mask.Ventilation = replace(Bag.valve.Mask.Ventilation, Bag.valve.Mask.Ventilation == "FALSE", "0")) %>%   
    mutate(Bag.valve.Mask.Ventilation = replace(Bag.valve.Mask.Ventilation, Bag.valve.Mask.Ventilation == "TRUE", "1")) %>%
    mutate(Bag.valve.Mask.Ventilation = as.factor(Bag.valve.Mask.Ventilation))
  
  df<- df %>% 
    mutate(CPAP = as.character(CPAP)) %>%
    mutate(CPAP = replace(CPAP, CPAP == "no", "0")) %>%
    mutate(CPAP = replace(CPAP, CPAP == "NORMAL", "0")) %>%
    mutate(CPAP = replace(CPAP, CPAP == "-", NA)) %>%
    mutate(CPAP = replace(CPAP, CPAP == "FALSE", "0")) %>%   
    mutate(CPAP = replace(CPAP, CPAP == "TRUE", "1")) %>%
    mutate(CPAP = as.factor(CPAP))
  
  df<- df %>% 
    mutate(Intubation = as.character(Intubation)) %>%
    mutate(Intubation = replace(Intubation, Intubation == "no", "0")) %>%
    mutate(Intubation = replace(Intubation, Intubation == "NORMAL", "0")) %>%
    mutate(Intubation = replace(Intubation, Intubation == "-", NA)) %>%
    mutate(Intubation = replace(Intubation, Intubation == "FALSE", "0")) %>%   
    mutate(Intubation = replace(Intubation, Intubation == "TRUE", "1")) %>%
    mutate(Intubation = as.factor(Intubation))
  
  df<- df %>% 
    mutate(Bronchodialators = as.character(Bronchodialators)) %>%
    mutate(Bronchodialators = replace(Bronchodialators, Bronchodialators == "no", "0")) %>%
    mutate(Bronchodialators = replace(Bronchodialators, Bronchodialators == "NO", "0")) %>%
    mutate(Bronchodialators = replace(Bronchodialators, Bronchodialators == "NORMAL", "0")) %>%
    mutate(Bronchodialators = replace(Bronchodialators, Bronchodialators == "-", NA)) %>%
    mutate(Bronchodialators = replace(Bronchodialators, Bronchodialators == "FALSE", "0")) %>%   
    mutate(Bronchodialators = replace(Bronchodialators, Bronchodialators == "TRUE", "1")) %>%
    mutate(Bronchodialators = as.factor(Bronchodialators))

  df<- df %>% 
    mutate(Cardiac.Massage = as.character(Cardiac.Massage)) %>%
    mutate(Cardiac.Massage = replace(Cardiac.Massage, Cardiac.Massage == "no", "0")) %>%
    mutate(Cardiac.Massage = replace(Cardiac.Massage, Cardiac.Massage == "NO", "0")) %>%
    mutate(Cardiac.Massage = replace(Cardiac.Massage, Cardiac.Massage == "NORMAL", "0")) %>%
    mutate(Cardiac.Massage = replace(Cardiac.Massage, Cardiac.Massage == "-", NA)) %>%
    mutate(Cardiac.Massage = replace(Cardiac.Massage, Cardiac.Massage == "FALSE", "0")) %>%   
    mutate(Cardiac.Massage = replace(Cardiac.Massage, Cardiac.Massage == "TRUE", "1")) %>%
    mutate(Cardiac.Massage = as.factor(Cardiac.Massage))
  
  df<- df %>% 
    mutate(Normal.Saline.Bolus = as.character(Normal.Saline.Bolus)) %>%
    mutate(Normal.Saline.Bolus = replace(Normal.Saline.Bolus, Normal.Saline.Bolus == "no", "0")) %>%
    mutate(Normal.Saline.Bolus = replace(Normal.Saline.Bolus, Normal.Saline.Bolus == "NO", "0")) %>%
    mutate(Normal.Saline.Bolus = replace(Normal.Saline.Bolus, Normal.Saline.Bolus == "-", NA)) %>%
    mutate(Normal.Saline.Bolus = replace(Normal.Saline.Bolus, Normal.Saline.Bolus == "FALSE", "0")) %>%   
    mutate(Normal.Saline.Bolus = replace(Normal.Saline.Bolus, Normal.Saline.Bolus == "TRUE", "1")) %>%
    mutate(Normal.Saline.Bolus = as.factor(Normal.Saline.Bolus))

  df<- df %>% 
    mutate(Inotrope = as.character(Inotrope)) %>%
    mutate(Inotrope = replace(Inotrope, Inotrope == "no", "0")) %>%
    mutate(Inotrope = replace(Inotrope, Inotrope == "NO", "0")) %>%
    mutate(Inotrope = replace(Inotrope, Inotrope == "-", NA)) %>%
    mutate(Inotrope = replace(Inotrope, Inotrope == "FALSE", "0")) %>%   
    mutate(Inotrope = replace(Inotrope, Inotrope == "TRUE", "1")) %>%
    mutate(Inotrope = replace(Inotrope, Inotrope == "DOPAMINE", "[Dopamine]")) %>%
    mutate(Inotrope = as.factor(Inotrope))
  
  df<- df %>% 
    mutate(X25..Dextrose = as.character(X25..Dextrose)) %>%
    mutate(X25..Dextrose = replace(X25..Dextrose, X25..Dextrose == "no", "0")) %>%
    mutate(X25..Dextrose = replace(X25..Dextrose, X25..Dextrose == "NO", "0")) %>%
    mutate(X25..Dextrose = replace(X25..Dextrose, X25..Dextrose == "-", NA)) %>%
    mutate(X25..Dextrose = replace(X25..Dextrose, X25..Dextrose == "FALSE", "0")) %>%   
    mutate(X25..Dextrose = replace(X25..Dextrose, X25..Dextrose == "TRUE", "1")) %>%
    mutate(X25..Dextrose = as.factor(X25..Dextrose))
  
  df<- df %>% 
    mutate(CSstabilization = as.character(CSstabilization)) %>%
    mutate(CSstabilization = replace(CSstabilization, CSstabilization == "no", "0")) %>%
    mutate(CSstabilization = replace(CSstabilization, CSstabilization == "NO", "0")) %>%
    mutate(CSstabilization = replace(CSstabilization, CSstabilization == "-", NA)) %>%
    mutate(CSstabilization = replace(CSstabilization, CSstabilization == "FALSE", "0")) %>%   
    mutate(CSstabilization = replace(CSstabilization, CSstabilization == "TRUE", "1")) %>%
    mutate(CSstabilization = as.factor(CSstabilization))
  
  df<- df %>% 
    mutate(Stomach.Wash = as.character(Stomach.Wash)) %>%
    mutate(Stomach.Wash = replace(Stomach.Wash, Stomach.Wash == "no", "0")) %>%
    mutate(Stomach.Wash = replace(Stomach.Wash, Stomach.Wash == "NO", "0")) %>%
    mutate(Stomach.Wash = replace(Stomach.Wash, Stomach.Wash == "-", NA)) %>%
    mutate(Stomach.Wash = replace(Stomach.Wash, Stomach.Wash == "FALSE", "0")) %>%   
    mutate(Stomach.Wash = replace(Stomach.Wash, Stomach.Wash == "TRUE", "1")) %>%
    mutate(Stomach.Wash = as.factor(Stomach.Wash))
  
  df<- df %>% 
    mutate(De.Contamination = as.character(De.Contamination)) %>%
    mutate(De.Contamination = replace(De.Contamination, De.Contamination == "no", "0")) %>%
    mutate(De.Contamination = replace(De.Contamination, De.Contamination == "NO", "0")) %>%
    mutate(De.Contamination = replace(De.Contamination, De.Contamination == "-", NA)) %>%
    mutate(De.Contamination = replace(De.Contamination, De.Contamination == "FALSE", "0")) %>%
    mutate(De.Contamination = replace(De.Contamination, De.Contamination == "TRUE", "1")) %>%
    mutate(De.Contamination = as.factor(De.Contamination))
  
  df<- df %>% 
    mutate(Thoracocentesis = as.character(Thoracocentesis)) %>%
    mutate(Thoracocentesis = replace(Thoracocentesis, Thoracocentesis == "no", "0")) %>%
    mutate(Thoracocentesis = replace(Thoracocentesis, Thoracocentesis == "NO", "0")) %>%
    mutate(Thoracocentesis = replace(Thoracocentesis, Thoracocentesis == "-", NA)) %>%
    mutate(Thoracocentesis = replace(Thoracocentesis, Thoracocentesis == "FALSE", "0")) %>%   
    mutate(Thoracocentesis = replace(Thoracocentesis, Thoracocentesis == "TRUE", "1")) %>%
    mutate(Thoracocentesis = as.factor(Thoracocentesis))
  
  df<- df %>% 
    mutate(Blood.Products = as.character(Blood.Products)) %>%
    mutate(Blood.Products = replace(Blood.Products, Blood.Products == "no", "0")) %>%
    mutate(Blood.Products = replace(Blood.Products, Blood.Products == "NO", "0")) %>%
    mutate(Blood.Products = replace(Blood.Products, Blood.Products == "-", NA)) %>%
    mutate(Blood.Products = replace(Blood.Products, Blood.Products == "FALSE", "0")) %>%   
    mutate(Blood.Products = replace(Blood.Products, Blood.Products == "TRUE", "1")) %>%
    mutate(Blood.Products = as.factor(Blood.Products))
  
  df<- df %>% 
    mutate(Prazosin = as.character(Prazosin)) %>%
    mutate(Prazosin = replace(Prazosin, Prazosin == "no", "0")) %>%
    mutate(Prazosin = replace(Prazosin, Prazosin == "NO", "0")) %>%
    mutate(Prazosin = replace(Prazosin, Prazosin == "-", NA)) %>%
    mutate(Prazosin = replace(Prazosin, Prazosin == "FALSE", "0")) %>%   
    mutate(Prazosin = replace(Prazosin, Prazosin == "TRUE", "1")) %>%
    mutate(Prazosin = as.factor(Prazosin))
  
  df<- df %>% 
    mutate(Anti.Snake.Venom = as.character(Anti.Snake.Venom)) %>%
    mutate(Anti.Snake.Venom = replace(Anti.Snake.Venom, Anti.Snake.Venom == "no", "0")) %>%
    mutate(Anti.Snake.Venom = replace(Anti.Snake.Venom, Anti.Snake.Venom == "NO", "0")) %>%
    mutate(Anti.Snake.Venom = replace(Anti.Snake.Venom, Anti.Snake.Venom == "-", NA)) %>%
    mutate(Anti.Snake.Venom = replace(Anti.Snake.Venom, Anti.Snake.Venom == "FALSE", "0")) %>%
    mutate(Anti.Snake.Venom = replace(Anti.Snake.Venom, Anti.Snake.Venom == "TRUE", "1")) %>%
    mutate(Anti.Snake.Venom = as.factor(Anti.Snake.Venom))
  
}

disease_preproc <- function(df, red_cols){
  df <- df %>%
    select(-all_of(red_cols))
  
  #Etiology coloumns 52:81
  df <- df %>%
    mutate(Etiology = gsub("\\[|\\]", "", Etiology)) %>%
    mutate(Etiology = as.character(Etiology)) %>%
    mutate(Etiology = strsplit(Etiology, ","))
  etiology <<- mtabulate(df$Etiology)
  etiology <<- etiology %>% 
                  select(-c(" Breathlessness", "Asthma")) %>%
                  mutate_all(funs(as.logical(.)))
  
  df <- cbind(df,mtabulate(df$Etiology))
  df <- df %>% select(-c("Etiology"," Breathlessness", "Asthma")) #Two columns formed with a space in between, hence remove it.
  
  df <- df %>%
    mutate(Cardiac = gsub("\\[|\\]", "", Cardiac)) %>%
    mutate(Cardiac = as.character(Cardiac)) %>%
    mutate(Cardiac = strsplit(Cardiac, ","))
  cardiac <<- mtabulate(df$Cardiac) %>%
              select(-c("NO","Normal","NORMAL","-")) %>% 
              mutate_all(funs(as.logical(.)))

  df <- cbind(df,mtabulate(df$Cardiac))
  df <- df %>% select(-c("Cardiac","NO","Normal","NORMAL","-"))
  
  df <- df %>%
    mutate(CNS = gsub("\\[|\\]", "", CNS)) %>%
    mutate(CNS = as.character(CNS)) %>%
    mutate(CNS = strsplit(CNS, ","))
  CNS <<- mtabulate(df$CNS) %>%
          select(-c("NO","Normal","NORMAL","-")) %>% 
          mutate_all(funs(as.logical(.)))
  
  df <- cbind(df,mtabulate(df$CNS))
  df <- df %>% select(-c("CNS","NO","Normal","NORMAL","-"))
  
  df <- df %>%
    mutate(RS = gsub("\\[|\\]", "", RS)) %>%
    mutate(RS = as.character(RS)) %>%
    mutate(RS = strsplit(RS, ","))
  
  RS <<- mtabulate(df$RS) %>%
    select(-c("NO","Normal","NORMAL","-")) %>% 
    mutate_all(funs(as.logical(.)))
  
  df <- cbind(df,mtabulate(df$RS))
  df <- df %>% select(-c("RS","NO","Normal","NORMAL","-"))
  
  
  df <- df %>%
    mutate(Bronchodialators = gsub("\\[|\\]", "", Bronchodialators)) %>%
    mutate(Bronchodialators = as.character(Bronchodialators)) %>%
    mutate(Bronchodialators = strsplit(Bronchodialators, ","))
  bronchodialators <<- mtabulate(df$Bronchodialators)%>% select(-c("0")) %>% mutate_all(funs(as.logical(.)))
  df <- cbind(df,mtabulate(df$Bronchodialators))
  df <- df %>% select(-c("Bronchodialators", "0")) #Removin unecessary columns
  
  df <- df %>%
    mutate(Inotrope = gsub("\\[|\\]", "", Inotrope)) %>%
    mutate(Inotrope = as.character(Inotrope)) %>%
    mutate(Inotrope = strsplit(Inotrope, ","))
  inotrope <<-  (mtabulate(df$Inotrope)) %>% select(-c("0"))%>% mutate_all(funs(as.logical(.)))
  df <- cbind(df,mtabulate(df$Inotrope))
  df <- df %>% select(-c("Inotrope","0"))
  
  df <- df %>%
    mutate(Anti.Fit.Medication = gsub("\\[|\\]", "", Anti.Fit.Medication)) %>%
    mutate(Anti.Fit.Medication = as.character(Anti.Fit.Medication)) %>%
    mutate(Anti.Fit.Medication = strsplit(Anti.Fit.Medication, ","))
  anti.Fit.Medication <<-  (mtabulate(df$Anti.Fit.Medication)) %>% 
                            select(-c("-","NO","FALSE","TRUE")) %>%
                            mutate_all(funs(as.logical(.)))
  
  df <- cbind(df,mtabulate(df$Anti.Fit.Medication))
  df <- df %>% select(-c("Anti.Fit.Medication","-","NO","FALSE","TRUE")) 
  
  df <- df %>%
    mutate(Antibiotic = gsub("\\[|\\]", "", Antibiotic)) %>%
    mutate(Antibiotic = as.character(Antibiotic)) %>%
    mutate(Antibiotic = strsplit(Antibiotic, ","))
  antibiotic <<-  (mtabulate(df$Antibiotic)) %>% 
                  select(-c("-","FALSE","NIL","NO")) %>%
                  mutate_all(funs(as.logical(.)))
  
  df <- cbind(df,mtabulate(df$Antibiotic))
  df <- df %>% select(-c("Antibiotic","-","FALSE","NIL","NO"))
  
  df <- df %>%
    mutate(Antidote.for.Poisons = gsub("\\[|\\]", "", Antidote.for.Poisons)) %>%
    mutate(Antidote.for.Poisons = as.character(Antidote.for.Poisons)) %>%
    mutate(Antidote.for.Poisons = strsplit(Antidote.for.Poisons, ","))
  antidote.for.Poisons <<-  (mtabulate(df$Antidote.for.Poisons)) %>% 
                            select(-c("-","FALSE","NO")) %>%
                            mutate_all(funs(as.logical(.)))
  
  df <- cbind(df,mtabulate(df$Antidote.for.Poisons))
  df <- df %>% select(-c("Antidote.for.Poisons","-","FALSE","NO"))
}

