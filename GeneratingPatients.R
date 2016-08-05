#Sampling age
sampleAge = function(n) { 
  ages<- c(60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102)
  distribution<- c(10, 19, 16, 19, 23, 30, 28, 34, 29, 36, 26, 37, 40, 49, 53, 58, 52, 48, 60, 85, 67, 80, 84, 78, 92, 75, 98, 100, 73, 79, 69, 57, 31, 29, 29, 19, 11, 12, 5, 7, 5, 2, 2);
  distribution<- distribution/sum(distribution); #Calculating probability
  sample(x = ages, n, replace = TRUE, prob = distribution) 
}

#Sampling the fragility
sampleFragility = function(n) { 
  fragilityHistory<- c(1,2) # 1-> Yes, 2-> No Fragility
  distribution<- c(0.5,0.5);
  sample(x = fragilityHistory, n, replace = TRUE, prob = distribution) 
}

#Sampling the fracture type
sampleFracType = function(n) { 
  fracType<- c(1,2,3,4) #1-> Intracapsular-Displaced, 2->Intracapsular-Undisplaced, 3->Intertrochanteric, 4->Subtrochanteric
  distribution<- c(593,171,697,194);
  distribution<- distribution/sum(distribution);# 1655 out of 1856, where  201 records have no recorded fracture type
  sample(x = fracType, n, replace = TRUE, prob = distribution) 
}

#Sampling the hospital accorindg to the CHO
sampleHospital = function(n,cho) { 
  if(cho=="CHO1")
  { 
  hospital<- c(500,501)# ID of the host hospital
  distribution<- c(117,54);
  }
  else if(cho=="CHO2")
  { 
    hospital<- c(802,802)# ID of the host hospital
    distribution<- NULL;
  }
  else if(cho=="CHO3")
  { 
    hospital<- c(300,300)# ID of the host hospital
    distribution<- NULL;
  }
  else if(cho=="CHO4")
  { 
    hospital<- c(300,724,726)# ID of the host hospital
    distribution<- c(100,25,3);
  }
  else if(cho=="CHO5")
  { 
    hospital<- c(600,600)# ID of the host hospital
    distribution<- NULL;
  }
  else if(cho=="CHO6")
  { 
    hospital<- c(910,910)# ID of the host hospital
    distribution<- NULL;
  }
  else if(cho=="CHO7")
  { 
    hospital<- c(904,1270)# ID of the host hospital
    distribution<- c(72,144);
  }
  else if(cho=="CHO8")
  { 
    hospital<- c(203,922)# ID of the host hospital
    distribution<- c(165,257);
  }
  else if(cho=="CHO9")
  { 
    hospital<- c(108,908,923)# ID of the host hospital
    distribution<- c(93,59,8);
  }
  
  if(is.null(distribution)==FALSE){ distribution<- distribution/sum(distribution);}
   
  sample(x = hospital, n, replace = TRUE, prob = distribution)
}

#Sampling the area of residence
sampleResArea = function(n,cho) { 
  if(cho=="CHO1")
  { 
    ResArea<- c(2100,200,1900,3305,3311,100,3310,2000,2600,2700,2800)# ID of the area of residence
    distribution<- c(1,1,1,1,1,2,2,4,18,26,114);
  }
  else if(cho=="CHO2")
  { 
    ResArea<- c(107,109,1900,3501,2000,3303,2100)# ID of the area of residence
    distribution<- c(1,1,1,1,2,3,41);
  }
  else if(cho=="CHO3")
  { 
    ResArea<- c(1000,100,200,3100,3501,800,1700,1401,1500,1600)# ID of the area of residence
    distribution<- c(1,1,1,1,1,4,34,38,69,73);
  }
  else if(cho=="CHO4")
  { 
    ResArea<- c(3303,1300,1101,1200,1700,1401,1500,1600)# ID of the area of residence
    distribution<- c(1,3,8,16,34,38,69,73);
  }
  else if(cho=="CHO5")
  { 
    ResArea<- c(200,212,216,300,1200,2500,2700,3307,3501,1700,400,3303,500,901,1000,700,800,600)# ID of the area of residence
    distribution<- c(1,1,1,1,1,1,1,1,1,2,3,6,29,29,35,56,76,100);
  }
  else if(cho=="CHO6")
  { 
    ResArea<- c(100,103,105,109,208,1500,2100,2700,3303,3501,202,600,217,216,206,218,214,204,400,200)# ID of the area of residence
    distribution<- c(1,1,1,1,1,1,1,1,1,1,3,3,10,11,13,15,15,21,63,77)
  }
  else if(cho=="CHO7")
  { 
    ResArea<- c(901,1500,1600,1900,2100,2500,3200,3303,3320,3501,3100,500,2300,220,202,200,210,400,214,222,208,224,206,216,212,300)# ID of the area of residence
    distribution<- c( 1,1,1,1,1,1,1,1,1,1,2,2,2,2,3,4,7,9,10,14,15,16,17,18,25,60)
  }
  else if(cho=="CHO8")
  { 
    ResArea<- c(200,500,1500,2100,2600,3303,3305,3310,100,300,1700,2200,2400,2500,3000,2300,2900,3200,3100)# ID of the area of residence
    distribution<- c(1,1,1,1,1,1,1,1,4,5,6,31,34,34,42,52,58,66,82)
  }
  else if(cho=="CHO9")
  { 
    ResArea<- c(210,400,800,2000,2200,2600,2800,3303,3311,208,101,105,220,200,100,109,103,300,115,3200,107,111)# ID of the area of residence
    distribution<- c(1,1,1,1,1,1,1,1,1,2,3,4,4,5,6,11,11,15,18,21,25,26)
    }
  
  if(is.null(distribution)==FALSE){ distribution<- distribution/sum(distribution);}
  sample(x = ResArea, n, replace = TRUE, prob = distribution)
}

#Sampling the diagnosis
sampleDiagnosis= function(n) { 
  diagnosisID<- c("S7205","S7203","S7211","S7200","S7210","S7201","S722","S7204","S729","Z491","S7240","S066","S681","S723","R296","S4221","S023","N179","I500","M8445","T856","S5250","S7208","M7966","I212","R568","S7243","J440","R000","K297","I638","T840","S798","M5496","K294","M8094","I639","T842","S8282","S923","C9000","I460","S4220","I214","J189","T1408","N390","R53","M161","E86","S7241","S324","S7300","M8095","S5251","H353","E1171","M8435","C61","I620","M7965","I48")
  distribution<- c(17, 210,529,612,43,182,90,28,13, 1,18,1,1,21,1,1,1,1,2,5,1,2,25,1,1,2,3,2,1,1,1,1,2,1,1,1,2,1,1,1,1,1,1,2,1,1,3,1,3,1,1,1,1,2,1,1,1,1,1,1,1,1);
  distribution<- distribution/sum(distribution);# 1655 out of 1856, where  201 records have no recorded fracture type
  sample(x = diagnosisID, n, replace = TRUE, prob = distribution) 
}

GeneratePatients= function(nPatients,patientSex,CHO,year) { 
#1->Male
#2->Female
   data.frame(Hospital=sampleHospital(nPatients,CHO),Sex=patientSex,ResID=sampleResArea(nPatients,CHO),Age=sampleAge(nPatients),Diag1=sampleDiagnosis(nPatients),FracType=sampleFracType(nPatients),Fragility=sampleFragility(nPatients),CHO=CHO,Year=year)
}

projections<-read.csv("E:\\corrected.csv")

RunExperiment= function(experimentNo) {
  generatedPatients <-  data.frame()
  for(n in 1:length(projections$Year)) #Running simulation experiments
    {
   #CHO1
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO1_Males[n],patientSex=1,"CHO1",projections$Year[n])) 
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO1_Females[n],patientSex=2,"CHO1",projections$Year[n])) 
    #CHO2
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO2_Males[n],patientSex=1,"CHO2",projections$Year[n])) 
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO2_Females[n],patientSex=2,"CHO2",projections$Year[n]))  
    #CHO3
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO3_Males[n],patientSex=1,"CHO3",projections$Year[n])) 
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO3_Females[n],patientSex=2,"CHO3",projections$Year[n]))   
    #CHO4
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO4_Males[n],patientSex=1,"CHO4",projections$Year[n])) 
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO4_Females[n],patientSex=2,"CHO4",projections$Year[n]))   
    #CHO5
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO5_Males[n],patientSex=1,"CHO5",projections$Year[n])) 
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO5_Females[n],patientSex=2,"CHO5",projections$Year[n]))   
    #CHO6
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO6_Males[n],patientSex=1,"CHO6",projections$Year[n])) 
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO6_Females[n],patientSex=2,"CHO6",projections$Year[n]))  
    #CHO7
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO7_Males[n],patientSex=1,"CHO7",projections$Year[n])) 
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO7_Females[n],patientSex=2,"CHO7",projections$Year[n]))   
    #CHO8
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO8_Males[n],patientSex=1,"CHO8",projections$Year[n])) 
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO8_Females[n],patientSex=2,"CHO8",projections$Year[n]))  
    #CHO9
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO9_Males[n],patientSex=1,"CHO9",projections$Year[n])) 
    generatedPatients<-rbind(generatedPatients,GeneratePatients( projections$CHO9_Females[n],patientSex=2,"CHO9",projections$Year[n])) 
  }
  generatedPatients$ExperimentNO<-experimentNo
  return (generatedPatients)
}

RunSimulation= function(n) {
  experiments <-  data.frame()
  for(n in 1:n) #Running simulation experiments
  {
    experiments<- rbind(experiments,RunExperiment(n))
  }
  write.csv(experiments,"E:\\GeneratedPatients.csv")
}

