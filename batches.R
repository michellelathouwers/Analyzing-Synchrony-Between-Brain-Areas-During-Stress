Relax_path <- "C:/Users/Daan-/Downloads/Relax-20231211T181913Z-001/Relax"
Stroop_path <- "C:/Users/Daan-/Downloads/Stroop-20231211T181911Z-001/Stroop"
Stroop_files <- list()
Relax_files <- list()
for (i in 1:40){
  new_stroop_file = paste(Stroop_path, "/Stroop_sub_", as.character(i) , ".csv", sep = "")
  new_relax_file = paste(Relax_path, "/Relax_sub_", as.character(i) , ".csv", sep = "")
  Stroop_files <- append(Stroop_files, new_stroop_file)
  Relax_files<- append(Relax_files, new_relax_file)
}

#For hypothesis 1: same electrodes, same participants (every channel for pre-frontal cortex) (1 for every category)
# If more participants needed just change number behind the Stroop_files and Relax_files in line 15 and 18, now its just getting all the pre-frontal cortex electrodes from participant 1 in both stroop and relax 

Stroop_participant <- Stroop_files[[1]]
read_data1 <- read.csv(Stroop_participant)

Relax_participant <- Relax_files[[1]]
read_data2 <- read.csv(Relax_participant)

Stroop_Fp1 <- read_data1[,4]
Stroop_Fp2 <- read_data1[,33]
Stroop_F7 <- read_data1[,5]
Stroop_F3 <- read_data1[,6]
Stroop_Fz <- read_data1[,3]
Stroop_F4 <- read_data1[,31]
Stroop_F8 <- read_data1[,32]

Relax_Fp1 <- read_data2[,4]
Relax_Fp2 <- read_data2[,33]
Relax_F7 <- read_data2[,5]
Relax_F3 <- read_data2[,6]
Relax_Fz <- read_data2[,3]
Relax_F4 <- read_data2[,31]
Relax_F8 <- read_data2[,32]

#For hypothesis 2: same electrodes, same participants (every channel for pre-frontal cortex, and the same amount of other randomly spread) (1 for every category)
# Since I already got the pre-frontal cortex ones from the previous bit of code, I will just add the randomly selected other electrodes here
# If more participants needed just change number behind the Stroop_files and Relax_files in line 15 and 18, now its just getting all the pre-frontal cortex electrodes from participant 1 in both stroop and relax 

Stroop_FC5 <- read_data1[,9]
Stroop_C4 <- read_data1[,29]
Stroop_T7 <- read_data1[,11]
Stroop_CP1 <- read_data1[,13]
Stroop_Pz <- read_data1[,18]
Stroop_O2 <- read_data1[,20]
Stroop_TP10 <- read_data1[,21]

Relax_FC5 <- read_data2[,9]
Relax_C4 <- read_data2[,29]
Relax_T7 <- read_data2[,11]
Relax_CP1 <- read_data2[,13]
Relax_Pz <- read_data2[,18]
Relax_O2 <- read_data2[,20]
Relax_TP10 <- read_data2[,21]

#For hypothesis 3: different participants, same electrodes (every channel for pre-frontal cortex, and the same amount of other randomly spread) 
# If more participants needed just change number behind the Stroop_files and Relax_files in line 15 and 18, now its just getting all the pre-frontal cortex electrodes from participant 1 in both stroop and relax 
# Now only 2 participants are being compared 

Stroop_participant <- Stroop_files[[1]]
read_data1 <- read.csv(Stroop_participant)

Stroop_participant2 <- Stroop_files[[2]]
read_data2 <- read.csv(Stroop_participant2)

Stroop_Fp1 <- read_data1[,4]
Stroop_Fp2 <- read_data1[,33]
Stroop_F7 <- read_data1[,5]
Stroop_F3 <- read_data1[,6]
Stroop_Fz <- read_data1[,3]
Stroop_F4 <- read_data1[,31]
Stroop_F8 <- read_data1[,32]

Stroop_FC5 <- read_data1[,9]
Stroop_C4 <- read_data1[,29]
Stroop_T7 <- read_data1[,11]
Stroop_CP1 <- read_data1[,13]
Stroop_Pz <- read_data1[,18]
Stroop_O2 <- read_data1[,20]
Stroop_TP10 <- read_data1[,21]

Stroop2_Fp1 <- read_data2[,4]
Stroop2_Fp2 <- read_data2[,33]
Stroop2_F7 <- read_data2[,5]
Stroop2_F3 <- read_data2[,6]
Stroop2_Fz <- read_data2[,3]
Stroop2_F4 <- read_data2[,31]
Stroop2_F8 <- read_data2[,32]

Stroop2_FC5 <- read_data2[,9]
Stroop2_C4 <- read_data2[,29]
Stroop2_T7 <- read_data2[,11]
Stroop2_CP1 <- read_data2[,13]
Stroop2_Pz <- read_data2[,18]
Stroop2_O2 <- read_data2[,20]
Stroop2_TP10 <- read_data2[,21]

Relax_participant <- Relax_files[[1]]
read_data3 <- read.csv(Relax_participant)

Relax_participant2 <- Relax_files[[2]]
read_data4 <- read.csv(Relax_participant2)

Relax_Fp1 <- read_data3[,4]
Relax_Fp2 <- read_data3[,33]
Relax_F7 <- read_data3[,5]
Relax_F3 <- read_data3[,6]
Relax_Fz <- read_data3[,3]
Relax_F4 <- read_data3[,31]
Relax_F8 <- read_data3[,32]

Relax_FC5 <- read_data3[,9]
Relax_C4 <- read_data3[,29]
Relax_T7 <- read_data3[,11]
Relax_CP1 <- read_data3[,13]
Relax_Pz <- read_data3[,18]
Relax_O2 <- read_data3[,20]
Relax_TP10 <- read_data3[,21]

Relax2_Fp1 <- read_data4[,4]
Relax2_Fp2 <- read_data4[,33]
Relax2_F7 <- read_data4[,5]
Relax2_F3 <- read_data4[,6]
Relax2_Fz <- read_data4[,3]
Relax2_F4 <- read_data4[,31]
Relax2_F8 <- read_data4[,32]

Relax2_FC5 <- read_data4[,9]
Relax2_C4 <- read_data4[,29]
Relax2_T7 <- read_data4[,11]
Relax2_CP1 <- read_data4[,13]
Relax2_Pz <- read_data4[,18]
Relax2_O2 <- read_data4[,20]
Relax2_TP10 <- read_data4[,21]

library(crqa)
library(readr)
library(pracma)
library(tseriesChaos)

CRQA_func <- function(file1, file2){
  optParams <- optimizeParam(file1, file2, par , min.rec = 2, max.rec = 5)
  
  print(unlist(optParams))
  
  delay = optParams$delay; embed = optParams$emddim;
  rescale = 0; radius = optParams$radius;
  normalize = 0; mindiagline = 2; minvertline = 2;
  tw = 0; whiteline = FALSE; recpt = FALSE; side = "both"
  method = 'crqa'; metric = 'euclidean';
  datatype = "continuous"
  
  ans <- crqa(file1,
              file2,delay, embed, rescale, radius,
              normalize,mindiagline, minvertline, tw, whiteline,
              recpt, side, method, metric, datatype)
  print(ans)
}

#change input files based on hypothesis
test_ans <- CRQA_func(test1_ch1, test2_ch1)