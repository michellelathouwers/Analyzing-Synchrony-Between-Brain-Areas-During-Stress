library(crqa)
library(readr)
library(pracma)
library(tseriesChaos)
library(nonlinearTseries)
library(tseriesChaos)

#might need to change the paths to your needs

#files are: stroop/relax_files_fp1 or stroop/relax_files_fp2 or stroop/relax_files_f7 <- c(), 
#depending on what combination suits the hypothesis
#radius must be set manually and good radius gives RR rate between 0.025-0.05

CRQA_func <- function(file1, file2, delay, emb, radius){
  delay = delay; embed = emb;
  rescale = 0; radius = radius;
  normalize = 0; mindiagline = 2; minvertline = 2;
  tw = 0; whiteline = FALSE; recpt = FALSE; side = "both"
  method = 'crqa'; metric = 'euclidean';
  datatype = "continuous"
  
  ans <- crqa(file1,
              file2,delay, embed, rescale, radius,
              normalize,mindiagline, minvertline, tw, whiteline,
              recpt, side, method, metric, datatype)
  return(ans)
}


#Relax_path <- "C:/Users/carlv/Complex Systems/Relax-20231211T181913Z-001/Relax"
#Stroop_path <- "C:/Users/carlv/Complex Systems/Stroop-20231211T181911Z-001/Stroop"

#Stroop_files <- list()
#Relax_files <- list()
#for (i in 1:40){
#  new_stroop_file = paste(Stroop_path, "/Stroop_sub_", as.character(i) , ".csv", sep = "")
#  new_relax_file = paste(Relax_path, "/Relax_sub_", as.character(i) , ".csv", sep = "")
#  Stroop_files <- append(Stroop_files, new_stroop_file)
#  Relax_files<- append(Relax_files, new_relax_file)
#}
#test_data1 <- Stroop_files[[1]]
#read_data1 <- read.csv(test_data1)

#test_data2 <- Stroop_files[[2]]
#read_data2 <- read.csv(test_data2)

#head(read_data1)
#head(read_data2)

#test1_ch1 <- read_data1[,2]
#test2_ch1 <- read_data2[,2]




#extract relevant files Stroop fp1, fp2 and f7
stroop_files_fp1 <- c()
stroop_files_fp2 <- c()
stroop_files_f7 <- c()

for (i in 1:40){
  f=read_csv(paste("C:/Users/carlv/Complex Systems/Stroop-20231211T181911Z-001/Stroop/Stroop_sub_",toString(i),".csv", sep = ""))
  stroop_files_fp1 <- append(stroop_files_fp1,f['2'])
  stroop_files_fp2 <- append(stroop_files_fp2,f['31'])
  stroop_files_f7 <- append(stroop_files_f7,f['3'])
}

#extract relevant files Relax fp1, fp2 and f7

relax_files_fp1 <- c()
relax_files_fp2 <- c()
relax_files_f7 <- c()

for (i in 1:40){
  f=read_csv(paste("C:/Users/carlv/Complex Systems/Relax-20231211T181913Z-001/Relax/Relax_sub_",toString(i),".csv", sep = ""))
  relax_files_fp1 <- append(relax_files_fp1,f['2'])
  relax_files_fp2 <- append(relax_files_fp2,f['31'])
  relax_files_f7 <- append(relax_files_f7,f['3'])
}
print(relax_files_fp1[[1]])
#Load the delay and dimension for each hypothesis
#as.matrix solved formatting issues

h1_relax_lags <- as.matrix(read_csv("C:/Users/carlv/Complex Systems/csvfiles_crqa/averages_rel_h1.csv")['Lag'])
h1_stroop_lags <- as.matrix(read_csv("C:/Users/carlv/Complex Systems/csvfiles_crqa/averages_str_h1.csv")['Lag'])


h2_relax_lags <- as.matrix(read_csv("C:/Users/carlv/Complex Systems/csvfiles_crqa/averages_rel_h2.csv")['Lag'])
h2_stroop_lags <- as.matrix(read_csv("C:/Users/carlv/Complex Systems/csvfiles_crqa/averages_str_h2.csv")['Lag'])
  
h3_relax_lag <- 21
h3_stroop_lag <- 20
  
h1_relax_dims <- as.matrix(read_csv("C:/Users/carlv/Complex Systems/csvfiles_crqa/averages_rel_h1.csv")['Dimension'])
h1_stroop_dims <- as.matrix(read_csv("C:/Users/carlv/Complex Systems/csvfiles_crqa/averages_str_h1.csv")['Dimension'])
  
h2_relax_dims <- as.matrix(read_csv("C:/Users/carlv/Complex Systems/csvfiles_crqa/averages_rel_h2.csv")['Dimension'])
h2_stroop_dims <- as.matrix(read_csv("C:/Users/carlv/Complex Systems/csvfiles_crqa/averages_str_h2.csv")['Dimension'])
  
h3_relax_dim <- 11
h3_stroop_dim <- 11

#You will need to play with the radius until the RR values are between 2.5% - 5%. 
#That's why I would not suggest to run crqa on all participants, just for the sake of time investment
#However, the runtime is not very long
radius <- 100
#specify participant
part <- 1
ans<-CRQA_func(relax_files_fp1[[part]],relax_files_fp2[[part]],h1_relax_lags[part,1],h1_relax_dims[part,1],radius)
print(ans$RR)

radii_h1 <- c(100,)
radii_h2 <- c()
radii_h3 <- c()
#Suggested loop H1. Problem is that you would have to run it with a fixed radius and cannot adapt the radius
#Therefore I recommend against using this loop.
#for (i in 1:40){
#  print("crqa results participant ", i)
#  print(CRQA_func(relax_files_fp1[[1]],relax_files_f7[[1]],h1_relax_lags[i,1],h1_relax_dims[i,1],radius))
#}

#H1 example with participant 1. Double brackets for the files, [i,1] for getting the lag and dimension