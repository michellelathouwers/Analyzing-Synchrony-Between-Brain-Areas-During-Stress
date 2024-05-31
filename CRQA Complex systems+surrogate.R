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
  f=read_csv(paste("C:Stroop-20231211T181911Z-001/Stroop/Stroop_sub_",toString(i),".csv", sep = ""))
  stroop_files_fp1[1] <- (f['2'])
  stroop_files_fp2[1] <- (f['31'])
  stroop_files_f7[1] <- (f['3'])
}

#extract relevant files Relax fp1, fp2 and f7

relax_files_fp1 <- c()
relax_files_fp2 <- c()
relax_files_f7 <- c()
test_file <- c()

for (i in 1:40){
  f=read_csv(paste("Relax-20231211T181913Z-001/Relax/Relax_sub_",toString(i),".csv", sep = ""))
  relax_files_fp1[1] <- (f['2'])
  relax_files_fp2[1] <- (f['31'])
  relax_files_f7[1] <- (f['3'])
}

#Load the delay and dimension for each hypothesis
#as.matrix solved formatting issues

h1_relax_lags <- as.matrix(read_csv("crqa/averages_rel_h1.csv")['Lag'])
h1_stroop_lags <- as.matrix(read_csv("crqa/averages_str_h1.csv")['Lag'])


h2_relax_lags <- as.matrix(read_csv("crqa/averages_rel_h2.csv")['Lag'])
h2_stroop_lags <- as.matrix(read_csv("crqa/averages_str_h2.csv")['Lag'])
  
h3_relax_lag <- 21
h3_stroop_lag <- 20
  
h1_relax_dims <- as.matrix(read_csv("crqa/averages_rel_h1.csv")['Dimension'])
h1_stroop_dims <- as.matrix(read_csv("crqa/averages_str_h1.csv")['Dimension'])
  
h2_relax_dims <- as.matrix(read_csv("crqa/averages_rel_h2.csv")['Dimension'])
h2_stroop_dims <- as.matrix(read_csv("crqa/averages_str_h2.csv")['Dimension'])
  
h3_relax_dim <- 11
h3_stroop_dim <- 11

#You will need to play with the radius until the RR values are between 2.5% - 5%. 
#That's why I would not suggest to run crqa on all participants, just for the sake of time investment
#However, the runtime is not very long
radius <- 100

print(CRQA_func(relax_files_fp1[[1]],relax_files_fp2[[1]],h1_relax_lags[1,1],h1_relax_dims[1,1],radius))

# Function to generate surrogate data
generate_surrogate <- function(data) {
  # Example: Shuffle the data
  return(sample(data))
}
surr_relax_files_fp1 <- generate_surrogate(relax_files_fp1)
surr_relax_files_fp2 <- generate_surrogate(relax_files_fp2)
surr_stroop_files_fp1 <- generate_surrogate(stroop_files_fp1)
surr_stroop_files_fp2 <- generate_surrogate(stroop_files_fp2)

observed_statistic_fp1 <- mean(stroop_files_fp1[[1]]) - mean(relax_files_fp1[[1]])
print(observed_statistic_fp1)
# Perform CRQA on original data
original_result <- CRQA_func(relax_files_fp1[[1]], relax_files_fp2[[1]], h1_relax_lags[1,1], h1_relax_dims[1,1], radius)
surr1_result <- CRQA_func(rand.surr.relax1[[1]], rand.surr.relax2[[1]], h1_relax_lags[1,1], h1_relax_dims[1,1], radius)


print(original_result)
print(relax_files_fp1)
# Extract time series data (assuming it's stored in relax_files_fp1[[1]] and relax_files_fp2[[1]])
time_series_1 <- relax_files_fp1[[1]]
time_series_2 <- relax_files_fp2[[1]]

# Compute sample entropy for the first time series
relax.SampEn_1 <- pracma::sample_entropy(time_series_1, edim=2, r=0.2*sd(time_series_1), tau=1)
print(paste("Sample Entropy for Time Series 1:", relax.SampEn_1))

# Compute sample entropy for the second time series
relax.SampEn_2 <- pracma::sample_entropy(time_series_2, edim=2, r=0.2*sd(time_series_2), tau=1)
print(paste("Sample Entropy for Time Series 2:", relax.SampEn_2))

set.seed(38472)
rand.surr.relax1 <- sample(time_series_1, replace = FALSE)
rand.surr.relax2 <- sample(time_series_2, replace = FALSE)

rand.surr.relax1.SampEn <- pracma::sample_entropy(rand.surr.relax1, edim=2,r=0.2*sd(rand.surr.relax1),tau=1)
print(rand.surr.relax1.SampEn)
rand.surr.relax2.SampEn <- pracma::sample_entropy(rand.surr.relax2, edim=2,r=0.2*sd(rand.surr.relax2),tau=1)
print(rand.surr.relax2.SampEn)

#Fast Fourier Transformer
FFT.surr.relax1 <- t(FFTsurrogate(time_series_1,n.samples=20))
FFT.surr.relax2 <- t(FFTsurrogate(time_series_2,n.samples=20))


plot(FFT.surr.relax1[,1], type='l', ylab="Beats Per Minute (BPM)", xlab = "Time", main = "AAFT Surrogate Heart Rate Data")

FFT.surr.relax1.SampEn.20 <- apply(FFT.surr.relax1, MARGIN=2, FUN = pracma::sample_entropy, edim=2,r=0.2*sd(FFT.surr.relax1),tau=1)
print(FFT.surr.relax1.SampEn.20)
FFT.surr.relax2.SampEn.20 <- apply(FFT.surr.relax2, MARGIN=2, FUN = pracma::sample_entropy, edim=2,r=0.2*sd(FFT.surr.relax2),tau=1)
print(FFT.surr.relax2.SampEn.20)

# Relax_files Calculate mean
mean1 <- mean(FFT.surr.relax1.SampEn.20)
mean2 <- mean(FFT.surr.relax2.SampEn.20)
mean1.1 <- mean(relax.SampEn_1)
mean2.2 <- mean(relax.SampEn_2)
# Calculate length
n1 <- length(FFT.surr.relax1.SampEn.20)
n2 <- length(FFT.surr.relax2.SampEn.20)
n1.1 <- length(relax.SampEn_1)
n2.2 <- length(relax.SampEn_2)
# Calculate standard deviation
sd1 <- sd(FFT.surr.relax1.SampEn.20)
sd2 <- sd(FFT.surr.relax2.SampEn.20)
sd1.1 <- sd(relax.SampEn_1)
sd2.2 <- sd(relax.SampEn_2)
# Calculate Standard Error
error1 <- qt(.975 + (1 - .975)/2, df = n1 - 1) * sd1/sqrt(n1)
error2 <- qt(.975 + (1 - .975)/2, df = n2 - 1) * sd2/sqrt(n2)
error1.1 <- qt(.975 + (1 - .975)/2, df = n1.1 - 1) * sd1.1/sqrt(n1.1)
error2.2 <- qt(.975 + (1 - .975)/2, df = n2.2 - 1) * sd2.2/sqrt(n2.2)
# FFT surrogate sample entropy
CI1 <- c(lower = mean1 - error1, mean = mean1, upper = mean1 + error1)
print(CI1)
CI2 <- c(lower = mean2 - error2, mean = mean2, upper = mean2 + error2)
print(CI2)
#original sample entropy
CI1.1 <- c(lower = mean1.1 - error1.1, mean = mean1.1, upper = mean1.1 + error1.1)
print(CI1.1)
print(relax.SampEn_1)
CI2.2 <- c(lower = mean2.2 - error2.2, mean = mean2.2, upper = mean2.2 + error2.2)
print(relax.SampEn_2)
print(CI2)

# Perform two-sample t-test
t_test_result <- t.test(FFT.surr.relax1.SampEn.20, FFT.surr.relax2.SampEn.20, alternative = "two.sided")

# Compute sample entropy for the first time series
stroop.SampEn_1 <- pracma::sample_entropy(stroop_files_fp1[[1]], edim=2, r=0.2*sd(stroop_files_fp1[[1]]), tau=1)
print(paste("Sample Entropy for Time Series 1:", stroop.SampEn_1))

# Compute sample entropy for the second time series
stroop.SampEn_2 <- pracma::sample_entropy(stroop_files_fp2[[1]], edim=2, r=0.2*sd(stroop_files_fp2[[1]]), tau=1)
print(paste("Sample Entropy for Time Series 2:", stroop.SampEn_2))

#Stroop_files
FFT.surr.stroop1 <- t(FFTsurrogate(stroop_files_fp1[[1]],n.samples=20))
FFT.surr.stroop2 <- t(FFTsurrogate(stroop_files_fp2[[1]],n.samples=20))

FFT.surr.stroop1.SampEn.20 <- apply(FFT.surr.stroop1, MARGIN=2, FUN = pracma::sample_entropy, edim=2,r=0.2*sd(FFT.surr.stroop1),tau=1)
print(FFT.surr.stroop1.SampEn.20)
FFT.surr.stroop2.SampEn.20 <- apply(FFT.surr.stroop2, MARGIN=2, FUN = pracma::sample_entropy, edim=2,r=0.2*sd(FFT.surr.stroop2),tau=1)
print(FFT.surr.stroop2.SampEn.20)

#Stroop_files Calculate mean
mean10 <- mean(FFT.surr.stroop1.SampEn.20)
mean20 <- mean(FFT.surr.stroop2.SampEn.20)
mean10.1 <- mean(stroop.SampEn_1)
mean20.2 <- mean(stroop.SampEn_2)

# Calculate length
n10 <- length(FFT.surr.stroop1.SampEn.20)
n20 <- length(FFT.surr.stroop2.SampEn.20)
n10.1 <- length(stroop.SampEn_1)
n20.2 <- length(stroop.SampEn_2)
# Calculate standard deviation
sd10 <- sd(FFT.surr.stroop1.SampEn.20)
sd20 <- sd(FFT.surr.stroop2.SampEn.20)
sd10.1 <- sd(stroop.SampEn_1)
sd20.2 <- sd(stroop.SampEn_2)
# Calculate Standard Error
error10 <- qt(.975 + (1 - .975)/2, df = n1 - 1) * sd1/sqrt(n1)
error20 <- qt(.975 + (1 - .975)/2, df = n2 - 1) * sd2/sqrt(n2)
error10.1 <- qt(.975 + (1 - .975)/2, df = n1.1 - 1) * sd1.1/sqrt(n1.1)
error20.2 <- qt(.975 + (1 - .975)/2, df = n2.2 - 1) * sd2.2/sqrt(n2.2)
# FFT surrogate sample entropy
CI10 <- c(lower = mean10 - error10, mean = mean10, upper = mean10 + error10)
print(CI10)
print(stroop.SampEn_1)
CI20 <- c(lower = mean20 - error20, mean = mean20, upper = mean20 + error20)
print(CI20)
print(stroop.SampEn_2)



# Print results
print(t_test_result)

#Participant shuffling


#Suggested loop H1. Problem is that you would have to run it with a fixed radius and cannot adapt the radius
#Therefore I recommend against using this loop.
#for (i in 1:40){
#  print("crqa results participant ", i)
#  print(CRQA_func(relax_files_fp1[[1]],relax_files_f7[[1]],h1_relax_lags[i,1],h1_relax_dims[i,1],radius))
#}

#H1 example with participant 1. Double brackets for the files, [i,1] for getting the lag and dimension

