#01_Extract_Raw_to_mzXML_Parallel.R

#Remember, must be run in Windows
#Set working directory to where the files are
setwd("D:/6a_TLE_ESI/")

#Assumes Raw files are in a folder named "Raw" within WD

#Creates 3 new folders within WD:
#  mzXML_ms1_two_mode/ (initial convert raw to mzXML)
#  mzXML_pos/          (extracted positive mode data)
#  mzXML_neg/          (extracted negative mode data)

library(parallel)
library(BiocParallel)

useful_cores <- detectCores()-1
param <- SnowParam(workers = useful_cores)
register(param)

#Get a list of all the raw file names
raw_files <- list.files("Raw")
sample_names <- gsub(pattern = ".raw", replacement = "", raw_files)

extractEverything <- function(file_name) {
  system(paste0("msconvert Raw/", file_name, ".raw --mzXML --filter \"peakPicking true 1-\" -o mzXML_two_mode -v"))
  system(paste0("msconvert mzXML_two_mode/", file_name, ".mzXML --mzXML --filter \"polarity positive\" -o mzXML_pos -v"))
  system(paste0("msconvert mzXML_two_mode/", file_name, ".mzXML --mzXML --filter \"polarity negative\" -o mzXML_neg -v"))
  print(paste("File", file_name, "complete"))
}

# init <- Sys.time()
# lapply(X = sample_names, FUN = extractEverything)
# print(Sys.time()-init)
# Time difference: 2.4 mins
# Time difference: 1.4 mins
# Time difference: 1.4 mins

init <- Sys.time()
bplapply(X = sample_names, FUN = extractEverything)
print(Sys.time()-init)
# Time difference of 1.05 mins
# Time difference of 0.98 mins
# Time difference of 0.95 mins