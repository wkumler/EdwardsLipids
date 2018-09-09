# prep_TLE_pos.R
#
################ Caveats and prerequisites #############
#
# Presumes user has installed the R packages "xcms", "CAMERA", "tools", "IPO", with all required dependencies
#
# If multicore tasking is desired, "snowfall" also required; Rmpi doesn't seem to be necessary
#
# This script the following inputs:
#
#  1. A series of .mzXML files from the same dataset, containing centroided ms1 data 
#     of a single ion mode. File conversion from the Thermo .raw format, centroiding of data, 
#     and extraction of + and - mode scans into separate files can be accomplished in batch 
#     using the script "Exactive_full_scan_process_ms1+.r", available from 
#     https://github.com/vanmooylipidomics/LipidomicsToolbox. The .mzXML files should be placed 
#     together in a single directory, which can be specified by the user below.
#
#  2. If the package IPO was previously used to optimize xcms peak-picking or group/retcor 
#     parameters AND automatic import of the optimized settings from an existing .csv file 
#     is desired, specification of the path to file "IPO_xcmsparamfits_ ... .csv," where ... 
#     is an ISO 8601 timestamp. A suitable .csv file will be generated if the user elects IPO 
#     at two user-input points in this script, or such a file can be generated from IPO using 
#     the helper script optim_centWaveParams_standalone.R, latest version at 
#     https://github.com/vanmooylipidomics/LipidomicsToolbox/blob/master/optim_centWaveParams_standalone.R

################ Initial setup and variable definition #############

# load required packages

library(tools)

library(xcms)

library(CAMERA)

library(rsm)

library(parallel)

library(IPO)

library(snow) # if multicore tasking is desired

library(BiocParallel)
register(bpstart(MulticoreParam(1)))

library(LOBSTAHS)

# ******************************************************************
################ Basic user begin editing here #############
# ******************************************************************

################ User: define locations of data files and database(s) #############
working_dir <- "/media/windows/Users/willi/Documents/Berkeley/Elab/SURFIN"
setwd(working_dir)

data_source <- "/media/wkumler/TheVault/6a_TLE_ESI" #Specify working directory for Ubuntu

mzXMLdirs <- c("/mzXML_pos", "/mzXML_neg")

# specify which of the directories above you wish to analyze this time through
chosenFileSubset = paste0(data_source, "/mzXML_pos/")

# specify the ID numbers (i.e., Orbi_xxxx.mzXML) of any files you don't want to push through xcms 
excluded.mzXMLfiles = NULL #leaving this blank (i.e. c() ) excludes all of them?

retcor.meth = "loess"
# retcor.meth = "obiwarp"

# ******************************************************************
################ Basic user stop editing here #############
# ******************************************************************

################# Define functions; run me first #############

# readinteger: for a given prompt, allows capture of user input as an integer; rejects non-integer input
readinteger = function(prompttext) {
  
  n = readline(prompt=prompttext)
  
  if (!grepl("^[0-9]+$", n)) {
    
    return(readinteger(prompttext))
    
  }
  
  as.integer(n)
  
}

# readyesno: for a given prompt, allows capture of user input as y or n; rejects other input
readyesno = function(prompttext) {
  
  n = readline(prompt=prompttext)
  
  if (!grepl("y|n", n)) {
    
    return(readyesno(prompttext))
    
  }
  
  as.character(n)
  
}

# verifyFileIonMode: return the ion mode of data in a particular mzXML file, 
# by examining "polarity" attribute of each scan in the file
verifyFileIonMode = function(mzXMLfile) {
  
  rawfile = xcmsRaw(mzXMLfile) # create an xcmsraw object out of the first file
  
  # determine ion mode by examining identifier attached to scan events
  
  if (table(rawfile@polarity)["negative"]==0 & (table(rawfile@polarity)["positive"]==length(rawfile@scanindex))) { 
    
    filepolarity = 1 # positive
    
  } else if (table(rawfile@polarity)["positive"]==0 & (table(rawfile@polarity)["negative"]==length(rawfile@scanindex))) { 
    
    filepolarity = -1 # negative
    
  } else if (table(rawfile@polarity)["positive"]>=1 & table(rawfile@polarity)["negative"]>=1) { 
    
    stop("At least one file in the current dataset contains scans of more than one ion mode. 
         Please ensure data for different ion modes have been extracted into separate files. Stopping...") 
    
  } else if (table(rawfile@polarity)["positive"]==0 & table(rawfile@polarity)["negative"]==0) {
    
    stop("Can't determine ion mode of data in the first file. Check manner in which files were converted. Stopping...") 
    
  }
  
  filepolarity
  
}

# getSubsetIonMode: return the ion mode of a subset of files, using sapply of verifyFileIonMode
getSubsetIonMode = function(mzXMLfilelist) {
  
  ionmodecount = sum(sapply(mzXMLfilelist, verifyFileIonMode)) # get sum of ion mode indicators for the files in the subset
  
  if (ionmodecount==length(mzXMLfilelist)) { # can conclude that all files contain positive mode data
    
    subset.polarity = "positive"
    
  } else if (ionmodecount==-length(mzXMLfilelist)) { # can conclude that all files contain negative mode data
    
    subset.polarity = "negative"
    
  }
  
  subset.polarity
  
}

# selectXMLSubDir: allows user to choose which subset of files to process
selectXMLSubDir = function(mzXMLdirList) {
  
  print(paste0("mzXML files exist in the following directories:"))
  
  for (i in 1:length(mzXMLdirList)) {
    
    # get number of mzXML files in this directory
    numGoodFiles = length(list.files(mzXMLdirList[i], recursive = TRUE, full.names = TRUE, pattern = "*(.mzXML|.mzxml)"))
    
    if (numGoodFiles>0) { # there are .mzXML data files in this directory
      
      print(paste0(i, ". ", numGoodFiles," .mzXML files in directory '",mzXMLdirList[i],"'"))
      
    }
    
  }
  
  processDecision = readinteger("Specify which subset you'd like to process, using integer input: ")
  
  mzXMLdirList[processDecision]
  
}

# getFNmatches: returns index(es) of file names in a given file list containing the ID numbers in a match list
getFNmatches = function(filelist,IDnumlist) {
  
  unique(grep(paste(IDnumlist,collapse="|"),filelist, value=FALSE))
  
}

# genTimeStamp: generates a timestamp string based on the current system time
genTimeStamp = function () {
  
  output_DTG = format(Sys.time(), "%Y-%m-%dT%X%z") # return current time in a good format
  output_DTG = gsub(" ", "_", output_DTG) # replace any spaces
  output_DTG = gsub(":", "-", output_DTG) # replaces any colons with dashes (Mac compatibility)
  
}


################# Load in mzXML files, get xcms settings from IPO or user input #############
# check to make sure user has specified at least something in mzXMLdirs
if (!exists("mzXMLdirs")) {
  
  stop("User has not specified any directories containing mzXML files. Specify a value for mzXMLdirs.")
  
}


# load selected subset for processing
mzXMLfiles.raw = list.files(chosenFileSubset, recursive = TRUE, full.names = TRUE)

# verify the ion mode of the data in these files
#WILLIAM IS CHEATING HERE, you should actually run the full thing on a new data set
#subset.polarity = getSubsetIonMode(mzXMLfiles.raw)
subset.polarity = "positive"

# provide some feedback to user
print(paste0("Loaded ",length(mzXMLfiles.raw)," mzXML files. These files contain ",
             subset.polarity," ion mode data. Raw dataset consists of:"))

print(mzXMLfiles.raw)

# check whether user has elected to exclude any files, and exclude them if they happen to be in this subset
if (exists("excluded.mzXMLfiles") & length(excluded.mzXMLfiles)>0) { #fixed this code by removing quotes
  
  excludedfiles = getFNmatches(IDnumlist = excluded.mzXMLfiles, filelist = mzXMLfiles.raw) # index files to be excluded
  
  print(paste0("The following files will be excluded from processing based on user's input:"))
  print(mzXMLfiles.raw[excludedfiles])
  
  mzXMLfiles = mzXMLfiles.raw[-excludedfiles] # exclude the files from mzXMLfiles
  
} else {
  
  mzXMLfiles = mzXMLfiles.raw
  
}

#####################################################################################
######## Peak-picking & creation of xcmsSet using xcms (and IPO, if desired) ########
#####################################################################################

print(paste0("Using values of centWave parameters specified in the script by user..."))

# "non-optimized" settings listed here are based on recommended "HPLC/Orbitrap settings" 
# from Table 1 of Patti et al., 2012, "Meta-analysis of untargeted metabolomic data from 
# multiple profiling experiment," Nature Protocols 7: 508-516

centW.min_peakwidth = 10
centW.max_peakwidth = 45 # lowered from Patti et al. recommended HPLC setting of 60 based 
                         #on visual inspection of a single sample with plotPeaks
centW.ppm = 2.5
centW.mzdiff = 0.005
centW.snthresh = 10
centW.prefilter = c(3,7500) # 3.5k recommended by Patti et al. appears to be too low
centW.noise = 500


# specify some additional settings we wish to keep constant, regardless of where the parameters above were obtained

centW.fitgauss = TRUE
centW.sleep = 1
centW.mzCenterFun = c("wMean")
centW.verbose.columns = TRUE
centW.integrate = 1
centW.profparam = list(step=0.01) # setting this very low, per Jan Stanstrup; low setting uses more memory but helps 
                                  # avoid the situation where mass accuracy eclipses the actual width of the m/z 
                                  #windows used to define each peak 
                                  #(a real possibility with Orbitrap data; see 
                                  #http://metabolomics-forum.com/viewtopic.php?f=8&t=598#p1853)
centW.nSlaves = 4 # if you have r package "snow" installed, can set to number of cores you wish to make use of

################# Create xcmsSet using selected settings #############

print(paste0("Creating xcmsSet object from ",length(mzXMLfiles),
             " mzXML files remaining in dataset using specified settings..."))

# create xcms xset object; runs WAY faster with multicore tasking enabled;

xset_centWave = xcmsSet(mzXMLfiles,
                        method = "centWave",
                        profparam = centW.profparam,
                        ppm = centW.ppm,
                        peakwidth = c(centW.min_peakwidth,centW.max_peakwidth),
                        fitgauss = centW.fitgauss,
                        noise = centW.noise,
                        mzdiff = centW.mzdiff,
                        verbose.columns = centW.verbose.columns,
                        snthresh = centW.snthresh,
                        integrate = centW.integrate,
                        prefilter = centW.prefilter,
                        mzCenterFun = centW.mzCenterFun,
                        #                 sleep = centW.sleep
                        BPPARAM = bpparam()
)

print(paste0("xcmsSet object xset_centWave created:"))

print(xset_centWave)
save(xset_centWave, file = "xset_CentWave")
#Conclude xcmsSet object creation, saved as xset_CentWave


#Begin retention time and grouping with xset_CentWave
load("xset_CentWave")
# Some notes:
#
#  1. If using massifquant or centWave and you are sure your input data are centroided, can ignore warning message 
#     "It looks like this file is in profile mode. [method] can process only centroid mode data !" since this is 
#     just based on a heuristic. That is, you can ignore the message if you are certain data are in centroid mode.
#     You can verify this by opening one of your converted .mzXML files in a text reader. You should see: 
#     <dataProcessing centroided="1"></dataProcessing> (a "0" is bad)
#
#     For more on this error, see http://metabolomics-forum.com/viewtopic.php?f=8&t=267 or
#                            https://groups.google.com/forum/#!topic/xcms/xybDDQTaQiY
#
#  2. So long as the number of peak data insertion problems is relatively low (i.e., < 100), 
#     you can safely ignore the error. Otherwise, might try lowering the ppm
#
#  3. On-the-fly plotting features (i.e., with sleep ≥ 0.001 enabled) don't appear to 
#     function properly in Mac RStudio

#####################################################################################
##### Grouping and retention time correction using xcms (and IPO, if desired) #######
#####################################################################################

print(paste0("Using values of group and retcor parameters specified in the script by user..."))

# retcor.loess settings below are the function defaults

loess.missing = 1
loess.extra = 1
loess.smoothing = "loess"
loess.span = c(0.2)
loess.family = "gaussian" # want to leave outliers in for the time being

# retcor.obiwarp settings below are the function defaults

obiwarp.center = NULL
obiwarp.profStep = 1
obiwarp.response = 1
obiwarp.distFunc = "cor_opt"
obiwarp.gapInit = NULL
obiwarp.gapExtend = NULL
obiwarp.factorDiag = 2
obiwarp.factorGap = 1
obiwarp.localAlignment = 0

# settings for group.density below are based on the recommended HPLC/Orbitrap settings 
# from Table 1 of Patti et al., 2012, "Meta-analysis of untargeted metabolomic data from 
# multiple profiling experiment," Nature Protocols 7: 508-516

density.bw = 5 # 15?
density.max = 50
density.minfrac = 0.25
density.minsamp = 2
density.mzwid = 0.015 # 0.001?

# specify some additional settings we wish to keep constant, regardless of where the parameters above were obtained

obiwarp.center = NULL
obiwarp.plottype = "deviation" # "none"
density.sleep = 0
loess.plottype = "mdevden" # none

################# Perform grouping and retention time correction on dataset #############

print(paste0("Performing grouping and retention time correction on dataset"))
print(paste0("Using group.density and retcor.",retcor.meth))

# initial grouping
# using method = "density" with settings from above

xset_gr = group(xset_centWave,
                method = "density",
                bw = density.bw,
                minfrac = density.minfrac,
                minsamp = density.minsamp,
                mzwid = density.mzwid,
                max = density.max,
                sleep = density.sleep
)
rm(xset_centWave)
# chromatographic alignment (retention time correction)

if (retcor.meth=="loess") {
  
  xset_gr.ret = retcor(xset_gr,
                       #                        method = "loess", # this appears unnecessary
                       missing = loess.missing,
                       extra = loess.extra,
                       smooth = "loess",
                       span = loess.span,
                       family = loess.family,
                       plottype = loess.plottype,
                       col = NULL,
                       ty = NULL
  )
  
} else if (retcor.meth=="obiwarp") {
  
  xset_gr.ret = retcor.peakgroups(xset_gr,
                                  method = "obiwarp",
                                  plottype = obiwarp.plottype,
                                  profStep = obiwarp.profStep,
                                  center = obiwarp.center,
                                  response = obiwarp.response,
                                  distFunc = obiwarp.distFunc,
                                  gapInit = obiwarp.gapInit,
                                  gapExtend = obiwarp.gapInit,
                                  factorDiag = obiwarp.factorDiag,
                                  factorGap = obiwarp.factorGap,
                                  localAlignment = obiwarp.localAlignment,
                                  initPenalty = 0
  )
  
}

# perform grouping again
rm(xset_gr)
print(paste0("Performing second peak grouping after application of retcor..."))

# using method = "density" with settings from above

xset_gr.ret.rg = group(xset_gr.ret,
                       method = "density",
                       bw = density.bw,
                       minfrac = density.minfrac,
                       minsamp = density.minsamp,
                       mzwid = density.mzwid,
                       max = density.max,
                       sleep = density.sleep
)

# fill missing peaks
rm("xset_gr.ret")
print(paste0("Filling missing peaks..."))

save(xset_gr.ret.rg, file="xset_gr.ret.rg")
#Conclude retention time and grouping and save to xset_gr.ret.rg




#Begin peak filling
load("xset_gr.ret.rg")

register(bpstart(MulticoreParam(1)))
xset_gr.ret.rg.fill = fillPeaks.chrom(xset_gr.ret.rg, BPPARAM = bpparam())

rm(xset_gr.ret.rg)

save(xset_gr.ret.rg.fill, file = "xset_gr.ret.rg.fill")
#Conclude peak filling, save as xset_gr.ret.rg.fill



#####################################################################################
##### Isotope peak identification, creation of xsAnnotate object using CAMERA #######
#####################################################################################
load("xset_gr.ret.rg.fill")

print(paste0("Applying CAMERA to identify isotopic peaks, create xsAnnotate object, and 
             create CAMERA pseudospectra using correlation of xcms peak groups between and 
             within samples. These pseudospectra are the groups within which the adduct 
             hierarchy and retention time screening criteria will be applied using LOBSTAHS"))

# first, a necessary workaround to avoid a import error; see https://support.bioconductor.org/p/69414/
imports = parent.env(getNamespace("CAMERA"))
unlockBinding("groups", imports)
imports[["groups"]] = xcms::groups
lockBinding("groups", imports)

# create annotated xset using wrapper annotate(), allowing us to perform all CAMERA tasks at once

xset_a = annotate(xset_gr.ret.rg.fill,
                  
                  quick=FALSE, 
                  sample=NA, # use all samples
                  nSlaves=1, # use 4 sockets
                  
                  # group FWHM settings
                  # using defaults for now
                  
                  sigma=6,
                  perfwhm=0.6,
                  
                  # groupCorr settings
                  # using defaults for now
                  
                  cor_eic_th=0.75,
                  graphMethod="hcs",
                  pval=0.05,
                  calcCiS=TRUE,
                  calcIso=TRUE,
                  calcCaS=FALSE, # weird results with this set to TRUE
                  
                  # findIsotopes settings
                  
                  maxcharge=4,
                  maxiso=4,
                  minfrac=0.5, # 0.25?
                  
                  # adduct annotation settings
                  
                  psg_list=NULL,
                  rules=NULL,
                  polarity=subset.polarity,
                  multiplier=3,
                  max_peaks=100,
                  
                  # common to multiple tasks
                  
                  intval="into",
                  ppm=2.5,
                  mzabs=0.0015
                  
)

# at this point, should have an xsAnnotate object called "xset_a" 
# in hand, which will serve as the primary input to the main screening and annotation function 
# "doLOBscreen" in LOBSTAHS
rm(xset_gr.ret.rg.fill)

print(paste0("xsAnnotate object 'xset_a' has been created. User can now use LOBSTAHS to perform screening..."))

print(xset_a)

save(xset_a, file = "prepCompletedxsA")
load("prepCompletedxsA")

########################################################################
#LOBSTAHS PART
#######################################################################

data(default.LOBdbase)

LOB <- doLOBscreen(xsA=xset_a, polarity = "positive", match.ppm = 2.5, 
                   retain.unidentified = F, rt.restrict = T)

LOBscreen_diagnostics(LOB)
LOBdata <- getLOBpeaklist(LOB) 
write.csv(LOBdata, file = "LOB_Peaklist_Pos.csv")

rm(xset_a, LOB, default.LOBdbase)
