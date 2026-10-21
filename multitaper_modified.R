# Multitaper Spectral Analysis on Sound Segments
# Created by Colin Wilson and Eleanor Chodroff
# 2014, 2016
# Last updated by Eleanor Chodroff: Dec 2020
# Modified by Ben Lang: Oct 2025

# This script reads in a series of sound extracts (set up here for sibilant extracts) and takes the
# following measures over the middle 20 ms of the extract (.wav) using a multitaper spectrum:
# spectral peak,
# COG (M1), spectral variance (M2), skewness (M3), kurtosis (M4)

# The analysis interval is by default set to the middle 20 ms but this can be modified to any proportional or durational width and centered over the beginning, middle, or end of the fricative

# For each directory of sound extracts, an output file is created with the above measures called DIRECTORYsibilants.csv

# For any further use of this particular script, PLEASE CITE:
# Chodroff, E., & Wilson, C. (2014). Burst spectrum as a cue for the stop voicing contrast in American English. The Journal of the Acoustical Society of America, 136(5), 2762-2772.

# ASSUMPTIONS OF THIS SCRIPT:
# - there is a directory with sibilant extracts with .wav extensions (mydir)

#################
### CHANGE ME ###
#################

sync_paths <- c('/Volumes/circe',
                '/Volumes/cassandra')

base_path <- sync_paths[dir.exists(sync_paths)][1]

# list of fricatives to process (each must match a sub-folder name)
fricatives <- c('h','ħ','s','ʕ','sˤ','ʁ','ðˤ','ð','ðħ','sʕ')   # <- edit this

fric_dir <- file.path(base_path, 'alldata/dissertation/vs/output_preproc/fricatives')
outdir   <- fric_dir   # output directory for the csv files

SAMPLE_RATE <- 44100

NW_TIME_BANDWIDTH <- 4
K_TAPERS <- 8

extract_location <- "mid"
extract_what <- "duration"
extract_duration <- 0.02
extract_proportion <- 0.5

################
### LET'S GO ###
################

require(tuneR)
require(multitaper)

#################
### FUNCTIONS ###
#################

# location can be "beg", "mid", or "end"
getPortion <- function(frici, n_samples, extract_samples, location) {
  if (location == "mid") {
    edges <- (n_samples - extract_samples)/2
    start <- round(edges)
    end <- round(n_samples - edges)
  } else if (location == "end") {
    start <- n_samples - extract_samples
    end <- n_samples
  } else if (location == "beg") {
    start <- 1
    end <- extract_samples
  }
  frici <- frici[start:end]
  return(frici)
}

getSpectrum <- function(frici) {
  xi <- ts(attributes(frici)$left, frequency = SAMPLE_RATE)
  mti <- spec.mtm(xi, nw = NW_TIME_BANDWIDTH, k = K_TAPERS, plot = FALSE)
  return(mti)
}

getSpectralMoments <- function(mti) {
  # cog
  cogi <- mti$freq %*% (mti$spec / sum(mti$spec))
  
  # variance
  X <- (mti$spec / sum(mti$spec))
  Y <- outer(cogi, mti$freq, function(x, y) { (y - x)^2 })
  spectral.vari <- X %*% Y
  
  # skewness
  X <- (mti$spec / sum(mti$spec))
  Y <- outer(cogi, mti$freq, function(x, y) { (y - x)^3 })
  skewi <- X %*% Y
  skewi <- skewi / spectral.vari^(3/2)
  
  # kurtosis
  X <- (mti$spec / sum(mti$spec))
  Y <- outer(cogi, mti$freq, function(x, y) { (y - x)^4 })
  kurti <- X %*% Y
  kurti <- (kurti / spectral.vari^2) - 3
  
  spectralMoments <- c(cogi, spectral.vari, skewi, kurti)
  return(spectralMoments)
}

getSpectralPeak <- function(mti) {
  peakamp <- max(mti$spec)
  loc <- which(mti$spec == peakamp)
  peaki <- mti$freq[loc]
  return(peaki)
}

getSpectralPeakAmp <- function(mti) {
  peakamp <- max(mti$spec)
}


getMidBandPeak <-function(mti, lowFreq, highFreq) {
  lowend <- which(abs(mti$freq - lowFreq) == min(abs(mti$freq - lowFreq)))
  highend <- which(abs(mti$freq - highFreq) == min(abs(mti$freq - highFreq)))
  peakamp <- max(mti$spec[lowend:highend])
  loc <- which(mti$spec[lowend:highend] == peakamp)
  midPeaki <- mti$freq[lowend:highend][loc]
  return(midPeaki)
}

getMidBandPeakAmp <-function(mti, lowFreq, highFreq) {
  lowend <- which(abs(mti$freq - lowFreq) == min(abs(mti$freq - lowFreq)))
  highend <- which(abs(mti$freq - highFreq) == min(abs(mti$freq - highFreq)))
  peakamp <- max(mti$spec[lowend:highend])
  return(peakamp)
}

getMinBandPeak <-function(mti, lowFreq, highFreq) {
  lowend <- which(abs(mti$freq - lowFreq) == min(abs(mti$freq - lowFreq)))
  highend <- which(abs(mti$freq - highFreq) == min(abs(mti$freq - highFreq)))
  peakamp <- max(mti$spec[lowend:highend])
  loc <- which(mti$spec[lowend:highend] == peakamp)
  midPeaki <- mti$freq[lowend:highend][loc]
  return(midPeaki)
}

getMinBandPeakAmp <-function(mti, lowFreq, highFreq) {
  lowend <- which(abs(mti$freq - lowFreq) == min(abs(mti$freq - lowFreq)))
  highend <- which(abs(mti$freq - highFreq) == min(abs(mti$freq - highFreq)))
  peakamp <- max(mti$spec[lowend:highend])
  return(peakamp)
}

getMinBandMin <-function(mti, lowFreq, highFreq) {
  lowend <- which(abs(mti$freq - lowFreq) == min(abs(mti$freq - lowFreq)))
  highend <- which(abs(mti$freq - highFreq) == min(abs(mti$freq - highFreq)))
  peakamp <- min(mti$spec[lowend:highend])
  loc <- which(mti$spec[lowend:highend] == peakamp)
  midPeaki <- mti$freq[lowend:highend][loc]
  return(midPeaki)
}

getMinBandMinAmp <-function(mti, lowFreq, highFreq) {
  lowend <- which(abs(mti$freq - lowFreq) == min(abs(mti$freq - lowFreq)))
  highend <- which(abs(mti$freq - highFreq) == min(abs(mti$freq - highFreq)))
  peakamp <- min(mti$spec[lowend:highend])
  return(peakamp)
}

##########################
### PER-FRICATIVE LOOP ###
##########################

process_fricative <- function(fric) {
  mydir   <- file.path(fric_dir, fric)
  outfile <- paste0(fric, ".csv")
  
  if (!dir.exists(mydir)) {
    warning("Folder not found, skipping: ", mydir)
    return(invisible(NULL))
  }
  
  files <- list.files(mydir, pattern = "\\.wav$")
  if (length(files) == 0) {
    warning("No .wav files found, skipping: ", mydir)
    return(invisible(NULL))
  }
  
  n <- length(files)
  cogs <- peaks <- peakamps <- rep(NA_real_, n)
  midbandpeaks <- midbandpeakamps <- rep(NA_real_, n)
  minbandpeaks <- minbandpeakamps <- rep(NA_real_, n)
  minbandmins <- minbandminamps <- rep(NA_real_, n)
  degsibilances <- specvars <- skews <- kurts <- rep(NA_real_, n)
  
  for (i in seq_len(n)) {
    frici <- readWave(file.path(mydir, files[i]))
    frici <- normalize(frici)
    n_samples <- length(frici)
    
    if (extract_what == "proportion") {
      extract_samples <- extract_proportion * n_samples
    } else {
      extract_samples <- min(extract_duration * SAMPLE_RATE, n_samples)
    }
    frici <- getPortion(frici, n_samples, extract_samples, extract_location)
    
    mti <- getSpectrum(frici)
    
    spectralMomentsi <- getSpectralMoments(mti)
    cogs[i]     <- spectralMomentsi[1]
    specvars[i] <- spectralMomentsi[2]
    skews[i]    <- spectralMomentsi[3]
    kurts[i]    <- spectralMomentsi[4]
    
    peaks[i]    <- getSpectralPeak(mti)
    peakamps[i] <- getSpectralPeakAmp(mti)
    
    midbandpeaks[i]    <- getMidBandPeak(mti, 3000, 7000)
    midbandpeakamps[i] <- getMidBandPeakAmp(mti, 3000, 7000)
    
    minbandpeaks[i]    <- getMinBandPeak(mti, 550, 3000)
    minbandpeakamps[i] <- getMinBandPeakAmp(mti, 550, 3000)
    
    minbandmins[i]    <- getMinBandMin(mti, 550, 3000)
    minbandminamps[i] <- getMinBandMinAmp(mti, 550, 3000)
    
    degsibilances[i] <- midbandpeakamps[i] - minbandminamps[i]
  }
  
  measures <- data.frame(stim = files, cog = cogs, peak = peaks, peakamp = peakamps,
                         midbandpeak = midbandpeaks, midbandpeakamp = midbandpeakamps,
                         minbandpeak = minbandpeaks, minbandpeakamp = minbandpeakamps,
                         minbandmin = minbandmins, minbandminamp = minbandminamps,
                         degsibilance = degsibilances, spectral.var = specvars,
                         skew = skews, kurtosis = kurts)
  
  write.csv(measures, file.path(outdir, outfile), row.names = FALSE, quote = FALSE)
  message("Done: ", fric, " (", n, " files) -> ", file.path(outdir, outfile))
  invisible(measures)
}

# run for every fricative in the list
for (fric in fricatives) {
  process_fricative(fric)
}

###########
### END ###
###########