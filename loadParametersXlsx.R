# # Clear all
# rm(list = ls(all.names = TRUE))

#Load required library to load parameters
library(xlsx)

#Name of file to load parameters from
fileName = "pbmCOVID_v2.3.xlsx"

#1. Load General Parameters
GenParam <- read.xlsx(fileName, "GenP", header = F)
#Assign variable names from first column, and then delete first column
row.names(GenParam) = GenParam[,1]
GenParam$X1 <- NULL

# Convert the data frame into columns
auxM = t(GenParam)
GenP = as.data.frame(auxM)

#2. Load General Parameters
IntParam <- read.xlsx(fileName, "IntP", header = F)

#Assign variable names from first column, and then delete first column
row.names(IntParam) = IntParam[,1]
IntParam$X1 <- NULL

# Convert the data frame into columns
auxM = t(IntParam)
IntP = as.data.frame(auxM)


#3. Load Epidemiological Parameters
EpiParam <- read.xlsx(fileName, "EpiP", header = F)

#Assign variable names from first column, and then delete first column
row.names(EpiParam) = EpiParam[,1]
EpiParam$X1 <- NULL

# Convert the data frame into columns
auxM = t(EpiParam)
EpiP = as.data.frame(auxM)


rm(auxM, GenParam, EpiParam, IntParam)

save.image(file='paramLoaded.RData')

# Code to check up what needs to load or not the previous file (TO DEVELOP FURTHER)
# file.info("paramLoaded.RData")
# file.info(fileName)
# fileInfo = file.info("paramLoaded.RData")
# fileInfo$mtime
