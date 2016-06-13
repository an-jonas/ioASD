#=========================================================
# Rename .asd files collected with ASD FieldSpec 4
#=========================================================
fip_rename_asd <- function(dirfrom, dirto, yyyymmdd, opt = "All", start="Lot1", rep = NULL){
  
#   dirfrom <- ("C:/Users/Public/4K/ASD/raw/20150611_leaf")
#   dirto   <- ("C:/Users/Public/4K/ASD/get/20150611_leaf")
#   yyyymmdd <- '20150611_leaf'
#   opt <- "Steven"
#   start <- "Lot1"
  
  library(xlsx)
  if (opt=="All"){
    dfname <- read.xlsx("C:/Users/Public/4K/FileRenaming.xlsx", "WW007")
    if (start=="Lot6"){
      dfname <- dfname[with(dfname, order(-Lot, Plot)), ]
    }
  }else if(opt=="Steven"){
    dfname <- read.xlsx ("C:/Users/Public/4K/FileRenaming.xlsx", "Selected_Steven")
    if (start=="Lot1NoCK_CK"){
      dfname <- dfname
    }
    if (start=="Lot1NoCK"){
      dfname <- subset(dfname, RunID != c(58,59,60))
    }
    if (start=="Lot1CK"){
      dfname <- dfname[with(dfname, order(PlotNo)), ]
    }
    if (start=="Lot6"){
      dfname <- dfname[with(dfname, order(-Lot, RunID)), ]
    }
  }else if(opt=="Subset"){
    dfname <- read.xlsx ("C:/Users/Public/4K/FileRenaming.xlsx", "Selected96")
    if (start=="Lot6"){
      dfname <- dfname[with(dfname, order(-Lot, RunID)), ]
    }
  }
  
  
  
  
  # fpattern = glob2rx('ww*.asd') 
  # use (full.names=TRUE) in list.files to sort files in alphabetical order
  filelist <- basename(list.files(path = dirfrom, pattern = "^ww.+[.]asd$", ignore.case = TRUE ))
  
  setwd(dirfrom) # necessary
  
  details <- file.info(filelist)
  # On Windows native file systems ctime is the file creation time.
  
#   details <- details[with(details, order(as.POSIXct(ctime))), ]
  details <- cbind(TheFileCreationName = as.character(rownames(details)), details, stringsAsFactors = FALSE)
  asd.df <- subset(details, select = c(TheFileCreationName, ctime))
#   colnames(asd.df)[which(names(asd.df) == "ctime")] <- "TheFileCreationTime"
# 
#   TheFileCreationDate <- as.Date(as.POSIXct(substr(asd.df[,2], 1,19)))
#   yyyymmdd <- gsub("-", "", TheFileCreationDate[1])

#   as.POSIXct(strptime("20150528 163416", "%Y%m%d %H%M%S"))
#   as.POSIXct(strptime("20150528", "%Y%m%d"))

  
  asd.file.rename <- function(from, to, copy.date = TRUE) {
    TODIR <- dirname(to)
    if (!isTRUE(file.info(TODIR)$isdir)) dir.create(TODIR, recursive=TRUE)
    file.copy(from = from,  to = to, copy.date = copy.date)
  }
  
  NamePathOld <- NULL
  NamePathNew <- NULL
  # copy and rename files 
  for (i in 1:length(filelist)){
    
    NamePathOld[i] <- paste(dirfrom, filelist[i], sep = "/")
    NamePathNew[i] <- paste(paste(paste0(dirto, "/", yyyymmdd), dfname$Plot_ID[i], sep = "_"), "asd", sep=".")
    asd.file.rename(NamePathOld[i], NamePathNew[i])
  }



# library(prospectr)
# FirstFilePathOld <- NamePathOld[1]
# FirstFilePathNew <- NamePathNew[i]
# asd <- readASD(FirstFilePathOld,'binary')
# rad <- sapply(asd, "[[", "radiance")
# ref <- sapply(asd, "[[", "reference")
# rflt <- sapply(asd, "[[", "reflectance")
# wvlt <- sapply(asd, "[[", "wavelength")
# par(mar=c(5,6,4,3) + 0.1, cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2)
# plot(wvlt, rflt, main = "Reflectance", ylim=c(0, 0.1))
# 
# plot(wvlt, asd, main = "radiance")
# plot(wvlt, rflt, main = "Reflectance")

  log <- data.frame("File.Creation.Name" = filelist, "File.Renaming.NewName" = basename(NamePathNew))
  write.table(log, paste(dirname(dirname(dirfrom)), paste0("renaminglog_", basename(dirfrom), ".txt"), sep="/"), 
              sep="\t", append = FALSE, row.names=FALSE)
  return(log) 

} # end fun

# demo --------------------------------------------------------------------
# opt = "All", "Subset", Steven", 


folderStr <- c('20160526')
fip_rename_asd(paste0("O:/FIP/2016/WW012/ASD/", folderStr),
               paste0("O:/FIP/2016/WW012/ASD/", folderStr), folderStr, "All", "Lot1", 0)

