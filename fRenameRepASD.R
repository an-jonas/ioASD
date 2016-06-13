#======================================================================================
# Rename ASD files of replicated measurements
#======================================================================================

fRenameRepASD <- function(dirfrom, dirto, yyyymmdd, pos="Can", expID = "WW012", start="Lot3", rep = NULL){
  
  # dirfrom <- "O:/FIP/2016/WW012/ASD/raw/20160526"
  # dirto <- "O:/FIP/2016/WW012/ASD/20160526"
  # yyyymmdd <- "20160526"
  # pos <- "Can"
  # expID <- "WW012"
  # rep <- 2
  
  ## prepare file names
  library(gdata)
  library(xlsx)
  
  dfnamesheet <- "O:/FIP/2016/Feldbuch/WW/2015_Design_FPWW012_FPWW016.xls"
  dfname <- read.xls(dfnamesheet, paste0("PlotList_", expID), stringsAsFactors=FALSE, skip=4)
  
  dfname <- dfname[-which(dfname$Plot == 0),]
  
  dfname1 <- dfname[rep(rownames(dfname), each = rep), ]
  suffix <- lapply(lapply(lapply(row.names(dfname1), strsplit, ".", fixed=TRUE), unlist), function(x) x=as.numeric(x[2]))
  suffix1 <- sapply(suffix, function(x) x = ifelse(is.na(x), 0, x) + 1)
  
  if (pos == "Leaf") dfname1$Plot_ID <- paste0(dfname1$Plot_ID, "_Leaf_", suffix1)
  if (pos == "Can") dfname1$Plot_ID <- paste0(dfname1$Plot_ID, "_Can_", suffix1)

  # order by row names if necessary
  # dfname1 <- dfname1[with(dfname1, order(as.numeric(row.names(dfname1)))), ]

  ## re-sort by Lot
  
  if (start == "Lot3"){
    dfname1 <- dfname1[with(dfname1, order(Lot, Sow_Nr, as.numeric(row.names(dfname1)))), ]
  }
  
  # if (start == "Lot4"){
  #   dfname1 <- dfname1[with(dfname1, order(-Lot, as.numeric(row.names(dfname1)))), ]
  # }
  

  df <- dfname1
  
  # list.files order acoording to file names
  filelist <- basename(list.files(path = dirfrom, pattern = "^ww.+[.]asd$", ignore.case = TRUE ))
  
  # check number of old and new names whether equal or not
  stopifnot(length(filelist) == nrow(df))
  
  # move soil spectral files first before renaming
  indSoil <- grep("soil", filelist)
  soilfile <- filelist[indSoil]
  if (length(soilfile) > 1){
    dirtoSoil <- file.path(dirto, "Soil")
    if (!isTRUE(file.info(dirtoSoil)$isdir)) dir.create(dirtoSoil, recursive=TRUE)
    file.copy(file.path(dirfrom, soilfile), dirtoSoil, copy.date = TRUE)
    
    # new list
    filelist <- filelist[-indSoil]
  }
  
  
  ## renaming func
  asd.file.rename <- function(from, to, copy.date = TRUE) {
    TODIR <- dirname(to)
    if (!isTRUE(file.info(TODIR)$isdir)) dir.create(TODIR, recursive=TRUE)
    file.copy(from = from,  to = to, copy.date = copy.date)
  }
  
  fullnameOld <- NULL
  fullnameNew <- NULL
  for (i in 1:length(filelist)){
    fullnameOld[i] <- paste(dirfrom, filelist[i], sep = "/")
    fullnameNew[i] <- file.path(dirto, paste(paste(yyyymmdd, "ASD", df$Plot_ID[i], sep = "_"), "asd", sep="."))
    asd.file.rename(fullnameOld[i], fullnameNew[i])
  }
  
  log <- data.frame("File.Creation.Name" = filelist, "File.Renaming.NewName" = basename(fullnameNew))
  write.table(log, paste(dirname(dirfrom), paste0("renaminglog_", basename(dirfrom), ".txt"), sep="/"), 
              sep="\t", append = FALSE, row.names=FALSE)
  return(log)
}

### example:
# yyyymmdd <- "20160526"
# fRenameRepASD(paste0("O:/FIP/2016/WW012/ASD/raw/", yyyymmdd),
#               paste0("O:/FIP/2016/WW012/ASD/", yyyymmdd),
#               yyyymmdd,
#               pos="Can",
#               expID = "WW012",
#               start = "Lot3",
#               rep = 2)