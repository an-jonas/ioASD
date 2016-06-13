
source(https://github.com/kang-yu/ioASD/blob/master/fRenameRepASD.R)

fRenameRepASD("O:/FIP/2016/WW013/ASD/raw/20160527", "O:/FIP/2016/WW013/ASD/20160527", "20160527", 
              expID = "WW013", idFilter="RunID_20", rep = 3, skipNoRow = 8)
