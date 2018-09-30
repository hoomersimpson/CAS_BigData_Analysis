# Read - Write XLS File
# 20180926 thomas.stump@bluewin.ch
#
# This R Script opens an XLS File and stores it under a filename extension yyyymmdd
# This script does not change any data in the XLS file
#

# Define path to the XLS file

## Source File (Input)
data_filename<-c("IAS_data.xlsx")
data_path<-"/Users/tstump/Dropbox/CAS/IAS/data"
data_file<-paste(data_path,data_filename,sep="/")
data_file

## Target File (Output)
today_date<-Sys.time()
data_file_ext<-format(today,format="%Y%m%d_%H%M.xlsx")
data_file_ext

date_file_ext_pos<-unlist(gregexpr(".xlsx",data_file))-1
data_file_save<-paste(substr(data_file,1,date_file_ext_pos),data_file_ext,sep="_")
data_file_save

# Read XLS into R
file_data<-read.xlsx(data_file)
tail(file_data)

# Write XLS to Path .. as <filename>_save.xlsx
write.xlsx(file_data,data_file_save)


