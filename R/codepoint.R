# CodePoint.r
# parse an unpacked CodePoint.zip and get all the details in the way I want it
#
# unpack to tmp folder, specify the paths, run below
# 
# 15 seconds approx
# CodePoint <- getCodePoint( "H:/DATASETS/OS/CodePoint/tmp",rds.output.folder="H:/DATASETS/OS/CodePoint" )
#

#library(dplyr)
#library(readr)
#library(readxl)


# constants ---------------------------------------------------------------
# stuff that we assume remains unchanged through different version of CodePoint Open, but maybe might change
.metadata.txt.filename <- "metadata.txt" # to extract version
.codelist.xls.filename <- "Codelist.xlsx" # 2015.4.0 version uses .xlsx; .xlsx needs $X0, not $X1
.codelist.xls.areacodes.sheetname <- "AREA_CODES" # name of sheet within Codelist.xls that has table of the different types of codes
.nhscodelist.xls.filename <- "NHS_Codelist.xls"
.csv.column.types <- "ciiicccccc" # format as per readr::read_csv
.headers.filename <- "Code-Point_Open_Column_Headers.csv" # contains the headers for all the .csv files
.mandatory.headers <- c("Postcode","Eastings","Northings")


# getCodePoint ------------------------------------------------------------

getCodePoint <- function( path, 
                          doc.folder = paste(path,"Doc",sep="/"), 
                          csv.folder = paste(path,"Data/CSV",sep="/"),
                          headers.we.want = c("Admin_county_code","Admin_district_code"),
                          index.of.header.to.join = 2,
                          rds.output.folder = path,
                          get.RDS.instead = FALSE ) {
  
  read_codelist <- function(sheet,path,NHS=FALSE) {
    z <- read_excel(path,sheet,col_names=FALSE)
    if(NHS){
      z[,3] <- "NHS"
    } else {
      z[,3] <- sheet # 3 letter abbreviation for the type of code, e.g. CTW=County
    }
    return(z)
  }
  
  codelist.xls.file <- paste(doc.folder,.codelist.xls.filename,sep="/")
  nhscodelist.xls.file <- paste(doc.folder,.nhscodelist.xls.filename,sep="/")
  
  # TODO: add code to check that the forward slash is not inadvertently added
  
  header <- read_csv( paste(doc.folder,.headers.filename,sep="/") )
  # TODO HERE: check that headers.we.want are valid, and double-check that mandatory headers are valid
  # TODO HERE: check that index.of.header.to.join is valid (in range)
  
  # compile all the .csv files
  files <- list.files(csv.folder,full.names=TRUE)
  CodePoint <- lapply( files, read_csv, col_names = FALSE, col_types = .csv.column.types ) # merge all the csv fils
  CodePoint <- do.call(rbind, CodePoint) # and make into a data frame
  names(CodePoint) <- header[1,] # apply the headers
  CodePoint<-subset(CodePoint,select=c(.mandatory.headers,headers.we.want)) # reduce size of dataframe  
  
  # compile all the area codes
  area_codes <- as.list(read_excel(codelist.xls.file,sheet=.codelist.xls.areacodes.sheetname,col_names=FALSE)[,1])$X0 # X1 for .xls, X0 for .xlsx
  area_codes <- lapply( area_codes, read_codelist,  path=codelist.xls.file )
  area_codes <- do.call(rbind,area_codes)
  names(area_codes) <- c("AreaName", "AreaID", "AreaType")
  nhs_areas <- excel_sheets(nhscodelist.xls.file)
  nhs_areas <- lapply( nhs_areas, read_codelist, path=nhscodelist.xls.file, NHS=TRUE )  
  nhs_areas <- do.call(rbind,nhs_areas)
  names(nhs_areas) <- c("AreaID", "AreaName", "AreaType")
  CodePointAreaCodes <- rbind(area_codes,nhs_areas)
  CodePointAreaCodes  <- CodePointAreaCodes[complete.cases(CodePointAreaCodes),]

  # join  
  CodePoint[,headers.we.want] <- lapply(CodePoint[,headers.we.want],factor)
  CodePointAreaCodes <- as.data.frame(unclass(CodePointAreaCodes)) # factorise all
  join.string <- "AreaID"
  names(join.string) <- headers.we.want[index.of.header.to.join]
  CodePoint<-left_join(CodePoint,CodePointAreaCodes,by=join.string)
  CodePoint[,headers.we.want] <- lapply(CodePoint[,headers.we.want],factor) # re factor, as join has coerced back to character

  if(get.RDS.instead){
    z<-readLines(paste(doc.folder,.metadata.txt.filename,sep="/"))  
    z<-z[grep("VERSION",z)] # find the line that says 'VERSION', assume only one line found
    z<-regmatches(z,gregexpr(pattern = "([[:alnum:][:punct:]]+)$",z))  
    # TODO HERE: what to do if not found
    
    RDS.name <- paste0(rds.output.folder,"/","CodePoint ",z," compiled.rds")
    saveRDS(CodePoint,RDS.name)
    return(RDS.name)
  } else {
    return(CodePoint)
  }
}


# getCodePointRDS ---------------------------------------------------------

getCodePointRDS <- function( path, ... ) {
  return(getCodePoint( path, ..., get.RDS.instead = TRUE ))
}
