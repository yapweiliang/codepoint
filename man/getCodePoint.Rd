\name{getCodePoint}
\alias{getCodePoint}
\alias{getCodePointRDS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compile/convert unzipped OS Code-Point(R) Open data into a form that I want
}
\description{
Compile/convert unzipped OS Code-Point(R) Open data into a form that I want.

Assumes you have downloaded and unzipped \file{codepo_gb.zip} to \file{path}.

Alternatively, saves the output as an .rds file for subsequent re-loading.
}
\usage{
getCodePoint( path, 
              doc.folder = paste(path, "Doc", sep = "/"), 
              csv.folder = paste(path, "Data/CSV", sep = "/"), 
              headers.we.want = c("Admin_county_code", "Admin_district_code"), 
              index.of.header.to.join = 2, 
              rds.output.folder = path,
              get.RDS.instead = FALSE )
              
getCodePointRDS( path, ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
the path where the unzipped codepoint files are
}
  \item{doc.folder}{
the path of the \file{Doc} sub-folder
}
  \item{csv.folder}{
the path of the \file{CSV} sub-folder
}
  \item{headers.we.want}{
a list of the headers to keep (apart from the mandatory headers, the rest are discarded). 

Available headers are \command{Positional_quality_indicator, Country_code, NHS_regional_HA_code, NHS_HA_code, Admin_county_code, Admin_district_code, Admin_ward_code}.  

Mandatory headers are \command{Postcode, Eastings, Northings} and do not need to be specified.
}
  \item{index.of.header.to.join}{
which header (in \command{headers.we.want}) to join with the names/types of the areas
}
  \item{rds.output.folder}{
where save the output as an .rds file, for faster re-loading in the future.  This identifies the version from \file{metadata.txt} and names the .rds file accordingly.
}
  \item{get.RDS.instead}{
return the full path of the RDS file that is saved instead of returning the data frame
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Depending on \command{get.RDS.instead}
  \item{TRUE}{
Returns a data frame consisting of the Code-Point csv files (all appended to each other), joined to the information in \file{Codelist.xls} (which is the names related to the codes)
}
  \item{FALSE}{
Saves the data frame to a \command{.rds} file and returns the full path of that file.
}
\command{getCodePointRDS} is a wrapper function for \command{getCodePoint} that specifies \command{get.RDS.instead = TRUE}.
}
\references{
}
\author{
Wei Liang Yap
}
\note{
This takes about 15 - 20 seconds on my computer.  Please be patient.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
}
\examples{
# do it
CodePoint <- getCodePoint( "H:/DATASETS/OS/CodePoint/tmp" )

# re-load the saved .rds file
filename <- getCodePointRDS( "H:/DATASETS/OS/CodePoint/tmp",rds.output.folder="H:/DATASETS/OS/CodePoint" )
CodePoint2 <- readRDS(filename)
}