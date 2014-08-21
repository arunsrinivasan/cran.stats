\name{read_logs}
\alias{read_logs}
\title{Fetch logs from Rstudio mirror.}
\description{
    Fetch R-package download logs from the Rstudio mirror to the directory specified if the logs don't already exist and read them in using \code{fread} function from the \code{data.table} package.
}
\usage{
read_logs(start, end, path="./", dir="cran-mirror", verbose=TRUE)
}
\arguments{
  \item{start}{ Fetch download logs from this date (format: YYYY-MM-DD). }
  \item{end}{ Fetch download logs until this date (format: YYYY-MM-DD) }
  \item{path}{ Path excluding the directory to download logs to}
  \item{dir}{ Directory within \code{path} where the logs will be downloaded to }
  \item{verbose}{ \code{TRUE} provides informative messages to the console }
}
\details{
    \code{read_logs} creates a folder under the path \code{path/dir} and downloads the logs from the date \code{start} until \code{end} on to it. After downloading all the logs, they will be automatically replaced with corresponding unzipped versions. If an unzipped log already exists, then download of that log will be skipped. If a log that was downloaded was corrupted, it'll be deleted autmatically and will be attemped again for download+unzip. If it fails again, the download of that log will be skipped.
    
    Following that the logs will be read in using \code{data.table}'s \code{fread} function.
    
    As long as unzipped logs exist in the directory, they won't be downloaded again. So, it is preferred to keep using the same directory without deleting the logs that've been already downloaded, to save time.
}

\value{
    A \code{data.table} of all the download logs that are available between \code{start} and \code{end} dates.
}

\examples{
\dontrun{
## download all logs for January 2014
dt = read_logs(start = as.Date("2014-01-01"), 
               end = as.Date("2014-01-31"), 
               path = "./", dir="cran-mirror", 
               verbose = TRUE)
}
}
\seealso{
  \code{\link{plot_logs}}, \code{\link{stats_logs}}
}
\keyword{ data }
