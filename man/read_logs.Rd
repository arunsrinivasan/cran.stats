\name{read_logs}
\alias{read_logs}
\title{Load logs from RStudio mirror}
\description{
Fetch R-package download logs from the \href{http://cran-logs.rstudio.com/}{RStudio mirror} to the directory specified if the logs don't already exist and read them with \code{\link{fread}} from the \code{data.table} package.
}
\usage{
read_logs(start=Sys.Date()-30L, end=Sys.Date(), path="./", dir="cran-mirror", verbose=TRUE)
}
\arguments{
\item{start}{Fetch download logs from this date (format: \code{YYYY-MM-DD}). Default is \code{Sys.Date()-30L}.}
\item{end}{Fetch download logs until this date (format: \code{YYYY-MM-DD}). Default is \code{Sys.Date()}.}
\item{path}{Path excluding the directory to download logs to.}
\item{dir}{Directory within \code{path} where the logs will be downloaded to.}
\item{verbose}{\code{TRUE} provides informative messages to the console.}
\item{select}{Character vector of column names to load from the logs. Default is \code{c("date", "time", "package", "country", "ip_id")}. \code{NULL} loads all columns.}
}
\details{
\code{read_logs} creates a folder under the path \code{path/dir} and downloads all missing logs from \code{start} until \code{end}. After downloading all the logs, they will be automatically replaced with corresponding unzipped versions. If an unzipped log already exists, then download of that log will be skipped. If a log that was downloaded was corrupted, download + unzip will be attemped once again. If it fails this time as well, the download of that log will be skipped.

Following that the logs will be read in using \code{data.table}'s \code{fread} function.

As long as unzipped logs exist in the directory, they won't be downloaded again. So, it is preferred to keep using the same directory without deleting the logs that've been already downloaded, to save time.
}

\value{
A \code{data.table} of all the download logs that are available between \code{start} and \code{end} dates.
}

\examples{
\dontrun{
## download all logs for January 2014
dt = read_logs(path = "./", dir="cran-mirror", verbose = TRUE)
}
}
\seealso{
\code{\link{stats_logs}}, \code{\link{plot_logs}}
}
\keyword{data}
