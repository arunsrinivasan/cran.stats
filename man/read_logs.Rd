\name{read_logs}
\alias{read_logs}
\title{Fetch and load logs from RStudio mirror}
\description{
Fetch R-package download logs from the \href{http://cran-logs.rstudio.com/}{RStudio mirror} to the directory specified if the logs don't already exist and read them with \code{\link{fread}} from the \code{data.table} package.
}
\usage{
read_logs(start=Sys.Date()-30L, end=Sys.Date(), dir="cran-mirror", verbose=TRUE)
}
\arguments{
\item{start}{Fetch download logs from this date (format: \code{YYYY-MM-DD}). Default is \code{Sys.Date()-30L}.}
\item{end}{Fetch download logs until this date (format: \code{YYYY-MM-DD}). Default is \code{Sys.Date()}.}
\item{dir}{Complete path to the directory where the logs should be downloaded to. Default is to download to \code{./cran-mirror}.}
\item{verbose}{\code{TRUE} provides informative messages to the console.}
\item{select}{Character vector of column names to load from the logs. Default is \code{c("date", "time", "package", "country", "ip_id")}. \code{NULL} loads all columns.}
}
\details{
\code{read_logs} downloads all missing logs from \code{start} until \code{end} to the directory provided in \code{dir}. After downloading all the logs, they will be automatically replaced with corresponding unzipped versions. Logs for which an unzipped version already exists will be skipped. If a log that was downloaded was corrupted, download + unzip will be attemped once again. If it fails the second time as well, that log is skipped.

Following that the logs will be read in using \code{data.table::fread}.

As long as unzipped logs exist in the directory, they won't be downloaded again. So, it is preferred to keep using the same directory without deleting the logs that've been already downloaded, to save time.
}

\value{
A \code{data.table} of all the download logs that are available between \code{start} and \code{end} dates.
}

\examples{
\dontrun{
## download all available logs for the last 31 days
dt = read_logs(dir="./cran-mirror", verbose=TRUE)
}
}
\seealso{
\code{\link{stats_logs}}, \code{\link{plot_logs}}
}
\keyword{data}
