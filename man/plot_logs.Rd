\name{plot_logs}
\alias{plot_logs}
\title{Plot download stats.}
\description{
  A dirty wrapper function to plot the download stats obtained by running \code{stat_logs} function. 
}
\usage{
plot_logs(dt)
}
\arguments{
  \item{dt}{ A \code{data.table} that's obtained from running \code{stat_logs} function. }
}
\details{
    At the moment, this is a dirty function. That is, it detects the column names and the number of columns to find out if \code{dependency} was set to \code{TRUE} or not. If the downloads for dependent packages doesn't exist, it plots just the total downloads for all the packages specified. 
    
    If the column for downloads from dependent packages were present, it provides two plots (stitched together). The first one provides the number of times packages were downloaded directly (meaning not through dependencies), and the second one provides the number of downloads due to dependencies alone.
}

\examples{
\dontrun{
dt = read_logs(start = as.Date("2014-01-01"), 
               end = as.Date("2014-01-31"), 
               path = "./", dir="cran-mirror", 
               verbose = TRUE)
dt_s = stats_logs(dt)
plot_logs(dt_s)
}
}
\seealso{
  \code{\link{read_logs}}, \code{\link{stat_logs}}
}
\keyword{ data }

