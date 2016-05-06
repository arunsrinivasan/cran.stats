\name{stats_logs}
\alias{stats_logs}
\title{Calculate yearly, monthly or daily download stats}
\description{
Once the logs have been read in, this function can be used to calculate \emph{yearly}, \emph{monthly} or \emph{daily} stats.
}
\usage{
stats_logs(dt, type="monthly", pacakges="data.table", 
          dependency=TRUE, duration=60L)
}
\arguments{
  \item{dt}{A \code{data.table} object with the download logs.}
  \item{type}{Calculates the number of downloads for specified packages \emph{monthly}, \emph{daily} or \emph{yearly}. Default is \code{monthly}.}
  \item{packages}{A character vector of package names for which to calculate the stats for. Default is \code{"data.table"}.}
  \item{dependency}{When \code{TRUE} (default), also computes stats for all packages that depend on \emph{this} package and were downloaded within +/- seconds specified in \code{duration} in an attempt to get a better representation of the number of times \emph{just this package} was downloaded.}
  \item{duration}{Default is 60 seconds. When \code{dependency} is \code{TRUE}, number of downloads for all packages that depend on \emph{this} package and downloaded within +/- \code{duration} seconds are calculated and provided as an additional column.}
}
\details{
By just looking at the number of downloads for each package provided in the \code{packages} argument, it'll not be possible to distinguish if the package was directly downloaded or was it because it was in the \code{depends}, \code{suggests}, \code{imports} etc. fields. In order to get the number of downloads for \emph{just} the package that is specified, we start by gathering the number of downloads for all the packages that depend on it, within \code{duration} seconds +/-. We also use the proxy info provided for IP address from Rstudio mirror logs to find out if the dependent package and the actual package were downloaded by the same user.
    
We add all the downloads for each package provided in the \code{package} argument which satisfy these criterion and provide this information as an additional column, in addition to the total number of downloads for the packages specified.
}
\value{
A \code{data.table} with the number of downloads for the specified time frame and \code{type} and an additional column indicating the number of downlaods most likely due to the packages that depend on it, if \code{dependency} is set to \code{TRUE}.
}


\examples{
\dontrun{
dt = read_logs(dir="cran-mirror", verbose = TRUE)
dt_s = stats_logs(dt)
}
}
\seealso{
\code{\link{read_logs}}, \code{\link{plot_logs}}
}
\keyword{data}
