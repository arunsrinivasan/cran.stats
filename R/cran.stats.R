## exported functions
read_logs <- function(start=Sys.Date()-30L, end=Sys.Date(), dir="cran-mirror", 
                verbose=TRUE, select=c("date", "time", "package", "country", "ip_id")) {

    if (class(start) != "Date") {
        warning("Coercing 'start' to Date class")
        start = as.Date(start)
    }
    if (class(end) != "Date") {
        warning("Coercing 'end' to Date class")
        end = as.Date(end)
    }
    urls = construct_urls(seq(start, end, by="days"))
    download_logs(urls, dir, verbose=verbose)
    unzip_logs(urls, dir, verbose=verbose)
    fread_logs(urls, dir, verbose=verbose, select=select)
}

# "weekly" and "hourly" are also possible, but not exported functionality yet
stats_logs <- function(dt, type=c("monthly", "daily", "yearly"), 
                packages="data.table", dependency=TRUE, duration=30L) {

    dependency = as.logical(dependency)
    if (is.na(dependency)) stop("'dependency' must be logical TRUE/FALSE")
    duration = suppressWarnings(as.integer(duration))
    if (is.na(duration) || duration < 0L) 
        stop("Argument 'duration' (in seconds) should be >= 0.")
    if (!"package" %in% names(dt)) stop("'package' column doesn't exist.")
    if (!is.character(packages)) 
        stop("Argument 'packages' must be a character vector.")
    if (dependency) {
        if (!suppressMessages(require(devtools)))
            stop("'devtools' could not be loaded")
        deps = dep_stats(dt, packages, match.arg(type), duration)
        deps = deps[, .(dep_N=sum(dep_N)), by=c("package", "key")]
        setorder(deps, package, key)
    }
    this = pkg_stats(dt, packages, type)
    
    # get final counts
    ans = if (dependency) this[deps, on=c("package", "key")] else this
    ans
}

plot_logs <- function(dt, type=c("ggplot2")) {
    type = match.arg(type)
    no_points = dt[, .N, by=package][, max(N)]
    chart = if (no_points<=31L) "bar" else "line"
    # if (type == "plotly") plotly_logs(dt, chart=chart)
    # else ggplot2_logs(dt, chart=chart)
    ggplot2_logs(dt, chart=chart)
}

ggplot2_logs <- function(dt, chart=c("bar", "line")) {
    ans = copy(dt)
    if (!require("ggplot2"))
        stop("'ggplot2' couldn't be loaded")
    chart = match.arg(chart)
    title = "Stats for the given duration"
    setnames(ans, "key", "Period")

    if ("dep_N" %chin% names(ans)) {
        if (!require("gridExtra"))
            stop("'gridExtra' couldn't be loaded")
        ans[, pkg_N := tot_N - dep_N][, tot_N := NULL]
        pl1 = ggplot(data=ans, aes(x=Period, y=pkg_N, fill=package)) +
                geom_bar(stat = "identity", position="dodge") + 
                theme_bw() + 
                labs(title=paste(title, ": Direct downloads only")) + 
                scale_fill_brewer(palette="Set1") + 
                # scale_y_continuous(breaks = round(seq(0L, max(ans$pkg_N), 
                # length.out=5L))) + 
                theme(axis.text.x = element_text(angle=45, hjust=1))
        pl2 = ggplot(data=ans, aes(x=Period, y=dep_N, fill=package)) +
                geom_bar(stat = "identity", position="dodge") + 
                theme_bw() + 
                labs(title=paste(title, ": Through dependencies only")) + 
                scale_fill_brewer(palette="Set1") + 
                # scale_y_continuous(breaks = round(seq(0L, max(ans$dep_N), 
                # length.out=5L))) + 
                theme(axis.text.x = element_text(angle=45, hjust=1))
        pl = list(pl1, pl2)
        pl = marrangeGrob(grobs=pl, nrow=2L, ncol=1L, top=NULL)
    } else {
        setnames(ans, "tot_N", "pkg_N")
        pl = ggplot(data=ans, aes(x=Period, y=pkg_N, fill=package)) + 
               geom_bar(stat="identity", position="dodge") + 
               theme_bw() + 
               labs(title=paste(title, ": Direct + Dependencies")) + 
               scale_fill_brewer(palette="Set1") + 
               # scale_y_continuous(breaks = round(seq(0L, max(ans$pkg_N), 
               # length.out=5L))) + 
               theme(axis.text.x = element_text(angle=45, hjust=1))
    }
    pl
}

## internal functions
construct_urls <- function(days) {
    if (is.character(days)) days=as.Date(days)
    paste('http://cran-logs.rstudio.com/', year(days), '/', days, '.csv.gz', sep="")
}

download_logs <- function(urls, path, verbose=TRUE) {
    dir.create(path, showWarnings=FALSE)
    idx  = !(match_(urls, path, regex="\\.gz$"))
    src  = urls[idx]
    dest = file.path(path, basename(src))
    tot  = length(src)
    ans  = vector("logical", tot)
    for (i in seq_along(src)) {
        if (verbose) verbose_("Fetching logs", i, tot, basename(src[i]))
        tryCatch({
            download_file(src[i], dest[i], quiet=TRUE)
            ans[i]=TRUE
        }, error=function(e) {
            warning(paste("Couldn't open url", src[i], sep=" "))
            system(paste("rm", dest[i]))
        })
    }
    invisible(ans)
}

unzip_logs <- function(urls, path, rec=1L, verbose=TRUE) {
    idx = match_(urls, path, pattern="\\.gz$")
    dest = file.path(path, basename(urls)[idx])
    tot  = length(dest)
    for (i in seq_along(dest)) {
        if (verbose) verbose_("Unzipping logs", i, tot, basename(dest[i]))
        status <- system(paste('gunzip', dest[i]), ignore.stderr=TRUE)
        if (status) {
            # Unzipping failed. Try downloading one more time
            system(paste("rm", dest[i]))
            this_url = construct_urls(gsub("\\..*$", "", basename(dest[i])))
            ans = download_logs(this_url, path, verbose=FALSE)
            if (ans && rec==1L) unzip_logs(this_url, path, rec+1L)
            else warning(paste("Recursion limit reached, 
               couldn't unzip file", basename(dest[i]), sep=" "))
        }
    }
    invisible(NULL)
}

fread_logs <- function(urls, path, verbose=TRUE, 
                select=c("date", "time", "package", "country", "ip_id")) {
    idx  = match_(urls, path, regex="\\.gz$", pattern="\\.csv$", reverse=TRUE)
    dest = file.path(path, list.files(path, pattern="\\.csv$")[idx])
    tot  = length(dest)
    ans  = lapply(seq_along(dest), function(i) {
        if (verbose) verbose_("Fread(ing) logs   ", i, tot, basename(dest[i]))
        fread(dest[i], select=select)
    })
    ans = rbindlist(ans)
    keycols = c("package", "date", "time")
    setcolorder(ans, c(keycols, setdiff(names(ans), keycols)))
}

dep_stats <- function(dt, packages, type, duration) {

    packages_dt = data.table(package=packages)
    this = dt[packages_dt, on="package"]
    this = na.omit(this)[, key := key_(date, time, type)][]
    this[, c("minust", "plust") := { 
            tmp = as.POSIXct(paste(date, time), tz="GMT");
            list(tmp-duration, tmp+duration)}]
    
    dep = dt[deps_(packages), on=.(package == dependency)]
    dep = na.omit(dep)[, key := key_(date, time, type)][]
    dep[, "datetime" := as.POSIXct(paste(date, time), tz="GMT")]

    ans = dep[this, 
            .(i.package, key, country, ip_id, date=date, time=i.time, package=x.package), 
            on=.(i.package==package, key, country, ip_id, datetime>=minust, datetime<=plust), 
            nomatch=0L]
    ans = unique(ans, by=names(ans)[1:6])[, .N, by=.(i.package, key, package)]
    setnames(ans, c("i.package", "package", "N"), c("package", "dep_pkg", "dep_N"))
    return(ans)
}

match_ <- function(urls, path, regex=NULL, pattern=NULL, reverse=FALSE) {
    f1 = basename(urls)
    f2 = list.files(path, pattern=pattern)
    if (!is.null(regex)) {
        f1 = gsub(regex, "", f1)
        f2 = gsub(regex, "", f2)
    }
    if (reverse) f2 %chin% f1 else f1 %chin% f2
}

verbose_ <- function(pre, i, tot, file) {
    str = paste("\r", pre, " ", sep="")
    str = paste(str, i, " of ", tot, " - ", sep="")
    str = paste(str, file, " (", sep="")
    str = paste(str, round(i/tot * 100), "%)", sep="")
    cat(str)
    NULL
}

download_file <- function(src, dest, ...) {
    suppressWarnings(download.file(url=src, destfile=dest, ...))
}

yearly  = function(x) { gsub("^(.*)-(.*)-(.*)$", "\\1", x, perl=TRUE) }
monthly = function(x) { gsub("^(.*)-(.*)-(.*)$", "\\1-\\2", x, perl=TRUE) }
daily   = function(x) { x }
weekly  = function(x) {
    week = yday(as.IDate(x)-1L)%/%7L + 1L
    paste(yearly(x), sprintf("%02d", week), sep='-')
}
hourly  = function(x, y) {
    hour = gsub("^(.*):(.*):(.*)$", "\\1", y, perl=TRUE)
    paste(daily(x), hour, sep=":")
}

key_ <- function(date, time, type) {
    switch(type, yearly = yearly(date), 
                monthly = monthly(date), 
                 weekly = weekly(date),
                  daily = daily(date),  
                 hourly = hourly(date, time))
}

deps_ <- function(packages) {
    dep_pkg = lapply(packages, revdep, bioconductor=TRUE)
    dep_len = vapply(dep_pkg, length, 0L)
    package = rep(packages, dep_len)
    uses_it = unlist(dep_pkg, use.names=FALSE)
    setDT(list(package=package, dependency=uses_it))
}

pkg_stats <- function(dt, packages, type) {

    packages_dt = data.table(package = packages)
    this = dt[packages_dt, on="package"]
    this = na.omit(this)[, key := key_(date, time, type)][]
    ans  = this[, .(tot_N = .N), by=.(package, key)]
    setorder(ans, package, key)
}

## Deprecated / not used for now ------------------------------------------

# dep_stats <- function(dt, packages, type, duration) {

#     if (is.null(key(dt)) || key(dt)[1L] != "package")
#         stop("dt's first key column must be 'package'")
#     if (!is.character(packages)) stop("'packages' must be a character vector")
#     types = c("yearly", "monthly", "weekly", "daily", "hourly")
#     if (!is.character(type) || length(type) != 1L) 
#         stop("'type' must be a character vector of length 1")
#     if (!type %chin% types) 
#         stop(paste("'type' must be one of:", paste(types, collapse=","), sep=" "))
#     duration = suppressWarnings(as.integer(duration))
#     if (duration < 0L || is.na(duration)) 
#         stop("'duration' is in seconds, can't be a negative value, expecting >= 0L")

#     this = dt[packages][!is.na(date) & !is.na(time)][, key := key_(date, time, type)]
#     this[, date_time := as.integer(as.POSIXct(paste(date, time), tz="GMT"))]
#     setkey(this, package, key)
    
#     dep = dt[deps_(packages)][!is.na(date) & !is.na(time)]
#     dep[, key := key_(date, time, type)]
#     dep[, date_time := as.integer(as.POSIXct(paste(date, time), tz="GMT"))]
#     setkey(dep, date_time)
    
#     range_counts <- function(x, y) {
#         range1 = x$date_time - duration
#         range2 = x$date_time + duration
        
#         index1 = y[.(range1), roll=-Inf, nomatch=NA, mult="first", which=TRUE]
#         index2 = y[.(range2), roll=+Inf, nomatch=NA, mult="last",  which=TRUE]
        
#         start = pmin(index1, index2, na.rm=TRUE)
#         end   = pmax(index1, index2, na.rm=TRUE)
        
#         diff_xy <- function(date_time, i) {
#             ii = start[i]:end[i]
#             aa = date_time
#             bb = y$date_time[ii]
#             ii[abs(bb-aa) <= duration]
#         }
#         ans = x[, .(yi=diff_xy(date_time, .I)), by=.(xi=seq_len(nrow(x)))]
#         ans[, `:=`(x.ip = x$ip_id[xi], y.ip = y$ip_id[yi])]
#         ans[, `:=`(x.cntry = x$country[xi], y.cntry = y$country[yi])]
#         idx = unique(ans[x.ip == y.ip & x.cntry == y.cntry]$yi)
#         y$package[idx]
#     }
    
#     ans <- dep[, .(dep_pkg=range_counts(this[.BY], .SD)), by=.(i.package, key)]
#     setnames(ans, "i.package", "package")
#     setkey(ans, package, key)
#     ans[, .(dep_N=.N), by=names(ans)]
# }

# plotly_logs <- function(dt, chart=c("bar", "line")) {
#     if (!requireNamespace("plotly"))
#         stop("'plotly' couldn't be loaded")
#     chart = match.arg(chart)
#     if ("dep_N" %chin% names(dt)) {
#         p1 = plot_ly(dt, x=key, y=tot_N-dep_N, type=chart, color=package)
#         p1 = plotly::layout(p1, legend=list(x=1.0, y=0.5), title="bla1")
#         p2 = plot_ly(dt, x=key, y=dep_N, type="bar", color=package, showlegend=FALSE)
#         p2 = plotly::layout(p2, title="bla2")
#         p = subplot(p1, p2, margin=0.1, nrows=2L)
#         p = plotly::layout(p, xaxis=list(title=""), xaxis2=list(title="Duration"), yaxis=list(title=""), yaxis2=list(title="N"))
#     } else {
#         p = plot_ly(dt, x=key, y=pkg_N, type="bar", color=package)
#         p = plotly::layout(p, title="Total downloads (direct + dependencies)", legend=list(x=1.0, y=0.5))
#     }
#     p
# }

# monthly_stats <- function(dat, packages=c("data.table")) {
#     plot_stats(dat, "month", packages)
# }
# 
# daily_stats <- function(dat, packages=c("data.table")) {
#     plot_stats(dat, "day", packages)
# }
# 
# yearly_stats <- function(dat, packages=c("data.table")) {
#     plot_stats(dat, "year", packages)    
# }
# 

# dep_stats2 <- function(dt, packages, type, duration) {

#     if (is.null(key(dt)) || key(dt)[1L] != "package")
#         stop("dt's first key column must be 'package'")
#     if (!is.character(packages)) stop("'packages' must be a character vector")
#     types = c("yearly", "monthly", "weekly", "daily", "hourly")
#     if (!is.character(type) || length(type) != 1L) 
#         stop("'type' must be a character vector of length 1")
#     if (!type %chin% types) 
#         stop(paste("'type' must be one of:", paste(types, collapse=","), sep=" "))
#     duration = suppressWarnings(as.integer(duration))
#     if (duration < 0L || is.na(duration)) 
#         stop("'duration' is in seconds, can't be a negative value, expecting >= 0L")

#     this = dt[packages][!is.na(date) & !is.na(time)][, key := key_(date, time, type)]
#     this[, c("minust", "plust") := { 
#             tmp = as.POSIXct(paste(date, time), tz="GMT");
#             list(tmp-duration, tmp+duration)}]
#     setkey(this, package, key, country, ip_id, minust, plust)
    
#     dep = dt[deps_(packages)][!is.na(date) & !is.na(time)][, key := key_(date, time, type)]
#     dep[, c("time1", "time2") := {
#             tmp = as.POSIXct(paste(date, time), tz="GMT");
#             list(tmp, tmp)}]
#     setkey(dep, i.package, key, country, ip_id, time1, time2)

#     ans = foverlaps(dep, this, type="within", nomatch=0L)
#     ans = unique(ans, by=names(ans)[1:6])[, .N, by=.(i.package, key, package)]
#     setnames(ans, c("i.package", "package", "N"), c("package", "dep_pkg", "dep_N"))
#     return(ans)
# }

