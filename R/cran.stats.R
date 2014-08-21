## exported functions
read_logs <- function(start, end, path="./", dir="cran-mirror", verbose=TRUE) {
    
    if (class(start) != "Date") {
        warning("Coercing 'start' to Date class")
        start = as.Date(start)
    }
    if (class(end) != "Date") {
        warning("Coercing 'end' to Date class")
        end = as.Date(end)
    }
    urls = urls_(seq(start, end, by="days"))
    odir = file.path(path, dir)
    read_logs_(urls, odir, verbose)
}

stats_logs <- function(dt, type="monthly", packages=c("data.table"), dependency=TRUE, duration=30L) {

    dependency = as.logical(dependency)    
    if (is.na(dependency))
        stop("'dependency' must be logical TRUE/FALSE")    
    if (dependency) {
        if (!suppressMessages(require(devtools)))
            stop("'devtools' could not be loaded")
        deps = dep_stats(dt, packages, type, duration)
        deps = deps[, list(dep_N=sum(dep_N)), by=c("package", "key")]
        setkey(deps, package, key)
    }
    this = pkg_stats(dt, packages, type)
    
    # get final counts
    if (dependency) ans = this[deps] else ans = this
    ans
}

plot_logs <- function(dt) {
    ans = copy(dt)
    if (!suppressMessages(require(ggplot2))) 
        stop("'ggplot2' couldn't be loaded")
    title = "Stats for the given duration"
    setnames(ans, "key", "Period")

    if ("dep_N" %chin% names(ans)) {
        if (!suppressMessages(require(gridExtra))) 
            stop("'gridExtra' couldn't be loaded")

        ans[, pkg_N := tot_N - dep_N][, tot_N := NULL]
        
        pl1 = ggplot(data = ans, aes(x=Period, y=pkg_N, fill = package)) + 
        geom_bar(stat = "identity", position="dodge") + 
        theme_bw() + labs(title=paste(title, ": Direct downloads only")) + 
        scale_fill_brewer(palette="Set1") + 
        # scale_y_continuous(breaks = round(seq(0L, max(ans$pkg_N), length.out=5L))) + 
        theme(axis.text.x = element_text(angle=45, hjust=1))

        pl2 = ggplot(data = ans, aes(x=Period, y=dep_N, fill = package)) + 
        geom_bar(stat = "identity", position="dodge") + 
        theme_bw() + labs(title=paste(title, ": Through dependencies only")) + 
        scale_fill_brewer(palette="Set1") + 
        # scale_y_continuous(breaks = round(seq(0L, max(ans$dep_N), length.out=5L))) + 
        theme(axis.text.x = element_text(angle=45, hjust=1))
        
        pl = list(pl1, pl2)
        pl = do.call(marrangeGrob, c(pl, list(nrow=2L, ncol=1L,  top=quote(NULL))))
    } else {
        setnames(ans, "tot_N", "pkg_N")
        pl = ggplot(data = ans, aes(x=Period, y=pkg_N, fill = package)) + 
        geom_bar(stat = "identity", position="dodge") + 
        theme_bw() + labs(title=paste(title, ": Direct + Dependencies")) + scale_fill_brewer(palette="Set1") + 
        # scale_y_continuous(breaks = round(seq(0L, max(ans$pkg_N), length.out=5L))) + 
        theme(axis.text.x = element_text(angle=45, hjust=1))
    }
    pl
}

## internal functions
urls_ <- function(days) {
    if (is.character(days)) days = as.Date(days)
    paste('http://cran-logs.rstudio.com/', year(days), '/', days, '.csv.gz', sep="")
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

download_ <- function(src, dest, ...) {
    suppressWarnings(
        download.file(url=src, destfile=dest, ...)
    )
}

download_logs <- function(urls, path, verbose=TRUE) {
    dir.create(path, showWarnings = FALSE)
    idx = !(match_(urls, path, regex="\\.gz$"))
    src  = urls[idx]
    dest = file.path(path, basename(src))
    tot  = length(src)
    ans  = vector("logical", tot)
    for ( i in seq_along(src)) {
        if (verbose) verbose_("Fetching logs", i, tot, basename(src[i]))
        tryCatch({
            download_(src[i], dest[i], quiet=TRUE)
            ans[i] = TRUE
        }, error = function(e) {
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
            this_url = urls_(gsub("\\..*$", "", basename(dest[i])))
            ans = download_logs(this_url, path, verbose=FALSE)
            if (ans && rec==1L) unzip_logs(this_url, path, rec+1L)
            else warning(paste("Recursion limit reached, 
               couldn't unzip file", basename(dest[i]), sep=" "))
        }
    }
    invisible(NULL)
}

fread_logs <- function(urls, path, verbose=TRUE) {
    idx  = match_(urls, path, regex="\\.gz$", pattern="\\.csv$", reverse=TRUE)
    dest = file.path(path, list.files(path, pattern="\\.csv$")[idx])
    tot  = length(dest)
    sel  = c("date", "time", "package", "country", "ip_id")
    ans  = lapply(seq_along(dest), function(i){
        if (verbose) verbose_("Fread(ing) logs   ", i, tot, basename(dest[i]))
        fread(dest[i], select=sel)
    })
    ans = rbindlist(ans)
    keycols = c("package", "date", "time")
    setkeyv(ans, keycols)
    setcolorder(ans, c(keycols, setdiff(names(ans), keycols)))
}

read_logs_ <- function(urls, odir, verbose) {
    download_logs(urls, odir, verbose)
    unzip_logs(urls, odir, 1L, verbose)
    fread_logs(urls, odir, verbose)
}

yearly  = function(x) { gsub("^(.*)-(.*)-(.*)$", "\\1", x, perl=TRUE) }
monthly = function(x) { gsub("^(.*)-(.*)-(.*)$", "\\1-\\2", x, perl=TRUE) }
daily   = function(x) { x }
weekly  = function(x) {
    week = yday(as.IDate(x)-1L)%/%7L + 1L
    paste(yearly(x), week, sep='-')
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
    dep_pkg = lapply(packages, revdep)
    dep_len = vapply(dep_pkg, length, 0L)
    package = rep(packages, dep_len)
    uses_it = unlist(dep_pkg, use.names=FALSE)
    dep_pkg = data.table(package = package, dependency = uses_it)
    setkey(dep_pkg, dependency)
}

dep_stats <- function(dt, packages, type, duration) {

    if (is.null(key(dt)) || key(dt)[1L] != "package")
        stop("dt's first key column must be 'package'")
    if (!is.character(packages)) stop("'packages' must be a character vector")
    types = c("yearly", "monthly", "weekly", "daily", "hourly")
    if (!is.character(type) || length(type) != 1L) 
        stop("'type' must be a character vector of length 1")
    if (!type %chin% types) 
        stop(paste("'type' must be one of:", paste(types, collapse=","), sep=" "))
    duration = suppressWarnings(as.integer(duration))
    if (duration < 0L || is.na(duration)) 
        stop("'duration' is in seconds, can't be a negative value, expecting >= 0L")

    this = dt[packages][!is.na(date) & !is.na(time)][, key := key_(date, time, type)]
    this[, date_time := as.integer(as.POSIXct(paste(date, time), tz="GMT"))]
    setkey(this, package, key)
    
    dep = dt[deps_(packages)][!is.na(date) & !is.na(time)]
    dep[, key := key_(date, time, type)]
    dep[, date_time := as.integer(as.POSIXct(paste(date, time), tz="GMT"))]
    setkey(dep, date_time)
    
    range_counts <- function(x, y) {
        range1 = x$date_time - duration
        range2 = x$date_time + duration
        
        index1 = y[J(range1), roll=-Inf, nomatch=NA, mult="first", which=TRUE]
        index2 = y[J(range2), roll=+Inf, nomatch=NA, mult="last",  which=TRUE]
        
        start = pmin(index1,index2,na.rm=TRUE)
        end   = pmax(index1,index2,na.rm=TRUE)
        
        diff_xy <- function(date_time, i) {
            ii = start[i]:end[i]
            aa = date_time
            bb = y$date_time[ii]
            ii[abs(bb-aa) <= duration]
        }
        ans = x[, list(yi=diff_xy(date_time, .I)), by=list(xi=seq_len(nrow(x)))]
        ans[, `:=`(x.ip = x$ip_id[xi], y.ip = y$ip_id[yi])]
        ans[, `:=`(x.cntry = x$country[xi], y.cntry = y$country[yi])]
        idx = unique(ans[x.ip == y.ip & x.cntry == y.cntry]$yi)
        y$package[idx]
    }
    
    ans <- dep[, list(dep_pkg=range_counts(this[as.data.table(.BY)], .SD)), by=list(i.package, key)]
    setnames(ans, "i.package", "package")
    setkey(ans, package, key)
    ans[, list(dep_N=.N), by=names(ans)]
}

pkg_stats <- function(dt, packages, type) {

    if (is.null(key(dt)) || key(dt)[1L] != "package")
        stop("dt's first key column must be 'package'")
    if (!is.character(packages)) stop("'packages' must be a character vector")
    types = c("yearly", "monthly", "weekly", "daily", "hourly")
    if (!is.character(type) || length(type) != 1L) 
        stop("'type' must be a character vector of length 1")
    if (!type %chin% types) 
        stop(paste("'type' must be one of:", paste(types, collapse=","), sep=" "))

    this = dt[packages][!is.na(date) & !is.na(time)][, key := key_(date, time, type)]
    ans = this[, list(tot_N = .N), by=list(package, key)]
    setkey(ans, package, key)
}


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
