# cran.stats

`cran.stats` is a package that provides functions to explore and visualise daily, monthly or yearly package download stats from Rstudio cran mirror logs. It also implements an approach to calculate the number of downloads for a given package alone by identifying and subtracting the downloads due to it's dependent packages.

# Installation

To install the package, use `devtools`:

```R
devtools::install_github("arunsrinivasan/cran.stats")
```

# Usage

There are three functions:

### read_logs

```R
read_logs(start, end, path="./", dir="cran-mirror", verbose=TRUE)
```

The function takes a `start` and `end` date and downloads all the logs available from the Rstudio mirror for that duration and places them in the directory `path/dir`. The downloaded files are automatically replaced with their unzipped versions. If an unzipped log file already exists, then the downloads are skipped. And if the downloaded file was corrupted (which would error while unzipping), then we attempt to download it just once again. If it fails again, we skip this log.

As an example, let's download all the logs between July 01, 2014 and July 31, 2014.

```R
dt = read_logs(start = as.Date("2014-07-01"), 
               end = as.Date("2014-07-31"), 
               path = "./", dir="cran-mirror", 
               verbose = TRUE)

dim(dt)
# [1] 4824243       5

key(dt)
# [1] "package" "date"    "time"   

dt["data.table"]
#          package       date     time country ip_id
#     1: data.table 2014-07-01 00:03:47      US    13
#     2: data.table 2014-07-01 00:04:30      US    13
#     3: data.table 2014-07-01 00:07:12      US     2
#     4: data.table 2014-07-01 00:09:10      US    51
#     5: data.table 2014-07-01 00:12:33      US    13
#    ---                                             
# 14393: data.table 2014-07-31 23:33:04      US  7159
# 14394: data.table 2014-07-31 23:33:09      US  7100
# 14395: data.table 2014-07-31 23:35:16      NZ  1185
# 14396: data.table 2014-07-31 23:52:41      US 11181
# 14397: data.table 2014-07-31 23:57:21      US  7159
```

### stats_logs

```R
stats_logs(dt, type="monthly", pacakges=c("data.table"), 
          dependency=TRUE, duration = 30L)
```

Have a look at `?stats_logs` for more info. Let's keep it the defaults as such and get the download stats for the month of July. Note that we've to set the `type` to `daily` as we'd like to get the number of downloads on each day.

```R
dt_s = stats_logs(dt, type="daily")
#        package        key tot_N dep_N
#  1: data.table 2014-07-01   456   136
#  2: data.table 2014-07-02   406    96
#  3: data.table 2014-07-03   393    78
#  4: data.table 2014-07-04   267    74
#  5: data.table 2014-07-05   178    60
#  6: data.table 2014-07-06   169    40
#  7: data.table 2014-07-07   450   102
#  8: data.table 2014-07-08   680   183
#  9: data.table 2014-07-09   663   147
# 10: data.table 2014-07-10   705   131
# 11: data.table 2014-07-11   709   176
# 12: data.table 2014-07-12   556    65
# 13: data.table 2014-07-13   676    35
# 14: data.table 2014-07-14   605   100
# 15: data.table 2014-07-15   637    98
# 16: data.table 2014-07-16   549   111
# 17: data.table 2014-07-17   586    91
# 18: data.table 2014-07-18   481   101
# 19: data.table 2014-07-19   230    59
# 20: data.table 2014-07-20   273    55
# 21: data.table 2014-07-21   525   137
# 22: data.table 2014-07-22   504    89
# 23: data.table 2014-07-23   605   142
# 24: data.table 2014-07-24   530   141
# 25: data.table 2014-07-25   394   109
# 26: data.table 2014-07-26   214    38
# 27: data.table 2014-07-27   215    46
# 28: data.table 2014-07-28   440   148
# 29: data.table 2014-07-29   456   106
# 30: data.table 2014-07-30   440   115
# 31: data.table 2014-07-31   405    86
#        package        key tot_N dep_N
```

### plot_logs

```R
plot_logs(dt)
```

Let's now plot it. Note that it detects the presence of `dep_N` column and automatically draws two plots, first with just the *direct* downloads for the packages specified, and the next with the number of downloads just due to dependencies.

```R
plot_logs(dt_s)
```

![Alt text](/../master/inst/examples/dt_july.png?raw=true "data.table daily download stats for July 2014")
