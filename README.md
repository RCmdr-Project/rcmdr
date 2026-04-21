# Rcmdr

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/Rcmdr)](https://cran.r-project.org/package=Rcmdr)
[![CRAN\_MonthlyDownloads](http://cranlogs.r-pkg.org/badges/Rcmdr)](https://cran.r-project.org/package=Rcmdr)

An [R](https://www.r-project.org/) package that provides a platform-independent basic-statistics GUI (graphical user interface) for R, based on the tcltk package.

## Installation

Install the released version of **Rcmdr** from CRAN using:

``` r
install.packages("Rcmdr")
```

or from GitHub using:

``` r
devtools::install_github("RCmdr-Project/rcmdr/pkg")
```

or the development version from GitHub using:

``` r
devtools::install_github("RCmdr-Project/rcmdr/pkg@devel")
```

Install the `devtools` package using 

``` r
install.packages("devtools")
```

if necessary, before installing from GitHub.

## History

From its initial release (version 0.8-2) on 2003-05-26, through version 2.9-5 on 2024-10-25, John Fox was the lead developer of the Rcmdr package. Following his passing in November 2025, version 2.12.2 (released on 2026-04-11) marks the transition to a new maintainer and development team.

For a complete list of previous released versions, see [https://cran.r-project.org/src/contrib/Archive/Rcmdr/](https://cran.r-project.org/src/contrib/Archive/Rcmdr/).

## Language

R typically detects the system's language automatically and, if a translation is available, uses it by default. If the automatic detection fails, or if you wish to change the language for any other reason, you can do so by following the steps below.

Starting with R-Commander version 2.12.2 (2026-04-11), you can change the language by selecting `Tools` -> `Options...` from the menu. 
A dialog box will appear showing the available languages. 
After selecting your preferred language and clicking the `Restart R Commander` button, the interface language will update.

To make this change permanent, select `Tools` -> `Save Rcmdr options...` from the menu. 
In the dialog box uncomment the following lines (remove the # characters) to start the R Commander automatically whenever R starts:

\# local({

\#    old <- getOption('defaultPackages')

\#    options(defaultPackages = c(old, 'Rcmdr'))

\# })

You may also make additional changes to the startup options at this time. 
Then, click the `Save` button and choose your user’s home directory to save the file. 
Note that if you modify the filename, it will not be loaded automatically upon startup.

## Release plan

The current devel version is available in the devel branch for testing purposes.

New versions are expected to be released at least every six months. 
A development version will be made available for testing one month prior to each official release.
