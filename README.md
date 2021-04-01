# DEPRECATED (as of 2020-07-01)

Back in 2020 I decided to split the GetDFPData package between [GetDFPData2](https://github.com/msperlin/GetDFPData2) and [GetFREData](https://github.com/msperlin/GetFREData). The current code will stay in Github but will not be maintained. 

Be aware that GetDFPData is **no longer available in CRAN**. Future development will be concentrated in both new packages. Read this [blog post](https://www.msperlin.com/blog/post/2020-07-18-new_packages-getfredata-getdfpdata2/) for the reasons behind my decision.


# Reading Annual Financial Reports from Bovespa's DFP, FRE and FCA systems

Package `GetDFPData` provides a R interface to all financial statements and companies information available in B3's [website](http://www.b3.com.br/pt_br/). It not only downloads the data but also organizes it in a tabular format and allows the use of inflation indexes. Users can select companies and a time period to download all available data. Several information about current companies, such as sector and available quarters are also at reach. The main purpose of the package is to make it easy to access financial statements in large scale research, facilitating the reproducibility of corporate finance studies.

# Shiny interface

A web interface of the package is available at [https://www.msperlin.com/shiny/GetDFPData/](https://www.msperlin.com/shiny/GetDFPData/).

# Installation 

The package is available in CRAN (release version) and in Github (development version). You can install any of those with the following code:

```
# Release version in CRAN
install.packages('GetDFPData') 

# Development version in Github
devtools::install_github('msperlin/GetDFPData')
```

# How to use GetDFPData

See manual and vignette in CRAN.
