## ---- eval=FALSE---------------------------------------------------------
#  # Release version in CRAN
#  install.packages('GetDFPData') # not in CRAN yet
#  
#  # Development version in Github
#  devtools::install_github('msperlin/GetDFPData')

## ------------------------------------------------------------------------
library(GetDFPData)
library(tibble)

gdfpd.search.company('petrobras',cache.folder = tempdir())


## ------------------------------------------------------------------------
df.info <- gdfpd.get.info.companies(type.data = 'companies', cache.folder = tempdir())

glimpse(df.info)

