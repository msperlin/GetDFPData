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

## ------------------------------------------------------------------------
name.companies <- 'PETRÓLEO BRASILEIRO  S.A.  - PETROBRAS'
first.date <- '2004-01-01'
last.date  <- '2006-01-01'
type.statements <- 'individual'

df.reports <- gdfpd.GetDFPData(name.companies = name.companies, 
                               first.date = first.date,
                               last.date = last.date,
                               type.info = type.statements, 
                               cache.folder = tempdir())

## ------------------------------------------------------------------------
glimpse(df.reports)

## ------------------------------------------------------------------------
df.income.long <- df.reports$fr.income[[1]]

glimpse(df.income.long)

## ------------------------------------------------------------------------
df.income.wide <- gdfpd.convert.to.wide(df.income.long)

knitr::kable(df.income.wide )

## ------------------------------------------------------------------------
my.companies <- c('PETRÓLEO BRASILEIRO  S.A.  - PETROBRAS',
                  'BANCO DO ESTADO DO RIO GRANDE DO SUL SA')

first.date <- '2005-01-01'
last.date  <- '2009-01-01'
type.statements <- 'individual'

df.reports <- gdfpd.GetDFPData(name.companies = my.companies, 
                               first.date = first.date,
                               last.date = last.date,
                               type.info = type.statements,
                               cache.folder = tempdir())

## ------------------------------------------------------------------------
glimpse(df.reports)

## ------------------------------------------------------------------------
df.assets <- do.call(what = rbind, args = df.reports$fr.assets)
df.liabilities <- do.call(what = rbind, args = df.reports$fr.liabilities)

df.assets.liabilities <- rbind(df.assets, df.liabilities)

## ------------------------------------------------------------------------
library(dplyr)

my.tab <- df.assets.liabilities %>%
  group_by(company.name, ref.date) %>%
  summarise(Liq.Index = acc.value[acc.number == '1.01']/ acc.value[acc.number == '2.01'])

my.tab

## ------------------------------------------------------------------------
library(ggplot2)

p <- ggplot(my.tab, aes(x = ref.date, y = Liq.Index, fill = company.name)) +
  geom_col(position = 'dodge' )
print(p)

## ---- eval=FALSE---------------------------------------------------------
#  my.basename <- 'MyExcelData'
#  my.format <- 'csv' # only supported so far
#  gdfpd.export.DFP.data(df.reports = df.reports,
#                        base.file.name = my.basename,
#                        type.export = my.format)

