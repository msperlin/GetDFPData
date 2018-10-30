## Version 1.2 (2018-10-11)

- Fixed [https://github.com/msperlin/GetDFPData/issues/8](issue 8), bug in remuneration data

## Version 1.1 (2018-10-10)

- Fixed bug in thousands flag for DFP data before 2007

## Version 1.0 (2018-08-28)

- Fixed bug in values of reports due to previous update. 
- Fixed bug in text of auditing reports
- Added tables 3.5 (payout e dividendos)
- Added table 15.3 (distribuição capital)
- Added table 18.1 (detalhes ações)
- Added choice for different data systems (DFP/FRE/FCA)

## Version 0.9 (2018-07-26)

- Fixed thousands bug. Some small companies had nominal values, while the majority had it multiplied by thousands. The new code will divide by 1000 for the small companies. Any reported value in the DFP is always multiplied by thousands.

## Version 0.8 (2018-07-16)

- Changed default value for get.new.files to FALSE. The code was not being able to scrape the data from B3 an returned an error. 

## Version 0.7 (2018-05-01)

- Fixed bugs

## Version 0.6 (2018-02-15)

- Fixed bugs
- added information about companies in output (cnpj, cvm registration and constitution)

## Version 0.5 (2017-11-01)

First version, with most code structure from GetITRData
