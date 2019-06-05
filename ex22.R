################################################################
#
# Creating dynamic reports
#
# communicate the results to others
#
install.packages(c("rmarkdown", "xtable", "knitr", "multcomp", "R2wd"))
# install Pandoc http://johnmacfarlane.net/pandoc/index.html
# install a LaTeX compiler, TeXworks editor for me
# add "C:\texlive\2017\bin\win32" into env_var/PATH
#
################################################################
#
# with R and Markdown
library(rmarkdown)
render("rinaction\\women.Rmd", "html_document")

## to print pdf
## replace  print(xtable(sfit), type="html", html.table.attributes="border=0")
## with     print(xtable(sfit), type="latex")
## then run
library(rmarkdown)
render("rinaction\\women.Rmd", "pdf_document")

## to print word
## Replace  library(xtable)
##          options(xtable.comment=FALSE)
##          print(xtable(sfit), type="html", html.table.attributes="border=0")
## with     library(knitr)
##          kable(sfit$coefficients)
## then run
library(rmarkdown)
render("rinaction\\women.Rmd", "word_document")

## RStudio File > New File > R Markdown for *.Rmd

################################################################
#
# with R and LaTeX
library(knitr)
## knit always output file in wd so set wd first
setwd("D:\\Dropbox\\Data\\R\\rinaction\\")
knit("drugs.Rnw")
knit2pdf("drugs.Rnw")

################################################################
#
# with R and open document (skipped)
library(odfWeave)
infile <- "salaryTemplate.odt"
outfile <- "salaryReport.odf"
odfWeave(infile, outfile)

################################################################
#
# with R and Microsoft Word (skipped)
library(R2wd)
library(devtools)
## RDCOMClient package is not available, 
## all 4 methods in https://github.com/omegahat/RDCOMClient not applicable
install.packages("RDCOMClient")
install.packages("rinaction\\RDCOMClient_0.93-0.zip", repos = NULL, type="source")
install_github("omegahat/RDCOMClient")

################################################################