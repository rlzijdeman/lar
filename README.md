lar
===

lar: History of labour relations package

This Github repository contains files and documentation for the R-package 'lar'. The 'lar' package is intended for researchers studying historical labour relations (see http://www.historyoflabourrelations.org). The package allows for easy access of excel files in the standard defined by the Global Collaboratory on the History of Labour Relations. The package also allows for visualisation of labour relations according to the Collaboratory's format. lar is available from CRAN.
  
~~You can install the lar package from R by typing:~~
**The package is available from CRAN only as source. This may change over the next few days (May 1, 2014)**
```{r}
  install.packages("lar")  
  library(lar)  
  ?lar  # to check the documentation
```
You can install the lar package from Github. The package on Github provides the latest additions to the package, but has not been checked by CRAN for inconsistencies. To install the package from Github:  
```{r}
  install.packages("devtools")  
  library(devtools)  
  install_github("lar", username = "rlzijdeman", subdir = "pkg")  
  library(lar)  
  ?lar #to check the documenten
```
As a first example of what the package can do, type:
```{r}
  data(spain.1900)
  draw.lar(spain.1900, sex = "female", mono.chrome = TRUE)
```





