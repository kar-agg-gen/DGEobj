## Comments from Maintainer

* This is a new package to be added to CRAN
* As many examples as possible were enabled, ones that require external (user) data are marked with \dontrun
* Code Coverage is 93%

---  

## Test environments

RStudio Server Pro (ubuntu 18.04.2)  

* R 3.5.3  
* R 3.6.3
* R 4.0.2

Travis-CI (ubuntu 16.04.6)

* R 3.6.3
* R 4.0.2
* R devel (2020-11-22 r79463)

WinBuilder

* devtools::check_win_devel()  
* devtools::check_win_release()  

RHub

* devtools::check_rhub(interactive = F)

---  

## R CMD check results


```
devtools::check()  

0 errors ✓ | 0 warnings ✓ | 0 notes ✓
```

---  

## Reverse dependencies


**NONE**

```
pdb <- available.packages()
tools::package_dependencies(packages = c('DGEobj'),
                            db = pdb, reverse = TRUE)
                            
$DGEobj  
NULL 
```
