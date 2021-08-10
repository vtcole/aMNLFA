## Test environments
* local R installation, R 4.0.0
* ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a resubmission. The original submission contained two notes. First, the package for the par() function had not been declared in the aMNLFA_DIFplot() function; this has been rectified. Second, there were a few words the automated check thought would be misspelled; the name of the author (Gottfredson) and the word "iteratively" have been retained.. The original version of the package was archived due to not passing CRAN checks. In particular, when run in the development version of R on Debian, it generated an error due to improperly calling order() on a data frame in the aMNLFA.scoreplots() function. The offending code has been replaced. Additionally, as the maintainer of this package I neglected to update my email address when I changed institutions; this caused me to miss repeated notifications from CRAN that the package had failed checks. I have updated my email address.