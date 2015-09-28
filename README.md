# pfunk
Retrieves US House and Senate data from Wikipedia, then makes parliament diagrams for each. 

## Introduction

This set of R code was originally intended to demonstrate [RVest](https://cran.r-project.org/web/packages/rvest/rvest.pdf) but grew into a useful set of code that makes Parliament Diagrams, after the ones I saw on Wikipedia weren't particularly uniform or easy to keep current (though see [this XKCD comic](https://xkcd.com/927/) for why I'm tilting at windmills). 

It handles small numbers of parties pretty well. Larger numbers of parties are less elegantly handled and are hard to keep contiguous, but I have made some improvements that help in recent revisions. It also provides more customizability than other scripts I have seen, though the computational geometry is no more robust or elegant than other examples I've seen. 
