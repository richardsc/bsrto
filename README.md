# Barrow Strait Real Time Observatory Shiny app

Running the app requires two parts:

1. Run the `download_data.R` script (on a cronjob) which will
   periodically fetch the near real time BSRTO data from the ftp site
   at ftp://ftp.dfo-mpo.gc.ca/pittmanm/bsrto/2017-2018/, and re-save
   it as a bunch of local `.rda` files for quick loading in the shiny
   app

2. Run the shiny app loading the local data.
