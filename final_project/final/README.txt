Team Charlie: Dahjin, Oyin, Diva, and Alex

We looked at the relationship between COVID-19 deaths and campaign contributions.
The interactive part of the project is in ShinyApp.

We did not include the raw data in this file because the data set is so large. For this reason, we also excluded
January and February individual contributions from our analyses.

The committee contribution data from the FEC data is available at the follwoing link under "Contributions from commitees 
 to candidates & independent expenditures" and "2019-2020":
https://www.fec.gov/files/bulk-downloads/2020/pas220.zip

The individual contribution data from the FEC is available at the following link under "Contributions by indiivduals" 
and "2019-2020":
https://www.fec.gov/files/bulk-downloads/2020/indiv20.zip

The COVID-19 deaths data from The COVID Tracking Project is available at the followng link:
https://covidtracking.com/api/v1/states/daily.csv

The FEC website is not yet updated with the April campaign contribution data. We can add these data to our analyses
when they become available.

We cleaned the data in data_cleaning.R and export it to fullweekly.csv.

We then made plots of the data (referred to in ShinyApp) in plots.R.