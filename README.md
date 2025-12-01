# bigfoot-sightings

## Why Bigfoot?

As science majors, our group wanted to step outside our usual realm and explore 
a topic that blurs the line between myth and reality; Bigfoot. 
Being able to research without having to read an exhausting scientific paper 
gave us much joy, and we hope you got a similar amount of joy going through our website!

If you've ever wondered where Bigfoot has been found or where you should look next 
in your search, then our app is the right place for you. 

## What we offer

We provide interactive maps showing sighting locations, weather data analysis for reported
encounters, multiple filtering tools to explore patterns and trends, detailed 
information about each sighting report, and a probability predictor of seeing Bigfoot based
on user input. Other highlights include a visual representation of sighting descriptions
in the form of a word cloud; correlations with bear observations, forest coverage/land area,
and census land area; simple visualizations of sightings per state, temperature, and season
with a clickable interface, and an awesome moon phase tracker, detailing the number of sightings
per moon phase, and the correlation between moon brightness and number of sightings. 

# Our Data Sources

### Bigfoot Sightings Data
Our data comes from the Bigfoot Field Researchers Organization (BFRO) database via Timothy Renner, which contains thousands of 
reported sightings dating back several decades. Weather data included comes from historic meteorological sources.

Please see Timothy Renner's GitHub repository to obtain data:
https://github.com/timothyrenner/bfro_sightings_data

### Forest and Land Data
The data used to calculate the correlation for land size, forest size, and percent of state covered in forest was taken from this pdf published by the US Department of Agriculture in 2016. Table B-11 was used. It was converted into an excel file from a pdf and edited further in excel.
The rows giving totals for regions of the U.S. were deleted. Any row that was not a U.S. state was deleted. the columns "Annual inventory entry date" and "State annualized as of 2016" were deleted. Commas were removed from all numbers.
The file was then converted into a csv file so that it could be used for data manipulation. 
https://www.fs.usda.gov/sites/default/files/fs_media/fs_document/publication-15817-usda-forest-service-fia-annual-report-508.pdf

### Bear Data
Data for bear observations was downloaded from GBIF:
GBIF.org (17 November 2025) GBIF Occurrence Download  https://doi.org/10.15468/dl.kp96h3

All columns except gbifID, species, stateProvince, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, eventDate, day, month, and year 
were hidden as these were the only columns relevant for our project.
License was unspecified.

## Acknowledgements

We acknowledge the use of Claude AI (Sonnet 4.5) in this project. We used it to generate code throughout
the project to connect our ideas into tangible code. It was also used to search for the bear data set. Finally,
we used Claude to detect any errors in our code.

