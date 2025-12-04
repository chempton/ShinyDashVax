# michigan-school-vaccination-dashboard

Update: This dashboard has been finalized for 2024 data, and can be accessed online through its [ShinyApps.io link](https://52hu04-cameron-hempton.shinyapps.io/shinydashvaxdeploy/)

This dashboard was created to satisfy the Applied Practice Experience requirement in fulfillment of a Master of Public Health from the University of Michigan

It is intended for quick and straightforward viewing and interpretation of immunization waiver rates at the K/7 level for local health departments, parents, or any individual interested in a breaking down school immunization data in a more accessible way than massive spreadsheets.


##Quick Use Guide
* Ensure that data from MDHHS is correctly downloaded and named in data/ folder
* If you elect to make any changes to dataScript.R, run it once to generate the data object used in the dashboard
* Open and run app.R 
* (Optional) - Open and run appTEST.R to check aggregation logic used in app.R


### File and Directory Structure:
app.R
appTEST.R
dataScript.R
data/
* kindergarten_waivers.xlsx
* seventhgrade_waivers.xlsx
* immunodata.RData

### File Descriptions:

app.R:
Main Shiny dashboard file:
* Loads pre-processed data from data/immunodata.RData
* Supports selection by county, district, school, and grade level aggregation and visualization of immunization waiver data
* Note that average vaccine waiver rates for district, or county in the card summaries are unweighted, and are simply averages of each school's overall value. To see overall district/county averages, go to the district/county tab rather than the sub-level averaging which is intended for comparison of schools against others in the district.

appTEST.R:
The same dashboard as contained in app.R, but with an additional *TEST* tab, which contains some internal validity checks for how the server aggregates data, making sure what is presented is valid

dataScript.R:
Data wrangling and cleaning script which handles
* Raw immunization waiver data downloaded from the MDHHS website (included in the .zip)
* Cleans and aggregates waiver data
* Generates feature columns used by dashboards
* Outputs a .RData file, immunodata.RData, containing processed data objects for use in dashboard applications

data/:
Directory containing all datafiles
immunizationKindergarten2024.xlsx - Kindergarten immunization waiver data
immunizationSeventh2024.xlsx - Seventh grade immunization waiver data 
immunodata.RData - data processed from the above xlsx files by dataScript.R

### Team
The project team responsible for developing this dashboard, as well as delivering a presentation on best practices for approaching Vaccine Misinformation is comprised of MPH students:
Cameron Hempton, Ashna Patel, Ashley Dittmar, Chetna Kumari, Zoe Gurney, and Sarah Olson 

### Contact and Support:
For any questions, reach out to Cameron Hempton at chempton@umich.edu



###
