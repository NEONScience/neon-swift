# neon-swift

Front end and back end code for Swift Shiny App

## Background  
The `swift` shiny application's main goal is to provide useful and actionable data to NEON staff members. Data driven desision making is key to ensuring we deliver high quality data to the NEON Data Portal.  
The data within this app are sourced from multiple locations: L0 (Presto database), the TIS LC's, CVAL (Mike Pursley data), and Maximo (asset management software). These data are sourced, cleaned, and saved to an S3 bucket that is readible to anyone on the network. This allows the app to not store any important credentials in it's directory or elsewhere.  

## Style  

### Server folder   
The style of organization was taken from Cove as a best practice of splitting out each tab into it's own server file. This is the case for this app. Each tab (less the Home Page) has it's own associated server file that controls all of the data ingest, cleaning, and plotting for that tab.
### Tab loading   
If a tab is automatically plots as a user changes inputs that tab does not load until the user clicks on it. If the user must press a button in order to plot, then this is just loaded on server start up as this does not impact load times. I.e. when you load in to the app, if the plot's inputs are there it will automatoicall start strying to render the plot. This is avoided by having those tabs load only when clicked. By allowing the other tabs to not wait on their tab to be clicked, they retain their plots and don't ahve to "re-loaded" when revisited.

## The UI  
The UI is broken out into several tabs and subtabs by the `shinydashboard` tabs and subtabs. When the app is initially launched you are brought to the home page.

### Home Page  
This landing page has details introducing what each of the leftside menu items do and how they are useful. Addditionally there is a link to the 

