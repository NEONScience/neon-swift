# neon-swift

Front end and back end code for Swift Shiny App

## Background  
The `swift` shiny application's main goal is to provide useful and actionable data to NEON staff members. Data driven desision making is key to ensuring we deliver high quality data to the NEON Data Portal.  
The data within this app are sourced from multiple locations: L0 (Presto database), the TIS LC's, CVAL (Mike Pursley data), and Maximo (asset management software). These data are sourced, cleaned, and saved to an S3 bucket that is readible to anyone on the network. This allows the app to not store any important credentials in it's directory or elsewhere.  
  
## Style  

### UI indents  
For this app I ignored most of the automatic indenting as this makes the `ui.R` more difficult to read as the x scroll distance increases rapidly as you add tabs, subtabs, columns, rows, etc. I try to reduce everything down to a single tab indent, rather than the indent to the `(` portion from the line above that `R` does automatically. I think this makes the `ui.R` more readible and easier to digest.  

### Server folder   
The style of organization was taken from Cove as a best practice of splitting out each tab into it's own server file. This is the case for this app. Each tab (less the Home Page) has it's own associated server file that controls all of the data ingest, cleaning, and plotting for that tab.
### Tab loading   
If a tab is automatically plots as a user changes inputs that tab does not load until the user clicks on it. If the user must press a button in order to plot, then this is just loaded on server start up as this does not impact load times. I.e. when you load in to the app, if the plot's inputs are there it will automatoicall start strying to render the plot. This is avoided by having those tabs load only when clicked. By allowing the other tabs to not wait on their tab to be clicked, they retain their plots and don't ahve to "re-loaded" when revisited.
### Namespace calls
90% of the fucntion calls in the app are called using their libraries namespace call ie `dplyr::filter()` this does two things. 
1. Makes it explicit what library you are using for each function (and is generally a best practice).
2. If for some odd reason your library is not loaded, the namespace call allows R to use the package without loading it.
### S3  
There's a lot of data that needs to be sourced and storing it locally in the `/srv/shiny-server/` folder is not advisable for a couple of reasons.
1. This results in a lot of disk space being used to host the data on the server. (IT will get mad and rightfully so).
2. What happens when you have to redeploy the app to the new server, or if you are developing in a Dev/Prod fashion. Then you have to have the data in two places. This is wasteful.  

So by storing it on S3, you make your app easier to deploy, reduce harddrive requirements, and keep IT happy!  
This style allows Swift to be deployed by a simple Docker command as well, further increasing the deployability and reproducibility of the application.   
### Docker
As perviously mentioned this app is deployable by Docker. This is the path we are headed down as this not only makes our apps scalable through `shinyproxy`, but also encourages best practices and removes the reliance of a server Admin installing all the libraries and dependencies on the server. Instead you control your own destiny.

## The UI  
The UI is broken out into several tabs and subtabs by the `shinydashboard` tabs and subtabs. When the app is initially launched you are brought to the home page.

### Home Page  
This landing page has details introducing what each of the leftside menu items do and how they are useful. Addditionally there is a link to the 

