# neon-swift

Front end and back end code for Swift Shiny App

## Background  
The `swift` shiny application's main goal is to provide useful and actionable data to NEON staff members. Data driven desision making is key to ensuring we deliver high quality data to the NEON Data Portal.  
The data within this app are sourced from multiple locations: L0 (Presto database), the TIS LC's, CVAL (Mike Pursley data), and Maximo (asset management software). These data are sourced, cleaned, and saved to an S3 bucket that is readible to anyone on the network. This allows the app to not store any important credentials in it's directory or elsewhere.  

## Docker
This app is deployable by Docker. This is the path we are headed down as this not only makes our apps scalable through `shinyproxy`, but also encourages best practices and removes the reliance of a server administrator installing all the libraries and dependencies on the server. Instead you control your own destiny.   
  
1. To build the image, first clone the repo (where doesn't really matter, but somewhere familiar):  
`git clone git@github.com:NEONScience/neon-swift.git`  
  
2. Change directory to the repo
`cd neon-swift`  
  
3. Build the image  
* For dev:  `sudo podman build -t neon-swift:dev .`  
* For prod: `sudo podman build -t neon-swift:prod .`  

4. Once the image has finished building run the container (still need to define acceptable port ranges):  
* For dev:  `sudo podman run -it -d -p 4782:3838 --name swift_dev neon-swift:dev`  
* For prod: `sudo podman run -it -d -p 4781:3838 --name swift neon-swift:prod`  

6. Set up gsutil (this is fun!)
* The setup is based upon [this SO post](https://stackoverflow.com/a/71605401) so bare with me.  
* To authenticate to the public gs data, you must have a user authenticated to access the data otherwise you get an error similar to this when trying to run `gsutil` commands.  
* `> ServiceException: 401 Anonymous caller does not have storage.objects.list access to the Google Cloud Storage bucket.`  
* So to authenticate you must first install the [Google Cloud CLI](https://cloud.google.com/sdk/docs/install-sdk) on your local machine, following along there.  
* On your local machine after you've install GC CLI, you will run `gcloud init`, where you will authenticate by following the prompts and logining in through google, allowing GSUtil to have access.  
* Now that you're set up on Windows, you can set up the container's permissions. Assumming you've already started the container, enter the container using the following command (container name may vary)  
* `podman exec -it swift bash`  
* Now that you are in the container as root, we must switch to the shiny user.
* `su shiny`  
* Verify you are the shiny user  
* `whoami`  
* Run `gcloud auth login` as the shiny user and you should see the following pop up. 
```
gcloud auth login --remote-bootstrap="https://accounts.google.com/o/oauth2/auth?response_type=code&client_id=****.apps.googleusercontent.com&scope=openid+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fcloud-platform+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fappengine.admin+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fcompute+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Faccounts.reauth&state=****&access_type=offline&code_challenge=****&code_challenge_method=S256&token_usage=remote"
```  
* Copy the whole output and paste that into a local `cmd` terminal and run it  
* This should take you to the same authentication page from before. Accept and move throught the prompts
* ![image](https://user-images.githubusercontent.com/45089457/164304070-c386555e-a334-40be-84ff-cd8c05c90eb7.png)  
* ![image](https://user-images.githubusercontent.com/45089457/164304289-2388f43c-dc84-4f23-a841-8c9d2e9d5d88.png)
* Once you see that you've been authenticated ![image](https://user-images.githubusercontent.com/45089457/164303207-50d1c10e-0598-4662-81fb-f37adb9a670d.png)  
* You should see a new output in the local `cmd` terminal window.  
```
https://localhost:8085/?state=****&code=****&scope=email%20openid%20https://www.googleapis.com/auth/userinfo.email%20https://www.googleapis.com/auth/cloud-platform%20https://www.googleapis.com/auth/appengine.admin%20https://www.googleapis.com/auth/compute%20https://www.googleapis.com/auth/accounts.reauth&authuser=0&hd=****&prompt=consent
```  
* Copy and paste this into your docker terminal where you are the shiny user from before and press ENTER.
* Congrats! 

5. Well these commands will stop and remove the old container
### DEV  
* `sudo podman container stop swift_dev`  
* `sudo podman container rm swift_dev`  
* `sudo podman run -it -d -p 4782:3838 --name swift_dev neon-swift:dev`  

### PROD  
* `sudo podman container stop swift`  
* `sudo podman container rm swift`  
* `sudo podman run -it -d -p 4781:3838 --name swift neon-swift:prod`  

Note: We gather usage data.. I wonder if I just stored that in the app or if I needed to volume mount?

5. You can now test the data, launch the app on the browser and the open up a bash into the container  
`podman exec -it swift bash`
  
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

## The UI  
The UI is broken out into several tabs and subtabs by the `shinydashboard` tabs and subtabs. When the app is initially launched you are brought to the home page.

### Home Page  
This landing page has details introducing what each of the leftside menu items do and how they are useful. Addditionally there is a link to the 
