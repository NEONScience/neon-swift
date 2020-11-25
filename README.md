# NEON-Swift

Front end and back end code for Swift Shiny App

<a href='http://den-devshiny-1.ci.neoninternal.org/swift/'><img src='www/swiftlogo.jpg' align="right" height="138.5" /></a>


Swift displays sensor data and diagnostics from a multitude of L0 sources.
- Mike Pursley's Status emails (IS3R Tab)
  - data aggregated daily
- Dan Allen's SoH server (IS3R Tab - LC Services Tab)
  - data aggregated daily
- Josh Roberti's EC_Plots data grabber (Eddy Co Plots Tab)
  - data aggregated weekly
- CVAL Presto Pulls (Calibrations and Validations Tab)
  - data aggregated daily

This data is used for identifying data quality issues, troubleshooting root causes for flagged data, and monitoring overall system health.

# Swift - Shiny App

Swift is broken out into 4 tabs:    
<a href='http://den-devshiny-1.ci.neoninternal.org/swift/'><img src='www/swiftSideBar.JPG' align="right" height="138.5" /></a>  
1. Home
   - This is the landing page when you load the application.
2. IS3R
   - This tab is the home for Gas Cylinder status, Wet Dep Temp Checks, Belfort levels, and LC-Services. Users can use data on this tab to check if a cylinder is leaky or is getting low and requires replacement, verify Wet Dep Chambers are maintaining temperature, if the Belfort is getting full and requires emptying/calibration.
3. Calibrations and Validations
   - Users can check on the Calibrations and Validations (CVAL) of their site's gas analyzers (Li-7200, Li-840A, G2131-I, and L2130-I) here as well as view diagnostic data such as MFC flows, Diagnostics, and differential pressures
4. Eddy Co Plots
   - The Eddy Co Plots tabs plots eddy covariance troubleshooting plots first generated by Josh Roberti in N:/Common/EC_Plots.
   - Users can use this tab to identify issues with isotope, gas concentration, measurement level inlets, and hut conditions.
   
# Repository Navigation
- Data  
This folder houses all of the data files for this application. Data is loaded conditionally by server-side code. IE if a user selects the IS3R tab, the data associated with this tab is loaded. The time to load is allows rendered on this tab.  
Data is batched together manually with code outside of this repo as its function is not isolated to Swift but also produces data for the IS3R reports.
  - ECDataInputPressureDiff.rds  
  Precalculated ECSE line pressure differtial between inlet and hut MFM.
  - EC_Data.rds  
  Batched and stacked EC data for EC plots
  - EC_WaterData.rds  
  Seperated out water data to perform unit conversions, loaded for all water ppm plots
  - LastCalValDates.rds  
  Loads last calibration and validation dates for Master Tables under the Calibrations and Validations tab
  - calValLast8Days.rds  
  Houses the Li-840, G2130 calibrations and validations data from the past `n` days
  - cncData.rds  
  Holds all the LC Services data gathered from Dan Allen's SOH MySQL server
  - co2CVAL.rds  
  Holds data subsetted from the co2NaN180.rds data to be loaded for the Calibrations and Validations tab.
  - co2NaN180.RDS  
  All the sensor data from Mike Pursley's sensor status emails from the past 180 days removing all NaNs
  - cylAssayMASTER.rds  
  Holds all cylinder assayed concentrations for identifying the CO2 concentration of each CVAL cylinder.
  - field7200.rds  
  Holds the past `n` days of Li-7200 calibrations/validations
  - gasMASTER.rds  
  Holds all the Gas pressures data
  - L2130NEWBatch.rds   
  Holds all the L2130 validation data  
  
  
- r_code  
This is where all the non-shiny code is housed. For instance the DropboxDataGrabber.R code is run every morning to grab the data from the Dropbox directory.
  
- server  
This houses all the server code. Each tab is broken out into its own r script to make things a little more manageable than a single 2,000 line long script
  - swiftCVAL.R - All scripts and data loading for the Calibrations and Validations tab.
  - swiftECPlots - All scripts and data loading for the EC_Plots tab
  - swift Functions - House some repeated ggplot functions for EC_Plots tab
  - swiftIS3R_TIS.R - load all the TIS Data for the IS3R tab except for LC Sernvices
  - swift_CnC.R - loads all the LC Services data and plots for the IS3R Tab
  - swiftUserConnections.R - grab simple user metrics (start and end time) for each Swift session.
  
- www  
This folder houses all the images and logos


