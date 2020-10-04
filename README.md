# Broadband maps shiny app

## To run this app locally

This repository includes all of the code you'll need but the data is too large to be stored here. The app uses a SQLite database to store data. You can download the database from s3 with this URL: https://broadband-maps.s3-us-west-2.amazonaws.com/ofg_local.db. 

To download the database from R, use `download.file("https://broadband-maps.s3-us-west-2.amazonaws.com/ofg_local.db", destfile = "app/ofg_local.db")` which will download the file into the `app/` directory.

The app can either be run from RStudio directly or through Docker with the included Dockerfile.
