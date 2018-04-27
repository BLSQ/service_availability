# Measuring Health Services availability

## Project Organization
------------



    ├── README.md         <- What you are currently reading
    ├── .env              <- .env file, storing all passwords, urls and relative paths used
    ├── data              <- Not commited in Git. 
    │   ├── raw           <- Data downloaded through the dedicated scripts
    │   ├── processed     <- The data that has been processed and ready for modeling
    │   └── references    <- All dictionnaries, entities mapping and other info useful in analysis
    │    
    │
    ├── src                <- Source code for use in this project.
    │   │
    │   ├── data     <- Scripts to prepare data
    |   |   ├── extract_pdss_data.R      <- Getting the needed PDSS data
    |   |   ├── extract_SNIS_data.R     <- Getting the needed SNIS data
    │   │   └── match_pdss_snis.R       <- Mapping all needed SNIS and PDSS entities
    │   │
    │   ├── explore         <- All data exploration files
    |   |   ├── First_Exploration.R         <- First maps and graphs for last week of April 2018
    |   |   └── make_facilities.pdf         <- Compiled version
    │
    └── tox.ini            <- tox file with settings for running tox; see tox.testrun.org

## Data

Access to DHIS2 using [dhisextractr](https://github.com/grlurton/dhisextractr), based on metadata extracted with the template script provided with dhisextractr.

All passwords, metadata directory and other useful information should be stored in the .env file at the root of the project.

