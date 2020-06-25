library(usethis)
library(devtools)
library(desc)

# Remove default DESC
unlink("DESCRIPTION")
# Create and clean desc
my_desc <- description$new("!new")

# Set your package name
my_desc$set("Package", "cfbpointsR")

#Set your name
my_desc$set("Authors@R", "person('Saiem', 'Gilani', email = 'saiem.gilani@gmail.com', role = c('cre', 'aut')),
             person('Meyappan', 'Subbaiah', email = 'meysubb@gmail.com', role = c('ctb')),
             person('Parker', 'Fleming', email = 'parkerf@smu.edu', role = c('ctb')))")

# Remove some author fields
my_desc$del("Maintainer")

# Set the version
my_desc$set_version("0.0.0.9000")

# The title of your package
my_desc$set(Title = "A Scraping and Aggregating interface to the collegefootballdata.com API by CFB Points")
# The description of your package
my_desc$set(Description = "An R package for working with College Football Data. It is an R API wrapper around https://collegefootballdata.com/ made available by CFB Points. It provides users the capability to get a plethora of endpoints, and supplement that data with additional information (Expected Points Added/Win Probability added).")
# The urls
my_desc$set("URL", "http://www.github.com/saiemgilani/cfbpointsR")
my_desc$set("BugReports", "http://www.github.com/saiemgilani/cfbpointsR/issues")
# Save everyting
my_desc$write(file = "DESCRIPTION")

# If you want to use the MIT licence, code of conduct, and lifecycle badge
use_mit_license(name = "Saiem Gilani")
use_code_of_conduct()
use_lifecycle_badge("Experimental")
use_news_md()

# Get the dependencies
use_package("httr")
use_package("jsonlite")
use_package("curl")
use_package("attempt")
use_package("purrr")
use_package("dplyr")
use_package("ggplot2")
use_package("ggrepel")
use_package("tidyr")
use_package("stringr")
use_package("stringi")
use_package("assertthat")
use_package("mgcv")
use_package("nnet")
use_package("purrr")
use_package("stats")
use_package("utils")

# Clean your description
use_tidy_description()