
*[![Build Status](https://travis-ci.com/espm-157/extinction-extinction-quam-hua.svg?token=7gSxV1VqqHz7TUXHHWGp&branch=master)](https://travis-ci.com/espm-157/extinction-extinction-quam-hua)*

## Team Members:

- Alice Hua, [@alicehua11](https://github.com/alicehua11)
- full name, github handle

This assignment examines whether we are undergoing a sixth mass extinction based on data from the [International Union for Conservation of Nature (IUCN) redlist](https://www.iucnredlist.org/). 
Throughout this assignment, we try to replicate a study by Gerardo Ceballos and colleagues in assessing the current rates of species extictions, particularly, the mammals, birds, vertebrates and other vertebrates and comparing them to the background extinction rates, which are assumed to be between 0.1 - 1 species extinction per million species per year E/MSY).
We will use REST API to pull data about species and regular expression to extract their extinction dates. Here, we transform the data into regular data to make our final graphs and draw a conclusion.

## assignment

All work for this assignment should be in the `assignment` directory.  You will work in the `.Rmd` notebook, and commit your rendered output files (`.md` and associated files) in the `assignment` directory as well.

## Special files

All team repositories will also include most of the special files found here:

### Common files
- Since we are using the REST API to pull data directly from the server, we do not have any data stored in our assignment folder. However you will find a cache folder that we stored our GET requests in to save time from running the GET requests everytime we revisit the markdown.

- `README.md` this file, a general overview of the repository in markdown format.  
- `.gitignore` Optional file, ignore common file types we don't want to accidentally commit to GitHub. Most projects should use this. 
- `<REPO-NAME>.Rproj` Optional, an R-Project file created by RStudio for it's own configuration.  Some people prefer to `.gitignore` this file.


### Infrastructure for Testing

- `.travis.yml`: A configuration file for automatically running [continuous integration](https://travis-ci.com) checks to verify reproducibility of all `.Rmd` notebooks in the repo.  If all `.Rmd` notebooks can render successfully, the "Build Status" badge above will be green (`build success`), otherwise it will be red (`build failure`).  
- `DESCRIPTION` a metadata file for the repository, based on the R package standard. It's main purpose here is as a place to list any additional R packages/libraries needed for any of the `.Rmd` files to run.




