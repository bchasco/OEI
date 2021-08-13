#Notes to myself

#Building vignettes. Use this code after you create the vignettes folder.
tools::buildVignettes(dir = ".", tangle=TRUE)
shell("mkdir inst\\doc")
file.copy(dir("vignettes", full.names=TRUE), "inst/doc", overwrite=TRUE)

#Remember to remove 
/inst/doc

#from the .gitignore file