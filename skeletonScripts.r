# Removing previous versions
remove(list=ls(all=T)) 
unlink(file.path(Sys.getenv("R_HOME"),"library/prevR"),rec=T)
# Required Library
library(sp)
library(gstat)
library(maptools)
# Creating all objects
files=dir("./Rcode",full=T,patt=".r") 
ind=grep("setClass",files)
# Creating the skeleton of the package
package.skeleton("prevR",code_file=c(files[ind],files[-ind]),namespace =TRUE, force =TRUE)
# Updating POT file
library(tools)
xgettext2pot('./prevR')