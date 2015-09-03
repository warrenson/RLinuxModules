# RLinuxModules
R package that makes linux [environment modules](http://modules.sourceforge.net/) available from R.

## installation
devtools::install_github("larsgr/RLinuxModules")

## use example:
library(RLinuxModules)

moduleInit( modulesHome = "yourpathToModulesEnvironment")

module("load samtools") # loads samtools into the environment

system("samtools") # samtools should now be available (if you have that module)

## How it works
The Modules Environment does not support R scripting but does support Python. This package works by using the python support and translating the python commands returned from *modulecmd python* into R commands. It has only been tested for version 3.2.10 