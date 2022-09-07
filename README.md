# README #

This repository stores the code to replicate the figures in:

# Tax Avoidance and Evasion in a Dynamic Setting #

by

Duccio Gamannossi degl'Innocenti, Rosella Levaggi and Francesco Menoncin 

Reference:

[Gamannossi degl'Innocenti, D., Levaggi, R. and Menoncin, F., 2021. Tax Avoidance and Evasion in a Dynamic Setting, forthcoming at the Journal of Economic Behavior & Organization](http://www.dgdi.me/publication/avoid_evade_dynamic/)

### Description of files in the Repo

* license.txt				    : 	license to be applied to the files in the repo	
* package_setup.R       :   script to install needed packages
* README.md             :   this file

+ **doc**									: 	folder containing the article

	* Tax Avoidance and Evasion in a Dynamic Setting.pdf 	:	article 
	
+ **fig**									: 	folder containing figures produced with the scripts

	* Figure_1.pdf			:   Figure 1
	* Figure_2.pdf			:   Figure 2

+ **R**									  : 	folder containing R scripts
	
  * Figure_1.R 			    :   script to compute and plot Figure 1
  * Figure_2.R				  : 	script to compute and plot Figure 2
	
+ **tikz**						: 	folder containing the tikz of the figures

	* Figure 1.tex 	:	  standalone tikz of Figure 1
	* Figure 2.tex 	:	  standalone tikz of Figure 2
	* tmp_Figure_1	:	  tikz dictionary of Figure 1
	* tmp_Figure_2	:	  tikz dictionary of Figure 2

### Instructions

In order to set up the working environment:

1. Start a new R-project in an empty folder
2. Copy the content of the repo to the project folder
3. Run the script package_setup.R to install the needed packages
4. Run any of the scripts

The scripts have been tested on Win 10, R-4.1.1, RStudio-1.4.1717

The files are distributed under BSD license. For more information, please check the license.txt file.

For any question, suggestion or comment, write to: mail@dgdi.me