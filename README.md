# dkb_analysis
The [DKB](https://www.dkb.de/) lets you download your transaction as .csv. 

This script uses that document to report a plethora of charts to you. You can choose whether you'd like to do as little as possible or use the provided mapping table to customize your report with your own categories for each account.

# installation

git clone 
cd dkb_analyze/
Rscript dkb_analyze.R

# dependencies

This script has been tested with R 4.0.4.

Note that the following packages get loaded and installed if they are not found
on your system.

Packages used:
* dplyr
* lubridate
* readr
* ggplot2
* svDialogs
* RColorBrewer
* gridExtra
* scales
* tidyr
* ggrepel

# to do
* add verbosity
* reduce code redundancy for plots
* rename variables in function scope
