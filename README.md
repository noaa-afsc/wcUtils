# wcUtils

The aim of `wcUtils` is to make your life as an analyst of Wildlife Computers data a wee bit easier. You do need a rudimentary understanding of R and the package does require the R package 'reshape' (available on CRAN). At this point, the purpose of `wcUtils` is to make your life easier, not to do the analysis for you. As such, the tools focus more on manipulation of data and less on analysis. 

Package source build: [wcUctils_0.1.tar.gz](https://github.com/downloads/NMML/wcUtils/wcUtils_0.1.tar.gz)  
Windows binary:
[wcUtils_0.1.zip](https://github.com/downloads/NMML/wcUtils/wcUtils_0.1.zip)

## Manipulating Histos Data

### MeltHistos:

One of the main reasons for deploying Wildlife Computers tags is to gather information related to dive and haul-out behavior. In order to work within the bandwidth limitations of Argos, Wildlife Computers tags typically transmit dive behavior back to the researcher in the form of histograms. Various dive data are collected in this manner and include *Dive-Depth*,*Time-At-Depth*, *Time-At-Temperature* and *Percent-Dry*. In the case of the *Percent-Dry* data, each record represents a single UTC day and each 'Bin' column represents an hour of that day ('Bin1' = 00:00, 'Bin2' = 01:00, ... 'Bin24 = 23:00). For data related to dive behavior, each 'Bin' column represents a range of depths or time durations specified by the user at the time of tag programming. Each record represents a specific duration of time (e.g. 6 hour period) also specified by the user. All of these data are represented in the *XX-Histos.csv* output from WC-DAP and the data structure is organized horizontally.

Often, it is desirable for the data to be represented in a more vertical nature where each record specifies a single hour of a day (for *Percent-Dry* data) or a specific Bin range for dive data. This vertical structure is more easily imported into relational databases or other analysis functions. Re-shaping (in this case 'melting') the data into this vertical structure is the purpose of this function.

Initially, the `MeltHistos` function has been written to process only those histogram data related to haul-out behavior. These records are identified within the *XX-Histos.csv* as having a HistType of 'Percent' (or, in the rarer case 'TwentyMinTimeline' ... not implemented yet). This function requires the user to provide the path to the *XX-Histos.csv* file and it returns a dataframe with three columns: **DeployID**, **DataDateTime** and **PercentDry**. All time values are in the UTC time zone.

## Import/Export to Other Packages

### PrepareToCrawl:

This function takes the output from `MeltHistos(d,hist_type="Percent")` and properly formats the data (converts the **PercentDry** column into values between 0 and 1 and renames **DataDateTime** as **Time**) for import into the `crawl` package's `mergeToStop` function. 