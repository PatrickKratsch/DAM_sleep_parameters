# DAM_sleep_parameters
*Author: Patrick Krätschmer*
## Introduction

This repository contains work in progress on an R-based analysis package for the Drosophila Activity Monitor (DAM) system.

In order to run it, R needs to be installed on the local computer, and preferably RStudio as an IDE:

1. Download R
  * R can be downloaded from CRAN: https://cran.r-project.org/ Simply follow the instructions (and note that XQuartz is needed for Mac users).

2. Download RStudio
  * RStudio can be downloaded from: https://www.rstudio.com/

Once R and RStudio are installed on the local computer, there are three (so far) extra packages that need to be installed before using DAM_analysR:

* xlsx
* plyr
* dplyr

To install these packages, and any other, type ````install.packages("<packagename>")```` into the console in RStudio. Once this is done, start a new R project in RStudio, which sets the working directory to be the new RProject directory. Now the DAM_analysR GitHub repository can be downloaded and all its contents be copied into that RProject directory on the local computer; alternatively, the remote repo (DAM_sleep_parameters) can be forked and set as the working directory in RStudio.

Once the working directory in RStudio contains the contents of DAM_sleep_parameters, the analysis of DAM raw output data can begin. First, run the raw data from the DAM through DAM FileScan; The output files from this need to be copied into the RStudio working directory. Second, source DAM_analysR.R by ````source("DAM_analysR.R")````. Third, run DAM_analysR with the name of the raw DAM FileScan output file as the first argument, and the length of the experiment in minutes as the second argument (the default experiment duration is 1440 min, i.e. one day). Store the output of DAM_analysR in a variable. Lastly, use this variable as input to DAM_sleep_parameters, and write the output as an Excel file into the working directory.

## DAM_analysR.R

`DAM_analysR.R` is the base function used to work through the DAM FileScan output. Its first input is the DAM FileScan output, which is a text file (don't forget quotation marks and file extension). Optionally, if the experiment was run for a different duration than 1440 min, the length of the experiment in minutes can be passed into DAM_analysR as the second argument. DAM_analysR's output is a list of 11 elements. Each element can be accessed with the `$` operator (you can also convert the data.frames into data.tables if that eases analysis). The following explains the different elements computed by DAM_analysR:

   1. `light_regime` is a list of 1's and 0's, where 1 indicates light-on, and 0 light-off. Its length is specified by the `days_in_minutes` argument to `DAM_analysR`. Hence, each element of the `light_regime` vector indicates whether during a given minute of the experiment the light was switched on or off.

   2. `transitions` is calculated by `DAM_light_dark_transitions.R`, which is sourced from within DAM_analysR. This function takes two arguments, `light_regime` and `days_in_minutes`. It walks through light_regime until it encounters a transition from 0 to 1 or vice versa, and stores the index of that transition in two separate vectors, one for dark-light transitions, and one for light-dark transitions, respectively. Note that the minute of transition stored is always the 1, i.e. for light-dark transitions, the last minute in `light_regime` associated with a 1 is stored, while for dark-light transitions, the first minute associated with a 1 is stored. This is because even if the lights were on for just 1 s during a whole 1 min bin, in the DAM raw data, this minute will be associated with a 1. Hence, by storing the indices of the 1's, we know that for light-dark transitions, this was definitely the last minute during which the lights were on, while for dark-light transitions, this was definitely the first minute during which the lights were on.

   3. `DAM_raw` is the raw output from the DAM system, which is exactly the content of the text file you pass into `DAM_analysR` as the first argument.

   4. `DAM_raw_clean` extracts columns 12 to 44 (or columns 10 to 42, depending on date parsing) from `DAM_raw`, which corresponds to the light regime (column 12 of `DAM_raw`), and the beam crosses per minute for each channel (32 channels in total, resulting in 33 columns). This is done because the first 11 columns are not used for downstream analyses.

   5. `DAM_raw_clean2` binds an extra row of 1's to the beginning of `DAM_raw_clean`, which is important for later calculations of sleep episodes: These are calculated in part by finding a transition from activity to inactivity: If a fly was inactive for 5 min at the start of the experiment, this can only be interpreted as sleep if we add a row of pseudo-activity to the beginning of each channel. Note that this does not mean anything biologically, it's just to allow for proper calculation. In addition, `DAM_raw_clean2` has the first column removed, which is stored in the vector `light_regime` as stated above, and not carried forward for downstream calculations.

   6. `DAM_raw_clean3` has an additional column added to the start of `DAM_raw_clean2`, which contains a sequence of numbers from 0 to `days_in_minutes`, the second input argument to `DAM_analysR`. This is done to index every row in `DAM_raw_clean3`.

   7. `analogue_to_binary` takes `DAM_raw_clean3[, 2:33]` (because the first column, `days_in_minutes`, is not used for this calculation), and transforms any 0 in the data frame into a 1, and any non-0 into a 0. Hence, every minute of inactivity is now represented by a 1, while every minute during which the fly crossed the infrared beam at least once is represented by a 0. This makes `analogue_to_binary` an inactivity representation. Note that `days_in_minutes` is appended as the first column in `analogue_to_binary` after the above calculation is done, so `analogue_to_binary` has 33 columns.

   8. `five_min_bouts` is calculated via the function `DAM_sleep_define.R` (`DAM_sleep_define.R` must be in the working directory). This function is sourced from within `DAM_analysR`. `DAM_sleep_define` runs through every column of `analogue_to_binary` (again, except the first one, which is `days_in_minutes`), and looks for five consecutive 1's, i.e. periods of five minutes of inactivity. It does so via a sliding window of length 5, for every column of `analogue_to_binary` from the first row to the fourth-last row (the window would otherwise exceed the length of the column). Whenever it finds a window of five consecutive 1's, it saves a 1 at the position 1 of the sliding window in `five_min_bouts`. E.g., if a fly didn't cross the infrared beam between minutes 223 and 227, a 1 is put at position 223 in `five_min_bouts`. As a result, `five_min_bouts` will consist of 0's and 1's, with every 1 indicating that at this moment in time during the experiment (e.g. at minute 223), the fly didn't move for five consecutive minutes, i.e. it neither moved during this minute (e.g. minute 223), nor during the next four, indicating a period of five minutes of inactivity. If this is interpreted as a single sleep bout, then `five_min_bouts` is a data.frame where every 1 indicates the beginning of a five minute sleep bout (which is how this data.frame got its name). E.g., a nine minute sleep bout in `five_min_bouts` would be represented by five 1's in a row, a 300 min sleep bout by 296 1's in a row, an n min sleep bout by (n - 4) 1's in a row, with n >= 5.

   9. `sleep_start_list` is calculated by the function `DAM_sleep_start.R`, which is sourced from within `DAM_analysR`. `DAM_sleep_start` uses a sliding window of length 2 to slide down each column of `five_min_bouts`. Every time it encounters a transition from 0 to 1, it identifies this as the start of a sleep episode, and saves the index of the 1 in a vector. The output is a list of vectors, where each vector contains the indices of sleep starts for one channel (i.e. one fly) for the course of the whole experiment (defined by `days_in_minutes` in `DAM_analysR`). Note that there is more explanation on how `DAM_sleep_start` works as a commentary in `DAM_sleep_start.R`.

   10. `sleep_end_list` is calculated by the function `DAM_sleep_end.R`, which is also sourced from within `DAM_analysR`. Like `DAM_sleep_start`, `DAM_sleep_end` also uses a sliding window of length 2 to slide through each column of `five_min_bouts`. It is important to note how `DAM_sleep_end` calculates the index of sleep end. When the window encounters a transition from 1 to 0, it saves the index of the first 1 plus 4 (due to the nature of what a 1 in `five_min_bouts` represents). E.g., if a fly exhibits a sleep bout of 7 minutes at minute 726, `five_min_bouts` will have 1's at positions 726, 727, and 728. Hence, a transition from 1 to 0 will be noticed by `DAM_sleep_end` when the window is placed over positions 728 and 729. Because the 1 at position 728 in `five_min_bouts` indicates a sleep bout of 5 minutes, the index that is saved by `DAM_sleep_end` is 728 + 4 = 732. Note, again, that there is more information on `DAM_sleep_end` in `DAM_sleep_end.R`.

   11. `sleep_bout_length` is calculated by subtracting every element in `sleep_start_list` from every corresponding element in `sleep_end_list`, and adding 1 to the result. Hence, every list element in `sleep_bout_length` is a list itself and corresponds to one channel. This sub-list contains the length of every sleep bout exhibited by a specific fly throughout the course of the whole experiment. To get the corresponding start and end times of each sleep bout, `sleep_start_list` and `sleep_end_list` must be indexed by the same index as the bout is associated with in `sleep_bout_length`.


## DAM_sleep_parameters.R

Once the output of `DAM_analysR` has been stored in a variable, this variable can be passed into `sleep_parameters`. The second input argument to `sleep_parameters` is the name of the Excel file that will be written by this function; this name needs to be in double quotes and end with the .xlsx file extension. This Excel file will be written into the working directory; its content is a table where each column indicates a separate channel and each row a different sleep parameter. Note that if only a subset of the 32 channels in a monitor is to be analysed, this can be specified in the third input argument to `sleep_parameters`; the default is channels 1 to 32. Moreover, the length of the experiment is set to 1440 min, but this can also be changed in the fourth input argument. Lastly, `sleep_parameters` can be run for data in light-dark, dark-light, light-light, and dark-dark; in case of light-dark and dark-light, the function will automatically extract the time of transition and use it for downstream calculations. If different photoperiods are to be run (such as 16L:8D), the user has to input the time of transition (theoretical transition) manually as the fifth input argument ('mytransition') to `sleep_parameters`: by default, the value is 721, which represents a theoretical 12:12 pattern. Altogether, there are 30 separate sleep parameters calculated by `DAM_sleep_parameters`, which are listed below. An example call of the function could be `DAM_sleep_parameters(<DAM_analysR_output>, '<filename.xlsx', channels = 5:18, days_in_minutes = 1440, mytransition = 481)`. The 30 sleep parameters are explained below:

  * `total_overall_sleep` shows the total sleep in minutes for each fly.

  * `total_bouts_overall` is the number of (*not* split into day and night phases) sleep bouts throughout the whole experiment.

  * `onset_first_max_overall_bout` is the time at which the first overall (no differentiation between light-on and light-off) maximum bout started.

  * `offset_first_max_overall_bout` is the time at which this first overall maximum bout ended.

  * `length_first_max_overall_bout` is the length of this/these overall maximum bout/s.

  * `number_of_max_overall_bouts` is the number of equally long maximum bouts overall.

  * `total_day_sleep` is the sum of minutes slept during the day.

  * `day_bout_number` is the number of day sleep bouts.

  * `onset_first_day` is the time point at which the first sleep bout during the day occurred.

  * `offset_first_day` is the time point at which the first sleep bout during the day ended.

  * `length_first_day_bout` is the length of the first day sleep bout.

  * `onset_first_max_day` is the time point at which the longest sleep bout during the day occurred.

  * `offset_first_max_day` is the time point at which the longest day sleep bout ended.

  * `length_first_max_day_bout` is the length of this/these maximum day sleep bout/s.

  * `number_of_max_day_bouts` shows how many maximum sleep bouts during the day a fly exhibited; this is relevant if a fly sleeps n > 1 times for a length that represents the longest sleep bout during the day.

  * `last_day_offset` shows the end of the last sleep bout during the day.

  * `transition_bout` is the number of transition bouts exhibited by a fly. By definition, this can only be 1 or 0.

  * `transition_bout_start` is the start time of the transition bout, if present.

  * `transition_bout_end` is the time at which the transition bout, if present, ends.

  * `transition_bout_length` is the length if the transition bout.

  * `total_night_sleep` is the sum in minutes of night sleep.

  * `night_bout_number` is the number of sleep bouts during the night.

  * `onset_first_night` is the time point at which the first night sleep bout occurred.

  * `offset_first_night` is the time point at which the first night sleep bout ended.

  * `length_first_night_bout` is the length of the first night sleep bout.

  * `onset_first_max_night` is the time at which the first maximum night bout occurred.

  * `offset_first_max_night` is the time at which the first maximum night bout ended.

  * `length_first_max_night_bout` represents the lengths of this/these maximum night bout/s.

  * `number_of_max_night_bouts` shows how many equally long maximum night sleep bouts occurred.

  * `last_night_offset` is the end of the last sleep bout during the night.


# Transition Bout Details

A transition bout is calculated by comparing the number of sleep bout starts to the number of sleep bout ends during the day and night: If the number of sleep bout starts during the day is larger than the number of sleep bout ends during the day, AND if the number of sleep bout ends during the night is larger than the number of sleep bout starts during the night, a transition bout is identified.

If the onset of a transition bout is less than 5 min before light-off, it is not counted as a separate day bout. Similarly, if a transition bout ends less than 5 min after light-off, it is not counted as a separate night bout. If a transition bout starts at least 5 min before light-off AND ends at least 5 min after light-off, the sum of `day_bout_number` and `night_bout_number` equals `total_bouts_overall + 1`. 
