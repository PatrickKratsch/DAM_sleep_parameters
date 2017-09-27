sleep_parameters <- function(analysR_output, xlsx_file, channels = 1:32, days_in_minutes = 1440, mytransition = 721){
    
    #################################################################
    ######################### SETUP #################################
    
    # NB: Need JAVA to execute this script because the xlsx package 
    # is JAVA-based
    library("xlsx")

    # Source light_dark_transitions.R to calculate ld/dl-transition 
    # from the input
    # Source test_empty_vec.R for analysis
    source("light_dark_transitions.R")
    source("test_empty_vec.R")
    
    # Extract the light regime from input data
    light_regime <- analysR_output$light_regime

    # Create empty data frame that will be the output, storing 
    # sleep parameters (28) as rows and channels (=flies) 
    # as columns
    output <- data.frame(row.names <- c("latency_first_day", "offset_first_day", 
                                        "length_first_day_bout", "latency_first_night", 
                                        "offset_first_night", "length_first_night_bout",
                                        "latency_first_max_day", "offset_first_max_day", 
                                        "number_of_max_day_bouts", "length_first_max_day_bout",
                                        "latency_first_max_night", "offset_first_max_night", 
                                        "number_of_max_night_bouts", "length_first_max_night_bout",
                                        "length_first_max_overall_bout", "number_of_max_overall_bouts", 
                                        "latency_first_max_overall", "offset_first_max_overall",
                                        "total_day_bouts", "total_night_bouts", 
                                        "total_bouts_overall", "transition_bout", 
                                        "transition_bout_length", "last_day_offset", 
                                        "last_night_offset", "total_day_sleep", 
                                        "total_night_sleep", "total_overall_sleep"))

    #################################################################
    ######################### MAIN ##################################
    
    # Loop through each channel, calculate sleep parameters, and 
    # add to output.
    i <- 1
    for(channel in channels){

        # Get transition time. If the data are in constant light 
        # or constant dark, transition will be equal to mytransition 
        # (default = 721). If the data are in light-dark, 
        # transition will be the time of light-dark transition. 
        # If the data are in dark-light, transition will be the 
        # time of dark-light transition.
        transitions <- light_dark_transitions(light_regime, days_in_minutes)
        if(test_empty_vec(transitions$ld_transitions) == TRUE){

            if(test_empty_vec(transitions$dl_transitions) == TRUE){

                transition <- mytransition
            }
            else{

                transition <- transitions$dl_transitions
            }
        }
        else{

            transition <- transitions$ld_transitions
        }
        
        # Check for multiple transitions during one day, which is likely an error.
        if(length(transition) != 1){
            
            print(sprintf("Channel: %s", channel))
            writeLines("Two transitions found. This is likely an error, e.g. light flikering in incubator!
I will use 721. If this is incorrect, please check analysR_output$transitions for details.\n")
            transition <- 721
        }

        # Split all data into day and night for downstream 
        # calculations. For now, this only works for 
        # light-dark, not for dark-light transitions.
        sleep_start_list <- analysR_output$sleep_start_list[[channel]]
        sleep_end_list <- analysR_output$sleep_end_list[[channel]]
        sleep_start_list_day <- analysR_output$sleep_start_list[[channel]][which(analysR_output$sleep_start_list[[channel]] <= transition)]
        sleep_start_list_night <- analysR_output$sleep_start_list[[channel]][which(analysR_output$sleep_start_list[[channel]] > transition)]
        sleep_end_list_day <- analysR_output$sleep_end_list[[channel]][which(analysR_output$sleep_end_list[[channel]] <= transition)]
        sleep_end_list_night <- analysR_output$sleep_end_list[[channel]][which(analysR_output$sleep_end_list[[channel]] > transition)]

        # Split the bout lengths first so that all bouts that 
        # started AND ENDED during the day or night,
        # respectively, are in separate lists. Note that there 
        # may be bouts that span the day/night transition.
        # These are dealt with by a separate parameter, which 
        # calculates the overall maximum bout length, and the 
        # existence of a 'transition bout'.
        sleep_bout_length <- analysR_output$sleep_bout_length[[channel]]
        sleep_bout_length_day <- analysR_output$sleep_bout_length[[channel]][which(analysR_output$sleep_end_list[[channel]] <= transition)]
        sleep_bout_length_night <- analysR_output$sleep_bout_length[[channel]][which(analysR_output$sleep_start_list[[channel]] > transition & analysR_output$sleep_end_list[[channel]] > transition)]
        
        # Test whether channel/fly didn't sleep at all; assign 
        # NaN to all parameters and continue with next loop interation
        if(test_empty_vec(sleep_bout_length) == TRUE){
            
            latency_first_day <- NaN
            offset_first_day <- NaN
            length_first_day_bout <- NaN
            
            latency_first_night <- NaN
            offset_first_night <- NaN
            length_first_night_bout <- NaN
            
            latency_first_max_day <- NaN
            offset_first_max_day <- NaN
            number_of_max_day_bouts <- NaN
            length_first_max_day_bout <- NaN
            
            latency_first_max_night <- NaN
            offset_first_max_night <- NaN
            number_of_max_night_bouts <- NaN
            length_first_max_night_bout <- NaN
            
            length_first_max_overall_bout <- NaN
            number_of_max_overall_bouts <- NaN
            latency_first_max_overall <- NaN
            offset_first_max_overall <- NaN
            
            total_day_bouts <- NaN
            total_night_bouts <- NaN
            total_bouts_overall <- NaN
            transition_bout <- NaN
            transition_bout_length <- NaN
            
            last_day_offset <- NaN
            last_night_offset <- NaN
            total_day_sleep <- NaN
            total_night_sleep <- NaN
            total_overall_sleep <- NaN
            
            # Create vector from above parameters and add to output
            fly_paras <- c(latency_first_day, offset_first_day, length_first_day_bout,
                           latency_first_night, offset_first_night, 
                           length_first_night_bout, latency_first_max_day, 
                           offset_first_max_day, number_of_max_day_bouts, 
                           length_first_max_day_bout, latency_first_max_night, 
                           offset_first_max_night, number_of_max_night_bouts, 
                           length_first_max_night_bout, length_first_max_overall_bout, 
                           number_of_max_overall_bouts, latency_first_max_overall, 
                           offset_first_max_overall, total_day_bouts, 
                           total_night_bouts, total_bouts_overall, 
                           transition_bout, transition_bout_length, 
                           last_day_offset, last_night_offset,
                           total_day_sleep, total_night_sleep, 
                           total_overall_sleep)
            
            output[[i]] <- fly_paras
     
            i <- i + 1
            
            # Begin next loop iteration
            next
        }
        
        
        # Latency to first day bout. We first test whether 
        # sleep_bout_length_day is empty. If it is not,
        # latency_first_day is the time of the beginning 
        # of the first bout. If it is empty, but there
        # is a transition bout that happened at least 
        # 5 min in the day, latency_first_day is equal to
        # the start time of that transition bout. 
        # Otherwise, if the transition bout happened less than
        # 5 min during the day, or if the fly never 
        # slept during the day, latency_first_day will be
        # NaN. offset_first_day is calculated alongside the latency.
        # It stores NaN if sleep_bout_length_day is 
        # empty and there is no transition bout that
        # lasted at least 5 min during the day.
        if(test_empty_vec(sleep_bout_length_day) == FALSE){

            latency_first_day <- sleep_start_list_day[1]
            offset_first_day <- sleep_end_list_day[1]
        }
        else if(length(sleep_start_list_day) == 1 && (transition - sleep_start_list_day[1] + 1) >= 5){
            
            latency_first_day <- sleep_start_list_day[1]
            offset_first_day <- transition
        }
        else{

            latency_first_day <- NaN
            offset_first_day <- NaN
        }
        # Length of first day bout
        length_first_day_bout <- offset_first_day - latency_first_day + 1


        # Latency to first night bout. We first test 
        # whether the fly shows a 'transition bout',
        # i.e. a bout that started during light on and ended 
        # during light off. If so, we test whether
        # this transition bout lasted at least 5 min into the 
        # light off period. If so, latency_first_night is equal to
        # the time of light-dark transition, because the fly was 
        # asleep during the transition. If 
        # that is not the case, we test whether
        # sleep_start_list_night is empty. If so, the fly never 
        # started sleeping during the entire light off
        # period. If that is the case, latency_first_night will be NaN. 
        # If sleep_start_list_night is not empty, 
        # latency_first_night will simply be the time point
        # during which the first night sleep bout started. 
        # offset_first_night is calculated alongside the latency.
        if(test_empty_vec(sleep_bout_length_night) == FALSE){

            latency_first_night <- sleep_start_list_night[1]
            offset_first_night <- sleep_end_list_night[1]

            if(length(sleep_end_list_night) > length(sleep_start_list_night) && (sleep_end_list_night[1] - transition + 1) >= 5){

                latency_first_night <- transition
            }
            else if(length(sleep_end_list_night) > length(sleep_start_list_night) && (sleep_end_list_night[1] - transition + 1) < 5){

                offset_first_night <- sleep_end_list_night[2]
            }
        }
        else if(length(sleep_end_list_night) == 1 && (sleep_end_list_night[1] - transition + 1) >= 5){

            latency_first_night <- transition
            offset_first_night <- sleep_end_list_night[1]
        }
        else{

            latency_first_night <- NaN
            offset_first_night <- NaN
        }
        # Length of first night bout.
        length_first_night_bout <- offset_first_night - latency_first_night + 1


        # Latency to first maximum day bout. 
        # First we test whether sleep_bout_length_day is empty.
        # If it isn't, latency_max_day will be the time to 
        # the maximum day bout. We further need to test whether
        # there is a transition bout that has a longer day 
        # period than the just calculated maximum bout.
        # If so, the time this transition bout started 
        # will be latency_max_day. If sleep_bout_length_day
        # is empty, but there is a transition bout that 
        # lasts at least 5 min during the day,
        # latency_first_max_day will be the time this 
        # transition bout started. If otherwise, there is
        # either no sleep start at all during the day, 
        # or only one sleep start less than 5 min
        # before transition, latency_first_max_day will be NaN, 
        # because there is no day bout at all.
        # We also initiate number_of_max_day_bouts to 
        # keep track of how many maximum sleep bouts
        # there are during the day. Simultaneously, 
        # we also calculate the offset of the first maximum
        # day bout in offset_first_max_day.
        number_of_max_day_bouts <- 0
        if(test_empty_vec(sleep_bout_length_day) == FALSE){

            latency_first_max_day <- sleep_start_list_day[which(sleep_bout_length_day == max(sleep_bout_length_day))[1]]
            offset_first_max_day <- sleep_end_list_day[which(sleep_bout_length_day == max(sleep_bout_length_day))[1]]
            number_of_max_day_bouts <- length(which(sleep_bout_length_day == max(sleep_bout_length_day)))

            if(length(sleep_start_list_day) > length(sleep_end_list_day) && ((transition - tail(sleep_start_list_day, 1) + 1) > max(sleep_bout_length_day))){

                latency_first_max_day <- tail(sleep_start_list_day, 1)
                offset_first_max_day <- transition
                number_of_max_day_bouts <- 1
            }
            else if(length(sleep_start_list_day) > length(sleep_end_list_day) && ((transition - tail(sleep_start_list_day, 1) + 1) == max(sleep_bout_length_day))){

                number_of_max_day_bouts <- number_of_max_day_bouts + 1
            }
        }
        else if(length(sleep_start_list_day) > length(sleep_end_list_day) && ((transition - sleep_start_list[1] + 1) >= 5)){

            latency_first_max_day <- sleep_start_list[1]
            offset_first_max_day <- transition
            number_of_max_day_bouts <- 1
        }
        else{

            latency_first_max_day <- NaN
            offset_first_max_day <- NaN
        }
        # Length of first maximum day bout.
        length_first_max_day_bout <- offset_first_max_day - latency_first_max_day + 1

        # Latency to first maximum night bout. 
        # First we test whether sleep_bout_length_night is empty.
        # If it isn't, latency_first_max_night is the time the 
        # first max night bout begins.
        # If there is a transition bout that lasted longer 
        # during the night than latency_first_max_night,
        # latency_first_max_night is equal to `transition`. 
        # If there was a transition bout that lasted exactly
        # the same length as latency_first_max_day, 
        # latency_first_max_day will also be `transition`. 
        # If sleep_bout_length_night
        # is empty, we need to test whether there was a 
        # transition bout that lasted at least 5 min during the night.
        # If so, latency_first_max_night will be `transition`. 
        # If sleep_bout_length_night is empty, and there was no such
        # transition bout, latency_first_max_night will be NaN. 
        # All the while, we do some extra calculations
        # to keep track of the number of maximum night bouts, 
        # stored in number_of_max_night_bouts.
        number_of_max_night_bouts <- 0
        if(test_empty_vec(sleep_bout_length_night) == FALSE){

            latency_first_max_night <- sleep_start_list_night[which(sleep_bout_length_night == max(sleep_bout_length_night))[1]]
            offset_first_max_night <- sleep_end_list_night[which(sleep_bout_length_night == max(sleep_bout_length_night))[1]]
            number_of_max_night_bouts <- length(which(sleep_bout_length_night == max(sleep_bout_length_night)))
            
            if(length(sleep_end_list_night) > length(sleep_start_list_night)){
                
                offset_first_max_night <- sleep_end_list_night[which(sleep_bout_length_night == max(sleep_bout_length_night))[1] + 1]
                
                if((sleep_end_list_night[1] - transition + 1) > max(sleep_bout_length_night)){
                    
                    latency_first_max_night <- transition
                    offset_first_max_night <- sleep_end_list_night[1]
                    number_of_max_night_bouts <- 1
                }
                else if((sleep_end_list_night[1] - transition + 1) == max(sleep_bout_length_night)){
                    
                    latency_first_max_night <- transition
                    offset_first_max_night <- sleep_end_list_night[1]
                    number_of_max_night_bouts <- number_of_max_night_bouts + 1
                }
            }
        }
        else if(length(sleep_end_list_night) > length(sleep_start_list_night) && (sleep_end_list_night[1] - transition + 1) >= 5){

            latency_first_max_night <- transition
            offset_first_max_night <- sleep_end_list_night[1]
            number_of_max_night_bouts <- 1
        }
        else{

            latency_first_max_night <- NaN
            offset_first_max_night <- NaN
        }
        # Length of first maximum night bout.
        length_first_max_night_bout <- offset_first_max_night - latency_first_max_night + 1


        # Since sleep_bout_length_day and sleep_bout_length_night 
        # do not consider bouts that span across
        # the light/dark transition in their entirety, 
        # we also calculate the overall maximum sleep bout length,
        # and then check whether it is different from the day 
        # and night maximum, respectively. Then
        # we calculate the respective latency and offset to 
        # the maximum overall bout, which can be
        # the same as for either the maximum day bout 
        # or the maximum night bout.
        # If a fly never slept at all, length_first_max_overall_bout
        # and number_of_max_overall_bouts will be NaN.
        length_first_max_overall_bout <- max(sleep_bout_length)
        number_of_max_overall_bouts <- length(max(sleep_bout_length))
        # Test whether the fly didn't sleep at all either during the day or during the night
        if(test_empty_vec(sleep_start_list) == FALSE && test_empty_vec(sleep_end_list) == FALSE){
            
            if(is.na(length_first_max_day_bout) && is.na(length_first_max_night_bout)){
                
                latency_first_max_overall <- sleep_start_list[which(sleep_bout_length == length_first_max_overall_bout)[1]]
                offset_first_max_overall <- sleep_end_list[which(sleep_bout_length == length_first_max_overall_bout)[1]]
            }
            else if(is.na(length_first_max_day_bout) && !is.na(length_first_max_night_bout)){
                
                if(length_first_max_overall_bout != length_first_max_night_bout){
                    
                    latency_first_max_overall <- sleep_start_list[which(sleep_bout_length == length_first_max_overall_bout)[1]]
                    offset_first_max_overall <- sleep_end_list[which(sleep_bout_length == length_first_max_overall_bout)[1]]
                }
                else{
                    
                    latency_first_max_overall <- latency_first_max_night
                    offset_first_max_overall <- offset_first_max_night
                }
            }
            else if(!is.na(length_first_max_day_bout) && is.na(length_first_max_night_bout)){
                
                if(length_first_max_overall_bout != length_first_max_day_bout){
                    
                    latency_first_max_overall <- sleep_start_list[which(sleep_bout_length == length_first_max_overall_bout)[1]]
                    offset_first_max_overall <- sleep_end_list[which(sleep_bout_length == length_first_max_overall_bout)[1]]
                }
                else{
                    
                    latency_first_max_overall <- latency_first_max_day
                    offset_first_max_overall <- offset_first_max_day
                }
            }
            else{
                
                if(length_first_max_overall_bout != length_first_max_day_bout && length_first_max_overall_bout != length_first_max_night_bout){
                    
                    latency_first_max_overall <- sleep_start_list[which(sleep_bout_length == length_first_max_overall_bout)[1]]
                    offset_first_max_overall <- sleep_end_list[which(sleep_bout_length == length_first_max_overall_bout)[1]]
                }
                else if(length_first_max_overall_bout == length_first_max_night_bout){
                    
                    latency_first_max_overall <- latency_first_max_night
                    offset_first_max_overall <- offset_first_max_night
                }
                else if(length_first_max_overall_bout == length_first_max_day_bout){
                    
                    latency_first_max_overall <- latency_first_max_day
                    offset_first_max_overall <- offset_first_max_day
                }
            }
        }
        else{
            
            latency_first_max_overall <- NaN
            offset_first_max_overall <- NaN
        }
        
        
        # Number of total bouts during day.
        total_day_bouts <- length(sleep_bout_length_day)

        # Number of total bouts during night.
        total_night_bouts <- length(sleep_bout_length_night)


        # Number of bouts that spanned over light/dark transition.
        total_bouts_overall <- length(sleep_bout_length)
        if(total_bouts_overall != (total_day_bouts + total_night_bouts)){

            transition_bout <- TRUE
        }
        else{

            transition_bout <- FALSE
        }
        
        # Get length of transition bout
        if(transition_bout){
            
            transition_bout_length <- sleep_bout_length[length(sleep_start_list_day)]
        }
        else{
            
            transition_bout_length <- NaN
        }
    
        # Test whether there are parts of the transition bout
        # that lasted either 5 min during the day
        # or during the night. If so, we need to add
        # 1 to total_day bouts or total_night_bouts.
        if(transition_bout){
            
            if(transition - tail(sleep_start_list_day, 1) >= 5){
                
                total_day_bouts <- total_day_bouts + 1
            }
            if(sleep_end_list_night[1] - transition >= 5){
                
                total_night_bouts <- total_night_bouts + 1
            }
        }
        

        # Last offset during day. 
        # First test whether sleep_bout_length_day is empty. If not,
        # last_day_offset will be the last element of 
        # sleep_end_list_day. We also account
        # for a possible transition bout that lasted at least 5 min 
        # during the day. If one
        # exists, last_day_offset will be transition. 
        # If sleep_end_list_day is empty,
        # but there is such a transition bout, 
        # last_day_offset will also be transition.
        # If sleep_end_list_day is empty, and there is 
        # either none or too-short a
        # transition bout, last_day_offset will be NaN.
        if(test_empty_vec(sleep_bout_length_day) == FALSE){

            last_day_offset <- tail(sleep_end_list_day, 1)

            if(transition_bout && (transition - tail(sleep_start_list_day, 1) + 1) >= 5){

                last_day_offset <- transition
            }
        }
        else if(transition_bout && (transition - tail(sleep_start_list_day, 1) + 1) >= 5){

            last_day_offset <- transition
        }
        else{

            last_day_offset <- NaN
        }

        # Last night offset. First we test whether 
        # sleep_bout_length_night is empty.
        # If it isn't, last_night_offset will be the 
        # last entry of sleep_end_list_night.
        # Note that flies that slept through to the 
        # end of the experiment will
        # be analysed to have woken up at time days_in_minutes, 
        # i.e. at the time point
        # which represent the end of the experiment, 
        # usually 1440 (i.e. if the experiment
        # lasted 1 day). Hence, we don't need to account for 
        # sleep periods that started before
        # days_in_minutes but didn't end at all; 
        # they will be accounted for in
        # sleep_end_list_night with the value 1440. If sleep_end_list is empty,
        # last_night_offset will be days_in_minutes.
        if(test_empty_vec(sleep_bout_length_night) == FALSE){

            last_night_offset <- tail(sleep_end_list_night, 1)
        }
        else{

            last_night_offset <- days_in_minutes
        }
        
        # Calculate total night sleep, total day sleep, 
        # and total overall sleep
        total_day_sleep <- sum(sleep_bout_length_day)
        if(transition_bout && (transition - tail(sleep_start_list_day, 1) + 1) >= 5){
            
            total_day_sleep <- total_day_sleep + (transition - tail(sleep_start_list_day, 1) + 1)
        }
        
        total_night_sleep <- sum(sleep_bout_length_night)
        if(transition_bout && (sleep_end_list_night[1] - transition + 1) >= 5){
            
            total_night_sleep <- total_night_sleep + (sleep_end_list_night[1] - transition + 1)
        }
        
        total_overall_sleep <- sum(sleep_bout_length)

        # Create vector from above parameters and add to output
        fly_paras <- c(latency_first_day, offset_first_day, 
                       length_first_day_bout,latency_first_night, 
                       offset_first_night, length_first_night_bout,
                       latency_first_max_day, offset_first_max_day, 
                       number_of_max_day_bouts, length_first_max_day_bout,
                       latency_first_max_night, offset_first_max_night, 
                       number_of_max_night_bouts, length_first_max_night_bout,
                       length_first_max_overall_bout, number_of_max_overall_bouts, 
                       latency_first_max_overall, offset_first_max_overall,
                       total_day_bouts, total_night_bouts, 
                       total_bouts_overall, transition_bout, 
                       transition_bout_length, last_day_offset,
                       last_night_offset, total_day_sleep, 
                       total_night_sleep, total_overall_sleep)

        output[[i]] <- fly_paras
        
        i <- i + 1
    }

    output <- cbind(c("latency_first_day", "offset_first_day", 
                      "length_first_day_bout", "latency_first_night", 
                      "offset_first_night", "length_first_night_bout",
                      "latency_first_max_day", "offset_first_max_day", 
                      "number_of_max_day_bouts", "length_first_max_day_bout",
                      "latency_first_max_night", "offset_first_max_night", 
                      "number_of_max_night_bouts", "length_first_max_night_bout",
                      "length_first_max_overall_bout", "number_of_max_overall_bouts", 
                      "latency_first_max_overall", "offset_first_max_overall",
                      "total_day_bouts", "total_night_bouts", "total_bouts_overall", 
                      "transition_bout", "transition_bout_length", 
                      "last_day_offset", "last_night_offset",
                      "total_day_sleep", "total_night_sleep", 
                      "total_overall_sleep"), output)

    colnames(output) <- c("parameter", channels)
    
    # Write data frame into xlsx file specified in argument 2
    write.xlsx(output, file = xlsx_file, sheetName = "sleep_parameters")
}

############################# END ###################################
#####################################################################