import csv
from collections import defaultdict
from collections import Counter
import pandas as pd
import numpy as np
import math
import os
from collections import OrderedDict

#Get the rows of the log file that contains the logpop measurements
def dimensions_of_log_file(log_file, burn_in, col_elem = "logPopSize"):

    overall_length = -1
    with open(log_file) as f:
        for l in f:
            if l[0] != "#":
                if overall_length < 0:
                    first_line = l
                    overall_length += 1
                else:
                    overall_length += 1
                
    first_line = first_line.split("\t")

    col_index = {}
    for count, element in enumerate(first_line):
        if col_elem in element:
            col_index[count] = element

    logpop_length = len(col_index)

    last_row = overall_length
    if burn_in != 0:
        first_row = int(round(last_row/burn_in,0))
    else:
        first_row = 1

    return first_row, last_row, logpop_length, col_index

#Get the columns of the log file that contains logpops
def get_all_skygrid_values(log_file, first_row, last_row, col_index):

    skygrid = defaultdict(list)
    row_count = -1
    with open(log_file) as f:
        for l in f:
            if l[0] != "#":
                row_count += 1
                if row_count >= first_row and row_count <= last_row:
                    row = l.split("\t")
                    for i, value in enumerate(row):
                        if i in col_index:
                            skygrid[col_index[i]].append(float(value))

    return skygrid
    
def log_pops(skygrid):

    mean_dict = {}
    lower_dict = {}
    upper_dict = {}

    for key, value in skygrid.items():
        mean_dict[key] = (sum(value)/len(value))
        lower_dict[key] = np.percentile(value, 2.5)
        upper_dict[key] = np.percentile(value, 97.5)
        
    mean_list = []
    lower_list = []
    upper_list = []

    for value in mean_dict.values():
        mean_list.append(value)
    for value in lower_dict.values():
        lower_list.append(value)
    for value in upper_dict.values():
        upper_list.append(value)

    revmean = list(reversed(mean_list))
    revlower = list(reversed(lower_list))
    revupper = list(reversed(upper_list))

    return revmean, revlower, revupper, mean_list, lower_list, upper_list

#Combining all - will return the mean logpop at each timepoint (minus burn-in), with lower and upper bounds, forwards in time
def get_pop_sizes(log_file, burn_in, col_elem = "logPopSize"):
    
    first_row, last_row, logpop_length, col_index = dimensions_of_log_file(log_file, burn_in, col_elem)
    
    skygrid = get_all_skygrid_values(log_file, first_row, last_row, col_index)
    
    revmean, revlower, revupper, mean_list, lower_list, upper_list = log_pops(skygrid)
    
    return revmean, revlower, revupper, mean_list, lower_list, upper_list, logpop_length

#Define the gridpoints from the skygrid so skyline can be forced into the same time structure
def get_gridpoints(end_time, skygrid_cutoff, logpop_length):
    
    begin_time = end_time - skygrid_cutoff

    total_time = end_time - begin_time

    gridpoint_intervals = total_time/logpop_length
    
    
    gridpoints = []
    new = 0

    for i in range(logpop_length):
        new += gridpoint_intervals
        gridpoints.append(new)

    revtimes = list(reversed(gridpoints))

    times_in_years = [end_time - i for i in revtimes]
#     times_in_years.append(end_time)
    
    return times_in_years, revtimes


def plot_skygrid(log_file, skygrid_cutoff, most_recent_obs, burn_in, produce_plot=True, col_elem="logPopSize"):

    revmean, revlower, revupper, means, lowers, uppers, logpop_length = get_pop_sizes(log_file, burn_in, col_elem)

    gridpoints = get_gridpoints(most_recent_obs, skygrid_cutoff, logpop_length)

    times_in_years, revtimes = gridpoints

    if produce_plot:
        import matplotlib.pyplot as plt

        fig = plt.subplots(1,1, figsize=(20,10))

        x = times_in_years
        y = revmean

        plt.plot(x,y,color="darkblue", linewidth=3)
        plt.fill_between(x, revlower, revupper, color="lightblue", alpha=0.5)

        plt.savefig("skygrid.pdf", format='pdf')

    skygrid_dict = {}
    skygrid_dict["times_in_years"] = times_in_years
    skygrid_dict["revlower"] = revlower
    skygrid_dict["revupper"] = revupper
    skygrid_dict["revmean"] = revmean

    return skygrid_dict