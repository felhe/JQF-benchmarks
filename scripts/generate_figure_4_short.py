#!/usr/bin/python
from __future__ import print_function
import sys
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
#import matplotlib
import re
import os.path

# Setting fonts so it's the correct type for papers
matplotlib.rcParams['pdf.fonttype'] = 42
matplotlib.rcParams['ps.fonttype'] = 42

matplotlib.rcParams.update({'font.size':'18'})

# Student's t Distribution List for 2-tail alpha = 0.05
CI_mult = [12.7062,4.3027,3.1824,2.7764,2.5706,2.4469,2.3646, 2.3060, 2.2622,2.2281,2.2010,2.1788,2.1604,2.1448, 2.131,2.120,2.110,2.101,2.093,2.086,2.080,2.074,2.069]

# Line colors and types for easy plotting
color_cycle=["#0072B2", "#E69F00", "#999999" ]
line_cycle=['-', "--", ":", "--"]

# Dictionaries for easy plotting
nice_names = {"zest": "Zest","pest":"Pest"}
benchmark_figs = {"ant": "b", "maven": "a", "closure": "c", "rhino": "d", "bcel": "e"}

# The benchmarks + techniques to generate graphs for
benchmarks = ["ant", "maven", "closure", "rhino", "bcel"]
techniques = ["zest","pest"]
folder_suffixes = []

# Given a file with format
# filename data
# Returns {input_id -> data}
def input_id_to_data(f):
    fname_re = re.compile("id.(?P<id>[0-9]+).*")
    # Deals with crashes, when there's a time when we have 0 crashes, thus the latest ID doesn't exist. Sorry. 
    return_dict = {-1 : 0}
    for line in f:
        line = line.rstrip()
        split_line = line.split()
        regex_match = fname_re.match(split_line[0])
        if (regex_match is None):
                    raise Exception("didn't match the filename %s" %split_line[0])
        input_id = int(regex_match.group('id'))
        data = float(split_line[1]) * 100
        return_dict[input_id] = data
    return return_dict

# Given a plot_data file
# Returns {relative_time -> latest_input_id}
def id_at_time_map(f):
    return_dict = {}
    base_time = 0
    first_line = True
    for line in f:
        if line.startswith("#"):
            continue
        elif first_line == True:
            first_line = False
            print(line)
            base_time = int(line.rstrip().split(',')[0])
        line = line.rstrip()
        split_line = line.split(',')
        relative_time = int(split_line[0]) - base_time
        current_input_id = int(split_line[3]) - 1
        return_dict[relative_time] = current_input_id
    return return_dict


### MAIN ###
if len(sys.argv) != 3:
    print("Usage: {0} RESULTS_DIR REPS")
    sys.exit(1)

parent_folder = sys.argv[1]
num_reps = int(sys.argv[2])

if not os.path.exists("%s/figures" % parent_folder):
    os.mkdir("%s/figures" % parent_folder)

for i in range(1, num_reps + 1):
    folder_suffixes.append("-results-" + str(i))

for benchmark in benchmarks:

    technique_prefixes = ["/%s-%s" % ( benchmark, technique) for technique in techniques]
    all_times = set([])
    all_data_values = {}
    times = {}
    data_values = {}


    # Collect all the observed relative times and data values (coverage percentages) 
    for technique_prefix in technique_prefixes:
        times[technique_prefix] = []
        data_values[technique_prefix] = []
        for folder_suffix in folder_suffixes:
            plot_file = open(parent_folder + technique_prefix + folder_suffix + "/plot_data")
            data_file = open(parent_folder + technique_prefix + folder_suffix + "/semantic-coverage-percent.ssv")
            file_times_to_id = id_at_time_map(plot_file)
            ids_to_data = input_id_to_data(data_file)
            file_times_to_data = { time : ids_to_data[id] for time, id in file_times_to_id.items()}
            all_times = all_times.union(set(file_times_to_data.keys()))
            times_in_order = sorted(file_times_to_data.keys())
            datas_in_order = [file_times_to_data[time] for time in times_in_order]
            times[technique_prefix].append(times_in_order)
            data_values[technique_prefix].append(datas_in_order)

    # Interpolate for every time interval so that we can easily average all the observations
    all_times_sorted = sorted(all_times)
    ignore_prefixes = []
    for prefix in technique_prefixes:
        all_data_values[prefix] = np.zeros((len(folder_suffixes),len(all_times_sorted)))
        for i in range(len(folder_suffixes)):
            if len(times[prefix][i]) == 0 and len(data_values[prefix][i]) == 0:
                ignore_prefixes.append(prefix)
                print("WARNING: missing coverage data for %s (repetition %i), omitting from plot." % (prefix.strip("/"), i))
                break
            all_data_values[prefix][i] =  np.interp(all_times_sorted, times[prefix][i], data_values[prefix][i])

    all_times_hrs = [i / 3600.0 for i in all_times_sorted]
    i = 0

    # Plot the means and 95% CI's 
    plt.figure()
    for technique_prefix in technique_prefixes:
        if technique_prefix in ignore_prefixes:
            continue
        means = np.mean(all_data_values[technique_prefix], axis = 0)
        nice_name = nice_names[technique_prefix.split('-')[1]]
        plt.plot(all_times_hrs, means, color=color_cycle[i], linestyle=line_cycle[i], label=nice_name, linewidth=3)
        stds = np.std(all_data_values[technique_prefix], axis = 0)
        stds = stds/np.sqrt(len(all_data_values[technique_prefix]))
        stds = stds * CI_mult[len(all_data_values[technique_prefix])-2] # -2 to make up for 0-indexing + degrees of freedom
        plt.fill_between(all_times_hrs, means - stds, means + stds, facecolor=color_cycle[i], alpha=0.2,linestyle='dashed', edgecolor=color_cycle[i])
        i += 1

    plt.legend(loc='best')
    plt.ylabel("% Semantic Branches Covered")
    plt.xlabel("Time (hrs)")
    plt.tight_layout()
    plt.savefig('%s/figures/Figure_4%s_%s.pdf'% (parent_folder, benchmark_figs[benchmark], benchmark))

