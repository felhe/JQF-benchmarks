### PestEval R File usage




## Suggested Directory Structure

`imgs/        -->` (output dir for plots)<br/>
`results_dir/ -->` (Benchmark raw data) <br/>
`PestEVAL.r   -->` (R script)<br/>

## Parameters in R script

* `Path_to_Table2_CSV_box` table2 csv file with one line per tool and benchmark and repetition 
* `Path_to_Table2_CSV_bar` table2 csv file with one line per tool and benchmark
* `results_main_bench` path to `results_dir/` benchmark raw data
* `h` & `w` height and width of plots in cm
* `number_of_rows` set to `1` for correct output of all.pdf/all.jpg and `2` for a more compact result. h and w have to be ajusted accordingly
* `label_leg` labels for Legend 
* `x_tic_labs` sets labels for x-axis ticks (only needed for some plots) 
