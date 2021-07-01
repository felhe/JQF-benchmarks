#!/bin/bash

if [ $# -lt 4 ]; then
  echo "Usage: $0 <NAME> <SEMANTIC-FILTER-REGEX> <RESULTS-DIR> <RUNS>"
  exit 1
fi


NAME=$1
FILTER_REGEX=$2
RESULTS_DIR=$3
RUNS="$4"
 
if [ ! -d $RESULTS_DIR ]; then
	echo "$RESULTS_DIR doesn't exist!"
	exit 1
fi

for e in $(seq $RUNS); do
  JQF_OUT_DIR="${RESULTS_DIR}/$NAME-zest-results-$e"
  AFL_OUT_DIR="${RESULTS_DIR}/$NAME-afl-results-$e"
  RND_OUT_DIR="${RESULTS_DIR}/$NAME-rnd-results-$e"

  OUT_DIRS=($JQF_OUT_DIR $AFL_OUT_DIR $RND_OUT_DIR)

  for dir_index in ${!OUT_DIRS[@]}; do
     OUT_DIR=${OUT_DIRS[$dir_index]}

     CSV_DIR=$OUT_DIR/jacoco-csv-semantic-fast
     if [ ! -d $CSV_DIR ] ; then
        echo "$CSV_DIR doesn't exist! Please run generate_raw_coverage_data.sh first."
        break
     fi

     rm -f $OUT_DIR/semantic-coverage-percent.ssv

     echo -n "Getting coverage percentages for $OUT_DIR ..."
     for input_file in $CSV_DIR/cov*; do
        file_basename=$(basename $input_file)
        id=${file_basename:4:5}
        printf "id_0%s %f\n" $id $(egrep $FILTER_REGEX $input_file | awk -F, '{missed+= $6; covered += $7} END {print covered/(missed+covered)}') >> $OUT_DIR/semantic-coverage-percent.ssv

     done
     echo "Done $OUT_DIR"
  
  done
  
done

