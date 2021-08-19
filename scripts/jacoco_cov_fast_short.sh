#!/bin/bash

set -e

if [ $# -lt 5 ]; then
  echo "Usage: $0 <NAME> <TEST_CLASS> <SEMANTIC-JAR> <RESULTS-DIR> <RUNS>"
     echo "hello"
  exit 1
fi

if [[ -z "$JQF_DIR" ]]; then
    echo "Please set JQF_DIR." 1>&2
    exit 1
fi

# Figure out script absolute path
pushd `dirname $0` > /dev/null
SCRIPT_DIR=`pwd`
popd > /dev/null


JQF_REPRO="$JQF_DIR/bin/jqf-repro -i"
NAME=$1
TEST_CLASS="edu.berkeley.cs.jqf.examples.$2"
SEMANTIC_JAR=$3
RESULTS_DIR=$4
RUNS="$5"
 
if [ ! -d $RESULTS_DIR ]; then
    echo "$RESULTS_DIR doesn't exist!"
    exit 1
fi


export JVM_OPTS="$JVM_OPTS -Djqf.repro.logUniqueBranches=true"

for e in $(seq $RUNS); do
  ZEST_OUT_DIR="${RESULTS_DIR}/$NAME-zest-results-$e"
  PEST_OUT_DIR="${RESULTS_DIR}/$NAME-pest-results-$e"

  OUT_DIRS=($ZEST_OUT_DIR $PEST_OUT_DIR )
  TEST_FUNCTIONS=(testWithGenerator testWithGenerator) 
  CORPUS_DIRS=( corpus/ corpus/)

  for dir_index in ${!OUT_DIRS[@]}; do
     OUT_DIR=${OUT_DIRS[$dir_index]}
     TEST_FUNCTION=${TEST_FUNCTIONS[$dir_index]}
     CORPUS_DIR=${CORPUS_DIRS[$dir_index]}

     CSV_DIR=$OUT_DIR/jacoco-csv-semantic-fast
     if [ -d $CSV_DIR ] ; then
        echo "$CSV_DIR already exists; might be overwriting..."
     else
        mkdir $CSV_DIR
     fi
 
     echo -n "Getting raw coverage for $OUT_DIR ... (this may take a while)"
     DEST_FILE=$(mktemp) 
     JVM_OPTS="-javaagent:${SCRIPT_DIR}/lib/jacocoagent.jar=append=false,destfile=${DEST_FILE} -Djqf.repro.jacocoAccumulateJar=$SEMANTIC_JAR -Djqf.repro.jacocoAccumulateDir=$CSV_DIR" $JQF_DIR/bin/jqf-repro -c $($JQF_DIR/scripts/examples_classpath.sh) $TEST_CLASS $TEST_FUNCTION $OUT_DIR/$CORPUS_DIR* > /dev/null 2>&1
     echo "Done $OUT_DIR"
     rm -f $DEST_FILE

  
  done
  
done

