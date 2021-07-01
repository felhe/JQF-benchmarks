#!/bin/bash

if [ $# -lt 6 ]; then
  echo "Usage: $0 <NAME> <TEST_CLASS> <IDX> <TIME> <DICT> <SEEDS>"
  exit 1
fi

set -e

pushd `dirname $0` > /dev/null
SCRIPT_DIR=`pwd`
popd > /dev/null

if [ ! -d "$AFL_DIR" ]; then
  echo "AFL_DIR is not set!"
  exit 2
fi

if [ ! -d "$JQF_DIR" ]; then
  echo "JQF_DIR is not set!"
  exit 2
fi
if [ ! -d "$JQF_PEST_DIR" ]; then
  echo "JQF_PEST_DIR is not set!"
  exit 2
fi

JQF_ZEST="$JQF_DIR/bin/jqf-zest"
JQF_AFL="$JQF_DIR/bin/jqf-afl-fuzz"
JQF_PEST="$JQF_PEST_DIR/bin/jqf-zest"
NAME=$1
TEST_CLASS="edu.berkeley.cs.jqf.examples.$2"
IDX=$3
TIME=$4
DICT="$JQF_DIR/examples/target/test-classes/dictionaries/$5"
SEEDS="$JQF_DIR/examples/target/seeds/$6"
SEEDS_DIR=$(dirname "$SEEDS")

e=$IDX

JQF_PEST_OUT_DIR="$NAME-pest-results-$e"
JQF_OUT_DIR="$NAME-zest-results-$e"
AFL_OUT_DIR="$NAME-afl-results-$e"
RND_OUT_DIR="$NAME-rnd-results-$e"

if [ -d "$JQF_OUT_DIR" ]; then
  echo "Error! There are already some results for $JQF_OUT_DIR in the given directory"
  echo "Please remove all the results in this directory or else use a different results directory"
  exit 3
fi

# Do not let GC mess with fuzzing
export JVM_OPTS="$JVM_OPTS -XX:-UseGCOverheadLimit"


# Run Zest
timeout $TIME $JQF_PEST -c $($JQF_PEST_DIR/scripts/examples_classpath.sh) $TEST_CLASS testWithGenerator $JQF_PEST_OUT_DIR || [ $? -eq 124 ]

# Run Zest
timeout $TIME $JQF_ZEST -c $($JQF_DIR/scripts/examples_classpath.sh) $TEST_CLASS testWithGenerator $JQF_OUT_DIR || [ $? -eq 124 ]

# Run AFL
timeout $TIME $JQF_AFL -m none -t 10000 -c $($JQF_DIR/scripts/examples_classpath.sh) -x $DICT -o $AFL_OUT_DIR -T $NAME-afl-$e -i $SEEDS_DIR $TEST_CLASS testWithInputStream || [ $? -eq 124 ]

# Run Random
JVM_OPTS="$JVM_OPTS -Djqf.ei.TOTALLY_RANDOM=true" timeout $TIME $JQF_ZEST -c $($JQF_DIR/scripts/examples_classpath.sh) $TEST_CLASS testWithGenerator $RND_OUT_DIR || [ $? -eq 124 ]


