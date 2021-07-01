#!/bin/bash

# Check usage
if [ $# -lt 3 ]; then
  echo "Usage: $0 RESULTS_DIR TIME REPS" 
  exit 1
fi

set -e

# Figure out script absolute path
pushd `dirname $0` > /dev/null
SCRIPT_DIR=`pwd`
popd > /dev/null

out_dir=$1
time=$2
id1=1
id2=$3

mkdir -p "$out_dir"
cd "$out_dir"

for id in $(seq $id1 $id2); do

  $SCRIPT_DIR/run_benchmark.sh ant ant.ProjectBuilderTest $id $time ant-project-afl.dict xml/build.xml

  $SCRIPT_DIR/run_benchmark.sh maven maven.ModelReaderTest $id $time maven-model-afl.dict xml/pom.xml

  $SCRIPT_DIR/run_benchmark.sh bcel bcel.ParserTest $id $time javaclass.dict javaclass/Hello.class

  $SCRIPT_DIR/run_benchmark.sh closure closure.CompilerTest $id $time javascript.dict js/react.production.min.js

  $SCRIPT_DIR/run_benchmark.sh rhino rhino.CompilerTest $id $time javascript.dict js/react.production.min.j

done
