#!/bin/bash

if [ $# -lt 2 ]; then
  echo "Usage: $0 <RESULTS_DIR> <RUNS>"
  exit 1
fi

if [ -z $JQF_DIR ]; then 
	echo "Please set JQF_DIR"
	exit 1
fi

# Figure out script absolute path
pushd `dirname $0` > /dev/null
SCRIPT_DIR=`pwd`
popd > /dev/null


JQF_JAR_PREFIX=$JQF_DIR/examples/target/dependency
RESULTS_DIR=$1
RUNS=$2

# Run Jacoco to get all the results
${SCRIPT_DIR}/jacoco_cov_fast_short.sh ant ant.ProjectBuilderTest $JQF_JAR_PREFIX/ant-1.10.2.jar ${RESULTS_DIR} ${RUNS}
${SCRIPT_DIR}/collect_semantic_percentages_short.sh ant 'org.apache.tools.ant' ${RESULTS_DIR} ${RUNS}
find ${RESULTS_DIR}/ant* -name jacoco-csv-semantic-fast -exec rm -rf {} +

${SCRIPT_DIR}/jacoco_cov_fast_short.sh maven maven.ModelReaderTest $JQF_JAR_PREFIX/maven-model-3.5.2.jar ${RESULTS_DIR} ${RUNS}
${SCRIPT_DIR}/collect_semantic_percentages_short.sh maven 'org.apache.maven.model' ${RESULTS_DIR} ${RUNS}
find ${RESULTS_DIR}/maven* -name jacoco-csv-semantic-fast -exec rm -rf {} +

${SCRIPT_DIR}/jacoco_cov_fast_short.sh closure closure.CompilerTest $JQF_JAR_PREFIX/closure-compiler-v20180204.jar ${RESULTS_DIR} ${RUNS}
${SCRIPT_DIR}/collect_semantic_percentages_short.sh closure 'com.google.javascript.jscomp.[A_Z]' ${RESULTS_DIR} ${RUNS}
find ${RESULTS_DIR}/closure* -name jacoco-csv-semantic-fast -exec rm -rf {} +

${SCRIPT_DIR}/jacoco_cov_fast_short.sh rhino rhino.CompilerTest  $JQF_JAR_PREFIX/rhino-1.7.8.jar ${RESULTS_DIR} ${RUNS}
${SCRIPT_DIR}/collect_semantic_percentages_short.sh rhino 'org.mozilla.javascript.(optimizer|CodeGenerator)' ${RESULTS_DIR} ${RUNS}
find ${RESULTS_DIR}/rhino* -name jacoco-csv-semantic-fast -exec rm -rf {} +

${SCRIPT_DIR}/jacoco_cov_fast_short.sh bcel bcel.ParserTest $JQF_JAR_PREFIX/bcel-6.2.jar ${RESULTS_DIR} ${RUNS}
${SCRIPT_DIR}/collect_semantic_percentages_short.sh bcel 'org.apache.bcel.verifier' ${RESULTS_DIR} ${RUNS}
find ${RESULTS_DIR}/bcel* -name jacoco-csv-semantic-fast -exec rm -rf {} +


