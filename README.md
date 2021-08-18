

## Build Container:
in JQF-benchmarks Dir run:
```
docker build -t pest_image .
docker run --name pest_Container -it pest_image
```

## First Steps inside Container:

A Benchmark can be started by running `./scripts/run_all.sh`.

Example (takes about 16h):
```
./scripts/run_all.sh <Results Dir> <time> <Repetitions> <FULL||SHORT>

./scripts/run_all.sh results 10m 10 SHORT
```

`SHORT`: only zest and Pest Algorithms are benchmarked 

`FULL`: zest, Pest, AFL, and randon/Quickcheck algorithms are evaluated

`COMPAREPEST`: zest, Pest, Pest2 used to compare two Pest versions against eachother and zest
