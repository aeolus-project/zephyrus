ROUGH GUIDE:


1. RUN A SERIES OF BENCHMARKS

./benchmark-script.bash "../zephyrus.native -solver [Solver:{g12,gecode}] -benchmark [BenchmarkChoice:{wordpress}] -benchmark-option wordpress_require [OptionWordpressRequire:{0,1,20,40,60,80,100,120,140,160,180,200,220,240,260,280,300}] -benchmark-option mysql_require [OptionMysqlRequire:{0,1,20,40,60,80,100,120,140,160,180,200,220,240,260,280,300}] -benchmark-option mysql_provide [OptionMysqlProvide:{3}] -benchmark-option webservers [OptionWebservers:{0}]"

GENERATES BENCHMARK RESULTS IN A SUBDIRECTORY (NAMED WITH THE CURRENT TIME) OF results/


EITHER WAY YOU SHOULD LAUNCH THE SAME COMMAND MULTIPLE TIMES (YOU CAN DO IT IN PARALLEL) IF YOU WANT TO HAVE MORE DATA POINTS FOR EACH BENCHMARK CASE.



2. TRANSFORM SOME BENCHMARK RESULTS TO A CSV FILE

./benchmarks-to-csv.bash results/benchmarks_2014-02-01_20:22:14/* results/benchmarks_2014-02-01_20:22:41/* results/benchmarks_2014-02-01_20:22:55/* results/benchmarks_2014-02-01_20:23:11/* | tee weekend_test.csv

./benchmarks-to-csv.bash results/tuesday_evening/benchmark_* | tee tuesday_evening.csv

WITH THE OPTION -append YOU CAN ADD MORE BENCHMARK RESULTS TO AN EXISTING CSV FILE (ONLY IF THEY HAVE THE SAME FIELDS)

./benchmarks-to-csv.bash -append results/benchmarks_2014-02-02_12:26:37/* results/benchmarks_2014-02-02_12:26:41/* results/benchmarks_2014-02-02_12:26:45/* results/benchmarks_2014-02-02_12:26:49/* | tee -a weekend_test.csv



3. AGGREGATE DATA IN THE CSV FILE (MIN, MAX, MEAN, STDDEV OF EVERY FIELD)

cat tuesday_evening.csv | ./aggregate-csv.bash 6 | tee tuesday_evening_aggregated.csv

THE PARAMETER (HERE: "6") IS THE NUMBER OF NON-DATA FIELDS ON THE BEGINNING (I.E. FIELDS DESCRIBING WHICH BENCHMARK CASE IT IS, LIKE "BenchmarkChoice" OR "Solver", WHICH SHOULD NOT BE AGGREGATED)
IT SHOULD CORRESPOND TO THE NUMBER OF PARAMETRIZABLE PARTS PROVIDED IN THE ./benchmark-script.bash PARAMETRIZABLE COMMAND

(OF COURSE AGGREGATION IS USEFUL ONLY IF YOU HAVE MULTIPLE INSTANCES OF RESULTS OF THE SAME TEST CASES)



4. GENERATE GRAPHS

IF YOU HAVE A PYTHON SCRIPT FOR GENERATING GRAPHS ALREADY PREPARED (LIKE monday_evening.py), YOU CAN RUN IT USING:
./run-ipython-pylab.bash monday_evening.py

OTHER WAY:

RUN ipython -pylab

COPY THE CONTENTS OF (FOR EXAMPLE) monday_evening.py
PASTE INTO ipython USING THE COMMAND "%paste" (SIMPLE Shift-Ctrl-v WILL NOT WORK WELL)


./benchmark-script.bash "../zephyrus.native -solver [Solver:{g12,gecode}] -benchmark [BenchmarkChoice:{wordpress}] -benchmark-option wordpress_require [OptionWordpressRequire:{20,40}] -benchmark-option mysql_require [OptionMysqlRequire:{20,40}] -benchmark-option mysql_provide [OptionMysqlProvide:{3}] -benchmark-option webservers [OptionWebservers:{1}]"