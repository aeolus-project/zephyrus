#!/bin/bash

/usr/bin/time ../zephyrus.native\
 -u           u-ex-2-3.json\
 -spec        spec-ex2.spec\
 -ic          ic-ex-empty-8loc.json\
 -out         json ic-ex-first-output-8loc-result.json\
 -out         graph                       result-first-output-8loc-graph.dot\
 -out         simplified-deployment-graph result-first-output-8loc-simplified-deployment-graph.dot\
 -out         components-graph            result-first-output-8loc-components-graph.dot\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         compact\
 -print-all > result-first-8loc.txt 2> time-first-8loc.txt