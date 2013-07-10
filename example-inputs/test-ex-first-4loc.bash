#!/bin/bash

/usr/bin/time ../zephyrus.native\
 -u           u-ex-2-3.json\
 -spec        spec-ex.spec\
 -ic          ic-ex-empty-4loc.json\
 -opt         compact\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -out         json ic-ex-first-output-4loc-result.json\
 -out         graph                       result-first-output-4loc-graph.dot\
 -out         simplified-deployment-graph result-first-output-4loc-simplified-deployment-graph.dot\
 -out         components-graph            result-first-output-4loc-components-graph.dot\
 -print-all > result-first-4loc.txt 2> time-first-4loc.txt