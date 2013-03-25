#!/bin/bash

/usr/bin/time ../zephyrus\
 -u           u-ex-2-3.json\
 -spec        spec-ex2.spec\
 -ic          ic-ex-empty-100loc.json\
 -out         json ic-ex-first-output-100loc-result.json\
 -out         graph            result-first-output-100loc.dot\
 -out         components-graph result-components-100loc.dot\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         none\
 -print-all > result-first-100loc.txt 2> time-first-100loc.txt