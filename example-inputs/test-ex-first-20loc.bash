#!/bin/bash

/usr/bin/time ../zephyrus.native\
 -u           u-ex-2-3.json\
 -spec        spec-ex2.spec\
 -ic          ic-ex-empty-20loc.json\
 -out         json ic-ex-first-output-20loc-result.json\
 -out         graph            result-first-output-20loc.dot\
 -out         components-graph result-components-20loc.dot\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         none\
 -print-all > result-first-20loc.txt 2> time-first-20loc.txt