#!/bin/bash

/usr/bin/time ../zephyrus\
 -u           u-ex-3-3.json\
 -spec        spec-ex.spec\
 -ic          ic-ex-first-output-4loc.json\
 -out         json ic-ex-second-output-4loc-result.json\
 -out         graph result-second-output-4loc.dot\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         conservative\
 -print-all > result-second-4loc.txt 2> time-second-4loc.txt