#!/bin/bash

/usr/bin/time ../zephyrus\
 -u           u-ex-2-3.json\
 -spec        spec-ex.spec\
 -ic          ic-ex-empty-5loc.json\
 -out         json ic-ex-first-output-5loc-result.json\
 -out         graph result-first-output-5loc.dot\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         compact\
 -print-all > result-first-5loc.txt 2> time-first-5loc.txt