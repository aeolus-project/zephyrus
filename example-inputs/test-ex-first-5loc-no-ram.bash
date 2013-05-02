#!/bin/bash

/usr/bin/time ../zephyrus.native\
 -u           u-ex-2-3-no-ram.json\
 -spec        spec-ex-no-ram.spec\
 -ic          ic-ex-empty-5loc-no-ram.json\
 -out         json ic-ex-first-output-5loc-no-ram-result.json\
 -out         graph result-first-output-5loc-no-ram.dot\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         compact\
 -print-all > result-first-5loc-no-ram.txt 2> time-first-5loc-no-ram.txt