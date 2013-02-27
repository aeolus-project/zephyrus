#!/bin/bash

/usr/bin/time ../zephyr\
 -u           u-ex-3-3-no-ram.json\
 -spec        spec-ex-no-ram.spec\
 -ic          ic-ex-first-output-5loc-no-ram.json\
 -out         ic-ex-second-output-5loc-no-ram-result.json\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         conservative\
 -out-format  json\
 -print-all > result-second-5loc-no-ram.txt 2> time-second-5loc-no-ram.txt