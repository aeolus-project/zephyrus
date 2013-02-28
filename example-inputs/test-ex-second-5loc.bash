#!/bin/bash

/usr/bin/time ../zephyrus\
 -u           u-ex-3-3.json\
 -spec        spec-ex.spec\
 -ic          ic-ex-first-output-5loc.json\
 -out         ic-ex-second-output-5loc-result.json\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         conservative\
 -out-format  json\
 -print-all > result-second-5loc.txt 2> time-second-5loc.txt