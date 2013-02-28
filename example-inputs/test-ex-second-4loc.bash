#!/bin/bash

/usr/bin/time ../zephyr\
 -u           u-ex-3-3.json\
 -spec        spec-ex.spec\
 -ic          ic-ex-first-output-4loc.json\
 -out         ic-ex-second-output-4loc-result.json\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         conservative\
 -out-format  json\
 -print-all > result-second-4loc.txt 2> time-second-4loc.txt