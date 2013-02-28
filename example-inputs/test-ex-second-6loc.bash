#!/bin/bash

/usr/bin/time ../zephyrus\
 -u           u-ex-3-3.json\
 -spec        spec-ex.spec\
 -ic          ic-ex-first-output-6loc.json\
 -out         ic-ex-second-output-6loc-result.json\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         conservative\
 -out-format  json\
 -print-all > result-second-6loc.txt 2> time-second-6loc.txt