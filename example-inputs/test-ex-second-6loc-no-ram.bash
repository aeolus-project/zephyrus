#!/bin/bash

/usr/bin/time ../zephyrus\
 -u           u-ex-3-3-no-ram.json\
 -spec        spec-ex-no-ram.spec\
 -ic          ic-ex-first-output-6loc-no-ram.json\
 -out         json ic-ex-second-output-6loc-no-ram-result.json\
 -out         graph result-second-output-6loc-no-ram.dot\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         conservative\
 -print-all > result-second-6loc-no-ram.txt 2> time-second-6loc-no-ram.txt