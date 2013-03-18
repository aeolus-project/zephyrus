#!/bin/bash

/usr/bin/time ../zephyrus\
 -u           u-ex-3-3-no-ram.json\
 -spec        spec-ex-no-ram.spec\
 -ic          ic-ex-first-output-4loc-no-ram.json\
 -out         json ic-ex-second-output-4loc-no-ram-result.json\
 -out         graph ic-ex-second-output-4loc-no-ram-result.dot\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         conservative\
 -print-all > result-second-4loc-no-ram.txt 2> time-second-4loc-no-ram.txt