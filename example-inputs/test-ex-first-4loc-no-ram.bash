#!/bin/bash

/usr/bin/time ../zephyrus\
 -u           u-ex-2-3-no-ram.json\
 -spec        spec-ex-no-ram.spec\
 -ic          ic-ex-empty-4loc-no-ram.json\
 -out         json ic-ex-first-output-4loc-no-ram-result.json\
 -out         graph result-first-output-4loc-no-ram.dot\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         compact\
 -print-all > result-first-4loc-no-ram.txt 2> time-first-4loc-no-ram.txt