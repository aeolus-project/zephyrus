#!/bin/bash

/usr/bin/time ../zephyrus\
 -u           u-ex-2-3-no-ram.json\
 -spec        spec-ex-no-ram.spec\
 -ic          ic-ex-empty-6loc-no-ram.json\
 -out         json ic-ex-first-output-6loc-no-ram-result.json\
 -out         graph ic-ex-first-output-6loc-no-ram-result.dot\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         compact\
 -print-all > result-first-6loc-no-ram.txt 2> time-first-6loc-no-ram.txt