#!/bin/bash

/usr/bin/time ../zephyrus\
 -u           u-ex-2-3-no-ram.json\
 -spec        spec-ex-no-ram.spec\
 -ic          ic-ex-empty-4loc-no-ram.json\
 -out         ic-ex-first-output-4loc-no-ram-result.json\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         compact\
 -out-format  json\
 -print-all > result-first-4loc-no-ram.txt 2> time-first-4loc-no-ram.txt