#!/bin/bash

/usr/bin/time ../zephyrus\
 -u           u-ex-2-3.json\
 -spec        spec-ex.spec\
 -ic          ic-ex-empty-6loc.json\
 -out         ic-ex-first-output-6loc-result.json\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         compact\
 -out-format  json\
 -print-all > result-first-6loc.txt 2> time-first-6loc.txt