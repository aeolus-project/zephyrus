#!/bin/bash

/usr/bin/time ../zephyrus\
 -u           u-ex-2-3.json\
 -spec        spec-ex.spec\
 -ic          ic-ex-empty-4loc.json\
 -out         ic-ex-first-output-4loc-result.json\
 -repo        debian-squeeze ../repositories/repo-debian-squeeze.json\
 -opt         compact\
 -out-format  json\
 -print-all > result-first-4loc.txt 2> time-first-4loc.txt