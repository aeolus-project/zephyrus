#!/bin/bash

./zephyr\
 -u u-ex-2-3-with-ram.json\
 -spec spec-ex.spec\
 -ic ic-ex-4.json\
 -repo debian-squeeze repositories/repo-debian-squeeze.json\
 -opt compact\
 -print-cstrs -print-facile-vars
