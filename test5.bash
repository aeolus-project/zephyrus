#!/bin/bash

./gentlewestwind\
 -u u_3.json\
 -spec spec_2.spec\
 -ic ic_1.json\
 -repo ubuntu repositories/repo-ubuntu-precise.json -print-u -print-tu -prefix-repos
