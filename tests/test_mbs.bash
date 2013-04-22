#!/bin/bash


../zephyrus\
    -u ../example-inputs/u-ex-2-3-no-ram-mbs.json\
    -spec ../example-inputs/spec-ex-no-ram-mbs.spec\
    -ic ../example-inputs/ic-ex-empty-4loc-no-ram-mbs.json\
    -repo mbs ../repositories/repo-mbs1.json\
    -print-solution\
    -out deployment-graph graph-mbs.dot

