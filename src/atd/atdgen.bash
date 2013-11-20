#!/bin/bash
atdgen -t             json.atd
atdgen -j -j-defaults json.atd
atdgen -v             json.atd
atdgen -t             json_versions.atd
atdgen -j -j-defaults json_versions.atd
atdgen -v             json_versions.atd
