#!/bin/bash

# Clean up just in case...
rm -f zephyrus-1.0.tar.gz
rm -rf zephyrus-1.0/

# Remember all existing files.
FILES=`ls` 

# Copy everything into a new temporary dir.
mkdir zephyrus-1.0
cp -r $FILES zephyrus-1.0/

# Create the archive.
tar -czf zephyrus-1.0.tar.gz zephyrus-1.0

# Remove the remaining temporary directory.
rm -rf zephyrus-1.0/

