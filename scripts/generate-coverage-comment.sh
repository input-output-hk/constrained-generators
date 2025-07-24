#!/bin/sh

echo "# Coverage report"
hpc report "$1" | awk '{sub(/^[ \t]*/, "&- "); sub(/^ /, ""); print}'
