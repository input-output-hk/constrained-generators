#!/bin/sh

echo "# Coverage report"
hpc report $1 | awk '{sub(/^[ \t]*/, "& - ")1; sub(/^  /, "", $0); print}'
