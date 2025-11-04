#!/bin/bash


# Modify these two depending the absolute/relative paths
# to out_dir (or wherever the project should write output files)
# and expected_out_dir (or whever the expected output
# of all tests should be)
out_dir="./out"
expected_out_dir="../coursework1/cw1-update3/tests/lexer"

count=$(find $out_dir -maxdepth 1 -type f | wc -l)
i=0

for f in ${out_dir}/*; do
  filename="$(basename $f)"
  res="$(diff $f ${expected_out_dir}/${filename})"
  if [ ! -z "$res" ]; then
    echo "Diff for $f failed"
  else
    ((i++))
  fi
done

echo "${i}/${count} tests succeeded"
