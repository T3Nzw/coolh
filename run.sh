#!/bin/bash

# Make sure the project is built first
# (lowkey redundant since it already does it when it runs)
cabal build coolh.cabal

# Modify both input (test_dir) and output (out_dir)
# directories as needed
target_dir="./tests"
out_dir="./out"

for f in ${target_dir}/*; do
  echo "Running lexer for $f"
  filename="$(basename $f .in)"
  # Run the project for each file in the test_dir directory.
  # Will eventually need to modify it so that you can pass
  # a flag that allows for dumping the contents of the lexer.
  # I don't remember if you need both "-- --" when running
  # cabal with command line arguments, I think it says otherwise
  # in the documentation, but for some reason that doesn't work for me, lol
  cabal run coolh.cabal -- -- "$f" "${out_dir}/${filename}.out"
done
