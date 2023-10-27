#!/usr/bin/env bash

rm -rf benchmark_results
mkdir -p benchmark_results

# Criterion tests

stack bench myers-diff:bench:myers-diff-criterion-small-inserts --flag myers-diff:diff --ba "--output benchmark_results/small_insert.html"
stack bench myers-diff:bench:myers-diff-criterion-small-deletes --flag myers-diff:diff --ba "--output benchmark_results/small_delete.html"

# Weigh tests

stack bench myers-diff:bench:myers-diff-weigh --flag myers-diff:diff &> benchmark_results/weigh.md
