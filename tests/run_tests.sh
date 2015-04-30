#!/usr/bin/env bash

set -e

. assert.sh

function run_sieve() {
  ../src/g_machine/stack_machine/stack_machine ../examples/sieve_example.slg
}

function run_cons() {
  ../src/g_machine/stack_machine/stack_machine ../examples/cons_example.slg
}

function run_cons() {
  ../src/g_machine/stack_machine/stack_machine ../examples/cons_example.slg
}

function run_double_if() {
  ../src/g_machine/stack_machine/stack_machine ../examples/double_if.slg
}

function run_fact() {
  ../src/g_machine/stack_machine/stack_machine ../examples/fact.slg
}

function run_inf() {
  ../src/g_machine/stack_machine/stack_machine ../examples/inf_example.slg
}

assert run_sieve "2\n3\n5\n7\n11"
assert run_cons "1\n2\n3"
assert run_double_if "42"
assert run_fact "120\n24\n6\n2\n1\n1"
assert run_inf "5"
assert_end
