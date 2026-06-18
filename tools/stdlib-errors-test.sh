#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
cat lib/errors.f tools/stdlib-errors-test.f | bin/hb
