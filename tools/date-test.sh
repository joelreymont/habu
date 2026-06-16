#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
cat tools/date.f tools/date-test.f | bin/hb
