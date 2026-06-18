#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
cat lib/errors.f lib/date.f tools/stdlib-date-test.f | bin/hb
