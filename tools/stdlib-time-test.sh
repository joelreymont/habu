#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
cat lib/errors.f lib/time.f tools/stdlib-time-test.f | bin/hb
