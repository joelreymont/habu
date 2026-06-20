#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
bin/hb --load lib/errors.f lib/time.f tools/stdlib-time-test.f
