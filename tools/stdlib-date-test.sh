#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
bin/hb --load lib/errors.f lib/date.f tools/stdlib-date-test.f
