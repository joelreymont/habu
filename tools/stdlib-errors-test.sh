#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
bin/hb --load lib/errors.f tools/stdlib-errors-test.f
