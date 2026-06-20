#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
bin/hb --load tools/date.f tools/date-test.f
