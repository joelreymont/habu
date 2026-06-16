#!/bin/sh
set -eu

cd "$(dirname "$0")/.."
cat tools/string.f tools/string-test.f | bin/hb
