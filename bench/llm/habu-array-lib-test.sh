#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."
cat bench/llm/habu-array-lib.f bench/llm/habu-array-lib-test.f | bin/hb
