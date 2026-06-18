#!/bin/sh
set -eu

cd "$(dirname "$0")/../.."
HB=${HABU_HB:-bin/hb}
cat lib/errors.f lib/array.f bench/llm/habu-array-lib.f bench/llm/habu-array-lib-test.f | "$HB"
