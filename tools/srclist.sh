#!/bin/sh
cd "$(dirname "$0")/.."
exec "${HABU_HB:-bin/hb}" tools/srclist.f "$@"
