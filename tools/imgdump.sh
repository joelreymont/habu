#!/bin/sh
# imgdump.sh <image> [image2] — per-word dict dump of a habu binary (runs
# tools/imgdump.f on bin/hbi). With two images: compare word sizes first
# (shift-insensitive), then offsets — answers "which word changed size" vs
# "everything just shifted".
set -e
cd "$(dirname "$0")/.."
[ -x bin/hbi ] || { echo "no bin/hbi — run tools/build.sh"; exit 1; }
dump() { cp "$1" /tmp/imgdump-in; bin/hbi < tools/imgdump.f; }
[ $# -ge 2 ] || { dump "$1"; exit 0; }
dump "$1" > /tmp/imgdump-a.txt
dump "$2" > /tmp/imgdump-b.txt
cmp -s /tmp/imgdump-a.txt /tmp/imgdump-b.txt && { echo "identical dicts"; exit 0; }
cut -d' ' -f1,3 /tmp/imgdump-a.txt > /tmp/imgdump-a.nl
cut -d' ' -f1,3 /tmp/imgdump-b.txt > /tmp/imgdump-b.nl
if cmp -s /tmp/imgdump-a.nl /tmp/imgdump-b.nl; then
  echo "word sizes identical; offsets shifted (first entry):"
  { head -1 /tmp/imgdump-a.txt; head -1 /tmp/imgdump-b.txt; }
else
  echo "word size/name differences (name len):"
  diff /tmp/imgdump-a.nl /tmp/imgdump-b.nl
fi
