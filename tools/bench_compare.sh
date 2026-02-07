#!/usr/bin/env bash
# Compare Habu vs SBCL benchmark results
# Usage: tools/bench_compare.sh [--iters N]
set -euo pipefail

ITERS=3
for arg in "$@"; do
    case "$arg" in
        --iters=*) ITERS="${arg#--iters=}" ;;
    esac
done

SCRIPT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$SCRIPT_DIR"

echo "Building Habu bench..."
zig build -Doptimize=ReleaseFast 2>&1

echo "Running Habu bench (iters=$ITERS)..."
HABU_JSON=$(./zig-out/bin/cl_bench --json --iters="$ITERS")

if ! command -v sbcl &>/dev/null; then
    echo "SBCL not found, printing Habu results only"
    echo "$HABU_JSON" | python3 -m json.tool 2>/dev/null || echo "$HABU_JSON"
    exit 0
fi

echo "Running SBCL bench (iters=$ITERS)..."
SBCL_JSON=$(sbcl --script bench/cl_bench.lisp --json --iters="$ITERS")

# Merge into comparison JSON
python3 -c "
import json, sys
habu = json.loads('''$HABU_JSON''')
sbcl = json.loads('''$SBCL_JSON''')

hmap = {b['name']: b for b in habu['benches']}
smap = {b['name']: b for b in sbcl['benches']}

result = {'benches': []}
for name in hmap:
    h = hmap[name]
    s = smap.get(name, {})
    hns = h['ns']
    sns = s.get('ns', 0)
    ratio = hns / sns if sns > 0 else 0
    result['benches'].append({
        'name': name,
        'habu_ns': hns,
        'sbcl_ns': sns,
        'ratio': round(ratio, 2),
    })

print(json.dumps(result, indent=2))

# Pretty table
print()
print(f'{\"Benchmark\":<16} {\"Habu (ms)\":>12} {\"SBCL (ms)\":>12} {\"Ratio\":>8}')
print('-' * 52)
for b in result['benches']:
    hms = b['habu_ns'] / 1e6
    sms = b['sbcl_ns'] / 1e6
    r = f'{b[\"ratio\"]:.2f}x' if b['ratio'] > 0 else 'N/A'
    print(f'{b[\"name\"]:<16} {hms:>12.3f} {sms:>12.3f} {r:>8}')
"
