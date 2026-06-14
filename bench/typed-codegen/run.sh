#!/bin/sh
set -e
cd "$(dirname "$0")/../.."
G=${GFORTH:-$HOME/.local/bin/gforth}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-/tmp/habu-gforth-cache}
[ -d "$XDG_CACHE_HOME/gforth" ] || "$G" -e 's" true" system bye' >/dev/null 2>&1
raw=$("$G" bench/typed-codegen/stats.fs -e bye)
RAW_STATS=$raw python3 - <<'PY'
import json
import os
import sys

categories = {
    "BOOLCTL": "bool_control",
    "ARITHLOOP": "arithmetic_loop",
    "QUOTCALL": "quotation_call",
    "PKEEP": "polymorphic_helper",
    "POLYHELP": "polymorphic_caller",
}
flag_names = [
    (1, "in_intlike"),
    (2, "out_intlike"),
    (4, "out_bool"),
    (8, "concrete_intbool"),
]
fixtures = []
for line in os.environ["RAW_STATS"].splitlines():
    line = line.strip()
    if not line.startswith("STAT "):
        continue
    _, name, arity, flags, ic_records, text_bytes, exit_code = line.split()
    flags_i = int(flags)
    fixtures.append(
        {
            "name": name,
            "category": categories[name],
            "arity": int(arity),
            "effect_flags": flags_i,
            "effect_flag_names": [label for bit, label in flag_names if flags_i & bit],
            "icode_records": int(ic_records),
            "text_bytes": int(text_bytes),
            "exit_code": int(exit_code),
        }
    )
doc = {
    "schema_version": 1,
    "suite": "typed-codegen",
    "fixtures": fixtures,
}
json.dump(doc, sys.stdout, indent=2)
sys.stdout.write("\n")
PY
