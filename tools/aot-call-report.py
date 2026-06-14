#!/usr/bin/env python3
"""Report patched AOT call stencils in a generated binary."""
from __future__ import annotations

import json
import pathlib
import struct
import sys

NOP = 0xD503201F
BL_MASK = 0xFC000000
BL_OP = 0x94000000


def usage() -> None:
    print("usage: tools/aot-call-report.py binary", file=sys.stderr)
    raise SystemExit(64)


def words(data: bytes):
    for off in range(0, len(data) - 15, 4):
        yield off, struct.unpack_from("<IIII", data, off)


def word32s(data: bytes):
    for off in range(0, len(data) - 3, 4):
        yield off, struct.unpack_from("<I", data, off)[0]


def main(argv: list[str]) -> int:
    if len(argv) != 1:
        usage()
    path = pathlib.Path(argv[0])
    data = path.read_bytes()
    sites = []
    for off, quartet in words(data):
        if quartet[0] == NOP and quartet[1] == NOP and quartet[2] == NOP and quartet[3] & BL_MASK == BL_OP:
            sites.append(off)
    direct_bl_sites = [off for off, word in word32s(data) if word & BL_MASK == BL_OP]
    doc = {
        "schema_version": 1,
        "file": str(path),
        "file_bytes": len(data),
        "patched_call_stencils": len(sites),
        "padding_bytes": len(sites) * 12,
        "compact_call_bytes": len(sites) * 4,
        "direct_bl_instructions": len(direct_bl_sites),
        "direct_bl_sites": direct_bl_sites,
        "sites": sites,
    }
    json.dump(doc, sys.stdout, indent=2)
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
