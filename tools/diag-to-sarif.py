#!/usr/bin/env python3
"""Convert Habu diagnostic JSONL to SARIF 2.1.0."""
from __future__ import annotations

import json
import pathlib
import sys


def iter_lines(paths: list[str]):
    if not paths:
        yield from sys.stdin
        return
    for name in paths:
        with open(name) as f:
            yield from f


def message_for(obj: dict) -> str:
    for key in ("suggestion", "reason", "verdict", "code"):
        val = obj.get(key)
        if val:
            return str(val)
    return "Habu diagnostic"


def region_for(obj: dict) -> dict:
    region: dict[str, int] = {}
    if "line" in obj:
        region["startLine"] = int(obj["line"])
    if "column" in obj:
        region["startColumn"] = int(obj["column"])
    if "byte_start" in obj:
        region["byteOffset"] = int(obj["byte_start"])
    if "byte_end" in obj and "byte_start" in obj:
        region["byteLength"] = max(0, int(obj["byte_end"]) - int(obj["byte_start"]))
    return region


def result_for(obj: dict) -> dict:
    file_name = str(obj.get("file", "<input>"))
    result = {
        "ruleId": str(obj.get("code", "HABU-DIAGNOSTIC")),
        "level": "error",
        "message": {"text": message_for(obj)},
        "properties": {
            "schema_version": obj.get("schema_version"),
            "word": obj.get("word"),
            "token": obj.get("token"),
            "verdict": obj.get("verdict"),
        },
        "locations": [{
            "physicalLocation": {
                "artifactLocation": {"uri": file_name},
                "region": region_for(obj),
            }
        }],
    }
    return result


def main(argv: list[str]) -> int:
    results = []
    rules: dict[str, dict] = {}
    for line_no, line in enumerate(iter_lines(argv), 1):
        stripped = line.strip()
        if not stripped:
            continue
        obj = json.loads(stripped)
        if not isinstance(obj, dict):
            raise SystemExit(f"diag-to-sarif: line {line_no}: expected JSON object")
        code = str(obj.get("code", "HABU-DIAGNOSTIC"))
        rules.setdefault(code, {
            "id": code,
            "name": code,
            "shortDescription": {"text": code},
        })
        results.append(result_for(obj))

    sarif = {
        "$schema": "https://json.schemastore.org/sarif-2.1.0.json",
        "version": "2.1.0",
        "runs": [{
            "tool": {
                "driver": {
                    "name": "habu",
                    "informationUri": "https://github.com/joelreymont/habu",
                    "rules": [rules[k] for k in sorted(rules)],
                }
            },
            "results": results,
        }],
    }
    json.dump(sarif, sys.stdout, indent=2)
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
