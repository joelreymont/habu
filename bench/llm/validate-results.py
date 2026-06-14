#!/usr/bin/env python3
import csv
import json
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
TASKS = ROOT / "bench/llm/tasks.tsv"
RESULTS = ROOT / "bench/llm/results/reference.jsonl"
REQUIRED = {
    "task_id",
    "name",
    "model",
    "first_pass_checker",
    "first_pass_tests",
    "repair_iterations",
    "tokens_used",
    "final_chars",
    "trust_uses",
    "signature_weakened",
}


def fail(msg):
    print(f"llm-results: {msg}")
    sys.exit(1)


with TASKS.open(newline="") as f:
    tasks = list(csv.DictReader(f, delimiter="\t"))
if len(tasks) < 30:
    fail(f"need at least 30 tasks, found {len(tasks)}")
task_names = {int(row["id"]): row["name"] for row in tasks}

seen = set()
with RESULTS.open() as f:
    for line_no, line in enumerate(f, 1):
        row = json.loads(line)
        missing = REQUIRED - row.keys()
        if missing:
            fail(f"{RESULTS}:{line_no}: missing fields {sorted(missing)}")
        tid = row["task_id"]
        if tid in seen:
            fail(f"{RESULTS}:{line_no}: duplicate task_id {tid}")
        seen.add(tid)
        if task_names.get(tid) != row["name"]:
            fail(f"{RESULTS}:{line_no}: task/name drift for id {tid}")
        if row["model"] != "reference":
            fail(f"{RESULTS}:{line_no}: reference file contains non-reference model")
        if row["first_pass_checker"] != "certified":
            fail(f"{RESULTS}:{line_no}: reference solution not certified")
        if row["first_pass_tests"] is not True:
            fail(f"{RESULTS}:{line_no}: reference tests not passing")
        if row["repair_iterations"] != 0:
            fail(f"{RESULTS}:{line_no}: reference should need zero repairs")
        if row["trust_uses"] != 0:
            fail(f"{RESULTS}:{line_no}: benchmark task used TRUST")
        if row["signature_weakened"] is not False:
            fail(f"{RESULTS}:{line_no}: reference weakened a signature")
        if not isinstance(row["final_chars"], int) or row["final_chars"] <= 0:
            fail(f"{RESULTS}:{line_no}: invalid final_chars")

if seen != set(task_names):
    fail(f"results/tasks mismatch: {len(seen)} result row(s), {len(task_names)} task(s)")

print(f"llm-results: {len(seen)} reference metric row(s), 0 finding(s)")
