---
title: "Switchover wave C: outcome sum + block ENUMs for in-process tags"
status: closed
priority: 2
issue-type: task
created-at: "2026-07-04T22:18:57.009104+02:00"
---

docs/census-switchover.md sections 2+5 wave C. PROC-STATUS>OUTCOME (process.f:78 kind+code) becomes SUMTYPE outcome: exited<n> | signaled<n> | timeout; retire PROC-OUTCOME>RC sentinel folding (:86-90) where callers can take the sum. Block ENUMs for in-process tag clusters: MAP-EMPTY/DELETED/OCCUPIED (map.f:15-17), FDEF-N/PTR/NOM/VOID (ffi.f:13-16), JSON-PARSE-OK/THROW (json.f:104-105). NO persisted-value clusters (T-*/VR-*/SC-*/TK-*/TL-* stay — wave E decision). DEPENDS: items 9, 14.

## SLICE 5 — LANDED (json parse status -> result<root,code>, NOT a block ENUM)

ENCODING AUDIT: JSON-PARSE-OK/THROW (tools/json.f:104-105; the census's
"tools/json.f" — lib/json-read.f has NO status tags, it is already honest pure
E-JR-* throws) were NOT a bare in-process tag pair. The status rode with
payloads and placeholders: JSON-PARSE-TRY returned `root JSON-PARSE-OK 0` on
success and `-1 JSON-PARSE-THROW code` on a caught throw — a success-value vs
failure-reason verdict with a dead placeholder slot in each arm. SHAPE CHOICE:
the map-loc payload-sum precedent, and specifically the SHARED result family
(ok = root node id, err = caught throw code) — a parse-try is literally
result<root,code>; a 2-tag ENUM would have kept both placeholder slots alive.
No new family, no WID pressure.

Conversions (one commit):
- `JSON-PARSE-TRY ( ptr u8 i64 -- result<i64,i64> )` (i64 params — the checker
  int-family type this tool spells; RESULT:OK root / RESULT:ERR code at the
  catch boundary). `JSONL-PARSE-TRY` passthrough.
- `JSONL-PARSE-ROW ( -- i64 i64 i64 bool )` UNCHANGED sig (the JSONL-ROW-* row
  cluster is a separate census item): body MATCHes the result — ok arm
  `( root ) JSONL-ROW-JSON 0 JSONL-TRUE`, err arm `( code ) -1 JSONL-ROW-ERROR
  rot JSONL-TRUE`. The JSON-TMP/JSON-TMP2/JSONL-ROOT store-reload plumbing in
  that word DELETED (it existed only to juggle the 3-slot return).
- BOTH constants DELETED (JSON-PARSE-OK, JSON-PARSE-THROW) — zero uses remain
  tree-wide; the `=` sentinel comparison at :916 replaced by the exhaustive
  MATCH (checker-enforced, no bad-tag runtime guard needed; JSONL-NEXT-OBJECT's
  bad-row JSON-TYPE-ERROR guard for the SEPARATE row cluster stays).
- json-test.f: PARSE-CODE (code extractor) + TEST-PARSE-TRY rewritten to MATCH
  both arms (ok: nested object parses + kind asserted; err: E-JSON-SYNTAX).
- require lib/adt/result.f added; tools/json.f is a TR-GATE-COMMON-FILES member
  but result.f is already in the closure union via TR-GATE-HARNESS-FILES (wave-B
  slice 1) — no run-files.f edit. tools/ manifest-exempt. lib/json-read.f:418's
  boundary-blocked STR>NUMBER? call NOT touched (untouched file).

CENSUS CORRECTION recorded: the "json status → block ENUM" wave-C line is
superseded — the honest shape was the payload sum (result), and lib/json-read.f
never had tags to convert.
