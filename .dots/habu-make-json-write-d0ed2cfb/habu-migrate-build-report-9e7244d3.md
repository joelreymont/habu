---
title: Migrate build-report JSON writer
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T16:07:53.479234+02:00"
blocks:
  - habu-build-explicit-json-399f5929
---

Why: HB-BUILD:REPORT$, PATH-ERROR$, and JSON quoting use the deleted singleton and return its borrowed buffer. Exact interface: REPORT and PATH-ERROR consume and return a caller-supplied JSON-WRITE:writer; PATH-ERROR-TEXT uses its own caller-supplied writer for quoted-string escaping; the hb-build command owns fixed writer state plus bounded scratch/output and MATCHes COPY:result before writing. Delete the three JSON raw-span return paths; preserve the existing canonical schemas and human text byte-for-byte. Acceptance: success, cache-path failure, adversarial escaping, and too-small-output tests use two interleaved writers; refusal writes no partial output and reports exact len; hb-build report and command suites pass. Smallest check: bin/hb --load tools/hb-build-test.f and bin/hb --load lib/build-cache-test.f. Depends: Build explicit JSON writer core. Ownership: tools/hb-build-report.f, tools/hb-build-lib.f, tools/hb-build-test.f, lib/build-cache-test.f, FILEMAP.md. Claim: unassigned.
