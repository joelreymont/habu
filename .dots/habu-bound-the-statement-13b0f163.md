---
title: "Bound the statement-throw record's re-read"
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T21:18:22.270351+03:00"
---

tools/check-all-errors-core.f CA-THROW-ORIGIN (:437, via BYTE-ORIGIN :71) reads the stopped file's re-read copy up to the recorded byte without checking it against CA-SRC-U, as CA-DUP-RECORD$ (:276) does. A file swapped for a shorter one between the scan and the report crashes --all-errors with SIGSEGV (rc 134) on a statement throw. Bound it at both re-read sites, CA-COMPOSE-STOPPED (:546) and CHK-STOPPED-SOURCE (tools/check-core.f:1081), and fall back to the unlocated record.
