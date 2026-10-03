---
title: "Bound the statement-throw record's re-read"
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T21:18:22.270351+03:00"
---

tools/check-all-errors-core.f CA-THROW-ORIGIN (via BYTE-ORIGIN) reads the stopped file's re-read copy up to the recorded byte without checking it against CA-SRC-U, the same root the duplicate record had until 1ca23983 bounded CA-DUP-RECORD$. A file swapped for a shorter one between the scan and the report crashes --all-errors with SIGSEGV (rc 134) on a statement throw (/private/tmp/claude-501/pdrev6/probe/race/race4.zsh, 7110 at d=0.60). Bound it at both re-read sites, CA-COMPOSE-STOPPED and CHK-STOPPED-SOURCE, and fall back to the unlocated record. Found by the Fable reviews pdup-rev6/rev7.
