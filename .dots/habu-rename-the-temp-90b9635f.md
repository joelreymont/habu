---
title: "Rename the temp directory words to FS:MKTEMP-DIR"
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T19:12:44.126341+02:00"
---

Joel 2026-10-28 (interview): the global TMPDIR-MKDIR ( prefix -- path ) becomes FS:MKTEMP-DIR and MAKE-TEMP-DIR ( base prefix -- path ) becomes FS:MKTEMP-DIR-IN, after mktemp -d; hard cutover across every caller (162 and 11 files) and docs/gate.md. Brief .jj-ws.noindex/carl-tmp/.out/brief-mktemp-dir.md. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-tmp.
