---
title: Read check subjects through one shebang reader
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T11:07:24.713169+02:00"
---

Problem (review 275 of shebang 584d9107): the run comments a leading #! line (src/core/include.f SOURCE-ROOT:SHEBANG-COMMENT; tools/check-core.f CHK-BUILD-RUN), but check.f's static stages re-read the subject from disk and tokenize line 1: tools/check-core.f discovery dep read (~:900), lints (~:1661), preverify (~:1896); tools/check-all-errors-core.f ~:510/:527/:384; tools/source-discovery.f ~:359 (its own lexer); tools/diag-origin-core.f ~:323 (READ-FILE). Six read sites, three lexers. Measured ($HOME/.cache/tmp/kestrel-r4-rev275/out/static-rc.log, probe/c-*.f): line 1 `#!/usr/bin/env hb :` loads rc 0 but checks rc 70 E-UNDEFINED; `#!/usr/bin/env hb : P3 ( -- n ) 1 ;` + real P3 checks rc 78; `: 42 ;` rc 1 E-NUMERIC-DEFINITION; `: I ( -- ) ;` rc 1 E-RESERVED-DEFINITION; `s"` rc 70 discovery unterminated string. Acceptance: those five subjects check as they load (rc 0) as a file, under --json-errors, from stdin and in a --source-list, cases through tools/check-test-lib.f seen to fail first; one shared reader word beside SOURCE-ROOT:SHEBANG-COMMENT (read the bytes, then comment a byte-0 #!) replaces every static-stage read, so no stage carries its own #! test, and CHK-BUILD-RUN's separate rewrite goes if the marked text already carries it; a #! not at byte 0 stays an ordinary token everywhere; docs/forth.md's check.f claim holds for the static stages. Also remove src/habu/habu2.f C-SOURCE-SKIP-SHEBANG (~:1397, called ~:1771) if dead (review 275: LSHBANG rewrites first, so it never sees #!). Base: after shebang 584d9107 and lintcap 650af471 (diag-origin now takes bytes) land.
