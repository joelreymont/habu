---
title: Reject a build whose program caught a rejection
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T14:41:12.643750+01:00"
---

Problem: src/core/rejection.f STOP prints a rejection, counts it in TALLY and throws, and nothing in a build reads TALLY, so a build's word that catches the throw goes on: a file holding `: W ( -- ) [: s" /tmp/x/fail.f" required ;] catch drop ; W : MAIN ( -- ) 1 . ;` prints include.f BUILD-REFUSE's error[E-SOURCE-ROOT] and hb-build links it, rc 0 (installed hb 38cc474b). On the undo stack (habu-keep-a-build-09b103ba) the same build ends in LINKER:E-ADDRESS on a host address left in ROWS. Acceptance: a build whose program caught a rejection exits 67 with that rejection's record and nothing else; test/file-objects.f holds the case. Depends: habu-keep-a-build-09b103ba, habu-reject-a-caught-0d928ba1. Claim: unassigned.
