---
title: Delete the checker's unreached capture chain
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T10:15:56.217074+02:00"
---

Problem: an image carries no record links (occurrences are issued at every start, src/habu/habu1.f:5628-5637, and never saved), and CHECKER-CAPTURE-PREPARE (src/core/checker.f:30044), which alone reaches CHECKER-PERSIST-CORE (:29984), RECORD-LINK-PRUNE, RECORD-LINK-CAPTURE, RECORD-CAPTURE-BODY, RECORD-CAPTURE-CLEAN and RECORD-SNAPSHOT-COPY, has no caller in src or tools (only test/storage-binding-e2e.f:59; the owner slot is only range-checked at src/habu/aot-capture.f:2106). PAYLOAD-MODE 2 (aot-capture.f:2192) is never stored (aot-arm.f:42). RECORD-CONTRACT (checker.f:27404) is read only by test/linker.f:367-368. With mode 2 gone, ACAP-PERSISTENT-OWNER (aot-capture.f) is unreachable, and UNIT-SCRATCH-RELEASE-CHECKER, UNIT-FAM-EXPORT-RESET, REG-POINTERS-CLEAR and CHECKER-STORAGE-UNBIND-ALL have no caller. Acceptance: the chain, RECORD-CONTRACT, its owner-record slots, the PAYLOAD-MODE 2 branch and these uncalled words are gone; the tests that reached them check the live behaviour they guard another way. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-xfer, .jj-ws.noindex/carl-xrec and .jj-ws.noindex/carl-xpay.
