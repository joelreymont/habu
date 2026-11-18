---
title: "Link references to hb's records outside facts"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T00:11:15.663236+01:00"
---

Problem: src/habu/linker.f ID-REF+ writes (HB-FILE, index, 0) for any record below FIRST-REC, but only a facts row's reader (FACT-REF-CK) accepts that form; ID-REF-CK marks any other such reference BAD, so the object is rewritten on every warm build. Three writers may pass one: SAME-START (:1631) looks for an alias's source only from FIRST-REC, so an alias whose code starts at one of hb's words answers -1; LATE+ (:1812) and SEAL+ (:2510) take a record not shown to be the build's. Whether a program reaches any of them is not known. Acceptance: a program file that makes an alias of one of hb's words, or whose late wordlist or seal names one of hb's records, builds and runs the same warm and cold with no rewrite, or is rejected at that word with a placed rejection; a test shows each case a program reaches. Depends: none. Claim: unassigned.
