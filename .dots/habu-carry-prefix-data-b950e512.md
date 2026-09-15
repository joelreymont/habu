---
title: Carry prefix data words into stripped applications
status: open
priority: 2
issue-type: task
created-at: "2026-09-15T16:08:07.407255+03:00"
---

Problem: a stripped application whose MAIN reaches a prefix data word fails to build: tools/hb-build.f with ': MAIN ( -- ) 0 SCRIPT-ARGV$ type cr ;' dies 'aot: address refers to data outside the restored span' (src/habu/aot-closure.f DATA-ADDRESS!, exit 74) because only the user DATA span is restored and SCRIPT-ARGV$ reaches env-base.f's created cells in the prefix heap. willow (switcher) works around it with --repl, which costs about 1.35 s of CPU per start. Acceptance: the closure carries the prefix data cells that retained code addresses (seeded like fresh fields, provenance kept) or the argv words read through a runtime cell the entry fills; a stripped MAIN that prints its arguments builds and runs; a regression in the stripped-* family. Files: src/habu/aot-closure.f, src/habu/aot-lib.f, src/os/script-argv.f, src/os/env-base.f, test/stripped-*.f. Verify: bin/hb --load tools/hb-build.f -- subject.f -o app with the MAIN above, then ./app one two. Depends: none. Ownership: AOT closure. Claim: unassigned.
