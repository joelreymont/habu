---
title: Boot the product inside install
status: active
priority: 2
issue-type: task
created-at: "2026-08-14T16:01:13.603457+02:00"
---

Claim: alder, .jj-ws/alder-install-boot from the reviewed line 775d4456.
The check runs before promotion, from a fresh copied src/lib tree, and exercises
the native compiler plus provided-module resolution. Use only private engines
and source trees for proof; no install --force. Hazel owns the serial full gate.

Found by the gate-scan lane's M4 mutant: install --force built an engine whose EVERY boot dies rc 82, printed 'bin/hb ready', and exited 0 - stage engines carry LAOTNREC=0 so nothing in the build exercises the seed pass. The build's last act should boot its product once (a trivial batch program through the installed binary) and fail the install on a nonzero exit - the cheapest end-to-end honesty check the fixpoint lacks. Files: tools/build-fixpoint.f. Depends: none.
