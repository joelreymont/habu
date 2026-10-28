---
title: Refuse reopening an image package under the seal
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T21:27:44.283332+02:00"
---

Problem: after RESTRICT sealed source may write `package NAME` for a pre-seal package (src/lib/vocabulary.f:25-26); the reopened package's private words are searched before `using` packages, so a bare call binds an image-private word: a sound design is refused with a misleading checker error and gains names it was never admitted (tim, Maki design-pkg mymowowr probes probe-a/probe-b). Ruling (carl): under the seal `package NAME` for a package that existed at the seal is refused, `hb: policy: package <name> predates the seal`, ENGINE-ERROR:POLICY 107, in every engine and both tiers; a package the sealed source makes itself still opens and reopens. Rejected: filtering lookups in reopened packages. Acceptance: test/policy cases for admitted and non-admitted pre-seal reopen refused, own-package reopen works; vocabulary.f header updated; Maki's test/designs/policy-reopen.f expectation moves (tim). Files: src/habu/habu2.f C-PACKAGE-SEAL-GUARD family, kernel-x64.f, interpreter package path if it applies, src/lib/vocabulary.f, test/policy/. Verify: policy rows, engine suite, full suite, x86-64 on pulsar if kernel-x64 changes. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-reopen.
