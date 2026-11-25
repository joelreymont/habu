---
title: Reject an unlinked image in OWN-PATH
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T08:33:31.677979+01:00"
---

Problem: on Linux, once the running executable is unlinked (an install replacing it, a deleted scratch copy), readlink of /proc/self/exe answers 'PATH (deleted)', and ENGINE-ID:OWN-PATH (src/lib/engine-id.f, which PROCESS re-exports) returns that string, which names no file: a spawn or re-exec of it fails far from the cause (own-path review probe, 2026-11-17). Acceptance: OWN-PATH answers a pathname that names the running image or throws E-ENGINE-PATH; a test runs a copied hb, unlinks it, and checks. Depends: none. Claim: unassigned.
