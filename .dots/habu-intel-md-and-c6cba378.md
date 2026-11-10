---
title: INTEL.md and stack-abi.f still describe a Gforth recovery
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T15:58:46.521747+02:00"
---

Problem: INTEL.md's recovery passage (If an engine is too old to build current master, recover through Gforth; tools/bootstrap.sh; and the line x86 recovery is a cross-build ... The Gforth chain emits ARM64 only) names a chain and a script the tree no longer has; src/habu/stack-abi.f line 1 says shared by source-loaded and recovered emitters. docs/bootstrap.md Recovery is the route: a trusted native hb of the same OS and machine. src/habu/habu2.f keeps the sentence The Gforth stage0 mirror reads the body text for the same reason: habu2.f is machinery being replaced, is not patched, and the sentence goes with the file. Found by the worker of habu-comments-name-a-0f8f9c1c. Source: Joel: "i want all issues identified to be dotted and fixed". Acceptance: INTEL.md follows docs/bootstrap.md Recovery; stack-abi.f's header names the emitters that share it. Files: INTEL.md, src/habu/stack-abi.f. Verify: rg -n -i 'gforth|bootstrap.sh|recovered emitters' INTEL.md src/habu/stack-abi.f prints nothing. Depends: none. Ownership: those two files. Claim: unassigned.
