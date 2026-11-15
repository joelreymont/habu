---
title: Model the install protocol in spec/Install.tla
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T00:16:17.460955+01:00"
---

Problem: installers, readers, the sweep and an installer that dies at any step race over ~/.local/lib/habu-trees and the link (tools/install.f), and no spec checks them; the assumption that an exec begun before one install's rename is visible to the next install's process scan is stated only in install.f's header. Acceptance: spec/Install.tla and .cfg hold the invariants (the link names only complete trees; a reader reads one tree nobody writes after it is named; no tree a visible process runs is removed; a leftover goes at a later install) and the exec assumption, and TLC passes them, run once Joel approves. Depends: none. Claim: unassigned.
