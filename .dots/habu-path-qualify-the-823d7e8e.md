---
title: Path-qualify the audited hook allowlist
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:50:34.841001+02:00"
---

Full context: UB-HOOK-ALLOWED? in tools/checked-boundary-lint-core.f:290 allowlists set-check hook installs by NAME ONLY (HOOK, USER-HOOK, CHECK-HOOK, CHK-CHECK-HOOK, LINT-CHECK-HOOK, ES-VERDICT-HOOK, PROP-CHECK-HOOK), so any file can define a word with a listed name and pass. This predates the snap-lib packaging (which only made one entry more generic: SNAP-CHECK-HOOK became CHECK-HOOK). The structural (path, name) authority already exists in tools/hook-sites.f. Drive the allowlist from that single table instead of a parallel name list: the lint knows the file it is scanning, so require (current-file, installed-name) to match a hook-sites row. Falsify by mutation: a hostile fixture defining CHECK-HOOK in a wrong file and installing it must become a finding; the seven legitimate sites stay green.
