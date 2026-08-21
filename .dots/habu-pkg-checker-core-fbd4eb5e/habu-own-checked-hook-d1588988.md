---
title: Own checked hook sites
status: closed
priority: 1
issue-type: task
created-at: "2026-07-23T02:09:58.322065+02:00"
closed-at: "2026-07-23T03:18:15.317882+02:00"
close-reason: Landed immutable checker-hook identity registry at 2db115be; verified in master@origin.
---

Why: trusted inventory classifies set-check installs through TRUSTED.md rows while checked-boundary lint keeps a different name-only allowlist, so file identity is not authoritative, set-top-check installs are not inventoried, and CGR-HOOK is missing from one policy. One immutable registry must own hook identity and audit classification before checked-boundary lint consumes it.

Owner and files: package HOOK-SITES; new tools/hook-sites.f, tools/trusted-inventory.f, tools/trusted-inventory-test.f, TRUSTED.md, and FILEMAP.md. Store exactly eleven immutable rows: the nine live set-check installs reported by the production trusted inventory plus src/core/top-row.f:TR-HOOK and test/top-row-hook-test.f:TRH-LOG as top-check rows. Each row owns canonical repository path, exact installed token, check or top kind, audit class, and owner cap:checker-hook-identity. Publish only COUNT ( -- n ), PATH$ ( n -- ptr u8 n ), NAME$ ( n -- ptr u8 n ), CLASS$ ( n -- ptr u8 n ), OWNER$ ( n -- ptr u8 n ), CHECK? ( n -- bool ), TOP? ( n -- bool ), CHECK-MATCH? ( ptr u8 n ptr u8 n -- bool ), and TOP-MATCH? ( ptr u8 n ptr u8 n -- bool ). Indexed reads reject out-of-range indices with the repository table-bounds error; match arguments are canonical path then installed token. No public mutation or raw storage.

Trusted inventory must require HOOK-SITES, recognize both tick forms for set-check and set-top-check, classify each install from the exact path/name/kind registry row, and prove both directions: every scanned install matches one row and every row matches exactly one scanned install. Registry classification replaces only the TRUSTED.md machine rows that currently cover hook installs; retain rows covering distinct TRUSTED definitions or 0 set-check sites, reduce dual-purpose counts from two to one, and remove the prose name list. Generated hook text inside string literals remains excluded and pinned by its existing production shape test.

Acceptance: baseline and strict inventory stay green; the report includes eleven HOOK-INSTALL sites with their existing classes and owner; missing, stale, duplicate, wrong-path, wrong-name, wrong-kind, qualified-spoof, comment, string, reordered-tick, dot-relative, absolute, escaped-workspace, and count-drift fixtures fail through the production inventory. The new module is package-owned, checked, load-side-effect-free, and listed once in FILEMAP.md.

Forbidden: a second hook list in TRUSTED.md or a consumer, suffix/path normalization inside authorization, caller-supplied allowlists, mutable registration, file-level authorization, compatibility globals, or duplicated scanner logic.

Pre-change proof: trusted inventory reports nine set-check rows and zero top-check rows, while moving an allowed installed name to a different file does not invalidate checked-boundary lint.

Verify: bin/hb --load tools/trusted-inventory-test.f; bin/hb --load tools/trusted-inventory.f -- strict; bin/hb --load tools/trusted-inventory.f -- baseline TRUSTED.md; exact-diff typed-local and package lints; hostile registry mutations; host-lint; filemap-lint.

Claim: agent=hook_sites workspace=.jj-ws/habu-own-checked-hook-d1588988.
