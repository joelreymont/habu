---
title: "Give the type schema's bare throws a record where a program reaches them"
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T21:13:31.545452+01:00"
---

Problem: src/core/type-schema.f throws E-SCHEMA-BAD (7103, a plain constant with no ERROR row) with no rejection record at KEEP-CK :151, SCHEMA-NEW :200, SCHEMA-PARAM :207, SCHEMA-APP :213, SCHEMA-ROW :276, SCHEMA-QUOT :303 and SCHEMA-PTR :353, so a build reaching one prints only 'hb-build: FILE threw 7103'. Acceptance: each site a program reaches through a public word with its own input (a definer's EFFECT words, a declaration) opens a Rust-style record naming the place and the fix; a site reached only by an engine defect is named as such in the hunt's report. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-schema.
