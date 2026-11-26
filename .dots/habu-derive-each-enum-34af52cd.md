---
title: "Derive each enum value's declared name"
status: active
priority: 2
issue-type: task
created-at: "2026-11-26T09:45:20.653154+01:00"
---

Problem: Maki logs each kernel request by tag only (Maki src/kernel.f:179-195, a 97-kind ENUM); a tag's meaning shifts when a kind is inserted. tim asked (/tmp/tim-habu-enum-name.md) for the name from the declaration itself. Joel 2026-11-26 named it: DERIVE name adds <ENUM>:NAME$ ( enum -- str ). Acceptance: an ENUM with DERIVE name answers each value's declared spelling through NAME$; enum-decl-suite covers it; forth.md and the card name it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-enum-name.
