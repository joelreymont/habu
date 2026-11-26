---
title: "Tell a compact enum's variants from its derive features"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T10:02:52.448241+01:00"
---

Problem: src/core/enum-decl.f ED-FIRST-VARIANT reads every token after DERIVE that DERIVE-FEATURE? accepts as a feature, so a compact ENUM whose first variant is named eq, hash, tag, name or order (`ENUM e DERIVE eq tag low ;ENUM`) silently derives it and loses the variant. Acceptance: such a declaration either keeps the variant or is rejected with a message naming the variant; enum-decl-suite covers it. Depends: none. Claim: unassigned.
