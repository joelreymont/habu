---
title: "Refuse a private word that takes a declaration word's name"
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T18:56:52.303577+02:00"
---

kevin's note 4: a private colon word given the name of a Habu declaration word (VALUE-RECORD) is accepted silently, and its callers then fail E-UNSAFE far from the cause. Evidence /tmp/kevin-secretspawn/only-*. Decide where the refusal belongs (the definition, by name against the declaration words) after a reduced reproducer. Owner: carl.


Not dispatchable (audit 2026-11-10): it states no acceptance and its evidence /tmp/kevin-secretspawn is gone. It needs a reduced reproducer and the refusal it should give.
