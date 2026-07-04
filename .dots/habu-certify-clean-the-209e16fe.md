---
title: Certify-clean the TFAM prefix files (type-schema/type-family/sumtype)
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:43:31.924652+02:00"
---

Discovered by the re-land worker (2026-07-04) once checker.f fully self-certified: the stage2/stdin certify now advances into the TFAM registry files that build-fixpoint.f puts in the certified prefix — src/core/type-schema.f, src/core/type-family.f, src/core/sumtype.f — which have a PRE-EXISTING (not revert-caused) certify gap: ~92 signatures across ~208 defs use informal role words as type tokens ('( tag a b c -- node )', '( id -- ptr )', kind, policy, ...). Never exposed before because certify died on checker.f's first word. First blocker: schema-new: unknown type 'tag'. Fix: re-type all three files with real checker types (n for ids/tags/kinds where they are plain cells, ptr a/ptr u8 correctly; NO trust rows; same thin-shim rule as the re-land dot if a pointer refinement is genuinely needed), oracle-driven via the certify probe until stage2-src certifies rc 0; then land the item-5 regression (stage2 certify RC 0 in build-fixpoint-test) and unblock habu-make-fixpoint-certify-a11dbad5 (flip certify to blocking). Note trivial bare-ptr fixes ('( -- ptr )' -> '( -- ptr a )') were staged and reverted by the re-land worker — redo them here. SEQUENCE: after TFAM item 7 merges (item 7 owns type-family.f + checker.f next); owner then takes type-family.f lane.
