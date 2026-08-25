---
title: Key operation attributes to schema keys
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T23:34:39.814655+02:00"
---

Full context: prerequisite leaf for habu-verify-frozen-compiler-224d78ad, decided 2026-07-30. IR-SCHEMA:ADD-ATTR records required attribute KEY symbols (src/compiler/ir/schema.f:872) and ATTR-EXT? says whether unlisted keys are admitted, but IR-OP:ADD-ATTR (src/compiler/ir/op.f:776) stores a bare attribute VALUE ordinal with no key, so design line 543 (match an operation's attributes against the schema's required keys, reject unknown keys unless the extension flag is set) is undecidable for any non-record attribute. Change IR-OP:ADD-ATTR to take the key symbol beside the value (IR-ID:ir-symbol-id IR-ID:ir-attr-id -- ) and store a key ordinal beside each attribute ordinal in the operation attribute window; update the window tiling, IR-BUILD's append surface (src/compiler/ir/build.f:817), and the operation-lane tests. The rejected alternative - treating op attributes as record attributes and taking the union of their pair keys - is a patch: it leaves a same-type semantic-role gap where an op can carry an unkeyed attribute no schema declared. Owner: agent irverify implements this as its own commit ahead of the verifier in .jj-ws/habu-verify-frozen-compiler-224d78ad.
