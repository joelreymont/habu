---
title: "Libraries: migrate string split"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-24T13:02:19.340240+02:00\\\"\""
closed-at: "2026-07-24T17:43:03.127242+02:00"
close-reason: Reviewed implementation landed and verified at master@origin 4fb6f52fb815.
---

Why: lib/string.f still declares STR:split with legacy PRODUCT although unified STRUCTURE already supplies the same generated record surface. Owner: lib/string.f and its focused string/split tests only. Replace PRODUCT split 0 ... ;PRODUCT with STRUCTURE split 0 ... ;STRUCTURE inside package STR, preserving field names ptr/len/next, their ptr u8 and CAD-NUM nominal schemas, declaration order, STR-SPLIT:MAKE/UNMAKE spelling, physical width, OPTION:SOME construction, SPLIT-NEXT behavior, and zero-allocation hot path. Update comments. Forbidden: compatibility aliases, legacy parser edits, raw casts, option/result changes, cursor semantics changes, unrelated string work. Acceptance: run the real split production entry through none, one-field, repeated, empty, delimiter-edge, and sentinel paths before/after; constructor and destructor effects remain exact; token-aware executable census finds no legacy declaration in lib/string.f; focused typed-local/package/trust gates pass. Dependency proof: master 227b5b349702 runs structure and existing string owning paths green.

Claim: agent=codex-string workspace=.jj-ws/habu-libs-migrate-str-145849bf
