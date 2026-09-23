---
title: Preserve exceptional stack types across catch and payload restoration
status: active
priority: 1
issue-type: task
created-at: "2026-09-13T21:26:24.684087+03:00"
---

Claim: agent=hazel workspace=.jj-ws/hazel-catch-stale, based on 45608866.
First lane: the conservative rule (window cells a throw path may have
overwritten become `stale<t>` after catch; reads refused by name); the
retained callee evidence is the second lane.

Confirmed 2026-09-13 by payload_resume under the current fresh source-checker owner in .jj-ws/cedar-owner-payload. CHECK! accepts both:
- SWAP-THROW ( n ptr u8 -- n ptr u8 n ) [: swap -99 throw ;] catch
- RETURN-THROW ( n | ptr u8 -- n n | ptr u8 ) [: r> swap >r -99 throw ;] catch
The stack-preserving control [: -99 throw ;] also accepts.

Safe runtime reproduction defines SWAP-THROW with that checked signature, passes 17 and an address of a one-byte CREATE buffer, and compares returned raw values in one TRUSTED test boundary without dereferencing either. The call returns (buffer-address, 17, -99), contradicting its certified (n, ptr u8, n) meaning. The runtime restores stack depths, not overwritten contents. Reproducer/log: build/payload/quote-exception-runtime.f and .log; checker reductions: build/payload/quote-exception-semantics.f and .log. Source-owner command is bin/hb --load test/native-window-owner-child.f -- build/payload/quote-exception-runtime.f src/core/structures.f src/core/bytes.f src/core/dynamic-storage.f src/os/linux/target.f src/os/linux/layout.f src/habu/layout.f src/os/env-base.f src/core/include.f src/core/sha256.f lib/prelude.f. Result: catch accepted n/ptr and returned ptr/17 without dereferencing; window: 0.

Responsible checker layer: THROW-EDGE retains only the first exceptional data/return rows; RSCATCH applies ordinary DIN/RIN but ignores exceptional DOUT/ROUT; stored QX rows are copied to effect EN.G/H as transient IDs and copied back unchanged after source arenas reset. No semantic reader does not prove these fields unnecessary. The partial graph WIP must not zero/discard them as a correctness argument.

Fix the exceptional contract at publication and catch application: preserve every admitted exceptional stack requirement, shared variables and data/return row semantics in persistent effects, freshening/copying and portable graphs; reject unsafe type changes before code is accepted. Preserve ordinary value changes and depth restoration where their retained cells keep valid types. Test safe numeric mutation plus throw, unsafe heterogeneous swaps on data/return stacks, multiple throw branches, nested execute/catch, source-state destruction and imported graphs, under JIT and optimizing tiers. Keep the graph milestone open until these cases prove preservation. Claim: unassigned (stale claim cleared 2026-09-16). Linked review: P01 in Downloads/habu-current-review-51546316/habu-current-review/REVIEW.md.


Design boundary from the reduction:
- A post-throw row alone cannot describe inactive cells restored above the current stack pointer. For example a pointer consumed before the numeric throw code is pushed can be replaced by that integer when catch restores the original depth. A sound rule must retain last-write/type evidence for restored cells or reject the restoration when no such evidence exists; ordinary row equality alone is insufficient.
- Persistent EN.G/H currently contain the old transient IDs. Changing them to persistent offsets also needs an effect-format/owner compatibility discriminator for E-INST-FROM and retained-checker transfer. An older owner's raw integer must never be followed as a new graph offset. The explicit partial artifact graph can version this transition; the full owner transfer also needs a defined boundary.
- The native exception dot 6ceb7667 explicitly requires depth restoration rather than original-value restoration, so changing catch to copy its input contents would silently change existing language behavior.
- Interim partial capture must refuse exception-bearing quotation effects by name rather than erase their rows and claim a successful graph roundtrip. This keeps P01 and this semantic leaf visibly open while the responsible checker contract is repaired.
