---
title: Widen if-conversion past single selects
status: active
priority: 2
issue-type: task
created-at: "2026-08-07T13:36:13.440402+02:00"
---

Claim: agent=ifconv workspace=.jj-ws/habu-widen-if-conversion-d7ec28d0

Audit: ~72 gap bytes — FROUND is branchy (44: clang is fully branchless via fcsel, and the chain materialises its 0.5 on both arms separately), LADDER's 7-arm ladder emits compare-branch per arm where clang cascades csel (28, overlapping the join-block class). The selector's if-conversion (csel/fcsel with the NaN table) exists but its recognized shape is too narrow. Widen: an arm whose body is a pure value computation converts even when it carries its own constant materialisation (hoist the constant to the preheader of the select — the literal memo's dominance rule applies); a ladder of exclusive guards cascades. WS?'s range-fold (sub;cmp;ccmp;cset — clang folds 9/10 into one unsigned test) is the ccmp cousin: the ccmp Insn row exists in the review's own list — verify modeled before emitting. All answers bit-for-bit incl. NaN rows; the fused-compare NaN table is the precedent.
