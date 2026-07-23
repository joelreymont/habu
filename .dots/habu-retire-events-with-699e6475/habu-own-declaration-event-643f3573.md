---
title: Own declaration event rollback phases
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T07:47:11.770644+02:00"
blocks:
  - habu-own-product-field-86660116
---

Problem: checker-scope rollback retires type and field rows but leaves declaration events that reference them. Owner: new sealed package DECL-EVENT-ROLLBACK over declaration-event marks only. Expose RESERVE, SAVE, RESTORE-READY, RESTORE, FINALIZE-READY, and FINALIZE. The strict-LIFO frame captures provisional and published event counts, base field count, field and variant ordinals, current variant, and event transaction depth without rewinding serial tokens. RESERVE grows before mutation; SAVE is infallible. RESTORE-READY validates frame depth, counter ranges, and every retiring FIELD event against the still-live field registry. RESTORE scrubs every retired pointer-free event row and restores all captured marks. FINALIZE-READY validates without mutation; FINALIZE releases one frame. No checker hook installation, public mutation surface, verifier cleanup, raw globals, or TRUSTED. Acceptance: nested frames restore only inner events; wrong depth/range/dangling field rejects before mutation; row-scrub canaries observe zero bytes, not only restored counts; surviving events always resolve live fields. Files: src/core/decl-event.f and one focused package-owned suite. Smallest check: public PRODUCT event rows plus direct package rollback phases; typed-local and package gates.
