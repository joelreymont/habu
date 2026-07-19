---
title: Type Maki store rows
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:53:31.707726+02:00"
---

Current master type defect: maki/store.f:89-100 defines record class and profitability verdict as raw integer domains; CLS-FILE$ at 121-129, STORE-APPEND at 222-225, and STORE-READ-CLASS at 245-251 accept arbitrary n, so class/verdict/candidate/count swaps certify until runtime. Evidence publication projects typed evidence into EVID-PUT-G at 402-416, an eight-argument raw API with four interchangeable verdict integers plus independent gdev? and precision. It represents host-golden with meaningless precision and device-golden without type-bound licensed precision; swaps can persist a plausible false row. PROFIT-PUT at 425-434 similarly takes three spans plus raw verdict. Define ENUM record-class and profitability-verdict; STRUCTURE schemas for schedule, measurement, evidence, profitability, and calibration; payload ENUM golden-leg = host(verdict) | device(verdict,precision); and payload ENUM store-record only at the generic transport boundary. Keep typed rows intact until exhaustive wire encoding, and decode a complete row transactionally before publication. Preserve exact wire bytes, latest-wins behavior, and error identity. Prove compile-negative evidence verdict/precision/span/class swaps and impossible golden-leg payloads; exhaustive encode/decode/mutation for every row/variant; malformed rows publish no callback/state; typed evidence-to-store end-to-end; existing byte goldens. Measure JIT/DATA/CODELEN, row storage, and parse/render throughput. Serialize before habu-factor-maki-store-24dc8f8b consumes typed callbacks; habu-structure-store-query-63edd08e retains option result ownership and promotion dots retain write authority.

Fix the canonical source shape, not only the store call. maki/evidence/schema.f:63-89 stores independent golden-leg and prec-class fields, while maki/cad.f:1065-1090 fabricates prec-f32 for host/external legs. Replace them with payload ENUM golden-provenance = host | external | device(prec-class); STRUCTURE golden contains artifact, provenance, achieved policy, and proof. Store encoding MATCHes provenance exhaustively. Add checker negatives for every illegal leg/precision combination and exhaustive host/external/device round trips. Legacy declaration-spelling cutover remains with the existing unified-type migration owner.
