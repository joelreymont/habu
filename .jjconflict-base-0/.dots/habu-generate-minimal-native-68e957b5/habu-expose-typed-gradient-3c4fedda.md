---
title: Expose typed gradient view
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T23:35:18.810254+02:00"
---

maki/pos-embed-test.f imports all of maki/from-scratch-train.f solely for SC-SLOT and SC-GRAD-AT. With identical other dependencies that require adds exactly 67 definitions, 9,188 JIT bytes and 1,784 DATA bytes and executes an unrelated SCRATCH-MLP MODEL capture before TOK-POS replaces the IR. The borrowed helper is also unsound as a general API: SC-GRAD-AT takes raw n, converts through SC-SLOT/MIR-SLOT-ID, then unconditionally treats BW-SLOT-GRAD@ as a node reference; an ADD input gradient may instead remain an input reference, so the positional trainer hardcodes TP-SEED and is coupled to current graph layout. Add one package-owned executor/backward gradient-view API accepting MIR:input-slot directly, resolving every legal input-ref/node-ref gradient representation internally and returning a checked stable tensor span. Reject missing, stale and foreign slots with named results; do not expose representation tags or raw slot conversions. Migrate positional, from-scratch and planned host-batch consumers, remove the heavyweight test dependency and hardcoded seed shortcut. Prove input-ref, node-ref, accumulated/multi-use, inserted-node, missing-gradient, rebuild/stale, every slot and two-graph isolation cases; exact gradients remain unchanged. Pin the dependency baseline and require the positional test to lose 9,188 JIT bytes, 1,784 DATA bytes and 67 definitions unless measured retained API code is justified. Files: Maki backward/executor API, trainers and tests. MODEL capture reclamation remains habu-reclaim-model-capture-84547d76.
