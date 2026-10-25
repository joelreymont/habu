---
title: Model clobber lint helper words
status: open
priority: 3
issue-type: task
created-at: "2026-10-25T02:12:52.140313+01:00"
---

Problem: the clobber lint models single register-writing mnemonics (nxrrsmxo) but not multi-instruction helper words that write registers: REC-AT, BAND, REAL-WID, CF-FLIP, ENC-MVN EMITW. A clobber through them goes unreported. Acceptance: each helper word's written registers are modelled; a lint test row per helper shows a clobber is reported. Verify: the clobber lint tests.
