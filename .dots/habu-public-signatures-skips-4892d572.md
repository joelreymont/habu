---
title: public-signatures skips number-claimable definition names
status: open
priority: 3
issue-type: task
created-at: "2026-07-12T03:52:11.891745+02:00"
---

FOR THE TOOLS/CORE LANE. Found by the fmt-rename worker: the public-signatures tokenizer silently skips definition names the number parser could claim - that is exactly how lib/fmt.f's .0 escaped its lib/std.manifest row (latent gap now closed by the rename + the added .INT row). Fix the tokenizer to see such names (or reject them - the new E-NUMERIC-DEFINITION reserved-name-lint rule makes new ones impossible, so the tool fix is mostly defensive for historical trees).
