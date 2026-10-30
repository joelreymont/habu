---
title: Wipe credential buffers when a BUF grows or is disposed
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T23:48:49.014194+02:00"
---

Finding (MCP-L3 fold-in): byte-buffer.f GROW and DISPOSE free the old storage without zeroing it, so a BUF holding a credential (MCP TOKEN-BUF and REQUEST-BUF's Authorization line, BEARER client secret and tokens) leaves copies in freed memory even when the owner wipes at CLOSE. curl.f wipes its own copies (TEXT-WIPE). Decide where the rule belongs (a wiping BUF mode or a credential store) and apply it to MCP and BEARER.

BEARER's FORM EMPTIED (H4b fold-in) clears forms within TOKEN-CAP only: a form larger than TOKEN-CAP can still grow the BUF after the client secret is written, leaving an unwiped copy. The root fix is wiping on GROW and DISPOSE, here.
