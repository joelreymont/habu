---
title: Compile the recursive call corpus word
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-01T14:44:47.732250+02:00\""
---

The last comparison-corpus row: FACT ( n -- n ) 'dup 1 <= if drop 1 exit then dup 1- RECURSE *'. Needed beyond the landed set: RECURSE (a call to the word being compiled) and exit as the last word of an if arm (landed, restricted exactly this way). A call needs: an a64.call form (BL through the engine's calling convention - the data-stack convention the chain already declares, so the callee's dtake/dpublish do the argument passing and the call itself moves nothing but control and x30), the link register saved and restored around it in a non-leaf routine (the routine contract's link field and frame machinery exist - a non-leaf FACT must reserve a frame, save x30, restore before ret; A64EFF-LINK and A64EFF-CONTROL already model this - read them first), and the recursion target being the routine's own entry (no relocation needed - a self-call's displacement is known at layout time exactly like a block branch; a call to ANOTHER word is a different capability, dot it). The trap-preservation question: RECURSE can overflow the machine stack - check what the engine's interpreted RECURSE does on deep recursion and whether the pinned inputs (10 and 1) stay shallow. Acceptance: FACT compiled, executed identically on the pinned inputs, row in the table - the table complete at 11 of 11, every row measured, gap list empty. Mutations: link register not saved (dies by execution - the return address is clobbered), callee's frame not released, call displacement to the wrong offset - execution or named refusal.

Claim: agent=calllane workspace=.jj-ws/habu-compile-the-recursive-705e797e
