---
title: "Single-pass checking: kill body re-parse"
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:31:41.054634+02:00"
---

Every definition is parsed twice: JIT compiles tokens while LBCAP/EMIT-BCAP (src/habu/habu1.f:1798-1817) copies body source into BODYBUF (appended per token, habu2.f:2626); at ; the publish hook hands the raw text to the checker which re-tokenizes and re-FINDs everything (EM-COMPILE-PUBLISH-HOOKED habu2.f:2539-2551). Fix: feed the checker an interned token/xt stream recorded during the single compile pass (tok -> resolved xt + effect ptr already known at compile time), so the checker unifies over pre-resolved refs instead of re-parsing text. Big constant-factor win on every checked load; keeps fail-closed property (capture overflow still fatal, habu1.f:1795-1810).

---

## STOP — requires engine change (out of checker territory), 2026-07-03

Investigated for the type-habu wave. This dot cannot be done in `src/core/checker.f`
alone: the checker entry is `CHECK ( a u -- )` (re-tokenizes) driven by the publish
hook `HOOK ( ptr u8 n -- n )` in `src/core/check-hook.f`, which receives the raw
BODYBUF *text*. Going single-pass means the COMPILER must record a resolved
token/xt(+effect-ptr) stream during its one compile pass and hand THAT to a new
checker entry that unifies over pre-resolved refs. That is engine work:

- `src/habu/habu1.f` `EMIT-BCAP`/`LBCAP` (~1823-1840) append body source text into
  `BODYBUF` per token; replace/augment with a resolved token+xt record stream.
- `src/habu/habu2.f` publish sites (~1588/1617/1751/1778) seed `"NAME "` and `BL`
  into `LBCAP`, then flush BODYBUF to the hook; must instead flush the token/xt
  stream and pass its base/len to the new checker entry.
- New checker entry point (checker.f, my territory) `CHECK-STREAM ( base n -- verdict )`
  that consumes `{tok, xt, effect-ptr}` records instead of `IS-TOK`/`DO-TOK1`
  re-tokenizing + `SYM-FIND`/`HIDX` re-resolving. Only this half is in-territory;
  it is blocked on the engine emitting the stream.

Keep fail-closed: capture overflow stays fatal (habu1.f ~1836 `16 5 CMP,`), and
the new entry must reject `E-UNCHECKED` on any token whose xt has no charted
effect exactly as `DO-TOK1` does. No measurement recorded — the interface does
not exist yet. Owner of the emit change: the habu2.f/build worker.
