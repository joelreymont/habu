---
title: Make TRUST refuse a name that resolves to nothing
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-10T22:06:16.598606+02:00\""
---

TRUST accepts a name with no definition and silently mints a global checker symbol for it: s" NO-SUCH-WORD-XYZ" s" n -- n" TRUST exits 0 (pkgasm lane 2026-08-11 - this is what turned insn-schema.f's stale encoder rows into E-USING-SHADOW-GLOBAL two layers away instead of an error at the row). Fail closed: TRUST refuses a name the dictionary cannot resolve, by name, at the row. Acceptance: the probe exits nonzero naming the word; every existing TRUST row still resolves (the full gate is the sweep); a stale-row fixture reds at the row not downstream. Files: src/core/checker.f (TRUST's reader). Depends: none. Ordered WITH habu-turn-the-registry-4c064064 (converting rows while TRUST is fail-open hides conversion mistakes).

Claim: agent=primsweep workspace=.jj-ws/habu-prim-sweep

FILE LIST CORRECTED 2026-08-11 (primsweep lane, measured). This is NOT a
checker.f-only change. TRUST is one spelling serving two questions, and the
second caller is baked into the engine.

WHAT WAS MEASURED. Instrumenting TRUST to report every call and whether the
engine can resolve the name:
  TRUSTCALL WQ-X          ABSENT     <- publish tail of `: WQ-X ( -- n ) 5 ;`
  TRUSTCALL WQ-X          RESOLVED   <- the bare row s" WQ-X" s" -- n" TRUST
  TRUSTCALL DQ-X          RESOLVED   <- defer DQ-X ( -- n )
  TRUSTCALL TQ-X          ABSENT     <- publish tail of `TRUSTED: TQ-X ...`
  TRUSTCALL ABSENT-XYZ-Q  ABSENT     <- the stale row this dot targets
The bare row resolves; `defer` resolves. What does not is the engine's own
publish tail, which calls `trust` to re-record a definition's declared signature
at a moment when the record is not yet findable (it becomes findable one
statement later). A whole-boot census counts 1222 such registrations, 1113
distinct names - so a check inside the shared TRUST refuses the boot prefix at
its first definition.

RULING (2026-08-11): two words, not one word guessing its caller - the same cure
as the CHECKER-DEFINED? split. The engine's publish tail calls a second,
definer-facing registration word; TRUST becomes the bare-row word alone and
carries the fail-closed check. Rejected: publishing the record before the trust
tail (a semantic change to publish order with unmeasured blast radius), and any
mode flag, name comparison, or caller-guess inside one word.

FILES: src/core/checker.f (TRUST plus the new registration word),
src/habu/habu2.f and bootstrap/cg/forth.fs (the engine's publish tail retargeted
from `trust`; C-FIND-TRUST, C-CALL-TRUST-LASTC, C-CALL-TRUST-PEND). Two-stage
landing per docs/bootstrap.md, authorized.

SCOPE OF THE CHECK. Bare names only. The engine exposes exactly one dictionary
primitive, `search-wl`, which answers per-wid on the raw spelling; measured, it
finds a CLOSED package's publics in no wid 0..63 (A64ASM:ENC-LDUR, and a test
package's own public), and a package name is not itself a dictionary word. 28 of
the tree's 429 bare-row names are qualified PKG:TAIL. That is acceptable because
only a bare top-level name mints the GLOBAL checker symbol behind
E-USING-SHADOW-GLOBAL - a qualified row mints a package-public symbol and can
never collide with a used public's bare tail. The engine capability for the
qualified leg is dot habu-a-qualified-name-3913fe54.

ORDER: Class A of habu-turn-the-registry-4c064064 lands FIRST. That dot's pass
adds PRIM: rows, not TRUST rows, so fail-open TRUST hides nothing from it - a
wrong row stops the owning file from loading, naming the word at the caller.
