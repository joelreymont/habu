---
title: Name-check the inline row key
status: active
priority: 2
issue-type: task
created-at: "2026-08-03T20:53:12.428518+02:00"
---

Destruction review of NINL, low. CALLEE-COPY? (elaborate.f:1516-1523) keys the splice on the caller-stated address and cross-checks only arity — an address+arity coincidence splices the wrong routine's body. The row already stores the callee's spelling (R-SPELL): add a name cross-check so a numeric coincidence becomes a structural refusal. Related latent hole: rows store spellings and INL-SYM re-resolves them in the CALLER's word model; a caller declaring the same spelling as fixed data at a different address would splice its own address silently. Guarded today only by CALLEES-NONE-CK (migrate.f:562-567), which dot habu-resolve-a-data-a1c8067f proposes to remove — that dot must not land before this check exists.

Claim: agent=splice-tables workspace=.jj-ws/habu-one-splice-meaning-03d36743

Correction to the finding as written: a row did NOT already store the callee's
spelling. R-SPELL is the per-token spelling array of the recorded BODY, one
entry per token; nothing in a row said which routine the row was about. The fix
therefore adds the name: NINL:NAME-MAX (one ceiling now shared by
NMIGRATE:NAME-CAP and NELAB's read-back buffer), R-NAME/R-NLEN, and
NINL:NAMED? — asked by NELAB:CALLEE-COPY? before the arity checks and refused
with E-NELAB-INLINE, the same code and the same class of event as the arity
disagreement.

The name enters through the CLAIM, not the commit. The claim-before-publish
seam that landed with the row-ceiling work owns every refusal the record can
make, and it owns them because a refusal on the far side of NPUB:REPUBLISH
would leave a word running new code while its migration reported failure. The
published name is known before the publication — it is the same NAME-BUF the
republication is handed — so it belongs there with the address:
NINL:CLAIM ( ptr u8 n n -- ) validates it (empty → E-NINL-STATE, over NAME-MAX
→ E-NINL-CAP) and stashes it in S-NAME/S-NLEN beside S-ENTRY/S-ROW, and
NINL:COMMIT ( -- ) writes R-NAME/R-NLEN from the claim and decides nothing.
NMIGRATE:CLAIM-ROW passes the name; KEEP-BODY stays argument-less. The name is
validated BEFORE the full-table decline, so a malformed claim is refused whether
or not there was room — otherwise a caller's protocol bug would appear and
disappear with the table's fill level. CAP-CASES pins that ordering with the
table full. The comparison is case-insensitive because a dictionary name is
the same name in either case; a byte comparison would refuse legal Habu that
writes `foo` for a routine published as `FOO`, and test/compiler/native-inline.f
NINL-CASE-NAME pins that.

STILL OPEN, not fixed here — the spelling side of the same key. A row's body
spellings are bytes, and NELAB:INL-SYM re-interns them in the CALLER's word
model (elaborate.f), so a caller that models the same spelling as something
else — a `create`d data word at a different address, meaning `fixed` — would
splice its own address in place of the callee's operation. The name check above
does nothing about this: it holds the ROW's identity, not the identity of each
token inside it. What holds it today is NMIGRATE:CALLEES-NONE-CK, which refuses
a migration that stages both a data word and a callee list, so the two models
cannot both be populated in one definition. Dot habu-resolve-a-data-a1c8067f
must not remove that guard until a real defence exists — most likely recording
each body token's own model answer beside its spelling, so a re-resolution that
lands on a different meaning is refused rather than staged.
