---
title: "Clear a refused generates: row's record after it"
status: closed
priority: 3
issue-type: task
created-at: "2026-11-01T02:54:28.542471+02:00"
closed-at: "2026-11-01T05:34:30.667329+02:00"
close-reason: "Landed as voqvtnro 'Drop a refused row's record; list errors once': GENERATES-REFUSE passes the row's name to its renderer, GENR-TOK-A/U gone; one checker word REPORTED-ERROR? lists the codes the checker renders before throwing (CAST-ERROR:SKIP, check-all-errors-core.f THREW?, check-core.f CHK-PREVERIFY-STOPPED). Opus review ACCEPT. Batch with unsafe: candidate build rc 0, full suite 514 of 514 rc 0 (/tmp/jerry-big/uc-suite.log). The remaining records: habu-hand-every-refused-ed537a7c."
---

Found by the casterr worker: GENERATES-REFUSE (src/core/checker.f) keeps GENR-TOK-A/U after a refused generates: row, so a refusal caught before an image save leaves cells that differ from a build that never ran the row, as CAST-ERROR's record did before CAST-ERROR:DECLARE. tools/check-all-errors-core.f REPORTED-THROW? also lists the ten cast codes CAST-ERROR's private RAISED? lists: one owner should answer which throws a declaration already rendered.

Claim: agent=jerry workspace=.jj-ws.noindex/jerry-clrref. Lead design (brief /tmp/jerry-big/clrref-brief.md): hand the row name to the renderer and delete GENR-TOK-A/U; one global checker word REPORTED-ERROR? ( n -- bool ) holds the 18 codes; CAST-ERROR:SKIP calls it (RAISED? goes); tools/check-all-errors-core.f REPORTED-THROW? goes.

Worker done (voqvtnro b1b3a7f8): GENR-TOK-A/U gone, GENERATES-DIAG-XT ( ptr u8 n n -- ), REPORTED-ERROR? holds the 18 codes with a PRIM row so the seal keeps it callable, RAISED? and REPORTED-THROW? gone; no SKIP case added (none of the seven non-cast codes is reachable from a cast:/linear: declaration, evidence in its report). Rows certify-does-definer, native-gate-diagnostics, load-reject-diag, check-cli-boundary, check-verify, tool-boundary-check-repair rc 0 on the candidate. Opus review running (brief /tmp/jerry-big/clrref-review-brief.md). The same record in PRS-TOK, TSR-TOK and USH-TOK: dot habu-hand-every-refused.
