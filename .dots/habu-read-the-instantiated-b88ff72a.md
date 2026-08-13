---
title: Read the instantiated width of a parametric family
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T06:55:29.411661+02:00"
---

A parametric family instantiated with a multi-cell argument (option<pt>) occupies more cells than its declaration reserves; the checker records the difference as WF-XPAD and the engine's emitter reads it at pass 2; the chain cannot ask, so MATCH over such a value refuses E-NELAB-MATCH by name (fail-closed, real fixture in native-match.f, match lane 2026-08-10). Export the fact the way EN.E travelled (a narrow checker export reading a stored fact), thread it through NFAM, and lift the refusal with an executing fixture over an instantiated family. Files: src/core/checker.f (export), src/compiler/native/{family,elaborate}.f. Depends: none.

RE-SCOPED 2026-08-13 (match-payload probe, dot d90177af closed as
duplicate of this leaf). CORRECTION to the prescription above: the
scrutinee's instantiated width is NOT stored today, and it must NOT
be stored as a WF/certificate row - pass 2's replay (habu2.f LP2CWAT,
EMIT-WIDTH-LOOKUP) is a strict in-order cursor, and a width row at
the MATCH family token would sit before the arms' xpad rows, never be
consumed, and drift the next arm query. The design: a per-CHECK
checker store of the MATCH scrutinee's instantiated width (the
checker already computes it at MATCH-SCRUT?, checker.f:9485, node
P>TYPE T-WIDTH) with a NARROW export keyed the way EFFECT-CATCH-CELLS
is keyed (token ordinal - the tape span offset IS the certificate key
by construction: checker.f:11174-11181 TSTART -> DO-TOK1 -> SCAN-
REPORT -> feed.f:211); the chain reads the width at the family token
and the EXISTING xpad rows at each of-arm (TFAM-MATCH-XPAD-RECORD,
type-family.f:2678; LOWER-CERT:CELL@ public and live after
MEASURE-HELD - measured off=41 pos=0 w=1 flags=4 for a wide none
arm). Refusing site: elaborate.f:5039 BUNDLE-CK lower-edge (base =
VN - declared width; base-1 still glued when inst > decl). The five
census rows, all this one class: lib/codesign.f CODESIGN-RUN,
lib/ptx/toolchain.f PROBE-VER-RAW, lib/object-resolve.f LOAD,
lib/object.f NEXT-LINE, lib/object.f LOAD. Growth: 44 MATCH
option/result sites across 20 files switch to multi-cell payloads
wave by wave. Rejected for the record: vector run-boundary mask
(3-5 lanes for a 5-row prize, still needs the certificate);
arm-pads-only inside elaborate.f (lifts nothing alone - a patch).
Wide-ctor path measured safe: wide none ctor refuses E-NELAB-JOIN,
no silent miscompile. Probes /tmp/mp-probe/.
