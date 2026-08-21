---
title: A package needs a subsystem-visible section between private and public
status: open
priority: 2
issue-type: task
created-at: "2026-08-21T15:17:53.798116+02:00"
---

MEASURED SPECIMEN, in the tree today: src/core/type-family.f:2270-2308 is
'package TYPE-FIELD' with fifteen one-line forwarders - ': ALIGN@ ( n -- n )
PF-ALIGN@ ;' and fourteen like it - each already carrying an API mark.
TYPE-FIELD:ALIGN@ is the genuine interface and TFAM:PF-ALIGN@ is the
implementation, which is exactly the split the governing model wants. The only
reason PF-ALIGN@ cannot be private is that TYPE-FIELD's body cannot see a TFAM
private, and nothing in the language moves it there: reopening TFAM puts the
forwarder in TFAM's wordlist (renaming the public and cascading its callers),
EXPORT republishes under the SAME tail so it cannot rename, and packages do
not nest (habu2.f C-PACKAGE exits 4B). TYPE-FIELD-OWNER, CHECKER-DECL-FRAME,
PREFIX-BOUND and TYPE-NAME are the same shape in the same file. Measured
scope: 195 of the 291 checked-route-reachable registry publics have a
closed-scope consumer inside the boot prefix, spread over thirteen packages in
nine files (TYPE-DECL in sumtype.f 136 sites, TFAM in type-family.f 107,
TYPE-FIELD-OWNER 105, CHECKER-DECL-FRAME 47, ENUM-DECL 37, STRUCTURE-DECL 32,
LOWER-CERT 21, PREFIX-BOUND 17, TYPE-FIELD 15, STRUCTURE-MAKE 14,
GENERATED-DECL-CTOR 8, TYPE-NAME 4, DECL-EVENT 2, AOT-LINK 1). CLOSED AS
UNNECESSARY FOR THE ROUTE-3 LANDING: the retirement pass (dot
habu-route-3-the-64078d43 section 16) closes the checked route by removing
unpublished publics from the dictionary after the last prefix consumer has
compiled, so the prefix siblings keep their access and user code loses it,
with no new visibility axis. This leaf exists because the language may want
the capability on its own evidence some day, and because the specimen should
not have to be rediscovered. ANY revival requires a probe through a real gate
and a named first consumer that the retirement pass cannot serve - the
Simplify Relentlessly rule; consensus that it would be nice is not evidence.
