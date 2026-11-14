---
title: "Re-enable hb's name stripping"
status: active
priority: 2
issue-type: task
created-at: "2026-11-07T07:59:56.932704+02:00"
blocks:
  - habu-move-engine-internal-fc2ba551
---

Problem: Joel 2026-10-08: a word is restricted by putting it in a package's private section, and the product build strips private names. Joel 2026-11-07: 'disable name stripping if it's slowing you down and make a dot to re-enable it later'. hb-build --engine links hb through src/habu/link-data.f LINK-ENGINE (:493), whose RECORD-IMAGE (:338) writes every record with its name (PRIM-REC, :226, BUILD-REC, :297, NAME-POOL :322), private ones included, and hb.map marks every row named (src/habu/link-map.f:221 NAMED). So checked source finds any engine private by name through public words: on the installed hb, `s" MACHO" BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE` finds the package and `s" MH-MAGIC64"` in its XREF-PKG-PRIVATE finds the private record (XREF-FOUND? both true, rc 0). A qualified private name is already E-UNDEFINED (`XREF:N>REC`, rc 70) and reopening an engine package is refused (`package XREF`: E-SEALED-PACKAGE, rc 84). hb itself reads private records by name: src/habu/linker.f HB-OWNED (:5577), through OWNED-TEXTS (:5623) in a build that states no kernel object (STATES-PRIMS? :5619: a kernel build or a Wasm link), for the owned primitive rows (src/habu/prims.f EPPRIM:, 53 rows). With no record CHECK:OWNED-FACTS gives no text (`XREF-NULL s" SOURCE-EXEC" s" source-exec-live?" CHECK:OWNED-FACTS nip .` prints 0 on the installed hb), so OWNED+ (src/habu/linker.f:5595) throws E-COMP-STATE and a stripped hb cannot build an engine. The owned rows are machinery being replaced: the strip lands as the last step of habu-move-engine-internal-fc2ba551, which removes them. docs/architecture.md:95-97 says hb keeps every package's private headers; the strip changes that sentence.
Acceptance: the hb that hb-build --engine writes keeps no name on a record in a package's private wordlist and keeps every public name: the WL-PROBE probe above finds the package but no private record; the linker and other source-loaded tools still run on it; hb.map keeps every name and marks stripped rows unnamed; an hb so built builds an engine, cold then warm, byte-equal.
Depends: habu-move-engine-internal-fc2ba551 (the owned rows gone, postpone found without a private name).
Also needed: the checker finds a record's symbol by its name (src/core/checker.f CHECK REC-SYM answers 0 when the name is empty; REC-HOME-SYM), so on a stripped hb no checked call to an hb-private record can be judged; a definer baked into hb that postpones a private word of its package would fail. The symbol lookup must not go through the name.
Claim: agent=worker workspace=.jj-ws.noindex/jerry-strip (the strip and its measurement, unlanded until fc2ba551 lands).
