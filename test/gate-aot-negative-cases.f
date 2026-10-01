\ gate-aot-negative-cases.f - the AOT closure rejection checks, run on the keyed
\ linker image by test/gate-aot-negative.f (see test/preloaded-engine.f).

require lib/source.f
require tools/json.f
require tools/gate-json-assert-core.f
require test/gate-common.f
require src/habu/app-image.f     \ ... and the assembler under it (src/habu/aot-lib.f)
require src/habu/aot-closure.f
require src/habu/aot-lib.f       \ MEMBER-ORDER, for the disjointness case below

\ The image restores at tier 0 and its require of src/habu/app-image.f is a
\ no-op, so this sets the tier that file sets on a source load: every fixture
\ below compiles in a fork of this process, under the optimizing tier the
\ linker's own callers use.
1 set-tier

LOWER-CERT-HOOK:INSTALL

\ Unit + registry coverage for the direct-branch closure capability: the decoder
\ recognizes B/BL and excludes conditional/compare branches, and the direct-branch
\ resolver (FINDADDR-PTR) resolves a direct-BL target to its record ONLY by exact
\ code entry - a registered engine helper's entry and an ordinary word's entry both
\ resolve (both carry a record), while a non-entry offset and an unregistered
\ (no-record) address resolve to nothing. Those negatives are the security boundary:
\ an unregistered direct-branch target is not followed and still fails closed.
package AOT-NEGATIVE

: BRANCH-SOURCE ( -- )
   GE-SRC-RESET
   s" package AOT-LINK" GE-SRC-LINE
   s" create ANT-CODE 4 allot" GE-SRC-LINE
   s" variable ANT-FX" GE-SRC-LINE
   s\" : ANT-EXPECT ( bool ptr u8 n -- ) {: ok:bool label:ptr labelu:n :} ok 0= if label labelu 74 die then ;" GE-SRC-LINE
   s" : ANT-TARGET= ( n ptr u8 ptr u8 n -- ) {: instr:n want:ptr label:ptr labelu:n :}" GE-SRC+
   s"  ANT-CODE instr TARGET want = label labelu ANT-EXPECT ;" GE-SRC-LINE
   s" : ANT-HELPER-REC ( -- ptr n ) 0 ANT-FX !" GE-SRC+
   s"  begin ANT-FX @ ndict@ < while ANT-FX @ REC dup REC-WID@ OWNER-API-PRI-WID =" GE-SRC+
   s"  if exit then drop ANT-FX @ 1+ ANT-FX ! repeat XREF-NULL ;" GE-SRC-LINE
   s\" : ANT-RUN ( -- ) $14000002 DIRECT? s\" AOT direct B decode\" ANT-EXPECT" GE-SRC+
   s\"  $94000003 DIRECT? s\" AOT direct BL decode\" ANT-EXPECT" GE-SRC+
   s\"  $54000000 DIRECT? 0= s\" AOT conditional branch exclusion\" ANT-EXPECT" GE-SRC+
   s\"  $34000000 DIRECT? 0= s\" AOT compare branch exclusion\" ANT-EXPECT" GE-SRC+
   s\"  $14000002 ANT-CODE 8 + s\" AOT forward B target\" ANT-TARGET=" GE-SRC+
   s\"  $94000003 ANT-CODE 12 + s\" AOT forward BL target\" ANT-TARGET=" GE-SRC+
   s\"  $17FFFFFF ANT-CODE 4 - s\" AOT backward B target\" ANT-TARGET=" GE-SRC+
   s\"  $97FFFFFE ANT-CODE 8 - s\" AOT backward BL target\" ANT-TARGET=" GE-SRC+
   s\"  ANT-HELPER-REC XREF-FOUND? s\" AOT registered helper present\" ANT-EXPECT" GE-SRC+
   s\"  ANT-HELPER-REC dup REC-CODE-PTR@ FINDADDR-PTR = s\" AOT registered helper resolved by direct target\" ANT-EXPECT" GE-SRC+
   s\"  ANT-HELPER-REC REC-CODE-PTR@ 4 + FINDADDR-PTR XREF-FOUND? 0= s\" AOT non-entry address excluded\" ANT-EXPECT" GE-SRC+
   s\"  ANT-CODE FINDADDR-PTR XREF-FOUND? 0= s\" AOT unregistered address excluded\" ANT-EXPECT" GE-SRC+
   s\"  0 REC dup REC-CODE-PTR@ FINDADDR-PTR = s\" AOT ordinary word resolved by direct target\" ANT-EXPECT ;" GE-SRC-LINE
   s" ANT-RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: BRANCH-RUN ( -- )
   BRANCH-SOURCE
   GE-EVAL-FORK-CAPTURE
   s" AOT private direct-branch fixture" GE-EXPECT-OK ;

\ THE BANDS CELL-MAPPED? EXCLUDES, asked of two addresses this process really
\ holds. The brk area is the one that matters here: arm64 randomizes it over a
\ gigabyte above the executable's end, so an ORDINARY 32-BIT-SHAPED INTEGER in a
\ persistent cell lands inside it every few hundred builds and was refused as a
\ pointer (measured: the hb-build fixture, tools/hb-build-test.f and the
\ stripped rows beside it, an undeclared cell holding 0x34B12C35, refused in
\ one build and linked in the next).
\ No Habu word allocates from the break - lib/memory.f maps - so nothing in the
\ band is ever a pointer and the linker excludes it. An mmap address is the
\ control: it is the class the refusal exists for, and it must still answer yes.
\ Both addresses are taken inside the fork, where the linker's own copy of
\ src/habu/proc-maps.f reads that process's map; src/habu/aot-closure.f
\ CELL-MAPPED? is private, which is why the fixture reopens its package.
\ This runs on the keyed linker image, so the fork's first question is also a
\ restored image's first question: a map the image carried from the process
\ that saved it would answer here instead (measured: a loaded flag carried in
\ DATA over the released rows threw E-BOUNDS, 7122; src/habu/proc-maps.f,
\ A CAPTURE DROPS THE MAP WHOLE).
\ THE ALLOCATION COMES FIRST because the map is a snapshot taken at the first
\ question and an area mapped after it is invisible (measured: with the order
\ reversed the mmap control answers no). That is the linker's order too - the
\ application takes its buffers as it loads, and the walk asks afterwards.
: MAPPED-BAND-SOURCE ( -- )
   GE-SRC-RESET
   s" package AOT-LINK" GE-SRC-LINE
   s\" : ANH-EXPECT ( bool ptr u8 n -- ) {: ok:bool label:ptr labelu:n :} ok 0= if label labelu 74 die then ;" GE-SRC-LINE
   s" TRUSTED: ANH-ADDR ( ptr u8 -- n ) ;" GE-SRC-LINE
   s" : ANH-RUN ( -- ) MEM-ALLOC-64K drop ANH-ADDR {: p:n :}" GE-SRC+
   HB-TARGET-LINUX? if
   s"  PROC-MAPS:HEAP-START {: h:n :}" GE-SRC+
   s\"  h PROC-MAPS:MAPPED? s\" AOT heap start is mapped\" ANH-EXPECT" GE-SRC+
   s\"  h PROC-MAPS:HEAP? s\" AOT heap start is in the brk band\" ANH-EXPECT" GE-SRC+
   s\"  h CELL-MAPPED? 0= s\" AOT heap band excluded from build mappings\" ANH-EXPECT" GE-SRC+
   then
   s\"  p PROC-MAPS:HEAP? 0= s\" AOT mmap address is outside the brk band\" ANH-EXPECT" GE-SRC+
   s\"  p CELL-MAPPED? s\" AOT mmap address is a build mapping\" ANH-EXPECT ;" GE-SRC-LINE
   s" ANH-RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: MAPPED-BAND-RUN ( -- )
   MAPPED-BAND-SOURCE
   GE-EVAL-FORK-CAPTURE
   s" AOT mapped-band fixture" GE-EXPECT-OK ;

34 constant DQ

create REPORT-PATH FS-PATH-CAP allot
variable REPORT-U

: REPORT$ ( -- ptr u8 n )
   REPORT-PATH REPORT-U @ ;

: REPORT! ( -- )
   s" hb-clo-limit.err" REPORT-PATH GT-PATH REPORT-U ! ;

: WRITE-ERR ( -- )
   REPORT$ GT-ERR$ WRITE-ALL ;

: J-DQ ( -- )
   DQ SB-APPEND-C ;

: J-COLON ( -- )
   s" :" SB-APPEND ;

: JKEY ( ptr u8 n -- )
   J-DQ
   SB-APPEND
   J-DQ ;

: EXPECT-RAW ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n raw:ptr rawu:n label:ptr labelu:n :}
   SB-RESET
   key keyu JKEY
   J-COLON
   raw rawu SB-APPEND
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: EXPECT-STR ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: key:ptr keyu:n val:ptr valu:n label:ptr labelu:n :}
   SB-RESET
   key keyu JKEY
   J-COLON
   J-DQ
   val valu SB-APPEND
   J-DQ
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: CLOSURE-NZ ( ptr u8 n -- ) {: label:ptr labelu:n :}
   REPORT!
   74 s" E-AOT-CLOSURE-LIMIT" label labelu GE-EVAL-FORK-BAD
   WRITE-ERR ;

: ERR-SCHEMA ( ptr u8 n -- )
   2drop
   REPORT$ GJA-FIRST-JSON GJA-SCHEMA1 ;

: CLO-LINE ( n -- ) {: n:n :}
   s" : W" GE-SRC+
   n GE-SRC-U+
   s"  ( n -- n ) W" GE-SRC+
   n 1+ GE-SRC-U+
   s"  dup 0< if negate then ;" GE-SRC-LINE ;

: SOURCE-CLOSURE-LIMIT ( -- )
   GE-SRC-RESET
   s" -1 JSON-DIAGS !" GE-SRC-LINE
   s" : W8 ( n -- n ) dup 0< if negate then ;" GE-SRC-LINE
   7 begin dup -1 > while
      dup CLO-LINE
      1-
   repeat drop
   s" : MAIN ( -- ) 1 W0 drop ;" GE-SRC-LINE
   s" package AOT-LINK" GE-SRC-LINE
   s" 8 CLO-LIMIT!" GE-SRC-LINE
   s" CLOSURE" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: CLOSURE-LIMIT ( -- )
   SOURCE-CLOSURE-LIMIT
   s" hb-build closure limit" CLOSURE-NZ
   s" code" s" E-AOT-CLOSURE-LIMIT" s" hb-build closure limit code" EXPECT-STR
   s" schema_version" s" 1" s" hb-build closure limit schema version" EXPECT-RAW
   s" reachable_count" s" 8" s" hb-build closure limit reachable count" EXPECT-RAW
   s" max_closure" s" 8" s" hb-build closure limit max closure" EXPECT-RAW
   s" root_word" s" MAIN" s" hb-build closure limit root word" EXPECT-STR
   s" hb-build closure limit JSON schema" ERR-SCHEMA ;

\ THE OTHER SIDE OF THE SAME KNOB. The tables are sized from the program being
\ linked, so the capacity is the walk's fail-closed invariant and CLO-LIMIT! may
\ only lower it: a limit above the capacity is refused where the two first meet
\ (aot-closure.f CLO-LIMIT-RESOLVE, at the sizing for a request made while the
\ source loaded) rather than silently clamped. The refusal names the capacity and
\ the three counts it is made of, never a constant - there is no constant left.
: SOURCE-CLOSURE-CAPACITY ( -- )
   GE-SRC-RESET
   s" : MAIN ( -- ) 1 drop ;" GE-SRC-LINE
   s" package AOT-LINK" GE-SRC-LINE
   s" 999999999 CLO-LIMIT!" GE-SRC-LINE
   s" CLOSURE" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: CLOSURE-CAPACITY ( -- )
   SOURCE-CLOSURE-CAPACITY
   74 s" aot: CLO-LIMIT above the closure capacity"
   s" hb-build closure capacity" GE-EVAL-FORK-BAD
   s" aot: CLO-LIMIT 999999999 above the closure capacity "
   s" hb-build closure capacity names the request" GE-EXPECT-ERR-HAS
   s"  = records " s" hb-build closure capacity names its parts" GE-EXPECT-ERR-HAS ;

\ THE DISJOINTNESS INVARIANT, REFUSED BY NAME. aot-lib.f MEMBER-ORDER sorts the
\ closure members by entry once per link and both member lookups binary-search
\ that order, which answers for an address only because no two members share a
\ byte. A walk cannot produce an overlap - the one nesting shape, two bodies of
\ one record, is dropped at its source (aot-closure.f DROP-NESTED-CLO) - so the
\ only way to reach the refusal is to fill the rows by hand, which is what this
\ case does: two members of the same 16 bytes, the second starting four bytes
\ into the first and ending with it. The refusal spells both members, and their
\ record is XREF-NULL here, which prints as <unknown> the way a stripped span's
\ member does.
: SOURCE-MEMBER-OVERLAP ( -- )
   GE-SRC-RESET
   s" package AOT-LINK" GE-SRC-LINE
   s" create ANT-MEM 16 allot" GE-SRC-LINE
   s" : ANT-MEMBER! ( n ptr u8 n -- ) {: i:n code:ptr len:n :}" GE-SRC+
   s"  code i CLO ! len i CLO-LEN ! XREF-NULL i CLO-REC ! ;" GE-SRC-LINE
   s" : ANT-OVERLAP ( -- ) 2 CLO-TABLES" GE-SRC+
   s"  0 ANT-MEM 12 ANT-MEMBER!" GE-SRC+
   s"  1 ANT-MEM 4 + 8 ANT-MEMBER!" GE-SRC+
   s"  2 NCLO ! MEMBER-ORDER ;" GE-SRC-LINE
   s" ANT-OVERLAP" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: MEMBER-OVERLAP ( -- )
   SOURCE-MEMBER-OVERLAP
   74 s" aot: closure members overlap"
   s" hb-build closure member overlap" GE-EVAL-FORK-BAD
   s" aot: closure members overlap site=<unknown>"
   s" hb-build closure member overlap names the site" GE-EXPECT-ERR-HAS
   s"  bytes=12 and=<unknown>"
   s" hb-build closure member overlap names both members" GE-EXPECT-ERR-HAS ;

\ Fail-closed abs-chain reject (red-first). The AOT linker contract is
\ DIRECT-BL-ONLY: no native emitter produces the absolute movz/movk/movk x16 +
\ blr x16 call form, so the copier and relocator (aot-lib.f COPY-COMPACT-BLOB /
\ RELOCATE) die with E-AOT-ABS-CHAIN if one is ever encountered. This hand-builds
\ one full chain as a synthetic member and drives the copier over it. Red-first:
\ before the retirement the copier silently collapsed/copied the chain and the
\ build exited 0; now it rejects with the named error (exit 74). hb-build's own
\ propagation of a maker die, the maker's rc with its diagnostic on stderr, is
\ tools/hb-build-stripped-test.f HBT-STRIPPED-NO-ENTRY's assertion.
: SOURCE-ABS-CHAIN ( -- )
   GE-SRC-RESET
   s" -1 JSON-DIAGS !" GE-SRC-LINE
   s" package AOT-LINK" GE-SRC-LINE
   s" create ABT-CHAIN 16 allot" GE-SRC-LINE
   s" : ABT-W! ( n ptr u8 -- ) {: w:n a:ptr :} w a c! w 8 rshift a 1+ c! w 16 rshift a 2 + c! w 24 rshift a 3 + c! ;" GE-SRC-LINE
   s" : ABT-BUILD ( -- ) $D2800010 ABT-CHAIN ABT-W! $F2A00010 ABT-CHAIN 4 + ABT-W! $F2C00010 ABT-CHAIN 8 + ABT-W! $D63F0200 ABT-CHAIN 12 + ABT-W! ;" GE-SRC-LINE
   s" : ABT-RUN ( -- ) 1 CLO-TABLES 1 PLAN-TABLES ABT-BUILD" GE-SRC+
   s"  ABT-CHAIN 0 CLO ! 16 0 CLO-LEN ! XREF-NULL 0 CLO-REC !" GE-SRC+
   s"  0 0 NEWOFF ! 1 NCLO ! 0 COPY-COMPACT-BLOB ;" GE-SRC-LINE
   s" ABT-RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: ABS-CHAIN ( -- )
   SOURCE-ABS-CHAIN
   74 s" E-AOT-ABS-CHAIN"
   s" hb-build AOT abs-chain reject" GE-EVAL-FORK-BAD ;

\ An ADR may not reach out of the member its site is in: the only ADR a compiled
\ body carries is a quotation's address, whose target is a later function of the
\ same emission and so of the same member (src/habu/aot-lib.f ADR-TARGET!). This
\ hand-builds the violation - `ADR x0, .+8` in member 0, aimed at member 1's
\ first byte - and drives the relocator over it. Both synthetic members carry
\ XREF-NULL, the record AEREC-TXT spells `<unknown>`, so the site prints that
\ name; the target is a DATA address above every record, which is `<unknown>`
\ too (aot-closure.f CODE-ABOVE?).
: SOURCE-ADR-MEMBER ( -- )
   GE-SRC-RESET
   s" package AOT-LINK" GE-SRC-LINE
   s" create AMT-CODE 16 allot" GE-SRC-LINE
   s" : AMT-RUN ( -- ) 2 CLO-TABLES 2 PLAN-TABLES" GE-SRC+
   s"  AMT-CODE 0 CLO ! 8 0 CLO-LEN ! XREF-NULL 0 CLO-REC !" GE-SRC+
   s"  AMT-CODE 8 + 1 CLO ! 8 1 CLO-LEN ! XREF-NULL 1 CLO-REC !" GE-SRC+
   s"  ASM-LEN 0 NEWOFF ! ASM-LEN 8 + 1 NEWOFF ! 2 NCLO !" GE-SRC+
   s"  0 AMT-CODE $10000040 RELOC-W32 drop ;" GE-SRC-LINE
   s" AMT-RUN" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: ADR-MEMBER ( -- )
   SOURCE-ADR-MEMBER
   74 s" aot: ADR target outside its member site=<unknown> target="
   s" hb-build AOT cross-member ADR reject" GE-EVAL-FORK-BAD
   s" target-word=<unknown>"
   s" hb-build AOT cross-member ADR reject names the target word" GE-EXPECT-ERR-HAS ;

\ Kept rejection: patch32 writes the code region, which a stripped binary has
\ no way to do (its __text is r-x and its code is at the PIE image base, not the
\ RBASE-VA region patch32 targets). The persistent data region does NOT make this
\ safe, so the closure walk must still reject it with E-AOT-UNSUPPORTED (exit 70).
: UNSAFE-NZ ( ptr u8 n -- ) {: label:ptr labelu:n :}
   REPORT!
   70 s" E-AOT-UNSUPPORTED" label labelu GE-EVAL-FORK-BAD
   WRITE-ERR ;

: SOURCE-PATCH32 ( -- )
   GE-SRC-RESET
   s" -1 JSON-DIAGS !" GE-SRC-LINE
   s" TRUSTED: MAIN ( -- ) 0 0 patch32 ;" GE-SRC-LINE
   s" package AOT-LINK" GE-SRC-LINE
   s" CLOSURE" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: PATCH32 ( -- )
   SOURCE-PATCH32
   s" hb-build AOT patch32 reject" UNSAFE-NZ
   s" code" s" E-AOT-UNSUPPORTED" s" hb-build AOT patch32 code" EXPECT-STR
   s" token" s" patch32" s" hb-build AOT patch32 token" EXPECT-STR
   s" word" s" MAIN" s" hb-build AOT patch32 word" EXPECT-STR
   s" hb-build AOT patch32 JSON schema" ERR-SCHEMA ;

public

: RUN ( -- )
   s" hb-gate-aot-negative" GT-START
   BRANCH-RUN
   MAPPED-BAND-RUN
   CLOSURE-LIMIT
   CLOSURE-CAPACITY
   MEMBER-OVERLAP
   ABS-CHAIN
   ADR-MEMBER
   PATCH32
   GT-CLEANUP
   s" PASS: native hb-build AOT negative tests" type cr ;

;package

' AOT-NEGATIVE:RUN GE-CHILD-RUN
