\ runtime-regression-test.f - unique engine runtime recovery regressions.

require test/gate-common.f

package RUNTIME-RUNNER

public

: SOURCE ( ptr u8 n -- ) {: src:ptr srcu:n :}
   GE-HB$ src srcu GE-TIMEOUT-MS GE-RUN-STDIN ;

: BUFFER ( -- )
   GE-SRC-BUF GE-SRC-U @ SOURCE ;

: LINE-RC ( ptr u8 n n ptr u8 n -- )
   {: src:ptr srcu:n want:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   src srcu GE-SRC-LINE
   BUFFER
   want label labelu GE-EXPECT-RC ;

: FILE-LOADER ( ptr u8 n -- ) {: path:ptr pathu:n :}
   GE-HB$ path pathu GE-TIMEOUT-MS GE-RUN-STDIN-FILE ;

: PTY ( -- )
   GE-HB$ GE-TIMEOUT-MS GE-RUN-ENV ;

;package

package RUNTIME-REGRESSION

67 constant GE-UNCAUGHT-RC

create GE-SCRIPT-PATH FS-PATH-CAP allot
variable GE-SCRIPT-U

: GE-UNCAUGHT-RUN ( ptr u8 n n ptr u8 n -- )
   {: src:ptr srcu:n want:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   src srcu GE-SRC-LINE
   GE-SRC-BUF GE-SRC-U @ RUNTIME-RUNNER:SOURCE
   want label labelu GE-EXPECT-RC ;



: GE-UNCAUGHT-CASE ( ptr u8 n n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n want:n needle:ptr needleu:n label:ptr labelu:n :}
   src srcu want label labelu GE-UNCAUGHT-RUN
   needle needleu label labelu GE-EXPECT-ERR-HAS ;

: GE-UNCAUGHT-THROW ( -- )
   s" -2816 throw" GE-UNCAUGHT-RC s" uncaught throw code -2816"
      s" uncaught throw -2816 (kernel-masks-to-0)" GE-UNCAUGHT-CASE
   s" -2802 throw" GE-UNCAUGHT-RC s" uncaught throw code -2802"
      s" uncaught throw -2802 (kernel-masks-to-14)" GE-UNCAUGHT-CASE
   s" 70 throw" 70 s" uncaught throw 70 representable passthrough" RUNTIME-RUNNER:LINE-RC
   s" uncaught throw 70 representable passthrough" GE-EXPECT-SILENT
   s" : GEUT ( -- ) [: -2816 throw ;] catch . ;  GEUT" 0
      s" caught throw stays in-process rc 0" RUNTIME-RUNNER:LINE-RC
   SB-RESET s" -2816" SB-APPEND GE-SB-LF
   SB$ s" caught throw control output" GE-EXPECT-OUT
   s" PASS: uncaught top-level throw exits are reported, never masked" type cr ;

\ Interpret-mode transports of a wide layout bundle SILENTLY CORRUPTED: the
\ top-level stack ops move one physical cell, so a TRUSTED-seeded 2-cell
\ bundle followed by `dup . . . .` printed the tag twice and then read below
\ the seed (9 9 7 <garbage>, rc 0) - fail-open through any TRUSTED boundary
\ at the unchecked REPL (dot habu-tfam-12-interpret-10b385b1). The engine
\ fails closed: executing (or ticking) a DNAME-WIDE-flagged word at interpret
\ level dies with a named diagnostic before the bundle can land on the
\ untyped interpret stack. The flag is CHECKER-COMPUTED: the record choke
\ point (E-ADD-EFFECT) scans the four effect rows with T-WIDTH (quotation
\ sub-effects included) and the engine publish tails consume the latch
\ (rec-wide-publish -> wide-mark) after ndict++ — no manual marking anywhere
\ in this fixture. Checked definitions own bundle work; the guard leg proves
\ a compiled call of the SAME marked word still compiles and runs at top
\ level, and the scalar leg proves a one-cell TRUSTED word stays unmarked.
: GE-ILAYOUT-PRELUDE ( -- )
   s" SUMTYPE gewide 2" GE-SRC-LINE
   s"   VARIANT ok a ;VARIANT" GE-SRC-LINE
   s"   VARIANT err b ;VARIANT" GE-SRC-LINE
   s" ;SUMTYPE" GE-SRC-LINE
   s" TRUSTED: GE-WMK ( -- gewide<n,n> ) 7 9 ;" GE-SRC-LINE ;

: GE-ILAYOUT-CASE ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   GE-ILAYOUT-PRELUDE
   src srcu GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 label labelu GE-EXPECT-RC
   s" interpret-mode layout value" label labelu GE-EXPECT-ERR-HAS ;

: GE-ILAYOUT-GUARD ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   GE-ILAYOUT-PRELUDE
   s" TRUSTED: GE-WUN ( gewide<n,n> -- n n ) ;" GE-SRC-LINE
   s" : GE-WRUN ( -- n n ) GE-WMK GE-WUN ;" GE-SRC-LINE
   s" GE-WRUN . ." GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" checked wide transport guard" GE-EXPECT-OK
   SB-RESET s" 9" SB-APPEND GE-SB-LF s" 7" SB-APPEND GE-SB-LF
   SB$ s" checked wide transport guard output" GE-EXPECT-OUT ;

\ negative control: a one-cell TRUSTED word is NOT marked by the checker scan
\ and still interprets at top level (rc 0, value printed).
: GE-ILAYOUT-SCALAR ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   GE-ILAYOUT-PRELUDE
   s" TRUSTED: GE-WN ( -- n ) 42 ;" GE-SRC-LINE
   s" GE-WN ." GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" scalar trusted word interprets" GE-EXPECT-OK
   SB-RESET s" 42" SB-APPEND GE-SB-LF
   SB$ s" scalar trusted word output" GE-EXPECT-OUT ;

\ does>-split wide facts fail closed at the pass-2 trigger with a fixed
\ label (previously a lone current-token write - unattributable; TFAM 12
\ item 3 verdict: the checker cannot see across the does> split, so the
\ labeled engine exit IS the permanent contract).
: GE-DOES-WIDE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   GE-ILAYOUT-PRELUDE
   s" : GE-WDOES ( gewide<n,n> -- gewide<n,n> ) dup drop create does> ( -- n ) drop 5 ;" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   75 s" does>-split wide facts fail closed" GE-EXPECT-RC
   s" does>-split cannot lower layout width facts" s" does>-split wide diagnostic" GE-EXPECT-ERR-HAS ;

\ A created word's clause effect is an effect like any other: `does>
\ ( -- gewide<n,n> )` publishes a wider-than-cell layout value, so the created
\ record must carry the same DNAME-WIDE mark a `:` word with that effect carries
\ and a bare call must fail closed. It did not: the does> publish tail skipped
\ the wide tail, so `64 SPAN-BUFFER: PBUF` then a bare `PBUF` landed the span's
\ two cells on the untyped interpret stack, rc 0 (dot habu-mark-a-wide-851cab15).
\ No library type is used here - the definer is local to the case, so the guard
\ is pinned by the engine's own publication, not by lib/span.f.
: GE-DOES-WIDE-BARE ( -- )
   s" : GE-WPAIR: ( -- ) create 0 , 0 , does> ( -- gewide<n,n> ) drop GE-WMK ; GE-WPAIR: GE-WP  GE-WP drop ."
      s" interp layout does> word fails closed" GE-ILAYOUT-CASE ;

\ the same marked created word still compiles and runs inside a checked body,
\ which is where bundle work belongs: two cells out, printed top first.
: GE-DOES-WIDE-COMPILED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   GE-ILAYOUT-PRELUDE
   s" TRUSTED: GE-WUN ( gewide<n,n> -- n n ) ;" GE-SRC-LINE
   s" : GE-WPAIR: ( -- ) create 0 , 0 , does> ( -- gewide<n,n> ) drop GE-WMK ;" GE-SRC-LINE
   s" GE-WPAIR: GE-WP" GE-SRC-LINE
   s" : GE-WPRUN ( -- n n ) GE-WP GE-WUN ;" GE-SRC-LINE
   s" GE-WPRUN . ." GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" checked does> wide transport guard" GE-EXPECT-OK
   SB-RESET s" 9" SB-APPEND GE-SB-LF s" 7" SB-APPEND GE-SB-LF
   SB$ s" checked does> wide transport output" GE-EXPECT-OUT ;

\ negative control: a does>-created word whose clause effect is one cell wide is
\ NOT marked and still interprets bare (rc 0, value printed).
: GE-DOES-SCALAR ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : GE-NPAIR: ( -- ) create 7 , does> ( -- n ) @ ;" GE-SRC-LINE
   s" GE-NPAIR: GE-NP" GE-SRC-LINE
   s" GE-NP ." GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" scalar does> word interprets" GE-EXPECT-OK
   SB-RESET s" 7" SB-APPEND GE-SB-LF
   SB$ s" scalar does> word output" GE-EXPECT-OUT ;

: GE-INTERP-LAYOUT ( -- )
   s" GE-WMK dup . . . ." s" interp layout dup fails closed" GE-ILAYOUT-CASE
   s" GE-WMK drop ." s" interp layout drop fails closed" GE-ILAYOUT-CASE
   s" 5 GE-WMK swap . . ." s" interp layout swap fails closed" GE-ILAYOUT-CASE
   s" ' GE-WMK execute" s" interp layout tick fails closed" GE-ILAYOUT-CASE
   s" : GE-WMK2 ( -- gewide<n,n> ) GE-WMK ; GE-WMK2 drop ." s" interp layout checked producer fails closed" GE-ILAYOUT-CASE
   s" defer GE-WD ( -- gewide<n,n> ) GE-WD" s" interp layout defer fails closed" GE-ILAYOUT-CASE
   GE-DOES-WIDE
   GE-DOES-WIDE-BARE
   GE-DOES-WIDE-COMPILED
   GE-DOES-SCALAR
   GE-ILAYOUT-GUARD
   GE-ILAYOUT-SCALAR
   s" PASS: interpret-mode layout transports fail closed" type cr ;

;package

package WIDE-FETCH
private

: SRC ( -- )                            \ wide instantiation + narrow control, with forges for both
   s" PRODUCT gwfp 0 FIELD x n FIELD y n ;PRODUCT" GE-SRC-LINE
   s" ENUM gwfo 1 VARIANT gwn ;VARIANT VARIANT gws FIELD value a ;VARIANT ;ENUM" GE-SRC-LINE
   s" ENUM gwfn 0 VARIANT gnn ;VARIANT VARIANT gns FIELD value n ;VARIANT ;ENUM" GE-SRC-LINE
   s" 4 TYPED-BUFFER GWF-AT gwfo<gwfp>" GE-SRC-LINE
   s" 4 TYPED-BUFFER GWN-AT gwfn" GE-SRC-LINE
   s" TRUSTED: GWF-FORGE ( -- gwfo<gwfp> ) 0 0 5 ;" GE-SRC-LINE
   s" TRUSTED: GWN-FORGE ( -- gwfn ) 0 5 ;" GE-SRC-LINE
   s" : GWF-MK ( n -- gwfo<gwfp> ) dup 3 * swap 5 * GWFP:MAKE GWFO:GWS ;" GE-SRC-LINE
   s" : GWF-PUT ( n n -- ) {: v:n k:n :} v GWF-MK k GWF-AT ! ;" GE-SRC-LINE
   s" : GWF-GET ( n -- n ) GWF-AT @ MATCH gwfo gwn OF 0 ENDOF gws OF GWFP:UNMAKE 7 * swap 11 * + ENDOF ;MATCH ;" GE-SRC-LINE
   s" : GWF-BAD ( n -- ) {: k:n :} GWF-FORGE k GWF-AT ! ;" GE-SRC-LINE
   s" : GWN-PUT ( n n -- ) {: v:n k:n :} v GWFN:GNS k GWN-AT ! ;" GE-SRC-LINE
   s" : GWN-GET ( n -- n ) GWN-AT @ MATCH gwfn gnn OF 0 ENDOF gns OF 13 * ENDOF ;MATCH ;" GE-SRC-LINE
   s" : GWN-BAD ( n -- ) {: k:n :} GWN-FORGE k GWN-AT ! ;" GE-SRC-LINE ;

: ROUND ( -- )                   \ both widths store and load their own values back
   GE-HB-RESET
   GE-SRC-RESET
   SRC
   s" : GO ( -- ) 6 0 GWF-PUT 0 GWF-GET .  4 1 GWN-PUT 1 GWN-GET . ;  GO" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" wide-instantiation fetch round-trip" GE-EXPECT-OK
   SB-RESET
   s" 408" SB-APPEND GE-SB-LF                  \ 18*11 + 30*7, weighted so an exchange would show
   s" 52" SB-APPEND GE-SB-LF                   \ 4*13, the narrow control
   SB$ s" wide-instantiation fetch round-trip output" GE-EXPECT-OUT ;

: BAD-WIDE ( -- )                \ forged tag at the wide instantiation: the FETCH guard fires
   GE-HB-RESET
   GE-SRC-RESET
   SRC
   s" : GO ( -- ) 0 GWF-BAD 0 GWF-GET . ;  GO" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   ENGINE-ERROR:BAD-TAG s" wide forged tag dies with ENGINE-ERROR:BAD-TAG" GE-EXPECT-RC
   s" hb: bad layout tag" s" wide forged tag reaches the fetch guard" GE-EXPECT-ERR-HAS ;

: BAD-NARROW ( -- )              \ the same forge where declared and instantiated agree
   GE-HB-RESET
   GE-SRC-RESET
   SRC
   s" : GO ( -- ) 1 GWN-BAD 1 GWN-GET . ;  GO" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   ENGINE-ERROR:BAD-TAG s" narrow forged tag dies with ENGINE-ERROR:BAD-TAG" GE-EXPECT-RC
   s" hb: bad layout tag" s" narrow forged tag reaches the fetch guard" GE-EXPECT-ERR-HAS ;

public

: RUN ( -- )
   ROUND
   BAD-WIDE
   BAD-NARROW
   s" PASS: wide-instantiation fetch validates its own tag cell; forged tags still die there" type cr ;

;package

package RUNTIME-REGRESSION


\ Dictionary-capacity exit diagnostic (dot habu-gate-runner-entry-81c84af0):
\ a tool closure needing more than DICT-CAP records died exit_group(77)
\ writing only the CURRENT TOKEN to fd 2 - a lone ':' byte, label-free and
\ unattributable. The definer capacity arms must emit a fixed label first:
\ `hb: dictionary full at: <token>`; rc 77 is the deterministic contract and
\ stays. The fixture is Habu-generated and scales with the baked DICT-CAP
\ (src/habu/layout.f is in the runtime prefix): DICT-CAP+1 unchecked trivial
\ definitions always overflow regardless of the boot dictionary count.
variable GE-DFULL-P                 \ generated-source cursor offset
variable GE-DFULL-DIV               \ decimal-render divisor
variable GE-DFULL-I                 \ copy/definition loop index

: GE-DFULL-C ( ptr u8 n -- ) {: buf:ptr c:n :}
   c buf GE-DFULL-P @ + c!
   GE-DFULL-P @ 1+ GE-DFULL-P ! ;

: GE-DFULL-S ( ptr u8 ptr u8 n -- ) {: buf:ptr a:ptr u:n :}
   0 GE-DFULL-I !
   begin GE-DFULL-I @ u < while
      buf  a GE-DFULL-I @ + c@  GE-DFULL-C
      GE-DFULL-I @ 1+ GE-DFULL-I !
   repeat ;

: GE-DFULL-DIGITS ( ptr u8 n -- ) {: buf:ptr i:n :}
   10000 GE-DFULL-DIV !
   begin GE-DFULL-DIV @ 0 > while
      buf  i GE-DFULL-DIV @ / 10 mod 48 +  GE-DFULL-C
      GE-DFULL-DIV @ 10 / GE-DFULL-DIV !
   repeat ;

: GE-DFULL-DEF ( ptr u8 n -- ) {: buf:ptr i:n :}      \ append `: wNNNNN ;\n`
   buf 58 GE-DFULL-C  buf 32 GE-DFULL-C  buf 119 GE-DFULL-C
   buf i GE-DFULL-DIGITS
   buf 32 GE-DFULL-C  buf 59 GE-DFULL-C  buf 10 GE-DFULL-C ;

: GE-DFULL-WRITE ( ptr u8 NUM:alloc-byte-len -- ) {: buf:ptr len :}   \ generate the define-past-cap program into the scoped buffer, then persist it
   0 GE-DFULL-P !
   buf s" 0 set-check" GE-DFULL-S  buf 10 GE-DFULL-C
   0 GE-DFULL-I !
   begin GE-DFULL-I @ DICT-CAP 1+ < while
      buf GE-DFULL-I @ GE-DFULL-DEF
      GE-DFULL-I @ 1+ GE-DFULL-I !
   repeat
   GE-SCRIPT-PATH GE-SCRIPT-U @ buf GE-DFULL-P @ WRITE-ALL ;

: GE-DICT-FULL ( -- )
   GT-ROOT s" hb-dict-full.f" GE-SCRIPT-PATH JOIN-PATH GE-SCRIPT-U !
   DICT-CAP 1+ 16 * 32 + MEM:BYTES-ALLOC-LEN [: GE-DFULL-WRITE ;] MEM:WITH-BYTES
   GE-HB-RESET
   GE-SCRIPT-PATH GE-SCRIPT-U @ RUNTIME-RUNNER:FILE-LOADER
   77 s" dict-capacity exit rc" GE-EXPECT-RC
   s" hb: dictionary full at: " s" dict-capacity exit diagnostic" GE-EXPECT-ERR-HAS
   s" PASS: dictionary-capacity exit is labeled" type cr ;

\ Per-definition body-text capacity (dot habu-name-the-per-56a594f3). One
\ definition's captured source text lives in BODYBUF-CAP bytes of the DATA header
\ (src/habu/layout.f), the buffer the check hook certifies from. A definition
\ holding about 8 KiB of string literals used to refuse with the offending
\ LITERAL echoed to fd 2 — no label, no newline, no count, no ceiling and no
\ definition name — so a consumer had only "status 71" to act on (aspen/Tender
\ 2026-09-17, nine 900-byte `s"` arms; eight loaded). The refusal now states the
\ buffer, the ceiling, the definition and the bytes the capture needed, and it is
\ a catchable rc-71 throw inside evaluate instead of a raw exit.
\ Both fixtures scale with BODYBUF-CAP: layout.f is in the runtime prefix, so the
\ suite reads the same constant the engine was built with.
27 constant GE-BCAP-FIXED   \ what the fixture's own tokens cost the capture: `GEBIG ( -- ptr u8 n ) s" ` is 25 bytes of tokens-plus-separators, and the literal is captured with its closing quote and one separator
variable GE-BCAP-P          \ generated-source cursor offset
variable GE-BCAP-I          \ copy/fill loop index
variable GE-BCAP-N          \ literal payload bytes for the fixture being written

: GE-BCAP-C ( ptr u8 n -- ) {: buf:ptr c:n :}
   c buf GE-BCAP-P @ + c!
   GE-BCAP-P @ 1+ GE-BCAP-P ! ;

: GE-BCAP-S ( ptr u8 ptr u8 n -- ) {: buf:ptr a:ptr u:n :}
   0 GE-BCAP-I !
   begin GE-BCAP-I @ u < while
      buf  a GE-BCAP-I @ + c@  GE-BCAP-C
      GE-BCAP-I @ 1+ GE-BCAP-I !
   repeat ;

: GE-BCAP-FILL ( ptr u8 n -- ) {: buf:ptr u:n :}      \ u payload bytes
   0 GE-BCAP-I !
   begin GE-BCAP-I @ u < while
      buf 97 GE-BCAP-C
      GE-BCAP-I @ 1+ GE-BCAP-I !
   repeat ;

\ `: GEBIG ( -- ptr u8 n ) s" aaa…" ;` with GE-BCAP-N bytes of payload. The
\ checker stays ON: the case this regression carries is checked application code.
: GE-BCAP-WRITE ( ptr u8 NUM:alloc-byte-len -- ) {: buf:ptr len :}
   0 GE-BCAP-P !
   buf s" : GEBIG ( -- ptr u8 n ) s" GE-BCAP-S
   buf 34 GE-BCAP-C  buf 32 GE-BCAP-C
   buf GE-BCAP-N @ GE-BCAP-FILL
   buf 34 GE-BCAP-C  buf 32 GE-BCAP-C  buf 59 GE-BCAP-C  buf 10 GE-BCAP-C
   GE-SCRIPT-PATH GE-SCRIPT-U @ buf GE-BCAP-P @ WRITE-ALL ;

: GE-BCAP-RUN ( n -- ) {: payload:n :}
   payload GE-BCAP-N !
   GT-ROOT s" hb-body-cap.f" GE-SCRIPT-PATH JOIN-PATH GE-SCRIPT-U !
   BODYBUF-CAP 128 + MEM:BYTES-ALLOC-LEN [: GE-BCAP-WRITE ;] MEM:WITH-BYTES
   GE-HB-RESET
   GE-SCRIPT-PATH GE-SCRIPT-U @ RUNTIME-RUNNER:FILE-LOADER ;

: GE-BCAP-DIAG$ ( n -- ptr u8 n ) {: needed:n :}
   SB-RESET
   s" hb: definition body text full at " SB-APPEND
   BODYBUF-CAP FMT:SB-U
   s"  bytes: GEBIG needs " SB-APPEND
   needed FMT:SB-U
   SB$ ;

: GE-BODY-CAP ( -- )
   \ a capture ending EXACTLY at BODYBUF-CAP is the most the bound admits
   BODYBUF-CAP GE-BCAP-FIXED - GE-BCAP-RUN
   0 s" body-capture at the ceiling compiles" GE-EXPECT-RC
   \ one byte more refuses, by name, with the count it needed
   BODYBUF-CAP GE-BCAP-FIXED - 1+ GE-BCAP-RUN
   71 s" body-capacity exit rc" GE-EXPECT-RC
   BODYBUF-CAP 1+ GE-BCAP-DIAG$ s" body-capacity exit diagnostic" GE-EXPECT-ERR-HAS
   s" PASS: per-definition body-text capacity is named with its count" type cr ;

\ BEGIN nesting per definition (dot habu-name-silent-engine-9b28ac13). The JIT
\ snapshots the abstract value stack once per BEGIN into JIT-SNAP:FRAMES frames
\ of DATA, and a definition that nested past them exited 75 with NOTHING on fd 2.
\ Same ceiling, same exit code, now named with the depth the definition asked for.
variable GE-NEST-P
variable GE-NEST-I

: GE-NEST-C ( ptr u8 n -- ) {: buf:ptr c:n :}
   c buf GE-NEST-P @ + c!
   GE-NEST-P @ 1+ GE-NEST-P ! ;

: GE-NEST-S ( ptr u8 ptr u8 n -- ) {: buf:ptr a:ptr u:n :}
   0 GE-NEST-I !
   begin GE-NEST-I @ u < while
      buf  a GE-NEST-I @ + c@  GE-NEST-C
      GE-NEST-I @ 1+ GE-NEST-I !
   repeat ;

variable GE-NEST-N          \ BEGIN depth for the fixture being written
variable GE-NEST-J

: GE-NEST-WRITE ( ptr u8 NUM:alloc-byte-len -- ) {: buf:ptr len :}
   0 GE-NEST-P !
   buf s" : GENEST ( n -- n )" GE-NEST-S  buf 10 GE-NEST-C
   0 GE-NEST-J !
   begin GE-NEST-J @ GE-NEST-N @ < while
      buf s"    begin dup 0 > while 1 -" GE-NEST-S  buf 10 GE-NEST-C
      GE-NEST-J @ 1+ GE-NEST-J !
   repeat
   0 GE-NEST-J !
   begin GE-NEST-J @ GE-NEST-N @ < while
      buf s"    repeat" GE-NEST-S  buf 10 GE-NEST-C
      GE-NEST-J @ 1+ GE-NEST-J !
   repeat
   buf s" ;" GE-NEST-S  buf 10 GE-NEST-C
   GE-SCRIPT-PATH GE-SCRIPT-U @ buf GE-NEST-P @ WRITE-ALL ;

: GE-NEST-RUN ( n -- ) {: depth:n :}
   depth GE-NEST-N !
   GT-ROOT s" hb-begin-nest.f" GE-SCRIPT-PATH JOIN-PATH GE-SCRIPT-U !
   depth 40 * 64 + MEM:BYTES-ALLOC-LEN [: GE-NEST-WRITE ;] MEM:WITH-BYTES
   GE-HB-RESET
   GE-SCRIPT-PATH GE-SCRIPT-U @ RUNTIME-RUNNER:FILE-LOADER ;

: GE-NEST-DIAG$ ( -- ptr u8 n )
   SB-RESET
   s" hb: BEGIN nesting full at " SB-APPEND
   JIT-SNAP:FRAMES FMT:SB-U
   s"  frames: GENEST needs " SB-APPEND
   JIT-SNAP:FRAMES 1+ FMT:SB-U
   SB$ ;

: GE-BEGIN-NEST ( -- )
   JIT-SNAP:FRAMES GE-NEST-RUN
   0 s" BEGIN nesting at the frame ceiling compiles" GE-EXPECT-RC
   JIT-SNAP:FRAMES 1+ GE-NEST-RUN
   75 s" BEGIN-nesting exit rc" GE-EXPECT-RC
   GE-NEST-DIAG$ s" BEGIN-nesting exit diagnostic" GE-EXPECT-ERR-HAS
   s" PASS: BEGIN-nesting capacity is named with its depth" type cr ;

\ DP heap (allot/,/c,/definer) must stop below the profiler counter band reserved at
\ the top PROF-CNT-BYTES of the DATA region (layout.f). DP-CHECK (habu1.f) caps the
\ heap at DATA-SIZE - PROF-CNT-BYTES so a large allot + prof-on can never let profiler
\ writes corrupt user data (dot habu-bound-profiler-counter-235c5f48). Over-bound fails
\ closed NAMED "hb: data space out of range" on fd 2 (catchable rc-76 throw inside
\ evaluate, exit 76 at top level). RED discriminator: on the unfixed base an allot one
\ byte INTO the band SUCCEEDS silently (rc 0, no message) and clobbers a counter.
\ `data-base`/`DATA-SIZE`/`PROF-CNT-BYTES`/`here` are runtime words, so the boundary is
\ computed against the live band base.
: GE-DATA-FULL ( -- )
   \ one byte past the band base rejects (base: succeeds silently — the RED discriminator)
   s" data-base DATA-SIZE PROF-CNT-BYTES - + here - 1+ allot"
      76 s" data-space over-band exit rc" RUNTIME-RUNNER:LINE-RC
   s" hb: data space out of range" s" data-space over-band diagnostic" GE-EXPECT-ERR-HAS
   \ allot ending EXACTLY at the band base (DP == DATA + DATA-SIZE - PROF-CNT-BYTES, the
   \ max the <= bound admits) must SUCCEED and exit clean.
   s" data-base DATA-SIZE PROF-CNT-BYTES - + here - allot"
      0 s" data-space band-base allot succeeds" RUNTIME-RUNNER:LINE-RC
   s" PASS: data-space profiler-band cap is labeled + off-by-one boundary holds" type cr ;

: GE-DIV-TRAP ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n label:ptr labelu:n :}
   GE-HB-RESET GE-SRC-RESET src srcu GE-SRC-LINE
   GE-SRC-BUF GE-SRC-U @ RUNTIME-RUNNER:SOURCE
   label labelu GE-EXPECT-NONZERO ;

: GE-DIV-MOD ( -- )
   s" 1 0 / ." s" divide by zero trap" GE-DIV-TRAP
   s" 1 0 mod ." s" modulo by zero trap" GE-DIV-TRAP
   GE-HB-RESET GE-SRC-RESET s" 7 2 / . 7 2 mod . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   SB-RESET s" 3" SB-APPEND GE-SB-LF s" 1" SB-APPEND GE-SB-LF GE-SB-LF
   SB$ s" nonzero div/mod output" GE-EXPECT-OUT
   s" PASS: div/mod by zero traps (no silent 0)" type cr ;


: GE-PROCESS-PTY ( -- )
   GE-HB-RESET
   s" --load" GE-ARG+
   s" lib/errors.f" GE-ARG+
   s" lib/process.f" GE-ARG+
   s" test/proc-pty.f" GE-ARG+
   s" --" GE-ARG+
   GE-HB$ GE-ARG+
   RUNTIME-RUNNER:PTY
   s" process/pty" GE-EXPECT-OK
   s" PASS: process/pty primitives" s" process/pty output" GE-EXPECT-OUT-HAS
   s" PASS: process/pty primitives" type cr ;

: GE-DEREF-1 ( ptr u8 n -- ) {: tok:ptr toku:n :}
   \ Run one deref/execute primitive as the LITERAL FIRST top-level token on an
   \ empty stack: the pre-exec arity guard must name E-UNDERFLOW + exit 70, never a
   \ signal (crash handler exit 134). Before the guard this faulted inside the prim.
   GE-HB-RESET
   GE-SRC-RESET
   tok toku GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" hb deref-first arity rc" GE-EXPECT-RC
   s" E-UNDERFLOW" s" hb deref-first arity diagnostic" GE-EXPECT-ERR-HAS
   tok toku s" hb deref-first arity token" GE-EXPECT-ERR-HAS ;

: GE-DEREF-ARITY-DIAG ( -- )
   s" @" GE-DEREF-1
   s" !" GE-DEREF-1
   s" execute" GE-DEREF-1
   \ positive control: a valid store satisfies min-in -> succeeds rc 0 (no false guard).
   GE-HB-RESET
   GE-SRC-RESET
   s" variable GAV 5 GAV !" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb valid deref store succeeds" GE-EXPECT-OK ;

: GE-NESTED-DEF-SRC ( ptr u8 n -- ) {: body:ptr bodyu:n :}
   \ Build: TRUSTED: W ( -- ) s" <body>" evaluate ;  then run W.
   \ W is TRUSTED: because `evaluate` is an uncheckable metaprogramming boundary
   \ (its effect is dynamic); the definition compiled BY <body> is still fully
   \ checked by the active hook, from inside W's execution.
   GE-SRC-RESET
   s" TRUSTED: W ( -- )" GE-SRC+  GE-SRC-SP
   body bodyu GE-SRC-S"
   s"  evaluate ;" GE-SRC-LINE
   s" W" GE-SRC-LINE ;

: GE-NESTED-CHECKED-DEF ( -- )
   \ Checker reentrancy across the word-execution boundary: a checked colon
   \ definition compiled WHILE a word executes must certify + publish correctly.
   \ Proven: ZZ compiles under the active hook from inside W, then runs -> 5, rc 0.
   GE-HB-RESET
   s" : ZZ ( -- n ) 5 ;" GE-NESTED-DEF-SRC
   s" ZZ ." GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb nested checked def rc" GE-EXPECT-OK
   SB-RESET s" 5" SB-APPEND GE-SB-LF
   SB$ s" hb nested checked def output" GE-EXPECT-OUT
   s" PASS: nested checked def certifies + runs (reentrant hook)" type cr ;

: GE-NESTED-BAD-DEF ( -- )
   \ The nested definition is NOT trusted just because its definer word is: a
   \ bad-effect nested def compiled from inside an executing word must still be
   \ REJECTED (rc 70). Proven: BAD ( -- n ) drop is rejected at 'drop'.
   GE-HB-RESET
   s" : BAD ( -- n ) drop ;" GE-NESTED-DEF-SRC
   RUNTIME-RUNNER:BUFFER
   70 s" hb nested bad def rc" GE-EXPECT-RC
   s" bad" s" hb nested bad def word" GE-EXPECT-ERR-HAS
   s" drop" s" hb nested bad def token" GE-EXPECT-ERR-HAS
   s" PASS: nested bad-effect def rejected from inside a word" type cr ;

: GE-EVAL-UNDEF-SRC ( -- )
   \ The dot reproducer: an undefined word aborts a nested `:`-compile INSIDE
   \ `evaluate` (called from GO via the TRUSTED evaluate wrapper). Mid-compile the
   \ JIT dict region is RW; the aborted definition must unwind cleanly, not fault.
   GE-SRC-RESET
   s" TRUSTED: EV ( ptr u8 n -- ) evaluate ;" GE-SRC-LINE
   s" : GO ( -- )" GE-SRC+  GE-SRC-SP
   s" : FOO ( -- ) UNDEFINED-WORD-XYZ ;" GE-SRC-S"
   s"  EV ;" GE-SRC-LINE ;

: GE-EVAL-UNDEF-CATCHABLE ( -- )
   \ Under an enclosing quotation catch, the aborted nested :-compile unwinds the
   \ eval frame (partial def dropped) and delivers a CATCHABLE throw (code 70) to
   \ the catch -> `. cr` prints 70 and the process exits 0. Was: native register
   \ dump / SIGBUS exit 134 (W^X: returned into RW dict code without restoring RX).
   GE-HB-RESET
   GE-EVAL-UNDEF-SRC
   s" : T1 ( -- ) [: GO ;] catch . cr ;" GE-SRC-LINE
   s" T1" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb eval-undef catch rc" GE-EXPECT-OK
   s" 70" s" hb eval-undef catch code" GE-EXPECT-OUT-HAS
   s" E-UNDEFINED" s" hb eval-undef catch diag" GE-EXPECT-ERR-HAS
   s" UNDEFINED-WORD-XYZ" s" hb eval-undef catch token" GE-EXPECT-ERR-HAS
   s" PASS: undefined in nested :-compile under catch -> catchable code 70, exit 0" type cr ;

: GE-EVAL-UNDEF-FAILCLOSED ( -- )
   \ Same mid-compile abort inside evaluate but NO handler: the throw finds no
   \ catch, so it fails closed with rc 70 + E-UNDEFINED (like the top-level LRDIE
   \ path), never a signal and never continuing past the abort.
   GE-HB-RESET
   GE-EVAL-UNDEF-SRC
   s" GO" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" hb eval-undef no-catch rc" GE-EXPECT-RC
   s" E-UNDEFINED" s" hb eval-undef no-catch diag" GE-EXPECT-ERR-HAS
   s" UNDEFINED-WORD-XYZ" s" hb eval-undef no-catch token" GE-EXPECT-ERR-HAS
   s" PASS: undefined in nested :-compile w/o catch -> fail-closed rc70" type cr ;

: GE-COMPILE-UNDEF-TOPLEVEL ( -- )
   \ The top-level undefined-in-:-compile path (EVALD==0, no eval frame) is
   \ unchanged by the eval-frame recovery fix: E-UNDEFINED + rc 70, never a signal.
   GE-HB-RESET
   GE-SRC-RESET
   s" : FOO ( -- ) UNDEFINED-WORD-XYZ ;" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" hb top-level undef-compile rc" GE-EXPECT-RC
   s" E-UNDEFINED" s" hb top-level undef-compile diag" GE-EXPECT-ERR-HAS
   s" UNDEFINED-WORD-XYZ" s" hb top-level undef-compile token" GE-EXPECT-ERR-HAS
   s" PASS: top-level undefined-in-compile fail-closed rc70 (unchanged)" type cr ;

: GE-EVAL-UNDEF-RECOVER ( -- )
   GE-EVAL-UNDEF-CATCHABLE
   GE-EVAL-UNDEF-FAILCLOSED
   GE-COMPILE-UNDEF-TOPLEVEL ;

: GE-EVAL-CATCH-SRC ( -- )
   \ The dot-pair reproducer wrapper (test/type-ctor-suite.f TCE-CATCH shape):
   \ a quotation catch over the audited INCLUDE-EVALUATE boundary. The caller
   \ appends one failing source string; the caught code prints to stdout.
   GE-SRC-RESET
   \ GECA holds an ADDRESS, so it is a declared cell: a raw `variable` admits
   \ scalars only and `GECA @` as a `ptr u8` is E-RAW-CELL-PTR. GECU holds a
   \ length and stays raw.
   s" TYPED-VARIABLE GECA ptr u8   variable GECU" GE-SRC-LINE
   s" : GEC-GO ( -- ) GECA @ GECU @ INCLUDE-EVALUATE ;" GE-SRC-LINE
   s" : GEC-CATCH ( ptr u8 n -- n ) GECU ! GECA ! [: GEC-GO ;] catch ;" GE-SRC-LINE ;

: GE-EVAL-CATCH-RUN ( ptr u8 n -- ) {: src:ptr srcu:n :}
   GE-HB-RESET
   GE-EVAL-CATCH-SRC
   src srcu GE-SRC-S"
   s"  GEC-CATCH . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER ;

: GE-EVAL-INTERP-UNDEF-CATCH ( -- )
   \ Dot habu-interpret-err-under-8876b500: an undefined INTERPRET-mode token
   \ inside [: INCLUDE-EVALUATE ;] catch delivers the catchable RC-REJECT (70)
   \ of the rc-70 load-path contract — never a swallowed 0.
   s" qwertyuiop" GE-EVAL-CATCH-RUN
   s" hb eval interp-undef catch rc" GE-EXPECT-OK
   s" 70" s" hb eval interp-undef catch code" GE-EXPECT-OUT-HAS
   s" E-UNDEFINED" s" hb eval interp-undef catch diag" GE-EXPECT-ERR-HAS
   s" qwertyuiop" s" hb eval interp-undef catch token" GE-EXPECT-ERR-HAS
   s" PASS: interpret undefined under catch+evaluate -> caught 70" type cr ;

: GE-EVAL-UNDERFLOW-CATCH ( -- )
   \ Dot 8876b500 residual: interpret-level UNDERFLOW inside the same wrapper
   \ was the one interpret failure still rolling the eval frame back with only
   \ EVALERR set — catch read 0 (fail-open). It must be caught 70 exactly like
   \ E-UNDEFINED, with the sentinel stack under the wrapper intact.
   s" drop drop drop" GE-EVAL-CATCH-RUN
   s" hb eval underflow catch rc" GE-EXPECT-OK
   s" 70" s" hb eval underflow catch code" GE-EXPECT-OUT-HAS
   s" E-UNDERFLOW" s" hb eval underflow catch diag" GE-EXPECT-ERR-HAS
   s" drop" s" hb eval underflow catch token" GE-EXPECT-ERR-HAS
   s" PASS: interpret underflow under catch+evaluate -> caught 70" type cr ;

: GE-EVAL-UNDERFLOW-FAILCLOSED ( -- )
   \ No handler: the underflow throw escapes the eval frame and fails closed rc
   \ 70 (uncaught-throw exit), never continuing past the failed evaluate. The
   \ rollback-and-return path printed the marker and exited 0 (fail-open).
   GE-HB-RESET
   GE-SRC-RESET
   s" drop drop drop" GE-SRC-S"
   s"  INCLUDE-EVALUATE" GE-SRC-LINE
   s" s" GE-SRC+ GE-DQ GE-SRC-C s"  ALIVE-AFTER" GE-SRC+ GE-DQ GE-SRC-C
   s"  type cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" hb eval underflow no-catch rc" GE-EXPECT-RC
   s" E-UNDERFLOW" s" hb eval underflow no-catch diag" GE-EXPECT-ERR-HAS
   s" " s" hb eval underflow no-catch dead marker" GE-EXPECT-OUT
   s" PASS: interpret underflow in evaluate w/o catch -> fail-closed rc70" type cr ;

: GE-UNDERFLOW-TOPLEVEL-UNCHANGED ( -- )
   \ Plain-stdin contract pin for the fix: top-level underflow (EVALD==0) keeps
   \ the E-UNDERFLOW diagnostic + rc 70 exactly.
   GE-HB-RESET
   GE-SRC-RESET
   s" drop drop drop" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" hb top-level underflow rc unchanged" GE-EXPECT-RC
   s" E-UNDERFLOW" s" hb top-level underflow diag unchanged" GE-EXPECT-ERR-HAS
   s" PASS: top-level underflow fail-closed rc70 (unchanged)" type cr ;

: GE-EVAL-INTERP-ERR-RECOVER ( -- )
   GE-EVAL-INTERP-UNDEF-CATCH
   GE-EVAL-UNDERFLOW-CATCH
   GE-EVAL-UNDERFLOW-FAILCLOSED
   GE-UNDERFLOW-TOPLEVEL-UNCHANGED ;

: GE-EVAL-DEF-REJECT-1 ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n diag:ptr diagu:n label:ptr labelu:n :}
   \ One rejected definition evaluated under the TCE-CATCH wrapper: the abort
   \ must deliver caught RC-REJECT (70) on stdout with the diagnostic on
   \ stderr and the process exiting 0 — never a SIGBUS register dump (rc 134).
   src srcu GE-EVAL-CATCH-RUN
   label labelu GE-EXPECT-OK
   s" 70" label labelu GE-EXPECT-OUT-HAS
   diag diagu label labelu GE-EXPECT-ERR-HAS ;

: GE-EVAL-DEF-REJECT-CATCH ( -- )
   \ Dot habu-def-compile-failure-7182eeb2 lock-in: a definition whose engine
   \ compile fails inside [: INCLUDE-EVALUATE ;] catch is a catchable throw
   \ with the eval frame rolled back. Exact dot repro (undefined in a :-body,
   \ formerly a habu-crash regs dump) plus the orderly reject battery — every
   \ shape that fail-closes rc 70 on plain stdin must be caught 70 here.
   s" : XG1 ( -- ) qwertyuiop ;" s" E-UNDEFINED"
      s" hb eval def-undef catch" GE-EVAL-DEF-REJECT-1
   s" : GDR1 ( -- ) drop ;" s" non-certified definition: gdr1"
      s" hb eval def-underdepth catch" GE-EVAL-DEF-REJECT-1
   s" : GDR2 ( n -- ) ;" s" non-certified definition: gdr2"
      s" hb eval def-unconsumed-in catch" GE-EVAL-DEF-REJECT-1
   s" : GDR3 ( -- n ) ;" s" non-certified definition: gdr3"
      s" hb eval def-missing-out catch" GE-EVAL-DEF-REJECT-1
   s" : GDR4 ( -- ) 1 2 ;" s" non-certified definition: gdr4"
      s" hb eval def-surplus-out catch" GE-EVAL-DEF-REJECT-1
   s" PASS: def-compile failures under catch+evaluate -> caught 70" type cr ;

: GE-ORPHAN-CLOSER-1 ( ptr u8 n ptr u8 n -- ) {: tok:ptr toku:n label:ptr labelu:n :}
   \ Plain stdin: a definition that opens no control-flow frame but names a closer
   \ must fail closed rc 70 with the named engine diagnostic + the offending token,
   \ NEVER a SIGBUS register dump (rc 134). Root cause (dot habu-orphan-control-
   \ word-0370b49d): every closer's compile-time patch pops the control-flow stack
   \ through LCFPOP; with an empty stack it underflowed to a bogus branch origin that
   \ LPAT then dereferenced. LCFPOP now guards depth 0 and rejects like E-UNDEFINED.
   GE-HB-RESET
   GE-SRC-RESET
   s" : XI ( -- ) " GE-SRC+
   tok toku GE-SRC+
   s"  ;" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 label labelu GE-EXPECT-RC
   s" control-flow closer without opener" label labelu GE-EXPECT-ERR-HAS
   tok toku label labelu GE-EXPECT-ERR-HAS
   s" habu-crash" label labelu GE-EXPECT-ERR-LACKS ;

: GE-ORPHAN-CLOSER ( -- )
   \ Every control-flow closer, orphaned at top level, and the catchable-under-eval
   \ subset. THEN/ELSE/REPEAT/LOOP/+LOOP/ENDOF crashed rc 134 before the fix;
   \ UNTIL/AGAIN/ENDCASE were orderly by luck (no LPAT deref / zeroed slack cell)
   \ but now share the one guarded LCFPOP reject path.
   s" THEN"    s" hb orphan then"    GE-ORPHAN-CLOSER-1
   s" ELSE"    s" hb orphan else"    GE-ORPHAN-CLOSER-1
   s" REPEAT"  s" hb orphan repeat"  GE-ORPHAN-CLOSER-1
   s" UNTIL"   s" hb orphan until"   GE-ORPHAN-CLOSER-1
   s" AGAIN"   s" hb orphan again"   GE-ORPHAN-CLOSER-1
   s" LOOP"    s" hb orphan loop"    GE-ORPHAN-CLOSER-1
   s" +LOOP"   s" hb orphan +loop"   GE-ORPHAN-CLOSER-1
   s" ENDOF"   s" hb orphan endof"   GE-ORPHAN-CLOSER-1
   s" ENDCASE" s" hb orphan endcase" GE-ORPHAN-CLOSER-1
   \ Under [: INCLUDE-EVALUATE ;] catch: the former SIGBUS closers deliver a
   \ catchable RC-REJECT (70), the eval frame rolled back, process exits 0.
   s" : XO1 ( -- ) THEN ;" s" control-flow closer without opener"
      s" hb eval orphan-then catch" GE-EVAL-DEF-REJECT-1
   s" : XO2 ( -- ) LOOP ;" s" control-flow closer without opener"
      s" hb eval orphan-loop catch" GE-EVAL-DEF-REJECT-1
   s" : XO3 ( -- ) REPEAT ;" s" control-flow closer without opener"
      s" hb eval orphan-repeat catch" GE-EVAL-DEF-REJECT-1
   s" PASS: orphan control-flow closers fail closed rc70 (no SIGBUS)" type cr ;

\ The loop family's own opener battery is kept in its own package.
;package

package LOOP-OPENER
private

: REJECT ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: open:ptr openu:n tok:ptr toku:n label:ptr labelu:n :}
   \ Plain stdin: a loop-family word (LOOP / +LOOP / LEAVE) whose innermost open
   \ control-flow frame is not a `do` frame -- or which has no open frame at all --
   \ must fail closed rc 70 with the named engine diagnostic and the offending
   \ token, NEVER a register dump (rc 134). Root cause (dot habu-fix-loop-closer-
   \ 9e5d012e): the DO/LEAVE level stack (LVD-CELL depth + the LVH/LVF level
   \ arrays) is the loop family's opener record and LCFPOP's orphan guard does not
   \ cover it. With an IF or BEGIN frame open, LOOP/+LOOP still popped the CF stack
   \ happily and then indexed level -1 of the level stack; LVH's cell -1 IS
   \ LVD-CELL itself, so LBCHAIN was handed a junk chain head and dereferenced it.
   \ J-LVREQUIRE now proves a level is open in every consumer of that stack.
   GE-HB-RESET
   GE-SRC-RESET
   s" : XM ( -- ) " GE-SRC+
   open openu GE-SRC+
   s"  " GE-SRC+
   tok toku GE-SRC+
   s"  ;" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 label labelu GE-EXPECT-RC
   s" control-flow closer without opener" label labelu GE-EXPECT-ERR-HAS
   tok toku label labelu GE-EXPECT-ERR-HAS
   s" habu-crash" label labelu GE-EXPECT-ERR-LACKS ;

: FORGE ( -- )
   \ A stray LEAVE used to STORE a code offset into LVH level -1, which aliases
   \ LVD-CELL: that forged a non-zero open-level count out of nothing, so a later
   \ LOOP would sail past any count-only check and walk a bogus LEAVE chain.
   \ Guarding LEAVE closes the forge at its source, which is what makes the
   \ LOOP/+LOOP guard an existence check on a sole-writer count rather than a
   \ value test. The definition is therefore rejected at LEAVE, not at LOOP.
   GE-HB-RESET
   GE-SRC-RESET
   s" : XMF ( -- ) 1 IF LEAVE drop LOOP ;" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" hb mispair leave-forge rc" GE-EXPECT-RC
   s" control-flow closer without opener" s" hb mispair leave-forge diag" GE-EXPECT-ERR-HAS
   s" LEAVE" s" hb mispair leave-forge token" GE-EXPECT-ERR-HAS
   s" habu-crash" s" hb mispair leave-forge no-crash" GE-EXPECT-ERR-LACKS ;

: LEGAL ( -- )
   \ The guard must not touch legal use: a plain DO/LOOP, a ?DO (whose own skip
   \ branch goes through the same LEAVE-chain code), a +LOOP, and a LEAVE nested
   \ inside a DO all still compile and run.
   GE-HB-RESET
   GE-SRC-RESET
   s" : XMOK ( -- ) 3 0 do i drop loop  6 0 ?do i drop 2 +loop" GE-SRC+
   s"  9 0 do i 2 = if leave then loop  4242 . cr ;" GE-SRC-LINE
   s" XMOK" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb mispair-loop legal" GE-EXPECT-OK
   s" 4242" s" hb mispair-loop legal output" GE-EXPECT-OUT-HAS ;

public

: RUN ( -- )
   \ Every loop-family word against an IF frame, a BEGIN frame, and no frame at
   \ all. The IF and BEGIN rows were rc 134 register dumps for LOOP/+LOOP and a
   \ silent LVD-CELL corruption for LEAVE before the guard.
   s" 1 IF drop"    s" LOOP"   s" hb mispair if-loop"     REJECT
   s" 1 IF drop"    s" +LOOP"  s" hb mispair if-+loop"    REJECT
   s" 1 IF"         s" LEAVE"  s" hb mispair if-leave"    REJECT
   s" 1 BEGIN drop" s" LOOP"   s" hb mispair begin-loop"  REJECT
   s" 1 BEGIN drop" s" +LOOP"  s" hb mispair begin-+loop" REJECT
   s" 1 BEGIN"      s" LEAVE"  s" hb mispair begin-leave" REJECT
   s" "             s" LOOP"   s" hb mispair bare-loop"   REJECT
   s" "             s" +LOOP"  s" hb mispair bare-+loop"  REJECT
   s" "             s" LEAVE"  s" hb mispair bare-leave"  REJECT
   FORGE
   LEGAL
   s" PASS: loop family over a wrong/absent DO opener fails closed rc70 (no SIGSEGV)" type cr ;

;package

package RUNTIME-REGRESSION


: GE-SET-CHECK-NEG ( -- )
   \ set-check is fail-closed at install (dot habu-stdlib-check-hook-fd883aea): a
   \ non-zero argument outside the live JIT code window [DBASE, CP) dies with a
   \ NAMED rc-70 diagnostic instead of BLRing into garbage at the next publish.
   \ 1 (below DBASE) and `dbase@ HOOK-CELL + @` (a code word mis-read from the
   \ wrong CODE base) are the two RCA shapes; both must exit 70, never signal.
   GE-HB-RESET
   GE-SRC-RESET s" 1 set-check" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" hb set-check tiny-xt rc" GE-EXPECT-RC
   s" set-check: invalid checker xt" s" hb set-check tiny-xt diag" GE-EXPECT-ERR-HAS
   GE-HB-RESET
   GE-SRC-RESET s" dbase@ $1B0 + @ set-check" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" hb set-check dbase-garbage rc" GE-EXPECT-RC
   s" set-check: invalid checker xt" s" hb set-check dbase-garbage diag" GE-EXPECT-ERR-HAS
   s" PASS: set-check fail-closed on garbage xt (rc 70, named diagnostic)" type cr ;

create GE-CF-BODY GE-SRC-CAP allot
variable GE-CF-BODY-U

: GE-CF-BODY-RESET ( -- )
   0 GE-CF-BODY-U ! ;

: GE-CF-BODY+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   GE-CF-BODY-U @ u + GE-SRC-CAP > if E-STR-CAPACITY throw then
   a GE-CF-BODY GE-CF-BODY-U @ + u BYTE-COPY
   GE-CF-BODY-U @ u + GE-CF-BODY-U ! ;

: GE-CF-BODY$ ( -- ptr u8 n )
   GE-CF-BODY GE-CF-BODY-U @ ;

\ Build ": DEEP ( -- ) " + n * "0 0= if " + n * "then " + " ;" — n balanced,
\ nested IF/THEN openers, each fed a real bool so a checkable depth certifies.
: GE-CF-NEST ( n -- ) {: n:n :}
   GE-CF-BODY-RESET
   s" : DEEP ( -- ) " GE-CF-BODY+
   n 0 ?do s" 0 0= if " GE-CF-BODY+ loop
   n 0 ?do s" then " GE-CF-BODY+ loop
   s"  ;" GE-CF-BODY+ ;

: GE-CF-OVERCAP-1 ( n ptr u8 n -- ) {: n:n label:ptr labelu:n :}
   \ Plain stdin: a definition nesting control flow past CFSTK-DEPTH-MAX must fail
   \ closed rc 70 with the named engine diagnostic + the offending opener token,
   \ NEVER a SIGABRT/SIGSEGV register dump. Root cause (dot habu-cap-native-
   \ control-a5669829): LCFPUSH had no overflow cap, so the depth-(cap) record
   \ spilled past [CFSTK-OFF, DICT-SIZE) into the JIT code area above it — the
   \ opposite-direction sibling of the LCFPOP orphan-underflow crash. LCFPUSH now
   \ guards depth == CFSTK-DEPTH-MAX and rejects like the orphan closer.
   GE-HB-RESET
   n GE-CF-NEST
   GE-CF-BODY$ RUNTIME-RUNNER:SOURCE
   70 label labelu GE-EXPECT-RC
   s" control-flow nesting too deep" label labelu GE-EXPECT-ERR-HAS
   s" if" label labelu GE-EXPECT-ERR-HAS
   s" habu-crash" label labelu GE-EXPECT-ERR-LACKS ;

: GE-CF-DEPTH-CAP ( -- )
   \ The over-cap battery. cap+1 is the exact overflow edge; a former hard-crash
   \ depth well past it both fail closed rc 70 with the diagnostic and no register
   \ dump. The region-full depth (exactly CFSTK-DEPTH-MAX records fit) is the
   \ checker's non-certified reject, NOT a cap reject — proving the native cap is
   \ the region edge. The checker's max checkable depth still compiles rc 0. The
   \ over-cap reject is catchable under evaluate (RC-REJECT 70 via LEVALREC).
   CFSTK-DEPTH-MAX 1 +  s" hb cf-cap plus1"      GE-CF-OVERCAP-1
   CFSTK-DEPTH-MAX 50 + s" hb cf-cap was-crash"  GE-CF-OVERCAP-1
   GE-HB-RESET
   CFSTK-DEPTH-MAX GE-CF-NEST
   GE-CF-BODY$ RUNTIME-RUNNER:SOURCE
   70 s" hb cf-cap region-full rc" GE-EXPECT-RC
   s" control-flow nesting too deep" s" hb cf-cap region-full not-cap" GE-EXPECT-ERR-LACKS
   s" habu-crash" s" hb cf-cap region-full no-crash" GE-EXPECT-ERR-LACKS
   GE-HB-RESET
   31 GE-CF-NEST
   GE-CF-BODY$ RUNTIME-RUNNER:SOURCE
   s" hb cf-cap legal-31" GE-EXPECT-OK
   CFSTK-DEPTH-MAX 1 + GE-CF-NEST
   GE-CF-BODY$ GE-EVAL-CATCH-RUN
   s" hb cf-cap eval-catch rc" GE-EXPECT-OK
   s" 70" s" hb cf-cap eval-catch code" GE-EXPECT-OUT-HAS
   s" control-flow nesting too deep" s" hb cf-cap eval-catch diag" GE-EXPECT-ERR-HAS
   s" PASS: control-flow depth cap fail-closed rc70 (no overflow, catchable)" type cr ;

: GE-RXE-CATCH-USABLE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n code:ptr codeu:n diag:ptr diagu:n :}
   \ dot habu-raw-exit-compile: one recoverable compile-error misuse evaluated
   \ under the TCE-CATCH wrapper. The die site (C-DUP-DEF-FAIL / C-PACKAGE-FAIL /
   \ C-LBRACE-DIE / C-LOCAL-REF / C-DIE-DOES) used to NR-EXIT-GROUP; it now writes
   \ its diagnostic to fd 2 and routes through LCOMPILEDIE, so inside evaluate the
   \ aborted compile is a catchable throw of its sysexits code. The caught code
   \ prints to stdout, then a FRESH definition (GEC-RXOK -> 12321) compiles and
   \ runs, proving the eval-frame rollback (input cursor + CP/NDICT truncation,
   \ HIDX skipping the stale rolled-back records) left a usable session. Exit 0,
   \ never a SIGBUS/register dump.
   GE-HB-RESET
   GE-EVAL-CATCH-SRC
   src srcu GE-SRC-S"
   s"  GEC-CATCH . cr" GE-SRC-LINE
   s" : GEC-RXOK ( -- n ) 12321 ; GEC-RXOK . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb rawexit eval-catch usable" GE-EXPECT-OK
   code codeu s" hb rawexit eval-catch code" GE-EXPECT-OUT-HAS
   s" 12321" s" hb rawexit eval-catch session-usable" GE-EXPECT-OUT-HAS
   diag diagu s" hb rawexit eval-catch diag" GE-EXPECT-ERR-HAS
   s" habu-crash" s" hb rawexit eval-catch no-crash" GE-EXPECT-ERR-LACKS ;

: GE-RXE-TOP ( ptr u8 n n ptr u8 n -- )
   {: src:ptr srcu:n rc:n diag:ptr diagu:n :}
   \ Same misuse at top level (EVALD==0, no eval frame): the recovery route only
   \ ADDS the inside-evaluate catch, so the fail-closed sysexits exit + diagnostic
   \ stay byte-identical to before the conversion.
   GE-HB-RESET
   GE-SRC-RESET
   src srcu GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   rc s" hb rawexit top-level rc" GE-EXPECT-RC
   diag diagu s" hb rawexit top-level diag" GE-EXPECT-ERR-HAS
   s" habu-crash" s" hb rawexit top-level no-crash" GE-EXPECT-ERR-LACKS ;

: GE-RAWEXIT-RECOVER ( -- )
   \ dot habu-raw-exit-compile: runtime-compiler die sites that used to
   \ NR-EXIT-GROUP now route through the shared LCOMPILEDIE tail — recoverable as
   \ a catchable throw of their sysexits code inside evaluate (eval frame rolled
   \ back, session usable), byte-identical fail-closed exit + diagnostic at top
   \ level. One representative misuse per converted family. Dict/code overflow
   \ (76/77) share the SAME LCOMPILEDIE tail; their top-level 77 contract is gated
   \ by GE-DICT-FULL, and the dup-def case here exercises the identical
   \ rollback-past-a-published-record + HIDX stale-tolerance path at unit cost.
   s" : RXF ( -- n ) 1 ; : RXF ( -- n ) 2 ;" s" 78" s" duplicate definition:" GE-RXE-CATCH-USABLE
   s" : RXF ( -- n ) 1 ; : RXF ( -- n ) 2 ;" 78 s" duplicate definition:" GE-RXE-TOP
   s" public" s" 75" s" public" GE-RXE-CATCH-USABLE
   s" public" 75 s" public" GE-RXE-TOP
   s" : RXQLB ( -- ) [: {: a :} a ;] drop ;" s" 75" s" local cannot be inside quotation" GE-RXE-CATCH-USABLE
   s" : RXQLB ( -- ) [: {: a :} a ;] drop ;" 75 s" local cannot be inside quotation" GE-RXE-TOP
   s" : RXQLR ( n -- ) {: myloc :} [: myloc drop ;] drop ;" s" 75" s" myloc" GE-RXE-CATCH-USABLE
   s" : RXQLR ( n -- ) {: myloc :} [: myloc drop ;] drop ;" 75 s" myloc" GE-RXE-TOP
   s" : RXMK ( -- ) create does> ( -- n ) ;" s" 70" s" does>" GE-RXE-CATCH-USABLE
   s" : RXMK ( -- ) create does> ( -- n ) ;" 70 s" does>" GE-RXE-TOP
   s" PASS: recoverable compile errors catchable inside evaluate (session usable) + fail-closed at top level" type cr ;

\ --- dot habu-convert-residual-compile-f460b9f2: residual compile-die conversions ---
\ The out-of-inventory recoverable die sites (J-DOES/J-QUOT/J-SEMIQUOT 75,
\ C-SIG-BAD 76, C-DEFER-DIE-TOKEN, C-QUOTE-EOF 74, counted-string 76,
\ and export-undefined 70) now route through the same LCOMPILEDIE tail: catchable
\ inside evaluate, byte-identical fail-closed at top level. C-LBRACE-STORE-ONE's
\ 65th-local refusal is a rejected definition since dot
\ habu-report-the-local-d20f243a: a named label plus the token, rc 70 on both legs.

create GE-RXE-TML-BUF 512 allot   variable GE-RXE-TML-U

: GE-RXE-TML-BUILD ( -- )   \ ": RXTML ( -- ) {: t0 t1 ... t64 :} ;" (65 locals: one over the 64 cap) into GE-RXE-TML-BUF via the GE-SRC scratch
   GE-SRC-RESET
   s" : RXTML ( -- ) {:" GE-SRC+
   65 0 ?do  GE-SRC-SP  s" t" GE-SRC+  i GE-SRC-U+  loop
   s"  :} ;" GE-SRC+
   GE-SRC-U @ GE-RXE-TML-U !
   GE-SRC-BUF GE-RXE-TML-BUF GE-RXE-TML-U @ BYTE-COPY ;

: GE-RXE-TML$ ( -- ptr u8 n )  GE-RXE-TML-BUF GE-RXE-TML-U @ ;

: GE-RXE-BS ( -- )  $5C GE-SRC-C ;                     \ backslash byte into the source builder

: GE-RXE-ESC-OPEN ( -- )                               \ append the `s\" ` escaped-string opener + delimiter space
   [char] s GE-SRC-C  GE-RXE-BS  GE-DQ GE-SRC-C  GE-SRC-SP ;

\ --- dot habu-report-the-local-d20f243a: a bare local name wider than the 16-byte
\ name field of a LOC-REC is refused by name before it is stored. The store used
\ to copy the whole name over the following record, so a second local overwrote
\ byte 17 and the engine reported the FIRST local as E-UNDEFINED; a long name in
\ the last of the 64 records wrote past LOCNAMES. The refusal is a rejected
\ definition (rc 70, catchable inside evaluate), never a crash or an undefined word.
: GE-LOC-WIDE-TOP ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   src srcu GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 label labelu GE-EXPECT-RC
   s" hb: local name over 16 bytes: abcdefghijklmnopq:n" label labelu GE-EXPECT-ERR-HAS
   s" E-UNDEFINED" label labelu GE-EXPECT-ERR-LACKS
   s" habu-crash" label labelu GE-EXPECT-ERR-LACKS ;

create GE-LOC-LAST-BUF 1024 allot   variable GE-LOC-LAST-U

: GE-LOC-LAST-BUILD ( -- )   \ ": RXLL ( -- ) {: t0 ... t62 abcdefghijklmnopq:n :} ;" the wide name in the last record
   GE-SRC-RESET
   s" : RXLL ( -- ) {:" GE-SRC+
   63 0 ?do  GE-SRC-SP  s" t" GE-SRC+  i GE-SRC-U+  loop
   s"  abcdefghijklmnopq:n :} ;" GE-SRC+
   GE-SRC-U @ GE-LOC-LAST-U !
   GE-SRC-BUF GE-LOC-LAST-BUF GE-LOC-LAST-U @ BYTE-COPY ;

: GE-LOCAL-NAME-WIDTH ( -- )
   s" : RXLW ( n -- n ) {: abcdefghijklmnopq:n :} abcdefghijklmnopq ;"
      s" hb local-width top-level" GE-LOC-WIDE-TOP
   s" : RXLW2 ( n n -- n ) {: abcdefghijklmnopq:n b:n :} abcdefghijklmnopq b + ;"
      s" hb local-width first-of-two" GE-LOC-WIDE-TOP
   GE-LOC-LAST-BUILD
   GE-LOC-LAST-BUF GE-LOC-LAST-U @ s" hb local-width last-record" GE-LOC-WIDE-TOP
   s" : RXLW ( n -- n ) {: abcdefghijklmnopq:n :} abcdefghijklmnopq ;" s" 70"
      s" local name over 16 bytes: abcdefghijklmnopq:n" GE-RXE-CATCH-USABLE
   GE-HB-RESET
   GE-SRC-RESET
   s" : RXL16 ( n -- n ) {: abcdefghijklmnop:n :} abcdefghijklmnop ;" GE-SRC-LINE
   s" 3 RXL16 . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb local-width 16-byte control" GE-EXPECT-OK
   s" 3" s" hb local-width 16-byte control output" GE-EXPECT-OUT-HAS
   s" PASS: local name over 16 bytes refused by name (rc 70, catchable, no E-UNDEFINED relabel, no crash)" type cr ;

\ counted-string >255 (C-ICQ/C-EICQ/C-CQ/C-ECQ). This cap now carries a named fd-2
\ label ("hb: counted string too long (max 255)", dot habu-recovery-pkg-scope-e0bd98e2)
\ that disambiguates the 76 it shares with C-SIG-BAD; the assertions add that label on
\ both the eval-catch and top-level legs. The evaluate target `c" <256 A>"` embeds a "
\ so it is passed through an s\" wrapper with \q-escaped quotes.
: GE-RXE-CSTR-CATCH ( -- )
   GE-HB-RESET
   GE-EVAL-CATCH-SRC
   GE-RXE-ESC-OPEN                                      \ s\"
   [char] c GE-SRC-C  GE-RXE-BS  [char] q GE-SRC-C  GE-SRC-SP   \ c\q (-> c" ) + delimiter
   256 [char] A GE-SRC-REPEAT-C
   GE-RXE-BS  [char] q GE-SRC-C  GE-DQ GE-SRC-C         \ \q closes the counted string; " closes the s\" wrapper
   s"  GEC-CATCH . cr" GE-SRC-LINE
   s" : GEC-RXOK ( -- n ) 12321 ; GEC-RXOK . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb rxe cstr eval-catch usable" GE-EXPECT-OK
   s" 76" s" hb rxe cstr eval-catch code" GE-EXPECT-OUT-HAS
   s" 12321" s" hb rxe cstr eval-catch session-usable" GE-EXPECT-OUT-HAS
   s" counted string too long" s" hb rxe cstr eval-catch label" GE-EXPECT-ERR-HAS
   s" habu-crash" s" hb rxe cstr eval-catch no-crash" GE-EXPECT-ERR-LACKS ;

: GE-RXE-CSTR-TOP ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   [char] c GE-SRC-C  GE-DQ GE-SRC-C  GE-SRC-SP         \ c" + delimiter
   256 [char] A GE-SRC-REPEAT-C
   GE-DQ GE-SRC-C  GE-SRC-LF
   RUNTIME-RUNNER:BUFFER
   76 s" hb rxe cstr top rc" GE-EXPECT-RC
   s" counted string too long" s" hb rxe cstr top label" GE-EXPECT-ERR-HAS
   s" habu-crash" s" hb rxe cstr top no-crash" GE-EXPECT-ERR-LACKS ;

\ unterminated string literal (C-QUOTE-EOF). The evaluate target `s" abc` (no
\ closing quote) is s\"-wrapped so its embedded " does not close the wrapper.
: GE-RXE-QEOF-CATCH ( -- )
   GE-HB-RESET
   GE-EVAL-CATCH-SRC
   GE-RXE-ESC-OPEN                                      \ s\"
   [char] s GE-SRC-C  GE-RXE-BS  [char] q GE-SRC-C  GE-SRC-SP   \ s\q (-> s" ) + delimiter
   s" abc" GE-SRC+  GE-DQ GE-SRC-C                      \ abc" : the target `s" abc` is unterminated; " closes the wrapper
   s"  GEC-CATCH . cr" GE-SRC-LINE
   s" : GEC-RXOK ( -- n ) 12321 ; GEC-RXOK . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb rxe qeof eval-catch usable" GE-EXPECT-OK
   s" 74" s" hb rxe qeof eval-catch code" GE-EXPECT-OUT-HAS
   s" 12321" s" hb rxe qeof eval-catch session-usable" GE-EXPECT-OUT-HAS
   s" bad string literal" s" hb rxe qeof eval-catch diag" GE-EXPECT-ERR-HAS
   s" habu-crash" s" hb rxe qeof eval-catch no-crash" GE-EXPECT-ERR-LACKS ;

: GE-RXE-QEOF-TOP ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   [char] s GE-SRC-C  GE-DQ GE-SRC-C  GE-SRC-SP  s" abc" GE-SRC+  GE-SRC-LF   \ `s" abc` unterminated
   RUNTIME-RUNNER:BUFFER
   74 s" hb rxe qeof top rc" GE-EXPECT-RC
   s" bad string literal" s" hb rxe qeof top diag" GE-EXPECT-ERR-HAS
   s" habu-crash" s" hb rxe qeof top no-crash" GE-EXPECT-ERR-LACKS ;

: GE-RAWEXIT-RESIDUAL ( -- )
   \ One caught-inside-evaluate + top-level pair per converted site. The eval-catch
   \ leg proves catchable code + usable session (GEC-RXOK -> 12321); the top leg
   \ proves byte-identical fail-closed exit + diagnostic.
   s" : RXDOES ( -- ) create 1 {: v :} does> ( -- ) ;" s" 75" s" does>" GE-RXE-CATCH-USABLE
   s" : RXDOES ( -- ) create 1 {: v :} does> ( -- ) ;" 75 s" does>" GE-RXE-TOP
   s" : RXQ ( -- ) [: [: 5 ;] drop ;] drop ;" s" 75"
      s" hb: a quotation may not open inside a quotation: RXQ" GE-RXE-CATCH-USABLE
   s" : RXQ ( -- ) [: [: 5 ;] drop ;] drop ;" 75
      s" hb: a quotation may not open inside a quotation: RXQ" GE-RXE-TOP
   s" : RXSQ ( -- ) 5 ;] drop ;" s" 75" s" ;]" GE-RXE-CATCH-USABLE
   s" : RXSQ ( -- ) 5 ;] drop ;" 75 s" ;]" GE-RXE-TOP
   s" defer RXDFR badsig" s" 76" s" RXDFR" GE-RXE-CATCH-USABLE
   s" defer RXDFR badsig" 76 s" RXDFR" GE-RXE-TOP
   s" defer" s" 74" s" defer" GE-RXE-CATCH-USABLE
   s" defer" 74 s" defer" GE-RXE-TOP
   s" package RXPKG public export RXNOEXPORT ;package" s" 70" s" RXNOEXPORT" GE-RXE-CATCH-USABLE
   s" package RXPKG public export RXNOEXPORT ;package" 70 s" RXNOEXPORT" GE-RXE-TOP
   GE-RXE-TML-BUILD
   GE-RXE-TML$ s" 70" s" hb: more than 64 locals in one definition: t64" GE-RXE-CATCH-USABLE
   GE-RXE-TML$ 70 s" hb: more than 64 locals in one definition: t64" GE-RXE-TOP
   GE-RXE-QEOF-CATCH
   GE-RXE-QEOF-TOP
   GE-RXE-CSTR-CATCH
   GE-RXE-CSTR-TOP
   s" PASS: residual compile dies recover inside evaluate + fail-closed at top level" type cr ;

\ A quotation opened while another is open (dot habu-name-the-nested-6a8e1b28).
\ The engine compiles at most one quotation per definition: QPATCH-CELL holds the
\ single `b-over` placeholder J-SEMIQUOT patches, so a second `[:` has nowhere to
\ record its own. J-QUOT refused it by echoing the CURRENT TOKEN to fd 2 — the
\ two bytes `[:`, no label, no newline, no definition, no reason — and exiting
\ 75 (measured on release 5a3d9f82 by the forth-card lane).
\ The refusing layer is the engine, not the checker: CF-QUOT/CF-SEMIQ keep a
\ QDEPTH counter and a CFS frame per open quotation (src/core/checker.f), so the
\ one-at-a-time limit is the engine's single cell and not a checker rule, and the
\ compile aborts before any check runs. Measured both ways on the same program:
\ `tools/check.f --json-errors --all-errors` printed the same two bytes and the
\ same 75 the load path did — the tool runs the program through the engine — so
\ this refusal carries a prose line and no JSON code.
\ The line now names the rule and the definition it aborted, through the same
\ LCOMPILEDIE tail EM-SNAP-NEST-DIE uses: same exit code, catchable inside
\ evaluate (the eval-catch leg is in GE-RAWEXIT-RESIDUAL above), fail-closed
\ exit 75 at top level.
: GE-QUOT-NEST ( -- )
   \ control: two SEQUENTIAL quotations in one definition still compile and run,
   \ so the refusal is the nest and not the second `[:` of a definition
   s" : GEQSEQ ( -- n ) [: 1 ;] execute [: 2 ;] execute + ; GEQSEQ . cr" 0
      s" sequential quotations compile" RUNTIME-RUNNER:LINE-RC
   s" 3" s" sequential quotations run" GE-EXPECT-OUT-HAS
   s" : GEQNEST ( -- n ) [: [: 1 ;] execute ;] execute ;" 75
      s" nested-quotation exit rc" RUNTIME-RUNNER:LINE-RC
   s" hb: a quotation may not open inside a quotation: GEQNEST"
      s" nested-quotation exit diagnostic" GE-EXPECT-ERR-HAS
   s" habu-crash" s" nested-quotation no-crash" GE-EXPECT-ERR-LACKS
   s" PASS: a quotation opened inside a quotation is named with its definition" type cr ;

\ --- Package-scope rollback across compile-error recovery (dot habu-recovery-pkg-scope-e0bd98e2) ---
\ A compile error aborting an in-package definition must roll the OPEN-PACKAGE scope
\ back to the boundary scope, exactly like CP/NDICT/DP. Before the fix the package
\ stayed dangling open and later top-level defines landed in it silently.

: GE-PKGSCOPE-EVAL-CLOSED ( -- )
   \ Package opened INSIDE the failing evaluate: caught, and the eval-frame recovery
   \ restores the evaluate-entry scope (global), so the package is CLOSED. Discriminator:
   \ a dangling package would make the following top-level `package` nest-fail (exit 75
   \ mid-batch, so GEC-RXOK never prints); a restored global scope opens+closes it and
   \ reaches the fresh global define.
   GE-HB-RESET
   GE-EVAL-CATCH-SRC
   s" package PKX public export PKNOPE ;package" GE-SRC-S"
   s"  GEC-CATCH . cr" GE-SRC-LINE
   s" package PKY ;package" GE-SRC-LINE
   s" : GEC-RXOK ( -- n ) 12321 ; GEC-RXOK . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb pkgscope eval-closed rc" GE-EXPECT-OK
   s" 70" s" hb pkgscope eval-closed code" GE-EXPECT-OUT-HAS
   s" 12321" s" hb pkgscope eval-closed scope-restored-global" GE-EXPECT-OUT-HAS
   s" habu-crash" s" hb pkgscope eval-closed no-crash" GE-EXPECT-ERR-LACKS ;

: GE-PKGSCOPE-CHECKER-RESYNC ( -- )
   \ With NO intervening package op between the caught error and a checked reference:
   \ the bare def GEC-W1 must record in GLOBAL checker scope, and the later checked
   \ GEC-W2 (after a real package op that would orphan a mis-recorded PKX:GEC-W1) must
   \ still resolve. An engine-only rollback (checker left stale-PKX) rc70s at GEC-W2.
   GE-HB-RESET
   GE-EVAL-CATCH-SRC
   s" package PKX public export PKNOPE ;package" GE-SRC-S"
   s"  GEC-CATCH . cr" GE-SRC-LINE
   s" : GEC-W1 ( -- n ) 321 ;" GE-SRC-LINE
   s" package PKZ ;package" GE-SRC-LINE
   s" : GEC-W2 ( -- n ) GEC-W1 ; GEC-W2 . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb pkgscope checker-resync rc" GE-EXPECT-OK
   s" 321" s" hb pkgscope checker-resync resolves-global" GE-EXPECT-OUT-HAS ;

: GE-PKGSCOPE-EVAL-STAYS ( -- )
   \ Package legitimately open at evaluate ENTRY must NOT be closed by recovery: the
   \ eval-frame rollback restores the evaluate-entry scope. AA open at top level; the
   \ failing string does not touch packages, so after the caught error AA is still open
   \ (AA:BEFOREW / AA:AFTERW both resolve public) and `;package` closes it cleanly.
   GE-HB-RESET
   GE-EVAL-CATCH-SRC
   s" package AA public" GE-SRC-LINE
   s" : BEFOREW ( -- n ) 11 ;" GE-SRC-LINE
   s" : FOO ( -- ) NOPEWORD ;" GE-SRC-S"
   s"  GEC-CATCH . cr" GE-SRC-LINE
   s" : AFTERW ( -- n ) 22 ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" AA:BEFOREW AA:AFTERW + . cr" GE-SRC-LINE     \ 11+22=33 proves both landed in AA:public (`.` is newline-terminated here)
   RUNTIME-RUNNER:BUFFER
   s" hb pkgscope eval-stays rc" GE-EXPECT-OK
   s" 70" s" hb pkgscope eval-stays code" GE-EXPECT-OUT-HAS
   s" 33" s" hb pkgscope eval-stays package-stays-open" GE-EXPECT-OUT-HAS ;

: GE-PKGSCOPE-TOP-EXIT ( -- )
   \ Top-level (EVALD==0, no eval frame, no handler): an in-package compile error is
   \ still a fail-closed exit — package-scope rollback does not change the exit path.
   GE-HB-RESET
   GE-SRC-RESET
   s" package PKT public export PKTNOPE ;package" GE-SRC-LINE
   s" : NEVER ( -- n ) 999 ; NEVER . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   \ process exits at the export error before NEVER/999 (fail-closed)
   70 s" hb pkgscope top-exit fail-closed rc" GE-EXPECT-RC ;

: GE-PKGSCOPE-RECOVERY ( -- )
   GE-PKGSCOPE-EVAL-CLOSED
   GE-PKGSCOPE-CHECKER-RESYNC
   GE-PKGSCOPE-EVAL-STAYS
   GE-PKGSCOPE-TOP-EXIT
   s" PASS: package scope rolls back on compile-error recovery (closed/stays-open/checker/top-exit)" type cr ;


\ Every load refusal names its file and line (dot habu-name-the-file-70acbf10).
\ `hb: bad string literal` — and every other refusal that reaches the LCOMPILEDIE
\ tail — used to print no path and no line, so a reader bisected the file by hand.
\ One place prints the location now: the tail writes ` at <path>:<line>` whenever
\ a source file is open and the newline in every case, so a die site cannot
\ forget it and cannot spell it differently. The path is the INNERMOST open
\ include frame's (src/core/include.f publishes it), which is what makes the
\ nested case below the one worth pinning; the line counts newlines from the
\ start of the buffer being evaluated, which for an include is the whole file.
: GE-LOC-FIXTURE ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n ta:ptr tu:n :}
   GT-ROOT na nu GE-SCRIPT-PATH JOIN-PATH GE-SCRIPT-U !
   GE-SCRIPT-PATH GE-SCRIPT-U @ ta tu WRITE-ALL ;

\ `--load <file>`, the real entry a reader uses, so the refusal travels the
\ include path this change publishes from. Feeding the same bytes on stdin (the
\ no-file case below) deliberately does not.
: GE-LOC-LOAD ( -- )
   GE-HB-RESET
   s" --load" GE-ARG+
   GE-SCRIPT-PATH GE-SCRIPT-U @ GE-ARG+
   s" --" GE-ARG+
   GE-HB$ GE-TIMEOUT-MS GE-RUN-ENV ;

: GE-LOC-BADSTR ( -- )
   \ The refusal is on line 3 of its own file; the two comment lines above it are
   \ what a line number has to count.
   s" hb-loc-badstr.f"
      S\" \\ one\n\\ two\n: GELOCBAD ( -- ) S\\\q a\\u \\\q drop drop ;\n" GE-LOC-FIXTURE
   GE-LOC-LOAD
   74 s" bad string literal rc" GE-EXPECT-RC
   s" hb: bad string literal at " s" bad string literal names its file" GE-EXPECT-ERR-HAS
   s" hb-loc-badstr.f:3" s" bad string literal names its line" GE-EXPECT-ERR-HAS ;

: GE-LOC-NESTED ( -- )
   \ A requires B and B holds the refusal: the location is B's, not the entry's.
   s" hb-loc-inner.f"
      S\" \\ inner\n: GELOCINNER ( -- ) S\\\q b\\u \\\q drop drop ;\n" GE-LOC-FIXTURE
   s" hb-loc-outer.f"
      S\" \\ outer\nrequire hb-loc-inner.f\n" GE-LOC-FIXTURE
   GE-LOC-LOAD
   74 s" nested include refusal rc" GE-EXPECT-RC
   s" hb-loc-inner.f:2" s" nested refusal names the inner file" GE-EXPECT-ERR-HAS
   s" hb-loc-outer.f" s" nested refusal does not name the entry" GE-EXPECT-ERR-LACKS ;

: GE-LOC-NO-FILE ( -- )
   \ The same program with no file open (source on stdin, the REPL's own path):
   \ the message and a newline, and no location to invent.
   GE-HB-RESET
   GE-SRC-RESET
   S\" : GELOCSTDIN ( -- ) S\\\q c\\u \\\q drop drop ;" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   74 s" stdin refusal rc" GE-EXPECT-RC
   s" hb: bad string literal" s" stdin refusal still names the cause" GE-EXPECT-ERR-HAS
   s"  at " s" stdin refusal claims no location" GE-EXPECT-ERR-LACKS ;

: GE-REFUSAL-LOCATION ( -- )
   GE-LOC-BADSTR
   GE-LOC-NESTED
   GE-LOC-NO-FILE
   s" PASS: load refusals name their file and line (file/nested/no-file)" type cr ;

public

: RUN ( -- )
   GE-UNCAUGHT-THROW
   GE-INTERP-LAYOUT
   WIDE-FETCH:RUN
   GE-DICT-FULL
   GE-BODY-CAP
   GE-BEGIN-NEST
   GE-DATA-FULL
   GE-DIV-MOD
   GE-PROCESS-PTY
   GE-DEREF-ARITY-DIAG
   GE-NESTED-CHECKED-DEF
   GE-NESTED-BAD-DEF
   GE-EVAL-UNDEF-RECOVER
   GE-EVAL-INTERP-ERR-RECOVER
   GE-EVAL-DEF-REJECT-CATCH
   GE-ORPHAN-CLOSER
   LOOP-OPENER:RUN
   GE-CF-DEPTH-CAP
   GE-RAWEXIT-RECOVER
   GE-RAWEXIT-RESIDUAL
   GE-QUOT-NEST
   GE-LOCAL-NAME-WIDTH
   GE-PKGSCOPE-RECOVERY
   GE-REFUSAL-LOCATION
   GE-SET-CHECK-NEG ;

: TEST ( -- )
   s" habu-runtime-regression" GT-START
   RUN
   GT-CLEANUP
   s" runtime-regression-test: ok" type cr ;

;package

RUNTIME-REGRESSION:TEST
