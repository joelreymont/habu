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

\ die writes its message as ONE LINE and exits: the span, then exactly one
\ newline (src/habu/habu1.f BDIE). An EMPTY span writes NOTHING, so a site that
\ already ended its own report with `cr` passes `s" "` and adds no blank line.
\ The rule is what keeps a parent that reports the child's exit on the same
\ stream from continuing the child's last line (dot habu-die-writes-its).
: GE-DIE-LINE ( -- )
   S\" s\q boom\q 7 die" 7 s" die exits with its own rc" RUNTIME-RUNNER:LINE-RC
   S\" boom\n" s" die ends its message with one newline" GE-EXPECT-ERR
   S\" s\q \q 7 die" 7 s" empty die span exits with its own rc" RUNTIME-RUNNER:LINE-RC
   s" " s" an empty die span writes nothing" GE-EXPECT-ERR
   S\" s\q report line\q type cr s\q \q 74 die" 74
      s" reported die exits with its own rc" RUNTIME-RUNNER:LINE-RC
   S\" report line\n" s" a report ending in cr keeps its one line" GE-EXPECT-OUT
   s" " s" a reported die adds no second line" GE-EXPECT-ERR
   s" PASS: die writes one line; an empty span writes nothing" type cr ;

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

\ A definer may create through another definer and replace its clause. The
\ newest signature owns the record's min-in and wide facts, at either tier.
: GE-DOES-REPATCH ( -- )
   2 0 do
      GE-HB-RESET GE-SRC-RESET
      GE-ILAYOUT-PRELUDE
      i GE-SRC-U+ s"  set-tier" GE-SRC-LINE
      s" : GE-INNER: ( -- ) create 7 , does> ( n n -- n ) @ + + ;" GE-SRC-LINE
      s" : GE-ONE: ( -- ) GE-INNER: does> ( n -- n ) @ + ;" GE-SRC-LINE
      s" : GE-ZERO: ( -- ) GE-INNER: does> ( -- n ) @ ;" GE-SRC-LINE
      s" : GE-WIDE: ( -- ) create 7 , does> ( -- gewide<n,n> ) drop GE-WMK ;" GE-SRC-LINE
      s" : GE-NARROW: ( -- ) GE-WIDE: does> ( -- n ) @ ;" GE-SRC-LINE
      s" : GE-EMPTY: ( -- ) GE-WIDE: does> ( -- ptr n ) ;" GE-SRC-LINE
      s" GE-ONE: GE-A GE-ZERO: GE-B GE-NARROW: GE-C GE-EMPTY: GE-D" GE-SRC-LINE
      s" : GE-READ ( -- n ) GE-D @ ;" GE-SRC-LINE
      s" 5 GE-A . GE-B . GE-C . GE-READ ." GE-SRC-LINE
      RUNTIME-RUNNER:BUFFER
      s" repeated does> replaces min-in and wide facts" GE-EXPECT-OK
      SB-RESET s" 12" SB-APPEND GE-SB-LF
      s" 7" SB-APPEND GE-SB-LF s" 7" SB-APPEND GE-SB-LF
      s" 7" SB-APPEND GE-SB-LF
      SB$ s" repeated does> output at both tiers" GE-EXPECT-OUT
   loop ;

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
   GE-DOES-REPATCH
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

\ The fetched value is dropped, so a lowering that elided an unused load would
\ take the tag walk with it and print the marker.
: BAD-DROP ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   SRC
   S\" : GO ( -- ) 1 GWN-BAD 1 GWN-AT @ drop s\" dropped-forge: marker\" type ;  GO" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   ENGINE-ERROR:BAD-TAG s" dropped forged tag dies with ENGINE-ERROR:BAD-TAG" GE-EXPECT-RC
   s" hb: bad layout tag" s" dropped forged tag reaches the fetch guard" GE-EXPECT-ERR-HAS
   s" " s" dropped forged tag prints no marker" GE-EXPECT-OUT ;

public

: RUN ( -- )
   ROUND
   BAD-WIDE
   BAD-NARROW
   BAD-DROP
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
\ closed NAMED "hb: data space out of range: DP <dp> of <cap> bytes" on fd 2 (catchable
\ rc-76 throw inside evaluate, exit 76 at top level). RED discriminator: on the unfixed
\ base an allot one byte INTO the band SUCCEEDS silently (rc 0, no message) and clobbers
\ a counter. `data-base`/`DATA-SIZE`/`PROF-CNT-BYTES`/`here` are runtime words, so the
\ boundary is computed against the live band base.
\ The two numbers are the whole point of the line (dot habu-name-the-ceiling-98ca1f47:
\ the label alone made a consumer rebuild the engine to read its own ceiling), so both
\ rows assert the WHOLE of stderr against the line composed from the same constants.
\ The child reads its program from stdin, so no source file is open and the LCOMPILEDIE
\ tail ends the line with the newline alone; a file-loaded refusal ends ` at <path>:<line>`.
: GE-DATA-DIAG$ ( n -- ptr u8 n ) {: dp:n :}
   SB-RESET
   s" hb: data space out of range: DP " SB-APPEND
   dp FMT:SB-U
   s"  of " SB-APPEND
   DATA-SIZE PROF-CNT-BYTES - FMT:SB-U
   s"  bytes" SB-APPEND
   GE-SB-LF
   SB$ ;

: GE-DATA-FULL ( -- )
   \ one byte past the band base rejects (base: succeeds silently — the RED discriminator).
   \ The refused DP is the one the program asked for: `here` is the DP the allot starts
   \ from and the count carries it to cap + 1, so the line reads DP cap+1 of cap bytes.
   s" data-base DATA-SIZE PROF-CNT-BYTES - + here - 1+ allot"
      76 s" data-space over-band exit rc" RUNTIME-RUNNER:LINE-RC
   DATA-SIZE PROF-CNT-BYTES - 1+ GE-DATA-DIAG$
      s" data-space over-band diagnostic" GE-EXPECT-ERR
   \ allot ending EXACTLY at the band base (DP == DATA + DATA-SIZE - PROF-CNT-BYTES, the
   \ max the <= bound admits) must SUCCEED and exit clean.
   s" data-base DATA-SIZE PROF-CNT-BYTES - + here - allot"
      0 s" data-space band-base allot succeeds" RUNTIME-RUNNER:LINE-RC
   \ the definer sink states ITS candidate: from the band base a `variable` advances the
   \ DP one cell (the create rounding is already a no-op at the cell-aligned base), so the
   \ same line reports cap + 8. A second sink, a second register carrying the candidate.
   s" data-base DATA-SIZE PROF-CNT-BYTES - + here - allot  variable GEDVAR"
      76 s" data-space definer-sink exit rc" RUNTIME-RUNNER:LINE-RC
   DATA-SIZE PROF-CNT-BYTES - 8 + GE-DATA-DIAG$
      s" data-space definer-sink diagnostic" GE-EXPECT-ERR
   s" PASS: data-space cap names the refused DP and the ceiling + off-by-one boundary holds" type cr ;

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
   \ empty stack: its seed record's min-in must make the band refuse it with the
   \ underdepth reject + exit 70 before the body runs, never a signal (crash
   \ handler exit 134). Unguarded, the body faults reading below the base.
   GE-HB-RESET
   GE-SRC-RESET
   tok toku GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" hb deref-first arity rc" GE-EXPECT-RC
   s" hb: interpret stack underdepth: " s" hb deref-first arity diagnostic" GE-EXPECT-ERR-HAS
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

\ The post-token depth floor's subject. TRUSTED: states ( -- ) over a body that
\ drops, so the band admits the word on an empty stack and only the floor sees
\ the stack go below its base. A bare `drop` never reaches the floor: its seed
\ record carries min-in 1, so the band refuses it first.
: GE-SINK-LINE ( -- )
   s" TRUSTED: GE-SINK ( -- ) drop ;" GE-SRC-LINE ;

: GE-EXECUTE-FLOOR ( -- )
   \ execute-floor runs an xt and flags whether it left the stack below its
   \ base. The sink leaves it one cell below: the flag is -1, and the stack was
   \ reset to the base before the flag was pushed, so `5 .` runs on a live
   \ stack and `depth` answers 0 (an unreset push lands in the low guard page,
   \ rc 102). The no-op flags 0. The wrappers are TRUSTED: because the row is
   \ trusted-only; they are also how an interpreter loop in Habu reaches it.
   GE-HB-RESET
   GE-SRC-RESET
   GE-SINK-LINE
   s" TRUSTED: GE-SINK-FLOOR ( -- bool ) [: GE-SINK ;] execute-floor ;" GE-SRC-LINE
   s" TRUSTED: GE-NOOP-FLOOR ( -- bool ) [: ;] execute-floor ;" GE-SRC-LINE
   s" GE-SINK-FLOOR . 5 . depth . cr" GE-SRC-LINE
   s" GE-NOOP-FLOOR . depth . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb execute-floor rc" GE-EXPECT-OK
   SB-RESET
   s" -1" SB-APPEND GE-SB-LF s" 5" SB-APPEND GE-SB-LF s" 0" SB-APPEND GE-SB-LF GE-SB-LF
   s" 0" SB-APPEND GE-SB-LF s" 0" SB-APPEND GE-SB-LF GE-SB-LF
   SB$ s" hb execute-floor flags and output" GE-EXPECT-OUT
   s" " s" hb execute-floor stderr" GE-EXPECT-ERR
   s" PASS: execute-floor flags an underflowed xt and resets the stack" type cr ;

: GE-NESTED-DEF-SRC ( ptr u8 n -- ) {: body:ptr bodyu:n :}
   \ Build: : W ( -- ) s" <body>" evaluate-closed ;  then run W.
   \ W itself certifies, since `evaluate-closed` is ( ptr u8 n -- ); the
   \ definition compiled BY <body> is checked by the active hook, from inside
   \ W's execution.
   GE-SRC-RESET
   s" : W ( -- )" GE-SRC+  GE-SRC-SP
   body bodyu GE-SRC-S"
   s"  evaluate-closed ;" GE-SRC-LINE
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
   \ The nested definition is checked on its own: a bad-effect nested def
   \ compiled from inside an executing word must still be REJECTED (rc 70).
   \ Proven: BAD ( -- n ) drop is rejected at 'drop'.
   GE-HB-RESET
   s" : BAD ( -- n ) drop ;" GE-NESTED-DEF-SRC
   RUNTIME-RUNNER:BUFFER
   70 s" hb nested bad def rc" GE-EXPECT-RC
   s" bad" s" hb nested bad def word" GE-EXPECT-ERR-HAS
   s" drop" s" hb nested bad def token" GE-EXPECT-ERR-HAS
   s" PASS: nested bad-effect def rejected from inside a word" type cr ;

: GE-EVAL-UNDEF-SRC ( -- )
   \ The dot reproducer: an undefined word aborts a nested `:`-compile INSIDE
   \ `evaluate-closed`, called from GO. Mid-compile the JIT dict region is RW;
   \ the aborted definition must unwind cleanly, not fault.
   GE-SRC-RESET
   s" : GO ( -- )" GE-SRC+  GE-SRC-SP
   s" : FOO ( -- ) UNDEFINED-WORD-XYZ ;" GE-SRC-S"
   s"  evaluate-closed ;" GE-SRC-LINE ;

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
   \ a quotation catch over the closed INCLUDE-EVALUATE boundary. The caller
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
   GE-HB-RESET
   GE-EVAL-CATCH-SRC
   GE-SINK-LINE
   s" GE-SINK" GE-SRC-S"
   s"  GEC-CATCH . cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   s" hb eval underflow catch rc" GE-EXPECT-OK
   s" 70" s" hb eval underflow catch code" GE-EXPECT-OUT-HAS
   s" E-UNDERFLOW" s" hb eval underflow catch diag" GE-EXPECT-ERR-HAS
   s" GE-SINK" s" hb eval underflow catch token" GE-EXPECT-ERR-HAS
   s" PASS: interpret underflow under catch+evaluate -> caught 70" type cr ;

: GE-EVAL-UNDERFLOW-FAILCLOSED ( -- )
   \ No handler: the underflow throw escapes the eval frame and fails closed rc
   \ 70 (uncaught-throw exit), never continuing past the failed evaluate. The
   \ rollback-and-return path printed the marker and exited 0 (fail-open).
   GE-HB-RESET
   GE-SRC-RESET
   GE-SINK-LINE
   s" GE-SINK" GE-SRC-S"
   s"  INCLUDE-EVALUATE" GE-SRC-LINE
   s" s" GE-SRC+ GE-DQ GE-SRC-C s"  ALIVE-AFTER" GE-SRC+ GE-DQ GE-SRC-C
   s"  type cr" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" hb eval underflow no-catch rc" GE-EXPECT-RC
   s" E-UNDERFLOW: GE-SINK" s" hb eval underflow no-catch diag" GE-EXPECT-ERR-HAS
   s" " s" hb eval underflow no-catch dead marker" GE-EXPECT-OUT
   s" PASS: interpret underflow in evaluate w/o catch -> fail-closed rc70" type cr ;

: GE-UNDERFLOW-TOPLEVEL-UNCHANGED ( -- )
   \ Plain-stdin contract pin for the fix: top-level underflow (EVALD==0) keeps
   \ the E-UNDERFLOW diagnostic + rc 70 exactly.
   GE-HB-RESET
   GE-SRC-RESET
   GE-SINK-LINE
   s" GE-SINK" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 s" hb top-level underflow rc unchanged" GE-EXPECT-RC
   s" E-UNDERFLOW: GE-SINK" s" hb top-level underflow diag unchanged" GE-EXPECT-ERR-HAS
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

: KIND$ ( -- ptr u8 n )  s" control-flow word does not match the open structure" ;
: ORPHAN$ ( -- ptr u8 n )  s" control-flow closer without opener" ;

: REJECT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: open:ptr openu:n tok:ptr toku:n diag:ptr diagu:n label:ptr labelu:n :}
   \ Plain stdin: a loop-family word (LOOP / +LOOP / LEAVE) whose innermost open
   \ control-flow frame is not a `do` frame -- or which has no open frame at all --
   \ must fail closed rc 70 with the named engine diagnostic and the offending
   \ token, NEVER a register dump (rc 134). Root cause (dot habu-fix-loop-closer-
   \ 9e5d012e): the DO/LEAVE level stack (LVD-CELL depth + the LVH/LVF level
   \ arrays) is the loop family's opener record and LCFPOP's orphan guard does not
   \ cover it. With an IF or BEGIN frame open, LOOP/+LOOP still popped the CF stack
   \ happily and then indexed level -1 of the level stack; LVH's cell -1 IS
   \ LVD-CELL itself, so LBCHAIN was handed a junk chain head and dereferenced it.
   \ LOOP/+LOOP now refuse an entry that is not a DO's (habu2.f C-CF-AT) and
   \ LEAVE refuses with no open DO level (LVREQUIRE).
   GE-HB-RESET
   GE-SRC-RESET
   s" : XM ( -- ) " GE-SRC+
   open openu GE-SRC+
   s"  " GE-SRC+
   tok toku GE-SRC+
   s"  ;" GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   70 label labelu GE-EXPECT-RC
   diag diagu label labelu GE-EXPECT-ERR-HAS
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
   s" 1 IF drop"    s" LOOP"   KIND$   s" hb mispair if-loop"     REJECT
   s" 1 IF drop"    s" +LOOP"  KIND$   s" hb mispair if-+loop"    REJECT
   s" 1 IF"         s" LEAVE"  ORPHAN$ s" hb mispair if-leave"    REJECT
   s" 1 BEGIN drop" s" LOOP"   KIND$   s" hb mispair begin-loop"  REJECT
   s" 1 BEGIN drop" s" +LOOP"  KIND$   s" hb mispair begin-+loop" REJECT
   s" 1 BEGIN"      s" LEAVE"  ORPHAN$ s" hb mispair begin-leave" REJECT
   s" "             s" LOOP"   ORPHAN$ s" hb mispair bare-loop"   REJECT
   s" "             s" +LOOP"  ORPHAN$ s" hb mispair bare-+loop"  REJECT
   s" "             s" LEAVE"  ORPHAN$ s" hb mispair bare-leave"  REJECT
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

\ Build "variable HITS : DEEP ( -- ) " + n * opener + "1 HITS +! " + n * closer
\ + "; DEEP ...": n nested counted loops of two turns each, run once, and a
\ check that prints true when they took 2^LV-LEVELS turns.
: GE-DO-NEST ( n ptr u8 n ptr u8 n -- ) {: n:n op:ptr opu:n cl:ptr clu:n :}
   GE-CF-BODY-RESET
   s" variable HITS : DEEP ( -- ) " GE-CF-BODY+
   n 0 ?do op opu GE-CF-BODY+ loop
   s" 1 HITS +! " GE-CF-BODY+
   n 0 ?do cl clu GE-CF-BODY+ loop
   s" ; DEEP HITS @ 1 LV-LEVELS lshift = . cr" GE-CF-BODY+ ;

: GE-DO-OVERCAP-1 ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: op:ptr opu:n cl:ptr clu:n label:ptr labelu:n :}
   \ One `do` level past LV-LEVELS must fail closed rc 70 with the named cap
   \ diagnostic, never a register dump: the JIT keeps a `leave` chain head
   \ (LVH) and a `?do` entry-test offset (LVQ) per level, the cell past LVQ's
   \ last is FRAME-CELL, and a `+loop` rewrites code at the offset it reads
   \ there. LVOPEN refuses before it writes either.
   GE-HB-RESET
   LV-LEVELS 1 + op opu cl clu GE-DO-NEST
   GE-CF-BODY$ RUNTIME-RUNNER:SOURCE
   70 label labelu GE-EXPECT-RC
   s" control-flow nesting too deep" label labelu GE-EXPECT-ERR-HAS
   s" habu-crash" label labelu GE-EXPECT-ERR-LACKS ;

: GE-DO-DEPTH-CAP ( -- )
   \ LV-LEVELS nested loops compile and take every turn, each `+loop` settling
   \ its own `?do`; one more `?do` or `do` is refused.
   GE-HB-RESET
   LV-LEVELS s" 2 0 ?do " s" 1 +loop " GE-DO-NEST
   GE-CF-BODY$ RUNTIME-RUNNER:SOURCE
   s" hb do-cap legal" GE-EXPECT-OK
   s" -1" s" hb do-cap legal turns" GE-EXPECT-OUT-HAS
   s" 2 0 ?do " s" 1 +loop " s" hb do-cap ?do plus1" GE-DO-OVERCAP-1
   s" 2 0 do " s" loop " s" hb do-cap do plus1" GE-DO-OVERCAP-1
   s" PASS: do nesting cap fail-closed rc70 (no overflow)" type cr ;

\ Build "<tier> set-tier", then a definition holding n values on the virtual
\ stack in a counted loop that leaves on its second turn and reads its local
\ after the loop, and print its answer for 100: each turn adds 1 + ... + n, so
\ the answer is 100 + n(n+1).
: GE-LEAVE-VS-CASE ( n n ptr u8 n -- ) {: n:n tier:n label:ptr labelu:n :}
   GE-HB-RESET
   GE-SRC-RESET
   tier GE-SRC-U+ s"  set-tier" GE-SRC-LINE
   s" : GE-LV ( n -- n ) {: a:n :} 0 3 0 do" GE-SRC-LINE
   n 0 ?do i 1+ GE-SRC-U+ GE-SRC-SP loop GE-SRC-LF
   n 0 ?do s" + " GE-SRC+ loop GE-SRC-LF
   s" i 1 = if leave then loop a + ;" GE-SRC-LINE
   s" 100 GE-LV ." GE-SRC-LINE
   RUNTIME-RUNNER:BUFFER
   label labelu GE-EXPECT-OK
   SB-RESET  n 1+ n * 100 + FMT:SB-U  GE-SB-LF
   SB$ label labelu GE-EXPECT-OUT ;

: GE-LEAVE-DEEP-VS ( -- )
   \ Tier 0 holds a body's values on the virtual stack, VVAL-OFF's VSMAX cells,
   \ and `leave` releases the locals bound since its loop was entered, counted
   \ from the frame bytes the LVF-OFF level array kept at `do`. That array lay
   \ inside the virtual stack ($2C0 is its slot 14), so a loop body holding 15
   \ or more values overwrote level 0 and its `leave` released a wrong byte
   \ count: 15 values crashed (rc 134), 16 to VSMAX stopped the compile with
   \ "transfer immediate out of range" (rc 75), and VSMAX + 1, which write every
   \ virtual-stack cell and then spill it, crashed.
   15 0 s" hb leave under 15 values tier 0" GE-LEAVE-VS-CASE
   16 0 s" hb leave under 16 values tier 0" GE-LEAVE-VS-CASE
   VSMAX 1+ 0 s" hb leave past a full virtual stack tier 0" GE-LEAVE-VS-CASE
   15 1 s" hb leave under 15 values tier 1" GE-LEAVE-VS-CASE
   16 1 s" hb leave under 16 values tier 1" GE-LEAVE-VS-CASE
   VSMAX 1+ 1 s" hb leave past a full virtual stack tier 1" GE-LEAVE-VS-CASE
   s" PASS: leave under a deep virtual stack keeps its frame at both tiers" type cr ;

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
   \ An inner quotation's `;]` reopens the enclosing quotation, not the definition.
   s" : RXQLN ( n -- ) {: myloc :} [: [: ;] drop myloc drop ;] drop ;" s" 75" s" myloc" GE-RXE-CATCH-USABLE
   s" : RXQLN ( n -- ) {: myloc :} [: [: ;] drop myloc drop ;] drop ;" 75 s" myloc" GE-RXE-TOP
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

\ ": RXQ ( -- )" is 12 bytes and each opener " [:" three more.
JIT-QUOT:LEVELS 1+ 3 * 12 + constant GE-RXE-QNEST-CAP
create GE-RXE-QNEST-BUF GE-RXE-QNEST-CAP allot   variable GE-RXE-QNEST-U

: GE-RXE-QNEST-BUILD ( -- )   \ ": RXQ ( -- ) [: [: ..." with one `[:` past JIT-QUOT:LEVELS
   GE-SRC-RESET
   s" : RXQ ( -- )" GE-SRC+
   JIT-QUOT:LEVELS 1+ 0 ?do  s"  [:" GE-SRC+  loop
   GE-SRC-U @ GE-RXE-QNEST-U !
   GE-SRC-BUF GE-RXE-QNEST-BUF GE-RXE-QNEST-U @ BYTE-COPY ;

: GE-RXE-QNEST$ ( -- ptr u8 n )  GE-RXE-QNEST-BUF GE-RXE-QNEST-U @ ;

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
   GE-RXE-QNEST-BUILD
   GE-RXE-QNEST$ s" 75" s" hb: quotation nesting full at " GE-RXE-CATCH-USABLE
   GE-RXE-QNEST$ 75 s" hb: quotation nesting full at " GE-RXE-TOP
   s" : RXSQ ( -- ) 5 ;] drop ;" s" 75" s" ;]" GE-RXE-CATCH-USABLE
   s" : RXSQ ( -- ) 5 ;] drop ;" 75 s" ;]" GE-RXE-TOP
   \ The mirror: `;` with one or two quotations open. Published, the body's
   \ unpatched b-over placeholder spins when called. TRUSTED: skips the checker
   \ hook, so the engine's own refusal at `;` is the one that stops it.
   s" TRUSTED: RXSC ( -- ) [: 1 ;" s" 75" s" hb: ; with a quotation open: RXSC" GE-RXE-CATCH-USABLE
   s" TRUSTED: RXSC ( -- ) [: 1 ;" 75 s" hb: ; with a quotation open: RXSC" GE-RXE-TOP
   s" TRUSTED: RXSC ( -- ) [: [: 1 ;" s" 75" s" hb: ; with a quotation open: RXSC" GE-RXE-CATCH-USABLE
   s" TRUSTED: RXSC ( -- ) [: [: 1 ;" 75 s" hb: ; with a quotation open: RXSC" GE-RXE-TOP
   \ `;` with a control structure open. Published, the forward branch that
   \ `0 if`, `while` or `endof` left unpatched is a branch to itself, so the
   \ word spins when called; `do` leaves its loop frame behind. An `if` left
   \ open inside a closed quotation is still on the control stack at `;`. A
   \ checked body is refused the same way, before the checker hook.
   s" TRUSTED: RXSF ( -- ) 0 if ;" s" 70" s" hb: ; with a control structure open: RXSF" GE-RXE-CATCH-USABLE
   s" TRUSTED: RXSF ( -- ) 0 if ;" 70 s" hb: ; with a control structure open: RXSF" GE-RXE-TOP
   s" TRUSTED: RXSF ( -- ) begin 0 while ;" 70 s" hb: ; with a control structure open: RXSF" GE-RXE-TOP
   s" TRUSTED: RXSF ( -- ) 1 case 1 of endof ;" 70 s" hb: ; with a control structure open: RXSF" GE-RXE-TOP
   s" TRUSTED: RXSF ( -- ) 1 0 do ;" s" 70" s" hb: ; with a control structure open: RXSF" GE-RXE-CATCH-USABLE
   s" TRUSTED: RXSF ( -- ) 1 0 do ;" 70 s" hb: ; with a control structure open: RXSF" GE-RXE-TOP
   s" TRUSTED: RXSF ( -- ) [: 0 if ;] drop ;" 70 s" hb: control-flow word does not match the open structure: ;]" GE-RXE-TOP
   s" : RXSF ( -- ) begin ;" 70 s" hb: ; with a control structure open: RXSF" GE-RXE-TOP
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

\ Quotation nesting per definition (dots habu-name-the-nested-6a8e1b28,
\ habu-open-a-quotation-f6c2a55f). Tier 0 compiles the innermost open quotation in
\ QPATCH-CELL..QFRAME-CELL and parks each enclosing one in JIT-QUOT's frames, so
\ JIT-QUOT:LEVELS quotations may be open at once: the checker's control-frame
\ capacity, so the loader refuses no nesting the checker certifies. One level
\ more refuses before anything is emitted, named with the definition and the
\ depth it needed, rc 75 - catchable inside evaluate (the RXQ pair in
\ GE-RAWEXIT-RESIDUAL above). The line is prose and the load path's alone:
\ tools/check.f refuses the same source before its run, E-UNCHECKABLE at the
\ checker's 33rd control frame.
: GE-QNEST-SRC ( n -- ) {: depth:n :}   \ ": GEQDEEP ( n -- n ) [: ... 1+ ;] execute ... ;" then 41 GEQDEEP
   s" : GEQDEEP ( n -- n )" GE-SRC+
   depth 0 ?do  s"  [:" GE-SRC+  loop
   s"  1+" GE-SRC+
   depth 0 ?do  s"  ;] execute" GE-SRC+  loop
   s"  ; 41 GEQDEEP . cr" GE-SRC-LINE ;

: GE-QNEST-RUN ( n -- )
   GE-HB-RESET
   GE-SRC-RESET
   GE-QNEST-SRC
   RUNTIME-RUNNER:BUFFER ;

: GE-QNEST-DIAG$ ( -- ptr u8 n )
   SB-RESET
   s" hb: quotation nesting full at " SB-APPEND
   JIT-QUOT:LEVELS FMT:SB-U
   s"  levels: GEQDEEP needs " SB-APPEND
   JIT-QUOT:LEVELS 1+ FMT:SB-U
   SB$ ;

: GE-QUOT-NEST ( -- )
   JIT-QUOT:LEVELS GE-QNEST-RUN
   0 s" quotation nesting at the level ceiling compiles" GE-EXPECT-RC
   s" 42" s" quotation nesting at the level ceiling runs" GE-EXPECT-OUT-HAS
   JIT-QUOT:LEVELS 1+ GE-QNEST-RUN
   75 s" quotation-nesting exit rc" GE-EXPECT-RC
   GE-QNEST-DIAG$ s" quotation-nesting exit diagnostic" GE-EXPECT-ERR-HAS
   s" habu-crash" s" quotation-nesting no-crash" GE-EXPECT-ERR-LACKS
   s" PASS: quotations nest to the level ceiling and one more is named with its depth" type cr ;

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

\ Source that ends inside a construct it opened (dot habu-eof-inside-a-7a539941).
\ A file whose last line was `: GEEND ( -- )` exited 0 at top level with GEEND
\ dropped, and the next --load row compiled into its body. Loaded through
\ `included` from a compiled word, the caller's next words compiled into it
\ too, at both tiers. Every ending is refused where the source ends: rc 74, named,
\ located on the line the definition opened on (a comment at its `(`), and
\ catchable with the definition rolled back.
create GE-END-PATH FS-PATH-CAP allot
variable GE-END-U
create GE-END-AT 16 allot

: GE-END-DEF$ ( -- ptr u8 n )  s" hb: source ended inside definition: GEEND at " ;
: GE-END-COM$ ( -- ptr u8 n )  s" hb: source ended inside a ( comment at " ;

\ Tier 1 compiles what follows its line at tier 1.
: GE-END-TIER ( n -- )
   1 = if s" 1 set-tier" GE-SRC-LINE then ;

\ The source built so far as hb-end.f.
: GE-END-SAVE ( -- )
   GT-ROOT s" hb-end.f" GE-END-PATH JOIN-PATH GE-END-U !
   GE-END-PATH GE-END-U @ GE-SRC-BUF GE-SRC-U @ WRITE-ALL ;

\ The ending as the last line of hb-end.f, after the tier line.
: GE-END-WRITE ( ptr u8 n n -- )
   {: a:ptr u:n tier:n :}
   GE-SRC-RESET
   tier GE-END-TIER
   a u GE-SRC-LINE
   GE-END-SAVE ;

\ `hb-end.f:<line>` and the newline that ends the refusal; line is one digit.
: GE-END-AT$ ( n -- ptr u8 n )
   {: line:n :}
   s" hb-end.f:" {: a:ptr u:n :}
   a GE-END-AT u BYTE-COPY
   line [char] 0 + GE-END-AT u + c!
   $0A GE-END-AT u 1 + + c!
   GE-END-AT u 2 + ;

\ hb-end.f on the --load line.
: GE-END-TOP ( -- )
   GE-HB-RESET
   s" --load" GE-ARG+  GE-END-PATH GE-END-U @ GE-ARG+  s" --" GE-ARG+
   GE-HB$ GE-TIMEOUT-MS GE-RUN-ENV ;

\ The file GE-LOC-FIXTURE wrote last on the --load line, hb-end.f its argument.
: GE-END-RUN-ARG ( -- )
   GE-HB-RESET
   s" --load" GE-ARG+  GE-SCRIPT-PATH GE-SCRIPT-U @ GE-ARG+
   s" --" GE-ARG+  GE-END-PATH GE-END-U @ GE-ARG+
   GE-HB$ GE-TIMEOUT-MS GE-RUN-ENV ;

\ hb-end.f through `included` from a word compiled at the tier.
: GE-END-INC ( n -- )
   GE-SRC-RESET
   GE-END-TIER
   s" : GEENDRUN ( -- ) 0 SCRIPT-ARGV$ included ;" GE-SRC-LINE
   s" GEENDRUN" GE-SRC-LINE
   s" hb-end-run.f" GE-SRC-BUF GE-SRC-U @ GE-LOC-FIXTURE
   GE-END-RUN-ARG ;

\ rc 74, the refusal and its line, and no crash dump.
: GE-END-EXPECT ( ptr u8 n n ptr u8 n -- )
   {: msg:ptr msgu:n line:n label:ptr labelu:n :}
   74 label labelu GE-EXPECT-RC
   msg msgu label labelu GE-EXPECT-ERR-HAS
   line GE-END-AT$ label labelu GE-EXPECT-ERR-HAS
   s" habu-crash" label labelu GE-EXPECT-ERR-LACKS ;

\ One ending at both tiers, on the --load line and through `included`. line is
\ the refusal's line at tier 0: the name token for a definition, the `(` for
\ a comment. Tier 1's `1 set-tier` line puts it one line later.
: GE-END-CASE ( ptr u8 n ptr u8 n n -- )
   {: src:ptr srcu:n msg:ptr msgu:n line:n :}
   2 0 do
      src srcu i GE-END-WRITE
      GE-END-TOP  msg msgu line i + src srcu GE-END-EXPECT
      i GE-END-INC  msg msgu line i + src srcu GE-END-EXPECT
   loop ;

\ An open colon body, stack comment, locals block and quotation, the tokens tier
\ 1 captures without closing anything, a comment in and out of a body, and a
\ TRUSTED: head that the source ends before or inside its signature.
: GE-END-ENDINGS ( -- )
   s" : GEEND ( -- )" GE-END-DEF$ 1 GE-END-CASE
   s" TRUSTED: GEEND" GE-END-DEF$ 1 GE-END-CASE
   s" TRUSTED: GEEND ( --" GE-END-DEF$ 1 GE-END-CASE
   s" : GEEND ( -- ) 1 drop" GE-END-DEF$ 1 GE-END-CASE
   s" : GEEND ( --" GE-END-DEF$ 1 GE-END-CASE
   s" : GEEND ( -- ) {: a:n" GE-END-DEF$ 1 GE-END-CASE
   s" : GEEND ( -- ) {: a:n :}" GE-END-DEF$ 1 GE-END-CASE
   s" : GEEND ( -- ) [:" GE-END-DEF$ 1 GE-END-CASE
   s" : GEEND ( -- ) [: 1 drop ;]" GE-END-DEF$ 1 GE-END-CASE
   s" : GEEND ( -- ) begin" GE-END-DEF$ 1 GE-END-CASE
   s" ( GEEND" GE-END-COM$ 1 GE-END-CASE
   s" : GEEND ( -- ) ( note" GE-END-COM$ 1 GE-END-CASE ;

\ `require` loads through the same frame: the refusal names the required file.
: GE-END-REQUIRE ( -- )
   s" : GEEND ( -- ) 1 drop" 0 GE-END-WRITE
   s" hb-end-req.f" S\" require hb-end.f\n" GE-LOC-FIXTURE
   GE-LOC-LOAD
   GE-END-DEF$ 1 s" require" GE-END-EXPECT ;

\ A definition left open with its body on the next line. Located on the
\ source's last line, the refusal named the body's line, or a blank line past
\ it; it names the line of the definition's name token. hb-end.f holds the tier
\ line, `: GEEND` on line 2, its body on line 3 and nl newlines: 0 ends it
\ without one, 2 leaves a blank line.
: GE-HALF-CASE ( n n ptr u8 n -- )
   {: tier:n nl:n label:ptr labelu:n :}
   GE-SRC-RESET
   tier GE-SRC-U+  s"  set-tier" GE-SRC-LINE
   s" : GEEND ( -- )" GE-SRC-LINE
   s" 1 drop" GE-SRC+
   nl 0 ?do GE-SRC-LF loop
   GE-END-SAVE
   GE-END-TOP  GE-END-DEF$ 2 label labelu GE-END-EXPECT ;

\ def-open leaves a definition open as `:` does, located at the word that ran
\ it, on line 2, with its body on line 3 and a blank line after it. GEENDOPEN
\ starts the capture the refusal names, as `:` does.
: GE-END-DEFOPEN ( -- )
   GE-SRC-RESET
   s" TRUSTED: GEENDOPEN ( ptr u8 n -- ) {: a:ptr u:n :} 0 data-base BODYLEN-CELL + ! a u get-current 0 def-open a u body-append ;" GE-SRC-LINE
   S\" s\" GEEND\" GEENDOPEN" GE-SRC-LINE
   s" 1 drop" GE-SRC-LINE
   GE-SRC-LF
   GE-END-SAVE
   GE-END-TOP
   GE-END-DEF$ 2 s" def-open" GE-END-EXPECT ;

: GE-END-OPENER ( -- )
   0 0 s" half body, no final newline" GE-HALF-CASE
   0 2 s" half body, a blank line" GE-HALF-CASE
   1 2 s" half body, a blank line, tier 1" GE-HALF-CASE
   GE-END-DEFOPEN ;

\ A caught refusal leaves nothing of the definition and the load going: the
\ name defines afresh, where a published GEEND would be a duplicate (rc 78),
\ and the rest of the file runs.
: GE-END-CAUGHT ( n -- )
   {: tier:n :}
   s" : GEEND ( -- ) 1 drop" tier GE-END-WRITE
   GE-SRC-RESET
   tier GE-END-TIER
   s" : GEENDTRY ( -- n ) [: 0 SCRIPT-ARGV$ included ;] catch ;" GE-SRC-LINE
   s" GEENDTRY ." GE-SRC-LINE
   s" : GEEND ( -- n ) 12321 ;" GE-SRC-LINE
   s" GEEND ." GE-SRC-LINE
   s" hb-end-catch.f" GE-SRC-BUF GE-SRC-U @ GE-LOC-FIXTURE
   GE-END-RUN-ARG
   s" caught refusal" GE-EXPECT-OK
   S\" 74\n12321\n" s" caught refusal" GE-EXPECT-OUT
   GE-END-DEF$ s" caught refusal" GE-EXPECT-ERR-HAS ;

\ A definition open as a buffer begins is an outer buffer's: an immediate
\ word's evaluate compiles into it and returns. The control for EVAL-FRAME:PEND.
: GE-END-MACRO ( -- )
   s" hb-end-macro.f"
      S\" 0 set-check\n: GEENDMAC ( -- ) s\" 5\" evaluate ; immediate\n: GEENDUSE ( -- n ) GEENDMAC ;\nGEENDUSE .\n"
      GE-LOC-FIXTURE
   GE-LOC-LOAD
   s" macro evaluate" GE-EXPECT-OK
   S\" 5\n" s" macro evaluate" GE-EXPECT-OUT ;

\ The closing direction. A `;` in a buffer that an immediate word's evaluate or
\ included began inside an open definition closed the outer buffer's
\ definition while the immediate word still ran, at both tiers. It is refused
\ at the `;`, rc 74, named and located on the first line
\ of the buffer that holds it.
: GE-CLOSE-DEF$ ( -- ptr u8 n )  s" hb: source closed a definition it did not open: GEEND at " ;

\ hb-end.f's immediate word evaluates the `;`: the line counts in the string.
\ TRUSTED: admits the evaluate, and parse-imm is what lets a checked body run
\ the immediate: tier 0 refuses any other one there (E-UNMODELED-IMMEDIATE)
\ and tier 1 captures it unrun.
: GE-CLOSE-EVAL ( n -- )
   {: tier:n :}
   GE-SRC-RESET
   S\" TRUSTED: GEENDMAC ( -- ) s\" ;\" evaluate ; immediate" GE-SRC-LINE
   S\" s\" GEENDMAC\" 0 parse-imm" GE-SRC-LINE
   tier GE-END-TIER
   s" : GEEND ( -- ) 1 drop GEENDMAC" GE-SRC-LINE
   s" GEEND 2 ." GE-SRC-LINE
   GE-END-SAVE
   GE-END-TOP
   GE-CLOSE-DEF$ 1 s" evaluate closes" GE-END-EXPECT ;

\ hb-end-run.f's immediate word includes hb-end.f, which holds only the `;`.
: GE-CLOSE-INC ( n -- )
   {: tier:n :}
   GE-SRC-RESET
   s" ;" GE-SRC-LINE
   GE-END-SAVE
   GE-SRC-RESET
   s" : GEENDINC ( -- ) 0 SCRIPT-ARGV$ included ; immediate" GE-SRC-LINE
   S\" s\" GEENDINC\" 0 parse-imm" GE-SRC-LINE
   tier GE-END-TIER
   s" : GEEND ( -- ) 1 drop GEENDINC" GE-SRC-LINE
   s" GEEND 2 ." GE-SRC-LINE
   s" hb-end-run.f" GE-SRC-BUF GE-SRC-U @ GE-LOC-FIXTURE
   GE-END-RUN-ARG
   GE-CLOSE-DEF$ 1 s" included closes" GE-END-EXPECT ;

: GE-END-CLOSE ( -- )
   2 0 do
      i GE-CLOSE-EVAL
      i GE-CLOSE-INC
   loop ;

\ A refusal an immediate word catches inside its evaluate, while an outer
\ buffer's definition is open. The unwind reset that definition under the
\ immediate word, and the rest of its body ran as top-level words. The catch
\ now returns the refusal's code
\ and the definition keeps compiling without the refused token, closes in its
\ own source and runs. mac defines GEENDCAT; out is the whole stdout.
: GE-CATCH-CASE ( ptr u8 n n ptr u8 n -- )
   {: mac:ptr macu:n tier:n out:ptr outu:n :}
   GE-SRC-RESET
   mac macu GE-SRC-LINE
   S\" s\" GEENDCAT\" 0 parse-imm" GE-SRC-LINE
   tier GE-END-TIER
   s" : GEEND ( -- ) 1 drop GEENDCAT" GE-SRC-LINE
   s" 2 . ;" GE-SRC-LINE
   s" GEEND" GE-SRC-LINE
   GE-END-SAVE
   GE-END-TOP
   mac macu GE-EXPECT-OK
   out outu mac macu GE-EXPECT-OUT
   s" habu-crash" mac macu GE-EXPECT-ERR-LACKS ;

\ A `{:` group refused part way declares none of its names: GEA stays the
\ word. A half-declared local GEA compiled a read of a frame never carved.
: GE-CATCH-LOCALS ( -- )
   GE-SRC-RESET
   s" : GEA ( -- n ) 5 ;" GE-SRC-LINE
   S\" TRUSTED: GEENDCAT ( -- ) [: s\" {: GEA averyveryverylongname :}\" evaluate ;] catch . ; immediate" GE-SRC-LINE
   S\" s\" GEENDCAT\" 0 parse-imm" GE-SRC-LINE
   s" : GEEND ( -- n ) GEENDCAT" GE-SRC-LINE
   s" GEA ;" GE-SRC-LINE
   s" GEEND ." GE-SRC-LINE
   GE-END-SAVE
   GE-END-TOP
   s" caught locals refusal" GE-EXPECT-OK
   S\" 70\n5\n" s" caught locals refusal" GE-EXPECT-OUT
   s" habu-crash" s" caught locals refusal" GE-EXPECT-ERR-LACKS ;

\ An undefined word refuses at tier 0 only: tier 1 resolves the body at `;`,
\ outside the catch. The `;` and an unterminated string refuse at both tiers.
: GE-END-CATCH ( -- )
   S\" TRUSTED: GEENDCAT ( -- ) [: s\" nosuchword\" evaluate ;] catch . ; immediate"
      0 S\" 70\n2\n" GE-CATCH-CASE
   2 0 do
      S\" TRUSTED: GEENDCAT ( -- ) [: s\" ;\" evaluate ;] catch . ; immediate"
         i S\" 74\n2\n" GE-CATCH-CASE
      S\" TRUSTED: GEENDCAT ( -- ) [: S\\\" s\\q abc\" evaluate ;] catch . ; immediate"
         i S\" 74\n2\n" GE-CATCH-CASE
   loop
   GE-CATCH-LOCALS ;

\ An immediate word's evaluate that emits code into the open definition
\ compiles as if written in its place, at both tiers: tier 0 reopens the code
\ window at the evaluate's first token (habu2.f EM-COMPILE-LEGACY). The source
\ built so far runs; out is its whole stdout.
: GE-EMIT-EXPECT ( ptr u8 n ptr u8 n -- )
   {: out:ptr outu:n label:ptr labelu:n :}
   GE-END-SAVE
   GE-END-TOP
   label labelu GE-EXPECT-OK
   out outu label labelu GE-EXPECT-OUT
   s" habu-crash" label labelu GE-EXPECT-ERR-LACKS ;

\ GEENDCAT evaluates ev; def uses it and run runs it.
: GE-EMIT-SHAPE ( ptr u8 n ptr u8 n ptr u8 n n ptr u8 n -- )
   {: ev:ptr evu:n def:ptr defu:n run:ptr runu:n tier:n out:ptr outu:n :}
   GE-SRC-RESET
   s" TRUSTED: GEENDCAT ( -- ) " GE-SRC+  ev evu GE-SRC-S"  s"  evaluate ; immediate" GE-SRC-LINE
   S\" s\" GEENDCAT\" 0 parse-imm" GE-SRC-LINE
   tier GE-END-TIER
   def defu GE-SRC-LINE
   run runu GE-SRC-LINE
   out outu ev evu GE-EMIT-EXPECT ;

: GE-EMIT-SHAPES ( -- )
   2 0 do
      s" drop" s" : GEEND ( n n -- n ) GEENDCAT ;" s" 1 2 GEEND . depth ."
         i S\" 1\n0\n" GE-EMIT-SHAPE
      s" if 5 else 6 then" s" : GEEND ( bool -- n ) GEENDCAT ;" s" 0 0= GEEND . depth ."
         i S\" 5\n0\n" GE-EMIT-SHAPE
      s" begin 1- dup 0= until" s" : GEEND ( n -- n ) GEENDCAT ;" s" 3 GEEND . depth ."
         i S\" 0\n0\n" GE-EMIT-SHAPE
      s" GEENDW2" s" : GEENDW2 ( n -- n ) 1+ ;  : GEEND ( n -- n ) GEENDCAT ;"
         s" 3 GEEND . depth ." i S\" 4\n0\n" GE-EMIT-SHAPE
   loop ;

\ A refusal that fires after its token spilled or emitted code, caught at tier
\ 0, the tier that compiles inside the evaluate. GEENDCAT opens n `if`s,
\ catches ref and then a `leave` (no DO level may stay open), evaluates fol and
\ closes the `if`s, so GEEND adds 7 only if the refused token left its 7, the
\ data stack and every compile cell as they were. Each failed, once the band
\ was open: the leftover pop, a lost or stale control-flow entry, DO level or
\ BEGIN snapshot gave a wrong sum, an orphaned `then` or a bounds fault. The
\ checker certifies a shallower nesting than the control-flow cap, so a row at
\ the cap compiles unchecked.
: GE-FAMILY ( n ptr u8 n ptr u8 n -- )
   {: n:n ref:ptr refu:n fol:ptr folu:n :}
   GE-SRC-RESET
   n CFSTK-DEPTH-MAX = if s" 0 set-check" GE-SRC-LINE  s" : " else s" TRUSTED: " then GE-SRC+
   s" GEENDCAT ( -- ) " GE-SRC+  n GE-SRC-U+  s"  0 ?do " GE-SRC+
   s" 0 0= if" GE-SRC-S"  s"  evaluate loop [: " GE-SRC+
   ref refu GE-SRC-S"  s"  evaluate ;] catch . [: " GE-SRC+
   s" leave" GE-SRC-S"  s"  evaluate ;] catch . " GE-SRC+
   fol folu GE-SRC-S"  s"  evaluate " GE-SRC+  n GE-SRC-U+  s"  0 ?do " GE-SRC+
   s" then" GE-SRC-S"  s"  evaluate loop ; immediate" GE-SRC-LINE
   n CFSTK-DEPTH-MAX <> if S\" s\" GEENDCAT\" 0 parse-imm" GE-SRC-LINE then
   s" : GEEND ( n -- n ) GEENDCAT ;" GE-SRC-LINE
   s" 5 GEEND . depth ." GE-SRC-LINE
   S\" 70\n70\n12\n0\n" ref refu GE-EMIT-EXPECT ;

: GE-EMIT-FAMILIES ( -- )
   CFSTK-DEPTH-MAX s" 7 if" s" +" GE-FAMILY
   CFSTK-DEPTH-MAX s" 7 while" s" +" GE-FAMILY
   CFSTK-DEPTH-MAX s" 7 of" s" +" GE-FAMILY
   CFSTK-DEPTH-MAX s" 7 0 do" s" + +" GE-FAMILY
   CFSTK-DEPTH-MAX s" 7 0 ?do" s" + +" GE-FAMILY
   CFSTK-DEPTH-MAX s" 7 begin" s" +" GE-FAMILY
   0 s" 7 until" s" +" GE-FAMILY
   0 s" 7 again" s" +" GE-FAMILY
   1 s" 7 repeat" s" +" GE-FAMILY
   1 s" 7 endcase" s" +" GE-FAMILY ;

\ A refused nested quotation must not park a frame. The outer definition
\ survives the caught evaluate, so its real quotation must still close once.
: GE-EMIT-QUOT-ROOM ( -- )
   GE-SRC-RESET
   s" 0 set-check" GE-SRC-LINE
   s" : GEENDCAT ( -- ) " GE-SRC+
   s" [:" GE-SRC-S"  s"  evaluate " GE-SRC+
   CFSTK-DEPTH-MAX 1- GE-SRC-U+  s"  0 ?do " GE-SRC+
   s" 0 0= if" GE-SRC-S"  s"  evaluate loop [: " GE-SRC+
   s" [:" GE-SRC-S"  s"  evaluate ;] catch . " GE-SRC+
   CFSTK-DEPTH-MAX 1- GE-SRC-U+  s"  0 ?do " GE-SRC+
   s" then" GE-SRC-S"  s"  evaluate loop " GE-SRC+
   s" 42 ;] execute" GE-SRC-S"  s"  evaluate ; immediate" GE-SRC-LINE
   s" : GEEND ( -- n ) GEENDCAT ;" GE-SRC-LINE
   s" GEEND . depth ." GE-SRC-LINE
   S\" 70\n42\n0\n" s" caught quotation capacity refusal" GE-EMIT-EXPECT ;

\ A refusal two evaluates down, caught one level up, keeps what the inner
\ evaluate completed, as at one level: GEENDIN's 7 stays in GEEND. Each popped
\ frame put back the mark of the token that called its evaluate, so the outer
\ pop dropped the 7 and the checker refused GEEND's `+` as an input underflow,
\ rc 70, at both tiers.
: GE-EMIT-NEST ( n -- )
   {: tier:n :}
   GE-SRC-RESET
   S\" TRUSTED: GEENDIN ( -- ) s\" 7 ;\" evaluate ; immediate" GE-SRC-LINE
   S\" s\" GEENDIN\" 0 parse-imm" GE-SRC-LINE
   S\" TRUSTED: GEENDCAT ( -- ) [: s\" GEENDIN\" evaluate ;] catch . ; immediate" GE-SRC-LINE
   S\" s\" GEENDCAT\" 0 parse-imm" GE-SRC-LINE
   tier GE-END-TIER
   s" : GEEND ( n -- n ) GEENDCAT + ;" GE-SRC-LINE
   s" 5 GEEND . depth ." GE-SRC-LINE
   S\" 74\n12\n0\n" s" nested evaluate" GE-EMIT-EXPECT ;

\ A structure that an evaluate two levels down opens before its refusal, which
\ a catch one level up takes, and the outer source closes; aft is what GEENDIN
\ runs after its evaluate, a throw once that evaluate has ended cleanly. Each
\ popped frame put back the mark of the token that called its evaluate, so the
\ outer pop rolled CP, the virtual stack and the body text back past the inner
\ buffer's completed tokens while the control-flow stack, DO level, BEGIN
\ snapshot and quotation kept them: the checker refused GEEND (rc 70), and
\ compiled unchecked (mode 1) it exceeded the data stack bounds (rc 102) or,
\ for `case`, returned 5 for 2. run runs GEEND; out is the whole stdout. The
\ rows run at tier 0 only: tier 1 defers an undefined word to `;`, which
\ refuses GEEND (rc 70) with no inner throw for the catch to take; GE-EMIT-NEST
\ holds the tier-1 case.
: GE-DEEP-ROW ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n ptr u8 n -- )
   {: ev:ptr evu:n aft:ptr aftu:n def:ptr defu:n run:ptr runu:n mode:n out:ptr outu:n :}
   GE-SRC-RESET
   s" TRUSTED: GEENDIN ( -- ) " GE-SRC+  ev evu GE-SRC-S"  s"  evaluate" GE-SRC+
   aft aftu GE-SRC+  s"  ; immediate" GE-SRC-LINE
   S\" s\" GEENDIN\" 0 parse-imm" GE-SRC-LINE
   S\" TRUSTED: GEENDCAT ( -- ) [: s\" GEENDIN\" evaluate ;] catch . ; immediate" GE-SRC-LINE
   S\" s\" GEENDCAT\" 0 parse-imm" GE-SRC-LINE
   mode 1 = if s" 0 set-check" GE-SRC-LINE then
   def defu GE-SRC-LINE
   run runu GE-SRC-LINE
   out outu ev evu GE-EMIT-EXPECT ;

: GE-EMIT-DEEP ( -- )
   2 0 do
      s" dup 0= if 1 nosuchword" s" " s" : GEEND ( n -- n ) GEENDCAT + then ;"
         s" 0 GEEND . 5 GEEND . depth ." i S\" 70\n1\n5\n0\n" GE-DEEP-ROW
      s" 0 swap 0 do 1 nosuchword" s" " s" : GEEND ( n -- n ) GEENDCAT + loop ;"
         s" 3 GEEND . depth ." i S\" 70\n3\n0\n" GE-DEEP-ROW
      s" [: 1 nosuchword" s" " s" : GEEND ( n -- n ) GEENDCAT + ;] execute ;"
         s" 5 GEEND . depth ." i S\" 70\n6\n0\n" GE-DEEP-ROW
      s" begin 1- dup nosuchword" s" " s" : GEEND ( n -- n ) GEENDCAT 3 < until ;"
         s" 9 GEEND . depth ." i S\" 70\n2\n0\n" GE-DEEP-ROW
      s" case 1 of nosuchword" s" " s" : GEEND ( n -- n ) GEENDCAT 5 endof 9 swap endcase ;"
         s" 1 GEEND . 2 GEEND . depth ." i S\" 70\n5\n9\n0\n" GE-DEEP-ROW
      s" dup 0= if 1" s"  70 throw" s" : GEEND ( n -- n ) GEENDCAT + then ;"
         s" 0 GEEND . 5 GEEND . depth ." i S\" 70\n1\n5\n0\n" GE-DEEP-ROW
   loop ;

\ A `does>` refused at its signature declares no clause. DOESB was set before
\ the signature: the definition's `;` then refused the `does>` at tier 0 (rc
\ 70) and died in the native compiler at tier 1 (rc 67).
: GE-EMIT-DOES ( n -- )
   {: tier:n :}
   GE-SRC-RESET
   S\" TRUSTED: GEENDCAT ( -- ) [: s\" does> 5\" evaluate ;] catch . ; immediate" GE-SRC-LINE
   S\" s\" GEENDCAT\" 0 parse-imm" GE-SRC-LINE
   tier GE-END-TIER
   s" : GEEND ( -- n ) GEENDCAT 9 ;" GE-SRC-LINE
   s" GEEND . depth ." GE-SRC-LINE
   S\" 76\n9\n0\n" s" malformed does> signature" GE-EMIT-EXPECT ;

: GE-END-EMIT ( -- )
   GE-EMIT-SHAPES
   GE-EMIT-FAMILIES
   GE-EMIT-QUOT-ROOM
   GE-EMIT-DEEP
   2 0 do
      i GE-EMIT-NEST
      i GE-EMIT-DOES
   loop ;

\ A control-flow word whose open entry is another structure's. The closers
\ proved only that enough entries were open: in `begin dup 0<> if 0<> until
\ then`, `until` took the IF entry for its loop top and `then` patched the
\ BEGIN's loop top as a forward branch, ORing its offset into the call there.
\ The call then landed where the engine's layout put it, in atomic-cas (the
\ habu-crash dump, rc 134) on one build and in 2over's tail (the run printed
\ 2) on its parent. Every closer, and `while` and `of`, now refuses an entry
\ of another kind at its token, rc 70, named; `;`, `does>` and `;]` refuse a
\ structure still open inside them.
: GE-KIND$ ( -- ptr u8 n )  s" hb: control-flow word does not match the open structure: " ;

create GE-KIND-LINE 96 allot

\ The whole refusal line for token t.
: GE-KIND-LINE$ ( ptr u8 n -- ptr u8 n )
   {: t:ptr tu:n :}
   GE-KIND$ {: m:ptr mu:n :}
   m GE-KIND-LINE mu BYTE-COPY
   t GE-KIND-LINE mu + tu BYTE-COPY
   $0A GE-KIND-LINE mu + tu + c!
   GE-KIND-LINE mu tu + 1 + ;

\ Opener o, the text around the word under test in a body ( n -- n ). The
\ body maps 5 to 13 only if the 7 that the word's own text pushed before it
\ survives its refusal and the opener's entry is as it was.
: GE-KIND-PRE+ ( n -- )
   case
      0 of s" 0 0= if " GE-SRC+ endof
      1 of s" begin " GE-SRC+ endof
      2 of s" begin dup 6 < while " GE-SRC+ endof
      3 of s" 1 0 do " GE-SRC+ endof
      4 of s" 1 0 ?do " GE-SRC+ endof
      5 of s" case " GE-SRC+ endof
      6 of s" dup case 5 of " GE-SRC+ endof
      7 of s" case 4 of 12 endof " GE-SRC+ endof
      8 of s" [: " GE-SRC+ endof
   endcase ;

: GE-KIND-POST+ ( n -- )
   case
      0 of s"  + 1+ then" GE-SRC+ endof
      1 of s"  + 1+ 0 0= until" GE-SRC+ endof
      2 of s"  + 1+ repeat" GE-SRC+ endof
      3 of s"  + 1+ loop" GE-SRC+ endof
      4 of s"  + 1+ loop" GE-SRC+ endof
      5 of s"  + dup endcase 1+" GE-SRC+ endof
      6 of s"  + 1+ endof endcase" GE-SRC+ endof
      7 of s"  + dup endcase 1+" GE-SRC+ endof
      8 of s"  + ;] execute 1+" GE-SRC+ endof
   endcase ;

\ Closer c's token; 13 is `;`.
: GE-KIND-TOK ( n -- ptr u8 n )
   case
      0 of s" then" endof
      1 of s" else" endof
      2 of s" until" endof
      3 of s" again" endof
      4 of s" while" endof
      5 of s" repeat" endof
      6 of s" loop" endof
      7 of s" +loop" endof
      8 of s" endof" endof
      9 of s" endcase" endof
      10 of s" of" endof
      11 of s" does>" endof
      12 of s" ;]" endof
      s" ;" rot
   endcase ;

\ The openers closer c closes or continues, one bit per opener.
: GE-KIND-OWN ( n -- n )
   case
      0 of 1 endof
      1 of 1 endof
      2 of 2 endof
      3 of 2 endof
      4 of 2 endof
      5 of 4 endof
      6 of 24 endof
      7 of 24 endof
      8 of 64 endof
      9 of 160 endof
      10 of 160 endof
      12 of 256 endof
      0 swap
   endcase ;

: GE-KIND-MAC+ ( n -- )  s" GEKC" GE-SRC+  GE-SRC-U+ ;

\ GEKC<c> evaluates `7` and closer c's token and prints its catch code.
: GE-KIND-MACRO ( n bool -- )
   {: c:n chk:bool :}
   chk if s" TRUSTED: " else s" : " then GE-SRC+
   c GE-KIND-MAC+
   S\"  ( -- ) [: s\" 7 " GE-SRC+  c GE-KIND-TOK GE-SRC+
   S\" \" evaluate ;] catch . ; immediate" GE-SRC-LINE
   chk if S\" s\" " GE-SRC+  c GE-KIND-MAC+  S\" \" 0 parse-imm" GE-SRC-LINE then ;

\ GEK<row>: opener o around GEKC<c>, inside a quotation for `;]`; run on 5.
: GE-KIND-ROW ( n n n -- )
   {: o:n c:n row:n :}
   s" : GEK" GE-SRC+  row GE-SRC-U+  s"  ( n -- n ) " GE-SRC+
   c 12 = if s" [: " GE-SRC+ then
   o GE-KIND-PRE+  c GE-KIND-MAC+  o GE-KIND-POST+
   c 12 = if s"  ;] execute" GE-SRC+ then
   s"  ;" GE-SRC-LINE
   s" 5 GEK" GE-SRC+  row GE-SRC-U+  s"  ." GE-SRC-LINE ;

\ GEKT: closer c's text straight in the body around opener o; `;` closes an
\ `if` that is still open.
: GE-KIND-BODY ( n n -- )
   {: o:n c:n :}
   c 13 = if s" : GEKT ( n -- n ) 0 0= if 1+ ;" GE-SRC-LINE exit then
   s" : GEKT ( n -- n ) " GE-SRC+
   c 12 = if s" [: " GE-SRC+ then
   o GE-KIND-PRE+  s" 7 " GE-SRC+  c GE-KIND-TOK GE-SRC+  o GE-KIND-POST+
   c 12 = if s"  ;] execute" GE-SRC+ then
   s"  ;" GE-SRC-LINE ;

create GE-KIND-WANT 2048 allot
variable GE-KIND-WANT-U
variable GE-KIND-N

: GE-KIND-WANT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   GE-KIND-WANT-U @ u + 2048 > if E-STR-CAPACITY throw then
   a GE-KIND-WANT GE-KIND-WANT-U @ + u BYTE-COPY
   GE-KIND-WANT-U @ u + GE-KIND-WANT-U ! ;

: GE-KIND-LABEL ( bool -- ptr u8 n )
   if s" closer kinds caught, checked" else s" closer kinds caught, unchecked" then ;

\ Every closer against every opener it neither closes nor continues, inside a
\ caught evaluate at tier 0: each prints the catch code, 70, and its word
\ maps 5 to 13. Then a `;` refused inside the evaluate that opened its
\ definition drops that definition, so GEKS defines afresh.
: GE-KIND-EVAL ( bool -- )
   {: chk:bool :}
   chk GE-KIND-LABEL {: l:ptr lu:n :}
   GE-SRC-RESET
   0 GE-KIND-N !  0 GE-KIND-WANT-U !
   chk if else s" 0 set-check" GE-SRC-LINE then
   13 0 do i chk GE-KIND-MACRO loop
   13 0 do
      9 0 do
         j GE-KIND-OWN 1 i lshift and 0= if
            i j GE-KIND-N @ GE-KIND-ROW
            GE-KIND-N @ 1+ GE-KIND-N !
            S\" 70\n13\n" GE-KIND-WANT+
         then
      loop
   loop
   chk if s" TRUSTED: " else s" : " then GE-SRC+
   S\" GEKSEMI ( -- ) [: s\" : GEKS ( n -- n ) 0 0= if 1+ ;\" evaluate ;] catch . ;" GE-SRC-LINE
   s" GEKSEMI  : GEKS ( n -- n ) 1+ ;  5 GEKS .  depth ." GE-SRC-LINE
   S\" 70\n6\n0\n" GE-KIND-WANT+
   GE-END-SAVE
   GE-END-TOP
   l lu GE-EXPECT-OK
   GE-KIND-WANT GE-KIND-WANT-U @ l lu GE-EXPECT-OUT
   GE-KIND$ l lu GE-EXPECT-ERR-HAS
   s" habu-crash" l lu GE-EXPECT-ERR-LACKS ;

\ The opener closer c meets at top level: the shapes that crashed or ran wrong
\ code, `begin ... then`, `if ... until` and `if ... repeat`.
: GE-KIND-TOP-OPENER ( n -- n )
   {: c:n :}
   c 2 < c 7 = or if 1 else 0 then ;

\ `;` names the unfinished definition and its source location as well.
: GE-KIND-SEMI-LINE$ ( bool -- ptr u8 n )
   {: chk:bool :}
   SB-RESET
   s" hb: ; with a control structure open: GEKT at " SB-APPEND
   GE-END-PATH GE-END-U @ SB-APPEND
   s" :" SB-APPEND
   chk if 1 else 2 then FMT:SB-U
   GE-SB-LF
   SB$ ;

\ Closer c straight in a definition at tier 0: rc 70, and the refusal naming
\ its token is the whole of stderr.
: GE-KIND-TOP ( n bool -- )
   {: c:n chk:bool :}
   GE-SRC-RESET
   chk if else s" 0 set-check" GE-SRC-LINE then
   c GE-KIND-TOP-OPENER c GE-KIND-BODY
   s" 5 GEKT ." GE-SRC-LINE
   GE-END-SAVE
   GE-END-TOP
   c GE-KIND-TOK {: t:ptr tu:n :}
   70 t tu GE-EXPECT-RC
   c 13 = if chk GE-KIND-SEMI-LINE$ t tu GE-EXPECT-ERR exit then
   t tu GE-KIND-LINE$ t tu GE-EXPECT-ERR ;

: GE-CF-KIND ( -- )
   true GE-KIND-EVAL
   false GE-KIND-EVAL
   14 0 do
      i true GE-KIND-TOP
      i false GE-KIND-TOP
   loop
   s" PASS: a control-flow word whose open entry is another structure's is refused at its token at tier 0" type cr ;

\ Source on stdin has no file to name: the same refusals, unlocated.
: GE-END-STDIN ( -- )
   s" : GEEND ( -- ) 1 drop" 74 s" stdin definition" RUNTIME-RUNNER:LINE-RC
   S\" hb: source ended inside definition: GEEND\n" s" stdin definition" GE-EXPECT-ERR
   s" ( GEEND" 74 s" stdin comment" RUNTIME-RUNNER:LINE-RC
   S\" hb: source ended inside a ( comment\n" s" stdin comment" GE-EXPECT-ERR ;

: GE-SOURCE-END ( -- )
   GE-END-ENDINGS
   GE-END-REQUIRE
   GE-END-OPENER
   0 GE-END-CAUGHT
   1 GE-END-CAUGHT
   GE-END-MACRO
   GE-END-CLOSE
   GE-END-CATCH
   GE-END-EMIT
   GE-END-STDIN
   s" PASS: a definition or comment the source did not close, or a definition it did not open, is refused and located, a definition at the line it opened on; a macro's evaluate compiles into the definition, and its caught refusal leaves it as before the refused token" type cr ;

\ An immediate word in a body whose wide local sends it through pass 2 at tier
\ 0. Pass 2 ran the word again over the body capture, which never holds what a
\ parsing word read: GEP2SKIP read the `1` (the word then ran `k +`: rc 102,
\ stack bounds exceeded); GEP2FLIP's second run undefined GEP2X (rc 70);
\ GEP2SAY's caught refusal printed twice; GEP2GRAB allotted twice; GEP2SEE's
\ second run ran with the lowering transaction open and its blob mapped. Each
\ runs once at both tiers, and the cell GEP2GRAB allots keeps its 77: pass 2
\ also took DP back to the definition's start, so with the word run once the
\ next allot wrote 99 there.

\ imm defines the immediate word, body follows GEP2W's locals and run is the
\ last line; hb-end.f runs at the tier.
: GE-P2-RUN ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: imm:ptr immu:n body:ptr bodyu:n run:ptr runu:n tier:n :}
   GE-SRC-RESET
   s" require lib/adt/option.f" GE-SRC-LINE
   imm immu GE-SRC-LINE
   tier GE-END-TIER
   s" : GEP2W ( option<n> n -- n ) {: o:option<n> k:n :} k " GE-SRC+
   body bodyu GE-SRC+
   s"  ;" GE-SRC-LINE
   s" : GEP2RUN ( -- ) 5 OPTION:SOME 41 GEP2W . ;" GE-SRC-LINE
   run runu GE-SRC-LINE
   GE-END-SAVE
   GE-END-TOP ;

\ rc 0 and the whole of stdout and stderr.
: GE-P2-EXPECT ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: out:ptr outu:n err:ptr erru:n label:ptr labelu:n :}
   label labelu GE-EXPECT-OK
   out outu label labelu GE-EXPECT-OUT
   err erru label labelu GE-EXPECT-ERR ;

: GE-P2-SHAPES ( n -- )
   {: tier:n :}
   S\" : GEP2SKIP ( -- ) parse-name 2drop ; immediate\ns\" GEP2SKIP\" 1 parse-imm"
      s" GEP2SKIP junk 1 +" s" GEP2RUN" tier GE-P2-RUN
   S\" 42\n" s" " s" pass 2 parsing immediate" GE-P2-EXPECT
   S\" variable GEP2RUNS\n: GEP2X ( n -- n ) 1 + ;\n: GEP2FLIP ( -- ) GEP2RUNS @ 0<> if s\" GEP2X\" UNDEFINE-NAME then 1 GEP2RUNS ! ; immediate\ns\" GEP2FLIP\" 0 parse-imm"
      s" GEP2FLIP GEP2X" s" GEP2RUN" tier GE-P2-RUN
   S\" 42\n" s" " s" pass 2 immediate undefining on a rerun" GE-P2-EXPECT
   S\" TRUSTED: GEP2SAY ( -- ) [: s\" ;\" evaluate ;] catch . ; immediate\ns\" GEP2SAY\" 0 parse-imm"
      s" GEP2SAY 1 +" s" GEP2RUN" tier GE-P2-RUN
   s" pass 2 immediate printing a caught refusal" GE-EXPECT-OK
   S\" 74\n42\n" s" pass 2 immediate printing a caught refusal" GE-EXPECT-OUT
   s" hb: source closed a definition it did not open: GEP2W at "
      s" pass 2 immediate printing a caught refusal" GE-EXPECT-ERR-HAS
   \ GEP2SAY's caught refusal is located on line 1 of the string it evaluates.
   1 GE-END-AT$ s" pass 2 immediate printing a caught refusal" GE-EXPECT-ERR-HAS
   S\" variable GEP2RUNS\nvariable GEP2AT\nTRUSTED: GEP2GRAB ( -- ) GEP2RUNS @ 1+ GEP2RUNS ! here GEP2AT ! 77 , ; immediate\ns\" GEP2GRAB\" 0 parse-imm"
      s" GEP2GRAB 1 +" s" create GEP2NEXT 99 , GEP2RUN GEP2RUNS @ . GEP2AT @ @ ." tier GE-P2-RUN
   S\" 42\n1\n77\n" s" " s" pass 2 immediate allotting" GE-P2-EXPECT
   S\" TRUSTED: GEP2SEE ( -- ) data-base TXN-ACTIVE-CELL + @ . data-base TXN-BLOB-A-CELL + @ . ; immediate\ns\" GEP2SEE\" 0 parse-imm"
      s" GEP2SEE 1 +" s" GEP2RUN" tier GE-P2-RUN
   S\" 0\n0\n42\n" s" " s" pass 2 immediate and the lowering transaction" GE-P2-EXPECT ;

: GE-PASS2-IMMEDIATE ( -- )
   2 0 do i GE-P2-SHAPES loop
   s" PASS: an immediate word in a body with a wide local runs once, on the input it was written with, at both tiers" type cr ;

public

: RUN ( -- )
   GE-UNCAUGHT-THROW
   GE-DIE-LINE
   GE-INTERP-LAYOUT
   WIDE-FETCH:RUN
   GE-DICT-FULL
   GE-BODY-CAP
   GE-BEGIN-NEST
   GE-DATA-FULL
   GE-DIV-MOD
   GE-PROCESS-PTY
   GE-DEREF-ARITY-DIAG
   GE-EXECUTE-FLOOR
   GE-NESTED-CHECKED-DEF
   GE-NESTED-BAD-DEF
   GE-EVAL-UNDEF-RECOVER
   GE-EVAL-INTERP-ERR-RECOVER
   GE-EVAL-DEF-REJECT-CATCH
   GE-ORPHAN-CLOSER
   LOOP-OPENER:RUN
   GE-CF-DEPTH-CAP
   GE-DO-DEPTH-CAP
   GE-LEAVE-DEEP-VS
   GE-RAWEXIT-RECOVER
   GE-RAWEXIT-RESIDUAL
   GE-QUOT-NEST
   GE-LOCAL-NAME-WIDTH
   GE-PKGSCOPE-RECOVERY
   GE-REFUSAL-LOCATION
   GE-SOURCE-END
   GE-CF-KIND
   GE-PASS2-IMMEDIATE
   GE-SET-CHECK-NEG ;

: TEST ( -- )
   s" habu-runtime-regression" GT-START
   RUN
   GT-CLEANUP
   s" runtime-regression-test: ok" type cr ;

;package

RUNTIME-REGRESSION:TEST
