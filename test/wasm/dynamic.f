\ dynamic.f - WDYN, src/arch/wasm/dynamic.f, run: a window compiled through an
\ open Wasm shadow (src/arch/wasm/backend.f) beside
\ src/arch/wasm/kernel-words.f, captured and linked by WASMLINK
\ (src/habu/link-wasm.f) once per entry, then validated by wasm-tools and run
\ by bun (test/wasm/harness.f).
\ Both tools must be on PATH, so this is no row of the ordinary gate:
\ test/wasm/device.f runs it, as does `bin/hb --load test/wasm/dynamic.f` from
\ the tree's root.
\
\ W06's dynamic half: a 17-input word, a function in the aligned frame,
\ answers through execute of its code literal what its direct call answers.
\ Quotations in their lanes, one taking a cell and answering one and one
\ answering two, reached through execute, take their input off the stack and
\ leave their outputs there. W07: catch of a quotation that throws MIN-N + 1,
\ which a double would round, answers the code whole over the cell beneath the
\ call; catch of a quotation that returns answers zero while ctx.throw-code
\ still holds an earlier code; a trap inside a quotation passes through catch;
\ and execute of an xt whose slot takes more cells than the stack holds traps
\ before its function runs. Execute and catch of xt 0, which names no adapter,
\ trap, and so does execute of an xt whose upper 32 bits are set though its
\ low half names a slot. A package's own execute and catch are ordinary words,
\ and a call to either answers what it answers natively. An entry reports
\ through its last throw's code, and each code is the one the same definition
\ throws natively.
\
\ .s calls kernel-words.f's `.` on each cell, deepest first, and leaves them.
\ The window's address cells, which no linked word reaches (a created word has
\ no routine), are in the module's data image: a code cell holds the slot its
\ word's code literal answers, a window word's or an engine word's, and a DATA
\ cell WPROF's data base plus the offset of the text it points at, which the
\ image holds there.

package WASM-DYNAMIC
public
ndict@ here  variable PRE-R  variable PRE-D  PRE-D !  PRE-R !
;package

require lib/test.f
require lib/le.f
require lib/fs.f
require lib/fs-mutate.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/compiler/native/string.f
require src/compiler/native/shadow.f
require src/arch/wasm/profile.f
require src/arch/wasm/backend.f
require src/arch/wasm/capture.f
require src/habu/link-wasm.f
require test/wasm/harness.f

package WASM-DYNAMIC
public

\ A window opened on an open Wasm shadow; a binding is a multi-cell value, which
\ only a compiled body may hold.
: OPEN ( -- )
   WBACK:BINDING NSHADOW:OPEN
   align AOT-ARM:WINDOW-OPEN
   NSTR:WINDOW-OPEN ;

: CAPTURE ( -- )
   AOT-ARM:WINDOW-CLOSE
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:WASM-TARGET-CAPTURE
   NSHADOW:CLOSE ;

\ The window offset of an address in the open window.
: OFF ( ptr u8 -- n )
   BYTE-VIEW data-base BYTE-VIEW - DATA-VA VA>N + AOT-ARM:D0 @ - ;

variable CODE-AT                     \ the window offsets of the address cells
variable NAMED-AT
variable DATA-AT

;package

WBACK:INSTALL
WASM-DYNAMIC:OPEN
1 set-tier
require src/arch/wasm/kernel-words.f
package WASM-DYNAMIC-WINDOW
private
\ A number as a cell's address, for the load that traps.
CAST: >CELL ( n -- ptr n )
\ A three-input word's xt as an xt of none, for the depth shortfall.
CAST: >NONE ( [ n n n -- ] -- [ -- ] )
\ A three-input word's xt as the number a throw carries.
CAST: >N ( [ n n n -- ] -- n )
\ A number as an xt of none, for the xts that name no adapter.
CAST: >VOID ( n -- [ -- ] )
\ An xt of none as a number, for the upper bits set beside its slot.
CAST: >BITS ( [ -- ] -- n )
\ An xt that reaches MARK throws 37, where one that reached DROP3's would trap
\ on the empty stack.
: MARK ( -- ) 37 throw ;
public
: DROP3 ( n n n -- ) drop drop drop ;
: W17 ( n n n n n n n n n n n n n n n n n -- n )
   3 * + 3 * + 3 * + 3 * + 3 * + 3 * + 3 * + 3 * +
   3 * + 3 * + 3 * + 3 * + 3 * + 3 * + 3 * + 3 * + ;
: DIRECT ( -- ) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 W17 throw ;
: VIA-EXECUTE ( -- ) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 ['] W17 execute throw ;
: VIA-LANES ( -- ) 5 [: 1 + ;] execute [: 1 2 ;] execute + + throw ;
: CAUGHT ( -- ) 7 [: $8000000000000001 throw ;] catch + throw ;
: RETURNED ( -- ) [: 9 throw ;] catch [: ;] catch + throw ;
: TRAPPED ( -- ) [: 0 >CELL @ drop ;] catch throw ;
: SHORT ( -- ) ['] DROP3 >NONE execute ;
: ZERO ( -- ) 0 >VOID execute ;
: ZERO-CAUGHT ( -- ) 0 >VOID catch throw ;
: HIGH ( -- ) ['] MARK >BITS $100000000 or >VOID execute ;
: DOTS ( -- ) 1 2 3 .s depth throw ;
: CODE-SLOT ( -- ) ['] DROP3 >N throw ;
: NAMED-SLOT ( -- ) ['] emit throw ;
private
: execute ( n -- n ) 1 + ;
: catch ( n -- n ) 1 + ;
public
: OWN-EXECUTE ( -- ) 4 execute throw ;
: OWN-CATCH ( -- ) 4 catch throw ;
private
1 TYPED-BUFFER CODE-CELL [ n n n -- ]
1 TYPED-BUFFER NAMED-CELL [ n -- ]
PERSISTED-PTR-VARIABLE DATA-CELL
' DROP3 0 CODE-CELL !
' emit 0 NAMED-CELL !
s" data cell text" drop DATA-CELL !
0 CODE-CELL WASM-DYNAMIC:OFF WASM-DYNAMIC:CODE-AT !
0 NAMED-CELL WASM-DYNAMIC:OFF WASM-DYNAMIC:NAMED-AT !
DATA-CELL WASM-DYNAMIC:OFF WASM-DYNAMIC:DATA-AT !
;package
0 set-tier
WASM-DYNAMIC:CAPTURE

package WASM-DYNAMIC
private
using WASM-DYNAMIC-WINDOW

\ ---- the runs ---------------------------------------------------------------------
FS-PATH-CAP BUFFER: DIR
variable DIR-U
FS-PATH-CAP BUFFER: PATH
variable PATH-U
DYNAMIC-BUFFER MODULE u8

: SETUP ( -- )
   s" wasm-dynamic" HB-TMP-MKDIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   s" wasm dynamic: modules in " type  DIR u type cr ;

\ The module whose entry is the word named, written as name: whether it
\ validates, and the status its run exits with.
: RUN-ENTRY ( ptr u8 n ptr u8 n -- bool n )
   {: e:ptr eu:n name:ptr nu:n :}
   DIR DIR-U @ name nu PATH JOIN-PATH PATH-U !
   e eu PATH PATH-U @ WASMLINK:LINK
   PATH PATH-U @ WASM-HARNESS:VALID?
   PATH PATH-U @ WASM-HARNESS:RUN ;

\ The module of entry e validates, exits 1, and its throw code is the one the
\ word throws natively.
: THROWS-AS-NATIVE ( ptr u8 n ptr u8 n [ -- ] -- )
   {: e:ptr eu:n name:ptr nu:n word :}
   word catch {: want:n :}
   e eu name nu RUN-ENTRY 1 T= TTRUE
   WASM-HARNESS:THROW-CODE want T= ;

: TRAPS ( ptr u8 n ptr u8 n -- )
   RUN-ENTRY 2 T= TTRUE ;

\ The data image of the module last written, which its data section ends with.
: IMAGE ( -- ptr u8 )
   PATH PATH-U @ FILE-SIZE {: u:n :}
   u MODULE-RESERVE
   PATH PATH-U @ 0 MODULE u READ-ALL u T=
   0 MODULE u AOT-BUF:AOT-DATA-SIZE @ - + ;

\ The module of entry e throws the slot its code literal answers, which the
\ image's code cell at off holds.
: HOLDS ( ptr u8 n ptr u8 n n -- )
   {: e:ptr eu:n name:ptr nu:n off:n :}
   e eu name nu RUN-ENTRY 1 T= TTRUE
   WASM-HARNESS:THROW-CODE  IMAGE off + LE:U64@  T= ;

: W06-CASE ( -- )
   s" W06: a 17-input word called directly, in the aligned frame, throws its answer as natively" T-LABEL
   s" WASM-DYNAMIC-WINDOW:DIRECT" s" direct.wasm" [: DIRECT ;] THROWS-AS-NATIVE
   s" W06: the same word through execute of its code literal's slot answers the same" T-LABEL
   s" WASM-DYNAMIC-WINDOW:VIA-EXECUTE" s" execute.wasm" [: VIA-EXECUTE ;] THROWS-AS-NATIVE
   s" execute of a quotation in its lanes takes its input off the stack and leaves its outputs there" T-LABEL
   s" WASM-DYNAMIC-WINDOW:VIA-LANES" s" lanes.wasm" [: VIA-LANES ;] THROWS-AS-NATIVE ;

: W07-CASE ( -- )
   s" W07: catch of a quotation throwing MIN-N + 1 answers the code whole over the cell beneath" T-LABEL
   s" WASM-DYNAMIC-WINDOW:CAUGHT" s" caught.wasm" [: CAUGHT ;] THROWS-AS-NATIVE
   s" catch of a quotation that returns answers zero, though ctx holds an earlier code" T-LABEL
   s" WASM-DYNAMIC-WINDOW:RETURNED" s" returned.wasm" [: RETURNED ;] THROWS-AS-NATIVE
   s" W07: a trap inside a quotation passes through catch" T-LABEL
   s" WASM-DYNAMIC-WINDOW:TRAPPED" s" trapped.wasm" TRAPS
   s" execute of an xt whose slot takes three cells, with none on the stack, traps" T-LABEL
   s" WASM-DYNAMIC-WINDOW:SHORT" s" short.wasm" TRAPS ;

: XT-CASE ( -- )
   s" execute of xt 0, which names no adapter, traps" T-LABEL
   s" WASM-DYNAMIC-WINDOW:ZERO" s" zero.wasm" TRAPS
   s" catch of xt 0 traps" T-LABEL
   s" WASM-DYNAMIC-WINDOW:ZERO-CAUGHT" s" zero-caught.wasm" TRAPS
   s" execute of an xt whose upper 32 bits are set traps, though its low half is MARK's slot" T-LABEL
   s" WASM-DYNAMIC-WINDOW:HIGH" s" high.wasm" TRAPS ;

: NAME-CASE ( -- )
   s" a package's own execute is an ordinary call and answers as natively" T-LABEL
   s" WASM-DYNAMIC-WINDOW:OWN-EXECUTE" s" own-execute.wasm" [: OWN-EXECUTE ;] THROWS-AS-NATIVE
   s" a package's own catch is an ordinary call and answers as natively" T-LABEL
   s" WASM-DYNAMIC-WINDOW:OWN-CATCH" s" own-catch.wasm" [: OWN-CATCH ;] THROWS-AS-NATIVE ;

\ 1 2 3 .s prints them and leaves them, so the depth thrown after it is 3.
: DOT-S-CASE ( -- )
   s" .s calls kernel-words.f's `.` on each cell, deepest first" T-LABEL
   s" WASM-DYNAMIC-WINDOW:DOTS" s" dots.wasm" RUN-ENTRY 1 T= TTRUE
   WASM-HARNESS:OUT$ S\" 1\n2\n3\n" T$=
   s" .s leaves the cells" T-LABEL
   WASM-HARNESS:THROW-CODE 3 T= ;

: CELL-CASE ( -- )
   s" a code cell holds the slot its window word's code literal answers" T-LABEL
   s" WASM-DYNAMIC-WINDOW:CODE-SLOT" s" code-slot.wasm" CODE-AT @ HOLDS
   s" a code cell holds the slot its engine word's code literal answers" T-LABEL
   s" WASM-DYNAMIC-WINDOW:NAMED-SLOT" s" named-slot.wasm" NAMED-AT @ HOLDS
   s" a DATA cell holds WPROF's data base plus its text's offset in the image" T-LABEL
   IMAGE {: m:ptr :}
   m DATA-AT @ + LE:U64@ WPROF:DATA-BASE - {: t:n :}
   m t + 14 s" data cell text" T$= ;

public

: RUN ( -- )
   T-RESET
   SETUP
   W06-CASE
   W07-CASE
   XT-CASE
   NAME-CASE
   DOT-S-CASE
   CELL-CASE
   T-REPORT ;

;using
;package

WASM-DYNAMIC:RUN
