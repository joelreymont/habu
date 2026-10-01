\ x86-64-link-records.f - src/habu/link-x64.f lays a captured window out for an
\ x86-64 image: the kernel's records and the window's at their final addresses,
\ every routine copied from the capture's shadow to a code slot, wids rebased
\ from the host's window to the image's numbering, the protected-wid bitmap and
\ the name index the writer builds.
\
\ WHAT IS DRIVEN. A small window compiled at tier 1 with an x86-64 shadow open,
\ as test/aot-shadow-capture.f compiles its own: a package, a leaf, a caller, a
\ `does>` definer, a word whose name is past sixteen bytes, and the package's
\ public wordlist protected. Then the real capture and the shadow reader, the
\ x86-64 kernel's rows emitted into a stream by test/x86-64-boot-harness.f, and
\ X64LINK:LAYOUT over both, as the image writer runs it before the stream links.
\
\ WHAT IS ASKED. Which record lies at each image index; that each code record
\ enters its own routine's bytes on a code slot and a does> companion enters at
\ its clause; a long name's bytes in the code band; a kernel body's record and
\ its min-in; each wid against the host's own, rebased; the bitmap bit; that the
\ index finds every record by its own name and wordlist, folded, and misses a
\ name in another wordlist.
\
\ THE INDEX IS MEASURED DETERMINISTIC. A child built with one record, one
\ wordlist and one DATA cell more ahead of the window lays out the same bytes:
\ records, code band, bitmap and index, by digest.
\
\ WHAT IS REFUSED. A capture the layout cannot place, by name, each in a child
\ because a refusal ends the process: a window record with no x86-64 routine (a
\ variable), a window that strips a shadowed private word, so its routine names
\ no shipped record, and a protected-wid row forged outside the window.
\
\ LOAD ORDER. The x86-64 side first, then the ARM64 code layer the capture
\ needs: src/arch/arm64/icode.f defines CODE, LBL and ASM-LEN as globals, and a
\ `using X64CODE` opened after them refuses (src/habu/link-x64.f "LOAD ORDER").
\
\ Registered as `SUITE x86-64-link-records`. Run standalone from the repository
\ root: bin/hb --load test/x86-64-link-records.f
\ A child: bin/hb --load test/x86-64-link-records.f -- shift|stray|strip|wid

package X64LT
public
ndict@ here  variable PRE-R  variable PRE-D  PRE-D !  PRE-R !
variable PUB-WID                     \ the window package's public wordlist, host's id
;package

require lib/string.f

package X64LT
public
: MODE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ a u STR= exit then
   false ;
\ The determinism child puts a record, a wordlist and a DATA cell ahead of it all.
: SHIFT$ ( -- ptr u8 n )
   s" shift" MODE? if s" variable X64LT-SHIFT wordlist drop" exit then
   s" " ;
\ What a refusal child adds to the window.
: EXTRA$ ( -- ptr u8 n )
   s" stray" MODE? if s" variable STRAY" exit then
   s" strip" MODE? if s" private : HIDDEN ( -- ) ; public" exit then
   s" " ;
;package
X64LT:SHIFT$ evaluate

require test/x86-64-boot-harness.f
require lib/le.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/habu/aot-shadow.f
require src/compiler/native/string.f
require src/compiler/native/x64ir.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/abi.f
require src/arch/x86-64/passes.f
require src/habu/link-x64.f
require src/core/sha256.f
require lib/process.f
require lib/process-argv.f

\ A binding is a multi-cell value, which only a compiled body may hold.
package X64LT
public
: OPEN-X64 ( -- ) X64ABI:BINDING NSHADOW:OPEN ;
;package
X64LT:OPEN-X64
AOT-ARM:WINDOW-OPEN
NSTR:WINDOW-OPEN

package X64LT-WIN
public
get-current X64LT:PUB-WID !

1 set-tier
: LEAF ( n n -- n ) + ;
: CALLER ( n -- n ) dup LEAF 1 + ;
: CONST ( n -- ) create , does> ( -- n ) @ ;
: SPELLED-PAST-SIXTEEN ( -- n ) 7 ;
X64LT:EXTRA$ evaluate
0 set-tier

get-current prot-wid-add
;package

AOT-ARM:WINDOW-CLOSE

package X64LT
using AOT-BUF

\ ---- the window, as the capture numbers it ----------------------------------
: WIDX ( ptr u8 n -- n ) XREF-FIND-INDEX AOT-ARM:R0 @ - ;
: W-LEAF ( -- n ) s" X64LT-WIN:LEAF" WIDX ;
: W-CALLER ( -- n ) s" X64LT-WIN:CALLER" WIDX ;
: W-CONST ( -- n ) s" X64LT-WIN:CONST" WIDX ;
: W-LONG ( -- n ) s" X64LT-WIN:SPELLED-PAST-SIXTEEN" WIDX ;

\ ---- the layout --------------------------------------------------------------
: IMG ( n -- n ) X64LINK:PRIMS + ;
: RF@ ( n n -- n ) {: k:n f:n :} X64LINK:DICT$ drop k DREC * + f + LE:U64@ ;
: NAME$ ( n -- ptr u8 n ) X64LINK:REC-NAME$ ;
: BAND ( n -- ptr u8 ) {: va:n :} X64LINK:CODE$ drop va X64LINK:CODE-VA - + ;
: SH@ ( n n -- n ) {: r:n f:n :}
   AOT-SHADOW:REC-BUF@ r AOT-SHADOW:REC-ROW * + f + LE:U32@ ;

\ The shadow row of window record w.
: ROW-OF ( n -- n ) {: w:n :}
   -1
   AOT-SHADOW:REC-N @ 0 ?do i 0 SH@ w = if drop i leave then loop ;

: PRIM-OF ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1
   ENGINE-PRIMS:COUNT 0 ?do i ENGINE-PRIMS:NAME$ a u STR= if drop i leave then loop ;

: LONG-PRIM ( -- n )
   -1
   ENGINE-PRIMS:COUNT 0 ?do i ENGINE-PRIMS:NAME-LEN DNAME-INL > if drop i leave then loop ;

: BIT? ( n -- bool ) {: w:n :}
   X64LINK:BITS$ drop w 3 rshift + c@ w 7 and rshift 1 and 0<> ;

: SLOTS-USED ( -- n )
   0
   HIDX-SLOTS 0 ?do X64LINK:INDEX$ drop i 4 * + LE:U32@ 0<> if 1+ then loop ;

\ ---- the capture and the kernel ------------------------------------------------
: CAPTURE ( -- )
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
   AOT-CAPTURE:SHADOW-CAPTURE ;

: KERNEL ( -- ) X64HARNESS:INIT  false X64HARNESS:BOOT-OPEN, ;

\ ---- the cases -----------------------------------------------------------------
: RECORDS-CASE ( -- )
   s" the image dictionary is the kernel's records, then the window's in capture order" T-LABEL
   AOT-REC-N @  AOT-ARM:R1 @ AOT-ARM:R0 @ -  T=
   X64LINK:PRIMS ENGINE-PRIMS:COUNT T=
   X64LINK:RECORDS X64LINK:PRIMS AOT-REC-N @ + T=
   X64LINK:DICT$ nip X64LINK:RECORDS DREC * T=
   s" dup" PRIM-OF NAME$ s" dup" T$=
   0 IMG NAME$ s" X64LT-WIN" T$=
   0 IMG X64KERNEL:REC-WID RF@ DICT-WL:NAMESPACE T=
   W-LEAF IMG NAME$ s" LEAF" T$=
   W-CALLER IMG NAME$ s" CALLER" T$=
   W-CONST IMG NAME$ s" CONST" T$=
   W-CONST 1+ IMG NAME$ s" CONST;does" T$= ;

: ROUTINE-CASE ( -- )
   s" a code record enters its own routine, copied from the shadow to a code slot and filled with int3" T-LABEL
   W-LEAF ROW-OF {: r:n :}
   W-LEAF IMG 0 RF@ {: va:n :}
   va X64LINK:CODE-VA -  X64KERNEL:CODE-SLOT mod 0 T=
   va  X64LINK:CODE-VA r 4 SH@ X64LINK:PLACED +  T=
   va BAND r 8 SH@  AOT-SHADOW:CODE-BUF@ r 4 SH@ + r 8 SH@  T$=
   va r 8 SH@ + BAND c@ $CC T=
   W-LEAF IMG 8 RF@  r 8 SH@ CODE-SPAN:EXACT  T=
   W-CALLER IMG 0 RF@ va <> TTRUE ;

: DOES-CASE ( -- )
   s" a does> companion enters its definer's routine at the clause, spanning the rest of it" T-LABEL
   W-CONST ROW-OF {: r:n :}
   r 1+ 12 SH@ {: entry:n :}
   entry 0 > TTRUE
   W-CONST 1+ IMG 0 RF@  W-CONST IMG 0 RF@ entry +  T=
   W-CONST 1+ IMG 8 RF@  r 8 SH@ entry - CODE-SPAN:EXACT  T= ;

: LONG-NAME-CASE ( -- )
   s" a name past sixteen bytes lies in the code band ahead of the routines, its record holding its address" T-LABEL
   W-LONG IMG 16 RF@ DNAME-EXT and 0<> TTRUE
   W-LONG IMG X64KERNEL:REC-NAME RF@ {: at:n :}
   at X64LINK:CODE-VA >=  at W-LEAF IMG 0 RF@ < and TTRUE
   at BAND 20 s" SPELLED-PAST-SIXTEEN" T$=
   LONG-PRIM {: p:n :}
   p 0 >= TTRUE
   p 16 RF@ DNAME-EXT and 0<> TTRUE
   p NAME$ p ENGINE-PRIMS:NAME$ T$= ;

: PRIM-CASE ( -- )
   s" a kernel body's record enters the text, spans its body exactly, and states the primitive's min-in" T-LABEL
   s" dup" PRIM-OF {: p:n :}
   p 0 RF@ {: at:n :}
   at X64LINK:TEXT-VA >=  at X64LINK:TEXT-VA X64CODE:ASM-LEN + < and TTRUE
   p 8 RF@ CODE-SPAN:FULL? TTRUE
   p 16 RF@ DNAME-MIN-IN-MASK and 52 rshift 1 T=
   p 16 RF@ DNAME-LEN-MASK and 3 T=
   p X64KERNEL:REC-WID RF@ 0 T= ;

: WID-CASE ( -- )
   s" every wid is the host's, rebased from the window's first wordlist to T0" T-LABEL
   PUB-WID @ AOT-ARM:W0 @ - X64LINK:T0 + {: w:n :}
   W-LEAF IMG X64KERNEL:REC-WID RF@ w T=
   W-CONST 1+ IMG X64KERNEL:REC-WID RF@ w T=
   0 IMG 0 RF@ w T=
   X64LINK:T0 FIRST-DYNAMIC-WID T=
   X64LINK:WIDN X64LINK:T0 AOT-WID-SPAN @ + T= ;

: BITMAP-CASE ( -- )
   s" the window's protected wordlist is the bit of its rebased wid, and the only one" T-LABEL
   AOT-PWIN-N @ 1 T=
   W-LEAF IMG X64KERNEL:REC-WID RF@ BIT? TTRUE
   0
   PROT-WID-MAX 0 ?do i BIT? if 1+ then loop
   1 T= ;

: INDEX-CASE ( -- )
   s" the writer's index finds every record by its own name and wordlist, folded, and nothing else" T-LABEL
   X64LINK:CLAIMS X64LINK:RECORDS T=
   SLOTS-USED X64LINK:RECORDS T=
   0
   X64LINK:RECORDS 0 ?do
      i NAME$ i X64KERNEL:REC-WID RF@ X64LINK:FIND i <> if 1+ then
   loop
   0 T=
   W-LEAF IMG X64KERNEL:REC-WID RF@ {: w:n :}
   s" leaf" w X64LINK:FIND W-LEAF IMG T=
   s" const;DOES" w X64LINK:FIND W-CONST 1+ IMG T=
   s" DUP" 0 X64LINK:FIND s" dup" PRIM-OF T=
   s" LEAF" 0 X64LINK:FIND -1 T=
   s" UNDEFINED-HERE" w X64LINK:FIND -1 T= ;

\ ---- the children --------------------------------------------------------------
create SHA-CTX SHA256-CTX-BYTES allot
create DIGEST 32 allot
create HEX 64 allot
create LINE 96 allot    variable LINE-U

: FEED ( ptr u8 n -- ) {: a:ptr u:n :} SHA-CTX a u SHA256-FEED ;
: HEX-DIGIT ( n -- n ) dup 10 < if $30 + exit then $57 + ;

\ What the layout wrote, as one digest in hex.
: LAYOUT-HEX ( -- ptr u8 n )
   SHA-CTX SHA256-BEGIN
   X64LINK:DICT$ FEED  X64LINK:CODE$ FEED  X64LINK:BITS$ FEED  X64LINK:INDEX$ FEED
   SHA-CTX DIGEST SHA256-END
   32 0 ?do
      DIGEST i + c@ {: b:n :}
      b 4 rshift HEX-DIGIT HEX i 2 * + c!
      b $F and HEX-DIGIT HEX i 2 * + 1+ c!
   loop
   HEX 64 ;

: DIGEST$ ( -- ptr u8 n ) s" x64-link: digest " ;

$8000 constant CAP
240000 constant CHILD-MS
create OUT CAP allot    variable OUT-U
create ERR CAP allot    variable ERR-U
create EMPTY 1 allot                 \ zero-length stdin
variable RC

: RUN-CHILD ( ptr u8 n -- ) {: m:ptr mu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/x86-64-link-records.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   m mu >LEN PROC-ARGV+
   s" bin/hb" >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  CHILD-MS >MS
   RUN-ARGV-STDIN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: SHOW-CHILD ( -- )
   s" x86-64-link-records: child rc=" type RC @ . cr
   s" x86-64-link-records: child stdout:" type cr OUT OUT-U @ type cr
   s" x86-64-link-records: child stderr:" type cr ERR ERR-U @ type cr ;

: SHIFT-CASE ( -- )
   s" a host with a record, a wordlist and a DATA cell more ahead of the window lays out the same bytes" T-LABEL
   DIGEST$ {: p:ptr pu:n :}
   p LINE pu BYTE-COPY
   LAYOUT-HEX {: h:ptr hu:n :}
   h LINE pu + hu BYTE-COPY
   pu hu + LINE-U !
   s" shift" RUN-CHILD
   OUT OUT-U @ LINE LINE-U @ CONTAINS? {: same:bool :}
   RC @ 0<> same 0= or if SHOW-CHILD then
   RC @ 0 T=
   same TTRUE ;

\ The child exits the layout's refusal status, naming `w` on stdout, where the
\ capture's refusals name what they refuse, and dying with `m`.
: REFUSED ( ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n w:ptr wu:n m:ptr mu:n :}
   mode modeu RUN-CHILD
   ERR ERR-U @ m mu CONTAINS?  OUT OUT-U @ w wu CONTAINS? and {: said:bool :}
   RC @ 74 <> said 0= or if SHOW-CHILD then
   RC @ 74 T=
   said TTRUE ;

: STRAY-CASE ( -- )
   s" a window record with no x86-64 routine is refused by name" T-LABEL
   s" stray" s" window record STRAY has no x86-64 routine"
   s" x64link: a code record the capture's shadow carries no routine for" REFUSED ;

: STRIP-CASE ( -- )
   s" a capture that strips a shadowed private word leaves a routine no shipped record takes" T-LABEL
   s" strip" s" names none of the"
   s" x64link: the shadow's routines do not match the shipped records" REFUSED ;

: WID-FORGED-CASE ( -- )
   s" a protected-wid row outside the capture window is refused by its row" T-LABEL
   s" wid" s" protected-wid row 0"
   s" x64link: a protected wid outside the window or at PROT-WID-MAX" REFUSED ;

\ The wid child moves the window's one protected row to its span.
: FORGE-WID ( -- ) AOT-WID-SPAN @ AOT-PWIN-BUF@ LE:U32! ;

public

: RUN ( -- )
   CAPTURE
   NSHADOW:CLOSE
   s" stray" MODE?  s" strip" MODE? or if
      X64LINK:LAYOUT s" x86-64-link-records: laid out" type cr exit
   then
   s" wid" MODE? if FORGE-WID X64LINK:LAYOUT s" x86-64-link-records: laid out" type cr exit then
   KERNEL
   X64LINK:LAYOUT
   s" shift" MODE? if DIGEST$ type LAYOUT-HEX type cr exit then
   T-RESET
   RECORDS-CASE
   ROUTINE-CASE
   DOES-CASE
   LONG-NAME-CASE
   PRIM-CASE
   WID-CASE
   BITMAP-CASE
   INDEX-CASE
   SHIFT-CASE
   STRAY-CASE
   STRIP-CASE
   WID-FORGED-CASE
   s" x86-64-link-records: prims=" type X64LINK:PRIMS .
   s" records=" type X64LINK:RECORDS .
   s" code=" type X64LINK:CODE$ nip . cr
   T-REPORT ;

;using
;package

X64LT:RUN
