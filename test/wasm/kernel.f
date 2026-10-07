\ kernel.f - src/arch/wasm/kernel-words.f beside the engine's primitives, on the
\ host. Each engine name that WKERNEL's map (src/arch/wasm/kernel.f) answers
\ with a word of kernel-words.f runs twice on the same arguments, each time in a
\ child of its own: once as the engine's primitive and once as the word the map
\ names. The two print the same bytes to stdout and to stderr and exit alike.
\ The arguments sit at each primitive's edges: zero, both signs, MIN-N and
\ MAX-N, a zero divisor; for `f.` the signed zeros and NaNs, the infinities, a
\ magnitude past MAX-N, a fraction that pads and one that truncates. A name no
\ provider answers is refused with E-WLINK-UNRESOLVED.
\
\ The rows the map answers with hand-built functions run in Wasm, in
\ test/wasm/device.f.
\
\ Registered as `SUITE wasm-kernel-words`. Run standalone from the repository
\ root: bin/hb --load test/wasm/kernel.f

require lib/test.f
require lib/test/subject.f
require lib/string.f
require lib/process.f
require lib/ieee754.f
require src/arch/wasm/link.f
require src/arch/wasm/kernel-words.f
require src/arch/wasm/kernel.f

package WKERNEL-TEST
private

$1000 constant CAP
10000 constant DEADLINE-MS
CAP BUFFER: OUT-E
CAP BUFFER: ERR-E
CAP BUFFER: OUT-K
CAP BUFFER: ERR-K

\ The word the map names for an engine name; a row is a failed case.
: WORD$ ( ptr u8 n -- ptr u8 n )
   WKERNEL:PROVIDER MATCH provider
      row OF drop  s" the map answers a word" T-LABEL  false TTRUE  s" " ENDOF
      word OF ENDOF
   ;MATCH ;

\ The text args, w and tail, run in a child with its stdout into o and its
\ stderr into e; answers their lengths and its exit status.
: CHILD ( ptr u8 n ptr u8 n ptr u8 n ptr u8 ptr u8 -- n n n )
   {: a:ptr au:n w:ptr wu:n t:ptr tu:n o:ptr e:ptr :}
   SB-RESET  a au SB-APPEND  s"  " SB-APPEND  w wu SB-APPEND
   s"  " SB-APPEND  t tu SB-APPEND
   SB$  o CAP >LEN  e CAP >LEN  DEADLINE-MS >MS  SUBJECT:RUN
   PROC-OUTCOME>RC RC>N {: ou:len eu:len rc:n :}
   ou LEN>N  eu LEN>N  rc ;

\ The engine name on args, its results consumed by tail, against the word the
\ map names for it.
: SAME ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: a:ptr au:n nm:ptr nu:n t:ptr tu:n :}
   SB-RESET  a au SB-APPEND  s"  " SB-APPEND  nm nu SB-APPEND  SB$ T-LABEL
   a au nm nu t tu OUT-E ERR-E CHILD {: oe:n ee:n re:n :}
   a au  nm nu WORD$  t tu OUT-K ERR-K CHILD {: ok:n ek:n rk:n :}
   OUT-K ok OUT-E oe T$=
   ERR-K ek ERR-E ee T$=
   rk re T= ;

\ ---- the printers ---------------------------------------------------------------
: INTEGERS ( -- )
   s" 0" s" ." s" " SAME
   s" 7" s" ." s" " SAME
   s" -7" s" ." s" " SAME
   s" 1234567890" s" ." s" " SAME
   s" $8000000000000000" s" ." s" " SAME
   s" $7FFFFFFFFFFFFFFF" s" ." s" " SAME
   s" 0" s" u." s" " SAME
   s" 10" s" u." s" " SAME
   s" -1" s" u." s" " SAME
   s" $8000000000000000" s" u." s" " SAME ;

\ Each real by its bits.
: REALS ( -- )
   s" $3FF8000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ 1.5
   s" $C004000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ -2.5
   s" $4008800000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ 3.0625, a fraction that pads
   s" $405EDD2F1A9FBE77 IEEE754:BITS>F64" s" f." s" " SAME   \ 123.456, one that truncates
   s" $3EB0C6F7A0B5ED8D IEEE754:BITS>F64" s" f." s" " SAME   \ 1e-6, at the sixth digit
   s" $0000000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ +0.0
   s" $8000000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ -0.0
   s" $7FF8000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ a NaN
   s" $FFF8000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ a NaN, bit 63 set
   s" $7FF0000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ +inf
   s" $FFF0000000000000 IEEE754:BITS>F64" s" f." s" " SAME   \ -inf
   s" $43E0000000000000 IEEE754:BITS>F64" s" f." s" " SAME ; \ 2^63, past MAX-N

: TEXT ( -- )
   s\" s\" kernel words\" " s" type" s" " SAME
   s\" s\" \" " s" type" s" " SAME
   s" " s" cr" s" " SAME
   s" " s" space" s" " SAME ;

\ ---- the arithmetic ----------------------------------------------------------------
: NUMBERS ( -- )
   s" 5" s" negate" s" ." SAME
   s" $8000000000000000" s" negate" s" ." SAME
   s" -7" s" abs" s" ." SAME
   s" 7" s" abs" s" ." SAME
   s" $8000000000000000" s" abs" s" ." SAME
   s" 3 5" s" min" s" ." SAME
   s" 5 3" s" min" s" ." SAME
   s" $8000000000000000 $7FFFFFFFFFFFFFFF" s" min" s" ." SAME
   s" 7 2" s" /mod" s" . ." SAME
   s" -7 2" s" /mod" s" . ." SAME
   s" 7 -2" s" /mod" s" . ." SAME
   s" -7 -2" s" /mod" s" . ." SAME
   s" $8000000000000000 -1" s" /mod" s" . ." SAME
   s" 5 0" s" /mod" s" . ." SAME ;

: TESTS ( -- )
   s" -1" s" 0<" s" ." SAME
   s" 0" s" 0<" s" ." SAME
   s" $8000000000000000" s" 0<" s" ." SAME
   s" 0" s" 0<>" s" ." SAME
   s" -1" s" 0<>" s" ." SAME
   s" " s" true" s" ." SAME
   s" " s" false" s" ." SAME ;

\ ---- the refusal -------------------------------------------------------------------
: UNANSWERED ( -- )
   s" key" WKERNEL:PROVIDER MATCH provider
      row OF drop ENDOF
      word OF 2drop ENDOF
   ;MATCH ;

: REFUSAL ( -- )
   s" a name no provider answers is refused" T-LABEL
   [: UNANSWERED ;] E-WLINK-UNRESOLVED TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   INTEGERS
   REALS
   TEXT
   NUMBERS
   TESTS
   REFUSAL
   T-REPORT ;

;package

WKERNEL-TEST:RUN
