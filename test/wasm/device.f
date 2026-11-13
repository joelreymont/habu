\ device.f - the wasm device check: WLINK's modules validated by wasm-tools and
\ run by bun through test/wasm/harness.f; the wasm-device suite runs it.
\ The wasm and browser suites it requires run first and report on their own;
\ the modules stay in the printed directories.

require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/arch/wasm/leb.f
require src/arch/wasm/profile.f
require src/arch/wasm/link.f
require test/wasm/harness.f
require test/wasm/w03.f

require test/wasm/calls-test.f
require test/wasm/hb-build.f
require test/wasm/data-test.f
require test/wasm/storage-test.f
require test/wasm/differential.f
require test/browser/echo-test.f
require test/browser/storage-test.f
require test/browser/navigation-test.f
require test/browser/request-test.f
require test/browser/app-test.f
require test/browser/display-test.f
require test/browser/take-test.f

package WASM-DEVICE
private
using WASM-W03

PATH-CAP BUFFER: DIR
variable DIR-U
PATH-CAP BUFFER: PATH
variable PATH-U

: SETUP ( -- )
   s" wasm-device" FS:MKTEMP-DIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   s" wasm device: modules in " type  DIR u type cr ;

\ The module run calling function e, written as name in DIR; answers its path.
: MODULE ( n ptr u8 n -- ptr u8 n )
   {: e:n name:ptr nu:n :}
   DIR DIR-U @ name nu PATH FS:JOIN-PATH PATH-U !
   PATH PATH-U @  e WLINK:LINK  FS:WRITE-ALL
   PATH PATH-U @ ;

\ ---- bodies, in WASM-W03's buffer, test/wasm/w03.f --------------------------------
: S32, ( n -- )   ROOM WLEB:S32! BODY-U +! ;
: S64, ( n -- )   ROOM WLEB:S64! BODY-U +! ;

\ The body as kernel function 0 of a new link, of no lanes.
: KERNEL0 ( -- )
   WLINK:RESET
   BODY$ 0 0 0 WLINK-ORIGIN:KERNEL WLINK:FUNCTION+ drop ;

\ ---- one function ------------------------------------------------------------------
$8000000000000001 constant CODE        \ MIN-N + 1, which a double rounds to MIN-N
: TEXT$ ( -- ptr u8 n )  S\" wasm ok\n" ;

\ i32.const ctx's base, the address of a store into one of its fields.
: CTX, ( -- )  $41 B, WPROF:CTX-BASE S32, ;

\ TEXT$ into OUT a byte at a time and its length into out-len, CODE into the
\ throw code, then status 1.
: THROWER ( -- )
   0 BODY-U !
   0 B,
   TEXT$ {: t:ptr tu:n :}
   tu 0 do
      $41 B, WPROF:OUT-BASE i + S32,  $41 B, t i + c@ S32,  $3A B, 0 B, 0 B,
   loop
   CTX,  $41 B, tu S32,  $36 B, 2 B, WPROF:CTX-OUT-LEN B,
   CTX,  $42 B, CODE S64,  $37 B, 3 B, WPROF:CTX-THROW-CODE B,
   $41 B, 1 S32,
   $0B B, ;

: ONE-ROW ( -- )
   THROWER KERNEL0
   0 s" one.wasm" MODULE {: a:ptr u:n :}
   s" a one-function module validates" T-LABEL
   a u WASM-HARNESS:VALID? TTRUE
   s" it exits 1, its entry's status" T-LABEL
   a u WASM-HARNESS:RUN 1 T=
   s" the bytes it wrote to OUT are read back" T-LABEL
   WASM-HARNESS:OUT$ TEXT$ T$=
   s" its throw code is read back whole" T-LABEL
   WASM-HARNESS:THROW-CODE CODE T= ;

: TRAP-ROW ( -- )
   0 BODY-U !  0 B,  $00 B,  $0B B,            \ unreachable
   KERNEL0
   0 s" trap.wasm" MODULE {: a:ptr u:n :}
   s" a body that traps validates" T-LABEL
   a u WASM-HARNESS:VALID? TTRUE
   s" it exits 2" T-LABEL
   a u WASM-HARNESS:RUN 2 T= ;

: INVALID-ROW ( -- )
   0 BODY-U !  0 B,  $42 B, 0 S64,  $0B B,     \ an i64 for its i32 status
   KERNEL0
   0 s" invalid.wasm" MODULE {: a:ptr u:n :}
   s" a body answering the wrong type is invalid" T-LABEL
   a u WASM-HARNESS:VALID? TFALSE
   s" it exits 3" T-LABEL
   a u WASM-HARNESS:RUN 3 T= ;

\ ---- W03 -------------------------------------------------------------------------
: W03-ROW ( -- )
   BUILD
   CHAIN s" w03.wasm" MODULE {: a:ptr u:n :}
   s" W03's module validates" T-LABEL
   a u WASM-HARNESS:VALID? TTRUE
   s" W03's module runs to status 0" T-LABEL
   a u WASM-HARNESS:RUN 0 T=
   s" W03's module writes no output" T-LABEL
   WASM-HARNESS:OUT$ nip 0 T= ;

public

: RUN ( -- )
   T-RESET
   SETUP
   ONE-ROW
   TRAP-ROW
   INVALID-ROW
   W03-ROW
   T-REPORT ;

;package

WASM-DEVICE:RUN
