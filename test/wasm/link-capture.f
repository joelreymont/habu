\ link-capture.f - WASMLINK, src/habu/link-wasm.f: a window compiled through an
\ open Wasm shadow with src/arch/wasm/kernel-words.f, captured and linked into
\ one module, and the windows the link refuses.
\
\ The module's data image is the window's DATA as the capture found it, byte
\ for byte; the window holds a word whose text literal lies in that DATA, and a
\ constant no call names, which links with no routine.
\ test/wasm/dynamic.f runs linked modules under wasm-tools and bun.
\
\ Each refused window is compiled, captured and linked in a child of its own,
\ forked before this process opens a window: a call to a defer names a record
\ with no Wasm routine, a call to `here` names a word no kernel row answers, and
\ a window without kernel-words.f ships no WKWORDS:DOT for `.`. Each exits 74
\ and says what it refuses.
\
\ Registered as `SUITE wasm-link-capture`. Run standalone from the repository
\ root: bin/hb --load test/wasm/link-capture.f

package LINK-CAPTURE
public
ndict@ here  variable PRE-R  variable PRE-D  PRE-D !  PRE-R !
;package

require lib/string.f
require lib/test.f
require lib/test/subject.f
require lib/process.f
require lib/fs.f
require lib/fs-mutate.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/compiler/native/string.f
require src/compiler/native/shadow.f
require src/arch/wasm/backend.f
require src/arch/wasm/capture.f
require src/habu/link-wasm.f

package LINK-CAPTURE
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

\ A refused window's link: its word W as the entry.
: REFUSED-LINK ( -- )
   CAPTURE
   s" W" s" /dev/null" WASMLINK:LINK ;

private

$1000 constant CAP
60000 constant DEADLINE-MS
CAP BUFFER: OUT
CAP BUFFER: ERR
FS-PATH-CAP BUFFER: PATH
variable PATH-U
DYNAMIC-BUFFER MODULE u8

\ ---- the refusals ----------------------------------------------------------------
\ A window of text, linked in a child; answers its stdout and stderr lengths
\ and its exit status.
: CHILD ( ptr u8 n -- n n n )
   {: a:ptr u:n :}
   SB-RESET
   s" LINK-CAPTURE:OPEN 1 set-tier " SB-APPEND
   a u SB-APPEND
   s"  0 set-tier LINK-CAPTURE:REFUSED-LINK" SB-APPEND
   SB$  OUT CAP >LEN  ERR CAP >LEN  DEADLINE-MS >MS  SUBJECT:RUN
   PROC-OUTCOME>RC RC>N {: ou:len eu:len rc:n :}
   ou LEN>N  eu LEN>N  rc ;

\ The window refused: it exits 74, stdout naming what and stderr the refusal.
: REFUSED ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: a:ptr u:n what:ptr wu:n why:ptr yu:n :}
   a u CHILD {: ou:n eu:n rc:n :}
   rc 74 T=
   OUT ou RTRIM what wu T$=
   ERR eu RTRIM why yu T$= ;

: REFUSALS ( -- )
   s" a call to a defer names a code record with no Wasm routine" T-LABEL
   s" require src/arch/wasm/kernel-words.f defer LCD ( -- ) : W ( -- ) LCD ;"
   s" wasmlink: window record LCD has no Wasm routine"
   s" wasmlink: a code record the capture's shadow carries no routine for" REFUSED
   s" a call to `here` names a word no kernel row answers" T-LABEL
   s" require src/arch/wasm/kernel-words.f : W ( -- ) here drop ;"
   s" wasmlink: here is a name no kernel row answers"
   s" wasmlink: a name WKERNEL's map does not answer" REFUSED
   s" without kernel-words.f the window ships no WKWORDS:DOT for `.`" T-LABEL
   s" : W ( -- ) 7 . ;"
   s" wasmlink: WKWORDS:DOT names no record the capture ships"
   s" wasmlink: a name no shipped record answers" REFUSED ;

\ ---- the linked window ----------------------------------------------------------
\ The window's DATA where this process holds it.
: WINDOW$ ( -- ptr u8 n )
   data-base BYTE-VIEW  AOT-ARM:D0 @ DATA-VA VA>N -  +  AOT-BUF:AOT-DATA-SIZE @ ;

\ The module, read back; its data section, last, ends with the image.
: LINKED ( -- )
   s" link-capture" HB-TMP-MKDIR  s" link-capture.wasm"  PATH JOIN-PATH PATH-U !
   s" LINK-CAPTURE-WINDOW:MAIN" PATH PATH-U @ WASMLINK:LINK
   PATH PATH-U @ FILE-SIZE {: u:n :}
   u MODULE-RESERVE
   PATH PATH-U @ 0 MODULE u READ-ALL u T= ;

: IMAGE-CASE ( -- )
   s" the module's data image is the window's DATA as the capture found it" T-LABEL
   PATH PATH-U @ FILE-SIZE {: u:n :}
   WINDOW$ {: a:ptr n:n :}
   0 MODULE u n - +  n  a n  STR= TTRUE ;

public

: REFUSE ( -- )
   T-RESET
   REFUSALS ;

: RUN ( -- )
   CAPTURE
   LINKED
   IMAGE-CASE
   T-REPORT ;

;package

WBACK:INSTALL
LINK-CAPTURE:REFUSE

LINK-CAPTURE:OPEN
1 set-tier
require src/arch/wasm/kernel-words.f
package LINK-CAPTURE-WINDOW
public
5 constant LCK
: MAIN ( -- ) s" linked text" type ;
;package
0 set-tier

LINK-CAPTURE:RUN
