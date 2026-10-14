\ app-build-image.f - a keyed REPL builder with fixed support compiled once.
\ The default engine remains suitable for stripped AOT images. This image is
\ selected only by hb-build --repl, before that command stages child argv/env.

require src/lib/errors.f
require src/lib/fs.f
require src/lib/source.f
require src/lib/process-argv.f
require src/lib/content-key.f
require tools/build-support.f
require tools/keyed-image.f

package APP-BUILD-IMAGE

create KEY-HEX KEYED-IMAGE:KEY-HEX-LEN allot
create PATH-BUF FS-PATH-CAP allot
create ENTRY-BUF FS-PATH-CAP allot
create ROOT-BUF FS-PATH-CAP allot
create SELECTED-BUF FS-PATH-CAP allot
create RUNNING-BUF FS-PATH-CAP allot
variable PATH-U
variable ENTRY-U
variable ROOT-U
variable SELECTED-U
variable RUNNING-U
TYPED-VARIABLE BEFORE [ ptr u8 n -- ]

: PATH-BYTES ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

\ APP-IMAGE:SAVE runs from outer stdin after the support include returns.
: PROGRAM$ ( -- ptr u8 n )
   S\" require tools/app-build.f\nAPP-BUILD:SAVE-ROOT\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" ;

: FAMILY$ ( -- ptr u8 n )
   s" app-builder" ;

: COPY-ENTRY! ( ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n dst:ptr up:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;


: FILL-KEY ( CONTENT-KEY:fold -- CONTENT-KEY:fold )
   s" app-build-image-v1" CONTENT-KEY:TEXT+
   BUILD-FIXPOINT:BF-ENGINE$ s" host-engine" CONTENT-KEY:FILE-NAMED+
   SELECTED-BUF SELECTED-U @ CONTENT-KEY:TEXT+
   PROGRAM$ CONTENT-KEY:TEXT+
   ENTRY-BUF ENTRY-U @ ROOT-BUF ROOT-U @ BEFORE @
   KEYED-IMAGE:CLOSURE-IN-EACH+ ;

: RESOLVE-CONTENT ( -- )
   s" tools/app-build.f" SOURCE-ROOT:RESOLVE drop
   ENTRY-BUF ENTRY-U COPY-ENTRY!
   SOURCE-ROOT:RESOLVED-ROOT$ ROOT-BUF ROOT-U COPY-ENTRY!
   CONTENT-KEY:OPEN {: fold:CONTENT-KEY:fold :}
   fold [: FILL-KEY ;] catch {: rc:n :} drop
   rc 0<> if fold CONTENT-KEY:DISCARD rc throw then
   fold KEY-HEX CONTENT-KEY:FINAL-HEX
   KEY-HEX FAMILY$ PATH-BUF PATH-U KEYED-IMAGE:PATH! ;

\ Discover the same support file the selected engine will load, with the
\ invocation CWD and that engine's fallback root. Restore the caller on error.
: RESOLVE ( -- )
   SOURCE-ROOT:ENGINE$ RUNNING-BUF RUNNING-U COPY-ENTRY!
   BUILD-FIXPOINT:BF-ENGINE$ SOURCE-ROOT:ENGINE-FOR$
   SELECTED-BUF SELECTED-U COPY-ENTRY!
   SELECTED-BUF SELECTED-U @ SOURCE-ROOT:ENGINE!
   [: ['] RESOLVE-CONTENT SOURCE-ROOT:WITH-CWD ;] catch {: rc:n :}
   RUNNING-BUF RUNNING-U @ SOURCE-ROOT:ENGINE!
   rc 0 <> if rc throw then ;

: OUT-ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: HEX? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u KEYED-IMAGE:KEY-HEX-LEN <> if false exit then
   u 0 ?do
      a i + c@ {: c:n :}
      c 48 >= c 57 <= and
      c 65 >= c 70 <= and or
      c 97 >= c 102 <= and or 0= if false unloop exit then
   loop
   true ;

public

\ The observer runs before each closure member is read. It is held only for
\ resolution; clear it on success and throw before any builder child runs.
: PATH-EACH$ ( [ ptr u8 n -- ] n -- ptr u8 n )
   {: before timeout:n :}
   before BEFORE !
   ['] RESOLVE catch {: rc:n :}
   [: 2drop ;] BEFORE !
   rc 0<> if rc throw then
   FAMILY$ PATH-BYTES BUILD-FIXPOINT:BF-ENGINE$ PROGRAM$ ['] OUT-ARG timeout
   KEYED-IMAGE:ENSURE-WITH drop
   PATH-BYTES ;

: PATH$ ( n -- ptr u8 n )
   [: 2drop ;] swap PATH-EACH$ ;

\ These two accessors let hb-build reuse a previously observed support closure.
\ The path is still checked through the same executable and retention rules as
\ KEYED-IMAGE:ENSURE-WITH; a missing or pruned image asks for rediscovery.
: SELECTED-KEY$ ( -- ptr u8 n )
   KEY-HEX KEYED-IMAGE:KEY-HEX-LEN ;

: CACHED-PATH$ ( ptr u8 n -- ptr u8 n bool )
   {: key:ptr keyu:n :}
   key keyu HEX? 0= if NULL-PTR 0 false exit then
   key FAMILY$ PATH-BUF PATH-U KEYED-IMAGE:PATH!
   PATH-BYTES EXECUTABLE? 0= if NULL-PTR 0 false exit then
   PATH-BYTES BUILD-CACHE:USED 0= if NULL-PTR 0 false exit then
   PATH-BYTES true ;

;package
