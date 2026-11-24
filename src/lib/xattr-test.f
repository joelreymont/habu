\ xattr-test.f - XATTR's words on a file under the test's temporary directory,
\ through `user.` attributes.
\ Run: bin/hb --load src/lib/xattr-test.f
\ Failure modes: ADD replaces an existing value; SET does not create or does
\ not replace; GET answers a missing name, or a value longer than its buffer,
\ as anything but absent and oversize, or an empty value as absent; ERASE of a
\ missing name throws; a 255-byte name is refused; a name the kernel refuses
\ reads as oversize; a refused call is not E-XATTR with its errno.

require src/lib/test.f
require src/lib/span.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/xattr.f

package XATTR-TEST
private

-1 constant ABSENT
-2 constant OVERSIZE

16 SPAN-BUFFER: BUF
256 SPAN-BUFFER: LONG
create ROOT PATH-CAP allot
variable ROOT-U
create FILE PATH-CAP allot
variable FILE-U
variable FILE-FD
variable CLOSED-FD

: ROOT$ ( -- str ) ROOT ROOT-U @ >str ;
: FILE$ ( -- str ) FILE FILE-U @ >str ;
: FD@ ( -- fd ) FILE-FD @ >FD ;
: NAME$ ( -- str ) s" user.hb-xattr-test" ;

: SETUP ( -- )
   s" hb-xattr" FS:MKTEMP-DIR {: path:str :}
   path STR:DATA ROOT path STR:LEN BYTE-COPY
   path STR:LEN ROOT-U !
   ROOT$ s" file" FILE FS:JOIN-PATH FILE-U !
   FILE$ s" " FS:WRITE-ALL
   FILE$ FS:>PATHZ FS:O-RDWR 0 open FILE-FD !
   FILE-FD @ 0 >= TTRUE
   $61 LONG SPAN:FILL
   s" user." LONG SPAN:COPY ;

: GOT>N ( XATTR:got -- n )
   MATCH XATTR:got
      absent OF ABSENT ENDOF
      value OF ENDOF
      oversize OF OVERSIZE ENDOF
   ;MATCH ;

: GET-N ( SPAN:span<u8> -- n )
   {: buf :}
   FD@ NAME$ buf XATTR:GET GOT>N ;

: HOLDS ( str -- )
   {: want:str :}
   BUF GET-N want STR:LEN T=
   BUF SPAN:$ STR:DATA want STR:LEN >str want T$= ;

: ADD-AGAIN ( -- ) FD@ NAME$ s" again" XATTR:ADD ;

: ADDS ( -- )
   s" add" T-LABEL
   FD@ NAME$ s" first" XATTR:ADD
   s" first" HOLDS
   [: ADD-AGAIN ;] XATTR:E-XATTR TTHROWSQ
   FS:ERRNO FS:EEXIST T=
   s" first" HOLDS
   FD@ NAME$ s" second!" XATTR:SET
   s" second!" HOLDS ;

: FRESH$ ( -- str ) s" user.hb-xattr-fresh" ;

: FRESH-SET ( -- )
   s" set of a fresh name" T-LABEL
   FD@ FRESH$ s" made" XATTR:SET
   FD@ FRESH$ BUF XATTR:GET GOT>N 4 T=
   BUF SPAN:$ STR:DATA 4 >str s" made" T$= ;

: NAME-255$ ( -- str ) LONG 255 SPAN:TAKE SPAN:$ ;

: NAME-MAX-USE ( -- )
   s" 255-byte name, empty value" T-LABEL
   FD@ NAME-255$ s" " XATTR:SET
   FD@ NAME-255$ BUF XATTR:GET GOT>N 0 T=
   FD@ NAME-255$ XATTR:ERASE
   FD@ NAME-255$ BUF XATTR:GET GOT>N ABSENT T= ;

\ An empty buffer is the size query fgetxattr answers with the size.
: MISSES ( -- )
   s" absent and oversize" T-LABEL
   FD@ s" user.hb-xattr-none" BUF XATTR:GET GOT>N ABSENT T=
   BUF 3 SPAN:TAKE GET-N OVERSIZE T=
   BUF 0 SPAN:TAKE GET-N OVERSIZE T= ;

: ERASES ( -- )
   s" erase" T-LABEL
   FD@ NAME$ XATTR:ERASE
   BUF GET-N ABSENT T=
   FD@ NAME$ XATTR:ERASE ;

: LONG-GET ( -- ) FD@ LONG SPAN:$ BUF XATTR:GET drop ;
: EMPTY-GET ( -- ) FD@ s" " BUF XATTR:GET drop ;

: NAMES ( -- )
   s" refused names" T-LABEL
   [: LONG-GET ;] XATTR:E-XATTR TTHROWSQ
   FS:ERRNO FS:ERANGE T=
   [: EMPTY-GET ;] XATTR:E-XATTR TTHROWSQ
   FS:ERRNO FS:ERANGE T= ;

: CLOSED-GET ( -- ) CLOSED-FD @ >FD NAME$ BUF XATTR:GET drop ;

\ Nothing opens a descriptor between the close and the call.
: CLOSED ( -- )
   s" closed descriptor" T-LABEL
   FILE$ FS:>PATHZ FS:O-RDWR 0 open CLOSED-FD !
   CLOSED-FD @ 0 >= TTRUE
   CLOSED-FD @ close
   [: CLOSED-GET ;] XATTR:E-XATTR TTHROWSQ
   FS:ERRNO FS:EBADF T= ;

: CASES ( -- )
   ADDS FRESH-SET NAME-MAX-USE MISSES ERASES NAMES CLOSED ;

: RUN ( -- )
   T-RESET SETUP
   [: CASES ;] catch {: code:n :}
   FILE-FD @ close
   ROOT$ FS:REMOVE-TREE
   code 0<> if code throw then
   T-REPORT ;
RUN

;package
