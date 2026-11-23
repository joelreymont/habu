split-str
\ inotify-test.f - INOTIFY's words on a directory under the test's temporary
\ directory.
\ Run: bin/hb --load src/lib/inotify-test.f
\ Failure modes: a fresh watch has events pending; a file made in the watched
\ directory leaves none pending, or DRAIN leaves some; IN-ONLYDIR admits a
\ regular file or sets another errno; a negative descriptor is watched; a
\ failed read is not E-INOTIFY with its errno.

require src/lib/test.f
require src/lib/adt/option.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/inotify.f

package INOTIFY-TEST
private

create ROOT PATH-CAP allot
variable ROOT-U
create FILE PATH-CAP allot
variable FILE-U
variable DIR-FD
variable FILE-FD
variable INST-FD

: ROOT$ ( -- str ) ROOT ROOT-U @ ;
: FILE$ ( -- str ) FILE FILE-U @ ;
: DIR@ ( -- fd ) DIR-FD @ >FD ;
: FILE@ ( -- fd ) FILE-FD @ >FD ;
: INST@ ( -- fd ) INST-FD @ >FD ;

: INST! ( option<fd> -- )
   MATCH option
      none OF false TTRUE -1 INST-FD ! ENDOF
      some OF FD>N INST-FD ! ENDOF
   ;MATCH ;

: SETUP ( -- )
   s" hb-inotify" FS:MKTEMP-DIR {: path:^ u:n :}
   path ROOT u BYTE-COPY
   u ROOT-U !
   ROOT$ s" file" FILE FS:JOIN-PATH FILE-U !
   ROOT$ FS:>PATHZ FS:O-RDONLY 0 open DIR-FD !
   DIR-FD @ 0 >= TTRUE
   INOTIFY:OPEN INST! ;

: MASK ( -- n )
   INOTIFY:IN-CREATE INOTIFY:IN-CLOSE-WRITE or INOTIFY:IN-ONLYDIR or ;

: NOTICES ( -- )
   s" a made file is pending once" T-LABEL
   INST@ DIR@ MASK INOTIFY:WATCH TTRUE
   INST@ INOTIFY:DRAIN 0 T=
   FILE$ s" x" FS:WRITE-ALL
   INST@ INOTIFY:DRAIN 0 > TTRUE
   INST@ INOTIFY:DRAIN 0 T= ;

: NOT-DIR ( -- )
   s" IN-ONLYDIR on a regular file" T-LABEL
   FILE$ FS:>PATHZ FS:O-RDONLY 0 open FILE-FD !
   FILE-FD @ 0 >= TTRUE
   INST@ FILE@ MASK INOTIFY:WATCH TFALSE
   FS:ERRNO FS:ENOTDIR T= ;

: NEGATIVE-FD ( -- )
   s" WATCH of a negative descriptor" T-LABEL
   INST@ -1 >FD MASK INOTIFY:WATCH TFALSE
   FS:ERRNO FS:ENOENT T= ;

: DIR-DRAIN ( -- ) DIR@ INOTIFY:DRAIN drop ;

\ A directory's read is EISDIR.
: NOT-INSTANCE ( -- )
   s" DRAIN of a directory" T-LABEL
   [: DIR-DRAIN ;] INOTIFY:E-INOTIFY TTHROWSQ
   FS:ERRNO FS:EISDIR T= ;

: CASES ( -- ) NOTICES NOT-DIR NEGATIVE-FD NOT-INSTANCE ;

: SHUT ( ^n -- ) @ dup 0 >= if close else drop then ;

: RUN ( -- )
   T-RESET
   -1 INST-FD !  -1 FILE-FD !  -1 DIR-FD !
   SETUP
   [: CASES ;] catch {: code:n :}
   INST-FD SHUT FILE-FD SHUT DIR-FD SHUT
   ROOT$ FS:REMOVE-TREE
   code 0<> if code throw then
   T-REPORT ;
RUN

;package
