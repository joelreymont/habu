\ inotify-test.f - INOTIFY's words on a directory under the test's temporary
\ directory.
\ Run: bin/hb --load src/lib/inotify-test.f
\ Failure modes: WATCH answers no descriptor, another one for the same
\ directory or the same one for another; a fresh watch has events pending; a
\ file made in the watched directory reaches EACH under another descriptor or
\ without IN-CREATE, or EACH leaves events pending; UNWATCH reports no
\ IN-IGNORED, or a dropped watch still reports; IN-ONLYDIR admits a regular
\ file or sets another errno; a negative descriptor is watched; a failed read
\ is not E-INOTIFY with its errno; a full queue's lost events are not
\ IN-Q-OVERFLOW on descriptor -1.

require src/lib/test.f
require src/lib/adt/option.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/string-option.f
require src/lib/inotify.f

package INOTIFY-TEST
private

create ROOT PATH-CAP allot
variable ROOT-U
create FILE PATH-CAP allot
variable FILE-U
create SUB PATH-CAP allot
variable SUB-U
create GONE PATH-CAP allot
variable GONE-U
create LONG PATH-CAP allot
variable LONG-U
create FLOOD PATH-CAP allot
variable FLOOD-U
create BLIP PATH-CAP allot
variable BLIP-U
variable DIR-FD
variable FILE-FD
variable INST-FD
variable SUB-FD
variable FLOOD-FD
variable FULL-FD
\ The root's watch descriptor; EACH's events for it by kind, and for others.
variable WD
variable CREATES
variable IGNORES
variable OTHERS
variable LOSSES

: ROOT$ ( -- str ) ROOT ROOT-U @ >str ;
: FILE$ ( -- str ) FILE FILE-U @ >str ;
: SUB$ ( -- str ) SUB SUB-U @ >str ;
: GONE$ ( -- str ) GONE GONE-U @ >str ;
: LONG$ ( -- str ) LONG LONG-U @ >str ;
: FLOOD$ ( -- str ) FLOOD FLOOD-U @ >str ;
: BLIP$ ( -- str ) BLIP BLIP-U @ >str ;
: DIR@ ( -- fd ) DIR-FD @ >FD ;
: FILE@ ( -- fd ) FILE-FD @ >FD ;
: INST@ ( -- fd ) INST-FD @ >FD ;
: SUB@ ( -- fd ) SUB-FD @ >FD ;
: FLOOD@ ( -- fd ) FLOOD-FD @ >FD ;
: FULL@ ( -- fd ) FULL-FD @ >FD ;

\ Keeps an opened instance's descriptor in the cell, -1 when none opened.
: FD! ( option<fd> ^n -- )
   {: p:^ :}
   MATCH option
      none OF false TTRUE -1 p ! ENDOF
      some OF FD>N p ! ENDOF
   ;MATCH ;

: SETUP ( -- )
   s" hb-inotify" FS:MKTEMP-DIR {: path:str :}
   path STR:DATA ROOT path STR:LEN BYTE-COPY
   path STR:LEN ROOT-U !
   ROOT$ s" file" FILE FS:JOIN-PATH FILE-U !
   ROOT$ s" sub" SUB FS:JOIN-PATH SUB-U !
   ROOT$ s" gone" GONE FS:JOIN-PATH GONE-U !
   SUB$ FS:MAKE-DIR
   ROOT$ FS:>PATHZ FS:O-RDONLY 0 open DIR-FD !
   DIR-FD @ 0 >= TTRUE
   SUB$ FS:>PATHZ FS:O-RDONLY 0 open SUB-FD !
   SUB-FD @ 0 >= TTRUE
   INOTIFY:OPEN INST-FD FD! ;

: MASK ( -- n )
   INOTIFY:IN-CREATE INOTIFY:IN-CLOSE-WRITE or INOTIFY:IN-ONLYDIR or ;

: WD! ( option<n> -- )
   MATCH option
      none OF false TTRUE -1 WD ! ENDOF
      some OF WD ! ENDOF
   ;MATCH ;

\ The descriptor WATCH answered, -1 when it answered none.
: WD-OF ( option<n> -- n )
   MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

: NONE? ( option<n> -- bool ) WD-OF -1 = ;

: HAS? ( n n -- bool ) and 0<> ;

\ Counts an event of the root's watch by its mask; others go to OTHERS.
: SEE ( n n -- )
   {: wd:n mask:n :}
   wd WD @ <> if 1 OTHERS +! exit then
   mask INOTIFY:IN-CREATE HAS? if 1 CREATES +! then
   mask INOTIFY:IN-IGNORED HAS? if 1 IGNORES +! then ;

: EVENTS ( -- n )
   0 CREATES !  0 IGNORES !  0 OTHERS !
   INST@ [: SEE ;] INOTIFY:EACH ;

: WATCHES ( -- )
   s" a directory has one watch descriptor" T-LABEL
   INST@ DIR@ MASK INOTIFY:WATCH WD!
   WD @ 0 >= TTRUE
   INST@ DIR@ MASK INOTIFY:WATCH WD-OF WD @ T=
   INST@ SUB@ MASK INOTIFY:WATCH WD-OF {: other:n :}
   other 0 >= TTRUE
   other WD @ T<> ;

: NOTICES ( -- )
   s" a made file reaches EACH once" T-LABEL
   EVENTS 0 T=
   FILE$ s" x" FS:WRITE-ALL
   EVENTS 1 >= TTRUE
   CREATES @ 1 T=
   OTHERS @ 0 T=
   EVENTS 0 T= ;

\ Files made in the root: their events, 16 bytes and the padded name each,
\ fill more than one read of EACH's buffer.
20 constant MANY
200 constant NAME-LEN
create LEAF NAME-LEN allot

\ Points LONG$ at the root's k-th file with a NAME-LEN byte name.
: LONG! ( n -- )
   {: k:n :}
   NAME-LEN 0 do [char] n LEAF i + c! loop
   [char] a k + LEAF NAME-LEN 1 - + c!
   ROOT$ LEAF NAME-LEN >str LONG FS:JOIN-PATH LONG-U ! ;

: DRAINS ( -- )
   s" one EACH reads every pending event" T-LABEL
   MANY 0 do i LONG! LONG$ s" x" FS:WRITE-ALL loop
   EVENTS drop
   CREATES @ MANY T=
   OTHERS @ 0 T=
   EVENTS 0 T= ;

: DROPPED ( -- )
   s" UNWATCH reports IN-IGNORED, then nothing" T-LABEL
   INST@ WD @ INOTIFY:UNWATCH
   EVENTS 1 T=
   IGNORES @ 1 T=
   GONE$ s" x" FS:WRITE-ALL
   EVENTS 0 T= ;

: NOT-DIR ( -- )
   s" IN-ONLYDIR on a regular file" T-LABEL
   FILE$ FS:>PATHZ FS:O-RDONLY 0 open FILE-FD !
   FILE-FD @ 0 >= TTRUE
   INST@ FILE@ MASK INOTIFY:WATCH NONE? TTRUE
   FS:ERRNO FS:ENOTDIR T= ;

: NEGATIVE-FD ( -- )
   s" WATCH of a negative descriptor" T-LABEL
   INST@ -1 >FD MASK INOTIFY:WATCH NONE? TTRUE
   FS:ERRNO FS:ENOENT T= ;

: DIR-EACH ( -- ) DIR@ [: SEE ;] INOTIFY:EACH drop ;

\ A directory's read is EISDIR.
: NOT-INSTANCE ( -- )
   s" EACH on a directory" T-LABEL
   [: DIR-EACH ;] INOTIFY:E-INOTIFY TTHROWSQ
   FS:ERRNO FS:EISDIR T= ;

create LIMIT-TEXT 32 allot

: LIMIT-OF ( option<n> -- n )
   MATCH option
      none OF false TTRUE 0 ENDOF
      some OF ENDOF
   ;MATCH ;

\ The most events an instance queues, fixed when it opens.
: LIMIT ( -- n )
   s" /proc/sys/fs/inotify/max_queued_events"
   LIMIT-TEXT 32 FS:READ-ALL {: u:n :}
   LIMIT-TEXT u >str STR:TRIM STR:>NUMBER? LIMIT-OF ;

: FLOOD-DIR ( -- )
   ROOT$ s" flood" FLOOD FS:JOIN-PATH FLOOD-U !
   FLOOD$ s" x" BLIP FS:JOIN-PATH BLIP-U !
   FLOOD$ FS:MAKE-DIR
   FLOOD$ FS:>PATHZ FS:O-RDONLY 0 open FLOOD-FD !
   FLOOD-FD @ 0 >= TTRUE ;

\ Queues n events, making and removing one file in turn: the kernel merges
\ an event equal to the one queued before it.
: FLOOD-EVENTS ( n -- )
   0 ?do
      i 1 and 0= if BLIP$ s" x" FS:WRITE-ALL
      else BLIP$ FS:REMOVE-FILE then
   loop ;

: SEE-LOST ( n n -- )
   {: wd:n mask:n :}
   mask INOTIFY:IN-Q-OVERFLOW HAS? wd -1 = and
   if 1 LOSSES +! then ;

: OVERFLOW ( -- )
   s" a full queue reports IN-Q-OVERFLOW on -1" T-LABEL
   FLOOD-DIR
   INOTIFY:OPEN FULL-FD FD!
   INOTIFY:IN-CREATE INOTIFY:IN-DELETE or {: mask:n :}
   FULL@ FLOOD@ mask INOTIFY:WATCH NONE? TFALSE
   LIMIT 1 + FLOOD-EVENTS
   0 LOSSES !
   FULL@ [: SEE-LOST ;] INOTIFY:EACH drop
   LOSSES @ 1 T= ;

: CASES ( -- )
   WATCHES NOTICES DRAINS DROPPED NOT-DIR NEGATIVE-FD
   NOT-INSTANCE OVERFLOW ;

: SHUT ( ^n -- ) @ dup 0 >= if close else drop then ;

: RUN ( -- )
   T-RESET
   -1 INST-FD !  -1 FILE-FD !  -1 DIR-FD !  -1 SUB-FD !
   -1 FLOOD-FD !  -1 FULL-FD !
   SETUP
   [: CASES ;] catch {: code:n :}
   INST-FD SHUT FILE-FD SHUT DIR-FD SHUT SUB-FD SHUT
   FLOOD-FD SHUT FULL-FD SHUT
   ROOT$ FS:REMOVE-TREE
   code 0<> if code throw then
   T-REPORT ;
RUN

;package
