\ check-verify-core.f - check a subject's bytes as its file, in its load context, without running it.
\
\ CHECK:VERIFY-BYTES is the check a language server makes of an open buffer and
\ `tools/check.f --verify-only` makes of a file, or of stdin under a path. It
\ takes the subject's bytes and the path they stand for and answers
\ what `bin/hb --load PATH` would refuse of them, and runs none of it:
\
\ - The closure is discovered over the bytes with PATH's directory as root and
\   PATH as the subject's identity, so a dependency that requires PATH back
\   meets the bytes, never the copy on disk.
\ - The verification runs in a short-lived child, tools/check-verify-child.f,
\   whose image is the engine's boot prefix plus the verifier: neither this
\   process's words nor an earlier check's can stand in for, or collide with, a
\   word of the subject. As check.f's run stage does, the child runs on the
\   engine lib/engine-candidate.f names, a gate's HABU_UNDER_TEST or else the
\   engine running this process, never a bin/hb of the working directory. The
\   child is the tools/check-verify-child.f of the tree this file was loaded
\   from, named absolutely, and runs in this process's working directory,
\   whatever tree that holds.
\ - The verdict is the child's own result line. A child that ends without one -
\   an exit, a signal, the deadline - is `incomplete` and carries that status,
\   with the packets it wrote before; its exit status alone is never a verdict.
\
\ VERIFY-OUT$ is the checker's schema-1 packets, one JSON object per line: the
\ subject's name PATH's canonical absolute path and count positions in the
\ bytes; a dependency's name the dependency and count in its file. A duplicate
\ definition, which the checker writes no packet for, is the record
\ --all-errors writes for it (CHECK-ALL-ERRORS:DUP-RECORD$). VERIFY-LOG$
\ is the prose: why a closure could not be discovered, and the child's stderr.
\ Both hold until the next call.
\
\ CHECK:PREVERIFY-BYTES is check.f's pre-pass, on the same child and image: the
\ first refused definition stops it, as it stops the load, and the subject's
\ packets carry the label check.f reports the subject by.
\
\ The require closure, discovered for the command line's named files as well,
\ is kept here, so a caller of the operation loads none of check.f's lints.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/adt/result.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require tools/dynamic-tail-manifest.f
require tools/source-discovery.f
require tools/check-all-errors-core.f

package CHECK
using SOURCE-ROOT

\ check.f's exit statuses. The closure walk refuses a source that does not exist
\ with CHK-E-NOINPUT.
64 constant CHK-E-USAGE
66 constant CHK-E-NOINPUT
69 constant CHK-E-UNAVAILABLE
70 constant CHK-E-CHECK

128 constant CHK-DEP-MAX

create CHK-DEP-PATHS CHK-DEP-MAX FS-PATH-CAP * allot
create CHK-DEP-US CHK-DEP-MAX cells allot
create CHK-DEP-ROOTS CHK-DEP-MAX FS-PATH-CAP * allot
create CHK-DEP-ROOT-US CHK-DEP-MAX cells allot
create CHK-DEP-STATES CHK-DEP-MAX cells allot
create CHK-DIR-IDS CHK-DEP-MAX cells allot
create CHK-DEP-ORDER CHK-DEP-MAX cells allot

variable CHK-DEP-N
variable CHK-DIR-N
variable CHK-DEP-ORDER-N
variable CHK-DISC-ID
variable CHK-BYTES-ID
TYPED-VARIABLE CHK-BYTES-A ptr u8
variable CHK-BYTES-U
variable CHK-EXPAND-TOP


: CHK-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;


: CHK-DEP-CHECK ( n -- ) {: id:n :}
   id 0 < if E-TBL-BOUNDS throw then
   id CHK-DEP-MAX >= if E-TBL-BOUNDS throw then ;

: CHK-DEP-PATH ( n -- ptr u8 ) {: id:n :}
   id CHK-DEP-CHECK
   CHK-DEP-PATHS id FS-PATH-CAP * + ;

: CHK-DEP-U ( n -- ptr n ) {: id:n :}
   id CHK-DEP-CHECK
   CHK-DEP-US id cells + ;

: CHK-DEP-STATE ( n -- ptr n ) {: id:n :}
   id CHK-DEP-CHECK
   CHK-DEP-STATES id cells + ;

: CHK-DEP$ ( n -- ptr u8 n ) {: id:n :}
   id CHK-DEP-PATH
   id CHK-DEP-U @ ;

: CHK-DEP-ROOT$ ( n -- ptr u8 n ) {: id:n :}
   id CHK-DEP-CHECK
   CHK-DEP-ROOTS id FS-PATH-CAP * +
   CHK-DEP-ROOT-US id cells + @ ;

: CHK-DEP-MATCH? ( ptr u8 n n -- bool ) {: a:ptr u:n id:n :}
   a u id CHK-DEP$ STR= ;

: CHK-DEP-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup CHK-DEP-N @ < while
      dup a u rot CHK-DEP-MATCH? if exit then
      1+
   repeat drop -1 ;

: CHK-DEP-NEW ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n root:ptr rootu:n :}
   u FS-PATH-CAP > rootu FS-PATH-CAP > or if E-FS-CAPACITY throw then
   CHK-DEP-N @ CHK-DEP-MAX >= if E-TBL-BOUNDS throw then
   CHK-DEP-N @ {: id:n :}
   a id CHK-DEP-PATH u BYTE-COPY
   u id CHK-DEP-U !
   root CHK-DEP-ROOTS id FS-PATH-CAP * + rootu BYTE-COPY
   rootu CHK-DEP-ROOT-US id cells + !
   0 id CHK-DEP-STATE !
   id 1+ CHK-DEP-N !
   id ;

: CHK-DEP-ID ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n root:ptr rootu:n :}
   a u CHK-DEP-FIND dup 0 >= if exit then
   drop a u root rootu CHK-DEP-NEW ;

: CHK-DIR-PUSH ( n -- ) {: id:n :}
   CHK-DIR-N @ CHK-DEP-MAX >= if E-TBL-BOUNDS throw then
   id CHK-DIR-IDS CHK-DIR-N @ cells + !
   CHK-DIR-N @ 1+ CHK-DIR-N ! ;

: CHK-DEP-ORDER-PUSH ( n -- ) {: id:n :}
   CHK-DEP-ORDER-N @ CHK-DEP-MAX >= if E-TBL-BOUNDS throw then
   id CHK-DEP-ORDER CHK-DEP-ORDER-N @ cells + !
   CHK-DEP-ORDER-N @ 1+ CHK-DEP-ORDER-N ! ;

: CHK-DEP-DIRECT+ ( ptr u8 n ptr u8 n -- )
   CHK-DEP-ID CHK-DIR-PUSH ;

\ Dependency closure: the shared whole-file ordered-event producer
\ (tools/source-discovery.f) scans every token of a file - colon bodies
\ included - and records one event per literal loader form
\ (include/included/require/required/provided). Every event path is a direct
\ dep, so the closure is a superset of the runtime load set; dynamic or
\ retired loader forms reject fail-closed unless manifested.
\
\ A walk ends at the first file it cannot follow: discovery refuses it, with an
\ E-DISC-* code, or it does not exist, CHK-E-NOINPUT. CHK-DISC-ID names it. The
\ file CHK-BYTES-ID names is read from the caller's bytes, never from disk.

: CHK-DISC-RC? ( n -- bool ) {: rc:n :}
   rc E-DISC-FIRST <= rc E-DISC-LAST >= and ;

: CHK-DISC-MSG$ ( n -- ptr u8 n ) {: rc:n :}
   rc E-DISC-SHADOW = if s" discovery rejected: loader word shadowed or undefined" exit then
   rc E-DISC-DYNAMIC = if s" discovery rejected: dynamic (non-literal) loader path" exit then
   rc E-DISC-OPENER = if s" discovery rejected: unsupported string opener before a loader word" exit then
   rc E-DISC-RETIRE = if s" discovery rejected: loader word retired (UNDEFINE-IF-DEFINED)" exit then
   rc E-DISC-UNTERM = if s" discovery rejected: unterminated string or locals group" exit then
   s" discovery rejected: capacity exceeded" ;

: CHK-DISCOVER-ACT ( -- )
   CHK-DISC-ID @ {: id:n :}
   id CHK-BYTES-ID @ = if
      id CHK-DEP$ id CHK-DEP-ROOT$ CHK-BYTES-A @ CHK-BYTES-U @ DISCOVER:RUN-BYTES exit
   then
   id CHK-DEP$ id CHK-DEP-ROOT$ DISCOVER:RUN-IN ;

: CHK-EVENT-DEP+ ( n -- ) {: ix:n :}
   ix EVENT-PATH@ ix SOURCE-EVENT:ROOT@ CHK-DEP-DIRECT+ ;

: CHK-EVENTS>DEPS ( -- )
   0 begin dup EVENT-COUNT < while
      dup CHK-EVENT-DEP+
      1+
   repeat drop ;

: CHK-EXPAND-ID ( n -- ) {: id:n :}
   id CHK-DEP-CHECK
   id CHK-DEP-STATE @ 2 = if exit then
   id CHK-DEP-STATE @ 1 = if exit then
   1 id CHK-DEP-STATE !
   id CHK-DISC-ID !
   CHK-DIR-N @
   id CHK-BYTES-ID @ <> if
      id CHK-DEP$ FILE? 0= if CHK-E-NOINPUT throw then
   then
   CHK-DISCOVER-ACT
   CHK-EVENTS>DEPS
   dup CHK-DIR-N @
   begin 2dup < while
      over cells CHK-DIR-IDS + @ RECURSE
      swap 1+ swap
   repeat
   2drop CHK-DIR-N !
   id CHK-DEP-ORDER-PUSH
   2 id CHK-DEP-STATE ! ;

: CHK-EXPAND-RESET ( -- )
   0 CHK-DEP-N !
   0 CHK-DIR-N !
   0 CHK-DEP-ORDER-N !
   -1 CHK-BYTES-ID ! ;

: CHK-EXPAND-TOP-ACT ( -- )
   CHK-EXPAND-TOP @ CHK-EXPAND-ID ;

\ Walk the closure below ID into the dependency order: 0, or the code of the
\ file that ended the walk. Any other throw goes on.
: CHK-EXPAND ( n -- n )
   CHK-EXPAND-TOP !
   [: CHK-EXPAND-TOP-ACT ;] catch {: rc:n :}
   rc CHK-DISC-RC? rc CHK-E-NOINPUT = or rc 0= or 0= if rc throw then
   rc ;

public

\ What one CHECK:VERIFY-BYTES found. verified and refused are the checker's
\ verdict on PATH in its load context, refused whenever a file of the closure,
\ or the closure itself, is refused. engine-provided: the engine provides PATH,
\ so nothing is verified, whatever the bytes hold. held: the verifier's own
\ image holds PATH though the engine does not, so it cannot be verified there.
\ incomplete: the child ended without a result line; status is how it ended.
ENUM verdict 0
   VARIANT verified ;VARIANT
   VARIANT refused ;VARIANT
   VARIANT engine-provided ;VARIANT
   VARIANT held ;VARIANT
   VARIANT incomplete FIELD status outcome ;VARIANT
;ENUM

private

$400000 constant VFY-OUT-CAP            \ the child's stdout: packets, then its result line
$40000 constant VFY-ERR-CAP             \ the child's stderr
$0A constant VFY-LF

\ The child's answer, from its result line.
0 constant VFY-NONE
1 constant VFY-VERIFIED
2 constant VFY-REFUSED
3 constant VFY-HELD
4 constant VFY-STOPPED

DYNAMIC-BUFFER VFY-OUT u8               \ the child's stdout, then VERIFY-OUT$
DYNAMIC-BUFFER VFY-LOG u8               \ VERIFY-LOG$
DYNAMIC-BUFFER VFY-REC u8               \ VFY-OUT, each duplicate's stopped line its record
DYNAMIC-BUFFER VFY-DEP u8               \ a duplicate's file, not the subject
DYNAMIC-BUFFER VFY-STOP-PATH u8         \ preserve the final stop across duplicate records
variable VFY-REC-U
variable VFY-OUT-U
variable VFY-LOG-U
variable VFY-ANSWER
TYPED-VARIABLE VFY-DEADLINE ms          \ the child's, for VFY-CAPTURE
create VFY-PATH FS-PATH-CAP allot
variable VFY-PATH-U
create VFY-CHILD FS-PATH-CAP allot
variable VFY-CHILD-U

\ While this file loads, SOURCE-ROOT:CURRENT$ is the root that resolved it.
\ The child beside it is fixed here before the working directory can name another tree.
SOURCE-ROOT:CURRENT$ s" tools/check-verify-child.f" VFY-CHILD JOIN-PATH VFY-CHILD-U !

TYPED-VARIABLE VFY-PREPASS bool         \ the child runs check.f's pre-pass ...
TYPED-VARIABLE VFY-LABEL-A ptr u8       \ ... naming the subject by this label
variable VFY-LABEL-U
variable VFY-STOP-RC                    \ a stopped child: its code, 0 for none,
variable VFY-STOP-AT                    \ the byte of the token it read last,
variable VFY-STOP-DUP-AT                \ where the name it refused as a duplicate
variable VFY-STOP-DUP-U                 \ starts and its length, 0 for none,
TYPED-VARIABLE VFY-STOP-SUBJ bool       \ whether that is in the subject's bytes,
variable VFY-STOP-OFF                   \ and the file it is in, in VFY-OUT,
variable VFY-STOP-U
TYPED-VARIABLE VFY-STOP-DISC bool       \ or the one discovery stopped in


: VFY-CHILD$ ( -- ptr u8 n )
   VFY-CHILD VFY-CHILD-U @ ;


: VFY-LOG+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   VFY-LOG-U @ u + VFY-LOG-RESERVE
   a VFY-LOG-U @ VFY-LOG u BYTE-COPY
   VFY-LOG-U @ u + VFY-LOG-U ! ;


: VFY-LOG-LN ( ptr u8 n -- )
   VFY-LOG+ s\" \n" VFY-LOG+ ;


: VFY-RESET ( -- )
   0 VFY-OUT-U !
   0 VFY-LOG-U !
   VFY-NONE VFY-ANSWER !
   0 VFY-STOP-RC !
   false VFY-STOP-DISC !
   false VFY-PREPASS ! ;


\ PATH's canonical absolute spelling, a relative PATH read from the working
\ directory: the identity the loader keys it by and the name its packets carry,
\ whether or not a file is there yet.
: VFY-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if E-FS-PATH throw then
   a u CANONICAL drop {: c:ptr cu:n :}
   cu FS-PATH-CAP > if E-FS-CAPACITY throw then
   c VFY-PATH cu BYTE-COPY
   cu VFY-PATH-U ! ;


: VFY-PATH$ ( -- ptr u8 n )
   VFY-PATH VFY-PATH-U @ ;


\ Walk the closure over the subject's bytes, PATH's directory the root: 0, or
\ the code of the file that ended the walk. Discovery takes PATH as loading
\ meanwhile, so a require of an absent PATH meets the bytes too.
: VFY-CLOSURE ( ptr u8 n -- n ) {: src:ptr srcu:n :}
   CHK-EXPAND-RESET
   VFY-PATH$ VFY-PATH$ DIRNAME CHK-DEP-ID {: id:n :}
   src CHK-BYTES-A !
   srcu CHK-BYTES-U !
   id CHK-BYTES-ID !
   VFY-PATH$ DISCOVER:LOADING!
   id [: CHK-EXPAND ;] [: NULL$ DISCOVER:LOADING! ;] finally ;


: VFY-CLOSURE-LOG ( n -- ) {: rc:n :}
   CHK-DISC-ID @ CHK-DEP$ VFY-LOG+
   s" : " VFY-LOG+
   rc CHK-E-NOINPUT = if s" no such source" VFY-LOG-LN exit then
   rc CHK-DISC-MSG$ VFY-LOG-LN ;


\ Discovery that ended the walk at a string or a locals group a file never
\ closes stops the verification at its opener, in the file CHK-DISC-ID names.
: VFY-DISC-STOP ( n -- ) {: rc:n :}
   rc E-DISC-UNTERM <> if exit then
   rc VFY-STOP-RC !
   DISCOVER:OPENER-AT VFY-STOP-AT !
   CHK-DISC-ID @ CHK-BYTES-ID @ = VFY-STOP-SUBJ !
   true VFY-STOP-DISC ! ;


: VFY-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" CHK-ARG+
   VFY-CHILD$ CHK-ARG+
   s" --" CHK-ARG+
   VFY-PATH$ CHK-ARG+
   VFY-PREPASS @ if VFY-LABEL-A @ VFY-LABEL-U @ CHK-ARG+ then
   PROC-ENV-INHERIT-MISSING ;


\ The child's run on the subject's bytes, CHK-BYTES-A and CHK-BYTES-U: its
\ stdout into VFY-OUT and its stderr into VFY-LOG, their lengths and its end
\ left where PROC-CAPTURE-OUTCOME@ reads them. The end is read there, so the
\ one returned here is dropped through the conversion that reads a deadline as
\ a status rather than throwing it.
: VFY-CAPTURE ( -- )
   VFY-ARGV
   VFY-OUT-CAP VFY-OUT-RESERVE
   VFY-ERR-CAP VFY-LOG-RESERVE
   ENGINE-CANDIDATE:PATH$ >LEN CHK-BYTES-A @ CHK-BYTES-U @ >LEN
   0 VFY-OUT VFY-OUT-CAP >LEN 0 VFY-LOG VFY-ERR-CAP >LEN
   VFY-DEADLINE @ RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME
   PROC-OUTCOME>DEADLINE-RC drop 2drop ;


: VFY-STOPPED$ ( -- ptr u8 n )
   s" check-verify: stopped " ;


\ Where the field of VFY-OUT that starts at AT ends: at its space, or at END.
: VFY-FIELD-END ( n n -- n ) {: at:n end:n :}
   at begin
      dup end < if dup VFY-OUT c@ $20 <> else false then
   while 1+ repeat ;


\ The number VFY-OUT spells from AT to STOP, and whether it spells one.
: VFY-FIELD-N ( n n -- n bool ) {: at:n stop:n :}
   at VFY-OUT stop at - STR>NUMBER? MATCH option
      some OF true ENDOF
      none OF 0 false ENDOF
   ;MATCH ;


\ The rest of a stopped line, from AT to END in VFY-OUT: RC BYTE DUP-AT DUP-LEN
\ IN-SUBJECT FILE. VFY-STOPPED with them kept, or VFY-NONE for a line that does
\ not read so; a stop is never code 0, and IN-SUBJECT is 1 or 0.
: VFY-STOP-PARSE ( n n -- n ) {: at:n end:n :}
   at end VFY-FIELD-END {: e1:n :}
   at e1 VFY-FIELD-N {: rc:n rc-ok:bool :}
   e1 1+ end VFY-FIELD-END {: e2:n :}
   e1 1+ e2 VFY-FIELD-N {: byte:n byte-ok:bool :}
   e2 1+ end VFY-FIELD-END {: e3:n :}
   e2 1+ e3 VFY-FIELD-N {: name-at:n name-at-ok:bool :}
   e3 1+ end VFY-FIELD-END {: e4:n :}
   e3 1+ e4 VFY-FIELD-N {: name-u:n name-u-ok:bool :}
   e4 1+ end VFY-FIELD-END {: e5:n :}
   e4 1+ e5 VFY-FIELD-N {: subj:n subj-ok:bool :}
   rc-ok byte-ok and name-at-ok and name-u-ok and subj-ok and rc 0<> and
   subj 0 = subj 1 = or and
   e5 1+ end < and 0= if VFY-NONE exit then
   rc VFY-STOP-RC !
   byte VFY-STOP-AT !
   name-at VFY-STOP-DUP-AT !
   name-u VFY-STOP-DUP-U !
   subj 1 = VFY-STOP-SUBJ !
   e5 1+ VFY-STOP-OFF !
   end e5 1+ - VFY-STOP-U !
   VFY-STOPPED ;


\ A result line is authoritative only after a clean child exit. Otherwise a
\ final complete stopped line may be a duplicate packet before the child died.
: VFY-CLEAN-EXIT? ( outcome -- bool )
   MATCH outcome
      exited OF 0= ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH ;


\ The answer the result line from AT to END in VFY-OUT gives, its line feed
\ excluded. A stopped line can also be a duplicate packet in the first form.
: VFY-ANSWER-AT ( n n bool -- n ) {: at:n end:n clean:bool :}
   clean 0= if VFY-NONE exit then
   at VFY-OUT end at - {: a:ptr u:n :}
   a u s" check-verify: verified" STR= if VFY-VERIFIED exit then
   a u s" check-verify: refused" STR= if VFY-REFUSED exit then
   a u s" check-verify: held" STR= if VFY-HELD exit then
   a u VFY-STOPPED$ STARTS-WITH? if at VFY-STOPPED$ nip + end VFY-STOP-PARSE exit then
   VFY-NONE ;


\ Where the line that ends at END in VFY-OUT starts.
: VFY-LINE-START ( n -- n )
   begin dup 0 > while
      dup 1- VFY-OUT c@ VFY-LF = if exit then
      1-
   repeat ;


\ Keep the complete lines of the child's OUTU bytes of stdout: the packets, and
\ the answer of a result line that ends them. The rest of a line the deadline or
\ the capture cut is dropped.
: VFY-TAKE ( n bool -- )
   {: outu:n clean:bool :}
   outu VFY-LINE-START {: end:n :}
   end VFY-OUT-U !
   end 0= if exit then
   end 1- VFY-LINE-START {: at:n :}
   at end 1- clean VFY-ANSWER-AT VFY-ANSWER !
   VFY-ANSWER @ VFY-NONE = if exit then
   at VFY-OUT-U ! ;


\ The file the last stopped line read names, in VFY-OUT.
: VFY-STOP-FILE$ ( -- ptr u8 n )
   VFY-STOP-OFF @ VFY-OUT VFY-STOP-U @ ;


\ That file's bytes: the subject's, or the file's own.
: VFY-STOP-SOURCE ( -- ptr u8 n )
   VFY-STOP-SUBJ @ if CHK-BYTES-A @ CHK-BYTES-U @ exit then
   VFY-STOP-FILE$ FILE-SIZE 1 max
   {: cap:n :}
   cap VFY-DEP-RESERVE
   VFY-STOP-FILE$ 0 VFY-DEP cap READ-ALL
   {: u:n :}
   0 VFY-DEP u ;


: VFY-REC+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if exit then
   VFY-REC-U @ u + VFY-REC-RESERVE
   a VFY-REC-U @ VFY-REC u BYTE-COPY
   VFY-REC-U @ u + VFY-REC-U ! ;


\ Where the line of VFY-OUT that starts at AT ends: at its line feed.
: VFY-LINE-END ( n -- n )
   begin
      dup VFY-OUT-U @ < if dup VFY-OUT c@ VFY-LF <> else false then
   while 1+ repeat ;


\ The line of VFY-OUT that starts at AT, to VFY-REC, a duplicate's stopped line
\ as the record --all-errors writes for it: where the next line starts.
: VFY-REC-LINE ( n -- n )
   {: at:n :}
   at VFY-LINE-END
   {: end:n :}
   at VFY-OUT end at - VFY-STOPPED$ STARTS-WITH?
   if at VFY-STOPPED$ nip + end VFY-STOP-PARSE VFY-STOPPED = else false then
   if
      true CHECK-ALL-ERRORS:JSON!
      VFY-STOP-DUP-AT @ VFY-STOP-DUP-U @ VFY-STOP-FILE$ VFY-STOP-SOURCE
      CHECK-ALL-ERRORS:DUP-RECORD$ VFY-REC+
   else
      at VFY-OUT end at - VFY-REC+
   then
   s\" \n" VFY-REC+
   end 1+ ;


\ The packets VFY-OUT holds, each stopped line the first form writes among them
\ for a duplicate made its record.
: VFY-DUP-RECORDS ( -- )
   VFY-PREPASS @ if exit then
   VFY-OUT-U @ 0= if exit then
   VFY-ANSWER @ VFY-STOPPED =
   VFY-STOP-RC @ VFY-STOP-AT @ VFY-STOP-DUP-AT @ VFY-STOP-DUP-U @
   VFY-STOP-SUBJ @ VFY-STOP-U @
   {: stopped:bool rc:n byte:n at:n u:n subj:bool fileu:n :}
   stopped if
      fileu VFY-STOP-PATH-RESERVE
      VFY-STOP-FILE$ drop 0 VFY-STOP-PATH fileu BYTE-COPY
   then
   0 VFY-REC-U !
   0 begin dup VFY-OUT-U @ < while VFY-REC-LINE repeat drop
   stopped if VFY-REC-U @ fileu + else VFY-REC-U @ then VFY-OUT-RESERVE
   0 VFY-REC 0 VFY-OUT VFY-REC-U @ BYTE-COPY
   VFY-REC-U @ VFY-OUT-U !
   stopped if
      rc VFY-STOP-RC !  byte VFY-STOP-AT !
      at VFY-STOP-DUP-AT !  u VFY-STOP-DUP-U !
      subj VFY-STOP-SUBJ !  fileu VFY-STOP-U !
      VFY-OUT-U @ VFY-STOP-OFF !
      0 VFY-STOP-PATH VFY-STOP-OFF @ VFY-OUT fileu BYTE-COPY
   else
      0 VFY-STOP-RC !
   then ;


\ How the child ended. More output than the capture holds kills it, and its
\ E-PROC-TRUNCATED is thrown on once what was captured is kept.
: VFY-RUN ( -- outcome )
   [: VFY-CAPTURE ;] catch {: rc:n :}
   rc 0<> rc E-PROC-TRUNCATED <> and if rc throw then
   PROC-CAPTURE-OUTCOME@ {: outu:len erru:len o :}
   erru LEN>N VFY-LOG-U !
   outu LEN>N o VFY-CLEAN-EXIT? rc 0= and VFY-TAKE
   VFY-DUP-RECORDS
   rc 0<> if rc throw then
   o ;


\ The child's answer once it has given one of the first form's and exited
\ clean, a stop refused; any other end is incomplete.
: VFY-VERDICT ( outcome -- verdict ) {: o :}
   o VFY-CLEAN-EXIT? if
      VFY-ANSWER @ VFY-VERIFIED = if CHECK-VERDICT:verified exit then
      VFY-ANSWER @ VFY-REFUSED = if CHECK-VERDICT:refused exit then
      VFY-ANSWER @ VFY-STOPPED = if CHECK-VERDICT:refused exit then
      VFY-ANSWER @ VFY-HELD = if CHECK-VERDICT:held exit then
   then
   o CHECK-VERDICT:incomplete ;


\ The pre-pass's answer once it has given one and exited clean: 0 verified,
\ else the code it stopped with. Any other end is the error.
: VFY-PREVERDICT ( outcome -- result<n,outcome> ) {: o :}
   o VFY-CLEAN-EXIT? if
      VFY-ANSWER @ VFY-VERIFIED = if 0 RESULT:OK exit then
      VFY-ANSWER @ VFY-STOPPED = if VFY-STOP-RC @ RESULT:OK exit then
   then
   o RESULT:ERR ;

public

\ The packets of the last VERIFY-BYTES or PREVERIFY-BYTES, one JSON object per
\ line.
: VERIFY-OUT$ ( -- ptr u8 n )
   VFY-OUT-U @ 0= if NULL$ exit then
   0 VFY-OUT VFY-OUT-U @ ;

\ The prose of the last VERIFY-BYTES or PREVERIFY-BYTES.
: VERIFY-LOG$ ( -- ptr u8 n )
   VFY-LOG-U @ 0= if NULL$ exit then
   0 VFY-LOG VFY-LOG-U @ ;

\ Check the bytes as the file at PATH, the child given DEADLINE. A throw that
\ ends the verification refuses it, as does a string or a locals group a file
\ of the closure never closes, and VERIFY-STOP and the words after it say with
\ what and where. An empty PATH is E-FS-PATH; a closure over CHK-DEP-MAX
\ files, an engine lib/engine-candidate.f refuses (E-FS-OPEN) or a failed spawn
\ throws as well.
\ More child output than the capture holds, 4 MiB of stdout or 256 KiB of
\ stderr, is E-PROC-TRUNCATED, VERIFY-OUT$ then holding every complete packet
\ received before it. A file of the closure that defined a name again and can
\ no longer be read throws as reading it does.
: VERIFY-BYTES ( ptr u8 n ptr u8 n ms -- verdict )
   {: src:ptr srcu:n path:ptr pathu:n deadline :}
   VFY-RESET
   path pathu VFY-PATH!
   VFY-PATH$ ENGINE-PROVIDES? if CHECK-VERDICT:engine-provided exit then
   src srcu VFY-CLOSURE {: rc:n :}
   rc 0<> if rc VFY-CLOSURE-LOG rc VFY-DISC-STOP CHECK-VERDICT:refused exit then
   deadline VFY-DEADLINE !
   VFY-RUN VFY-VERDICT ;

\ check.f's pre-pass of the bytes as the file at PATH, the subject named LABEL
\ in its packets, the child given DEADLINE. The child's image is the one
\ VERIFY-BYTES verifies on, so the pre-pass resolves the engine's words and the
\ subject's loads, as the run does, never a word only this process loaded. It
\ stops at the first definition the checker refuses, as the load does: ok 0 when
\ nothing stopped it, else ok the code it stopped with, and VERIFY-STOP-AT,
\ PREVERIFY-DUPLICATE, VERIFY-STOP-SUBJECT? and VERIFY-STOPPED$ say where.
\ err is how a child ended that gave no answer. An empty PATH is E-FS-PATH; a
\ failed spawn throws, and more child output than the capture holds is
\ E-PROC-TRUNCATED, as for VERIFY-BYTES.
: PREVERIFY-BYTES ( ptr u8 n ptr u8 n ptr u8 n ms -- result<n,outcome> )
   {: src:ptr srcu:n path:ptr pathu:n label:ptr labelu:n deadline :}
   VFY-RESET
   path pathu VFY-PATH!
   true VFY-PREPASS !
   label VFY-LABEL-A !
   labelu VFY-LABEL-U !
   src CHK-BYTES-A !
   srcu CHK-BYTES-U !
   deadline VFY-DEADLINE !
   VFY-RUN VFY-PREVERDICT ;

\ The code a throw stopped the last VERIFY-BYTES or PREVERIFY-BYTES with,
\ E-DISC-UNTERM for discovery's stop at a string or group, 0 when none did.
: VERIFY-STOP ( -- n )
   VFY-STOP-RC @ ;

\ Where it stopped: the byte where the token it stopped at starts, the one the
\ verifier read last or the opener of the statement it was in, whether that is
\ in the bytes it was given, and the file it is in, PATH's canonical spelling or
\ the LABEL for those bytes.
: VERIFY-STOP-AT ( -- n )
   VFY-STOP-AT @ ;

\ The name the last PREVERIFY-BYTES refused as a duplicate, in the same file:
\ the byte where it starts and its length, 0 when it kept no name
\ (VERIFY:DUPLICATE).
: PREVERIFY-DUPLICATE ( -- n n )
   VFY-STOP-DUP-AT @ VFY-STOP-DUP-U @ ;

: VERIFY-STOP-SUBJECT? ( -- bool )
   VFY-STOP-SUBJ @ ;

: VERIFY-STOPPED$ ( -- ptr u8 n )
   VFY-STOP-DISC @ if CHK-DISC-ID @ CHK-DEP$ exit then
   VFY-STOP-FILE$ ;

;using
;package
