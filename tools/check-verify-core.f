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
\   word of the subject. The child runs on bin/hb in this process's working
\   directory, the tree root, as check.f's run stage does. lib/engine-candidate.f
\   would make check.f's own image hold it and lib/engine-id.f, so a default
\   check of either would be unavailable.
\ - The verdict is the child's own result line. A child that ends without one -
\   an exit, a signal, the deadline - is `incomplete` and carries that status,
\   with the packets it wrote before; its exit status alone is never a verdict.
\
\ VERIFY-OUT$ is the checker's schema-1 packets, one JSON object per line: the
\ subject's name PATH's canonical absolute path and count positions in the
\ bytes; a dependency's name the dependency and count in its file. VERIFY-LOG$
\ is the prose: why a closure could not be discovered, and the child's stderr.
\ Both hold until the next call.
\
\ The require closure, discovered for the command line's named files as well,
\ is kept here, so a caller of the operation loads neither the lints nor the
\ all-errors core.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require tools/dynamic-tail-manifest.f
require tools/source-discovery.f

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
   rc E-DISC-UNTERM = if s" discovery rejected: unterminated string" exit then
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

: CHK-TARGET-LAYOUT-ACTIVE? ( ptr u8 n -- bool ) {: path:ptr pathu:n :}
   path pathu s" src/os/linux/layout.f" STR= if HB-TARGET-LINUX? exit then
   path pathu s" src/os/macos/layout.f" STR= if HB-TARGET-MACOS? exit then
   path pathu s" src/os/linux-x86-64/layout.f" STR= if
      HB-TARGET-LINUX-X86-64? exit
   then
   true ;

\ Discovery deliberately over-approximates guarded loaders. The three
\ executable layouts cannot share a checker scope: each publishes the same
\ global names, while only the current target branch is loadable.
: CHK-DEP-LOADABLE? ( n -- bool ) {: id:n :}
   id CHK-DEP$ SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE CHK-TARGET-LAYOUT-ACTIVE? ;

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

DYNAMIC-BUFFER VFY-OUT u8               \ the child's stdout, then VERIFY-OUT$
DYNAMIC-BUFFER VFY-LOG u8               \ VERIFY-LOG$
variable VFY-OUT-U
variable VFY-LOG-U
variable VFY-ANSWER
TYPED-VARIABLE VFY-DEADLINE ms          \ the child's, for VFY-CAPTURE
create VFY-PATH FS-PATH-CAP allot
variable VFY-PATH-U


: VFY-CHILD$ ( -- ptr u8 n )
   s" tools/check-verify-child.f" ;


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
   VFY-NONE VFY-ANSWER ! ;


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


\ Every file of the closure but the subject is the child's to verify, in
\ order, except another target's layout.
: VFY-DEP-ARG+ ( n -- ) {: id:n :}
   id CHK-BYTES-ID @ = if exit then
   id CHK-DEP-LOADABLE? 0= if exit then
   id CHK-DEP$ CHK-ARG+ ;


: VFY-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" CHK-ARG+
   VFY-CHILD$ CHK-ARG+
   s" --" CHK-ARG+
   VFY-PATH$ CHK-ARG+
   CHK-DEP-ORDER-N @ 0 ?do i cells CHK-DEP-ORDER + @ VFY-DEP-ARG+ loop
   PROC-ENV-INHERIT-MISSING ;


\ The child's run on the subject's bytes, CHK-BYTES-A and CHK-BYTES-U: its
\ stdout into VFY-OUT and its stderr into VFY-LOG, their lengths and its end
\ left where PROC-CAPTURE-OUTCOME@ reads them.
: VFY-CAPTURE ( -- )
   VFY-ARGV
   VFY-OUT-CAP VFY-OUT-RESERVE
   VFY-ERR-CAP VFY-LOG-RESERVE
   s" bin/hb" >LEN CHK-BYTES-A @ CHK-BYTES-U @ >LEN
   0 VFY-OUT VFY-OUT-CAP >LEN 0 VFY-LOG VFY-ERR-CAP >LEN
   VFY-DEADLINE @ RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME
   PROC-OUTCOME>RC drop 2drop ;


: VFY-ANSWER-CODE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" check-verify: verified" STR= if VFY-VERIFIED exit then
   a u s" check-verify: refused" STR= if VFY-REFUSED exit then
   a u s" check-verify: held" STR= if VFY-HELD exit then
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
: VFY-TAKE ( n -- )
   VFY-LINE-START {: end:n :}
   end VFY-OUT-U !
   end 0= if exit then
   end 1- VFY-LINE-START {: at:n :}
   at VFY-OUT end 1- at - VFY-ANSWER-CODE VFY-ANSWER !
   VFY-ANSWER @ VFY-NONE = if exit then
   at VFY-OUT-U ! ;


\ How the child ended. More output than the capture holds kills it, and its
\ E-PROC-TRUNCATED is thrown on once what was captured is kept.
: VFY-RUN ( -- outcome )
   [: VFY-CAPTURE ;] catch {: rc:n :}
   rc 0<> rc E-PROC-TRUNCATED <> and if rc throw then
   PROC-CAPTURE-OUTCOME@ {: outu:len erru:len o :}
   erru LEN>N VFY-LOG-U !
   outu LEN>N VFY-TAKE
   rc 0<> if rc throw then
   o ;


: VFY-CLEAN-EXIT? ( outcome -- bool )
   MATCH outcome
      exited OF 0= ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH ;


: VFY-ANSWERED ( -- verdict )
   VFY-ANSWER @ VFY-VERIFIED = if CHECK-VERDICT:verified exit then
   VFY-ANSWER @ VFY-REFUSED = if CHECK-VERDICT:refused exit then
   CHECK-VERDICT:held ;


\ The child's answer once it has given one and exited clean; any other end
\ is incomplete.
: VFY-VERDICT ( outcome -- verdict ) {: o :}
   o VFY-CLEAN-EXIT? VFY-ANSWER @ VFY-NONE <> and if VFY-ANSWERED exit then
   o CHECK-VERDICT:incomplete ;

public

\ The packets of the last VERIFY-BYTES, one JSON object per line.
: VERIFY-OUT$ ( -- ptr u8 n )
   VFY-OUT-U @ 0= if NULL$ exit then
   0 VFY-OUT VFY-OUT-U @ ;

\ The prose of the last VERIFY-BYTES.
: VERIFY-LOG$ ( -- ptr u8 n )
   VFY-LOG-U @ 0= if NULL$ exit then
   0 VFY-LOG VFY-LOG-U @ ;

\ Check the bytes as the file at PATH, the child given DEADLINE. An empty PATH
\ is E-FS-PATH; a closure over CHK-DEP-MAX files or a failed spawn throws as
\ well. More child output than the capture holds, 4 MiB of stdout or 256 KiB
\ of stderr, is E-PROC-TRUNCATED, VERIFY-OUT$ then holding every complete
\ packet received before it.
: VERIFY-BYTES ( ptr u8 n ptr u8 n ms -- verdict )
   {: src:ptr srcu:n path:ptr pathu:n deadline :}
   VFY-RESET
   path pathu VFY-PATH!
   VFY-PATH$ ENGINE-PROVIDES? if CHECK-VERDICT:engine-provided exit then
   src srcu VFY-CLOSURE {: rc:n :}
   rc 0<> if rc VFY-CLOSURE-LOG CHECK-VERDICT:refused exit then
   deadline VFY-DEADLINE !
   VFY-RUN VFY-VERDICT ;

;using
;package
