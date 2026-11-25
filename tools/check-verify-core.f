\ check-verify-core.f - check a subject's bytes as its file, in its load context, without running it.
\ LSP-CHECK:VERIFY-BYTES answers what `bin/hb --load PATH` would refuse of the bytes, for a language
\ server's buffer and `tools/check.f --verify-only`. The closure is composed over the bytes with PATH
\ as the subject's identity, so a dependency that requires PATH back meets the bytes; a load of an
\ open document other than PATH meets its text, the first of two for one path holding. A child
\ (VFY-EXE) runs the check; LSP-CHECK:PREVERIFY-BYTES is check.f's pre-pass on the same child.
\
\ A batch wave assembles one immutable input snapshot per lane on the calling
\ task before it starts; capture tasks touch only their lane's output buffers
\ and command context. The ordinary batch parser is restored and run on the
\ calling task, in subject order, after every capture has joined.

require src/core/exit-status.f
require src/lib/error.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/fmt.f
require src/lib/memory.f
require src/lib/adt/result.f
require src/lib/fs.f
require src/lib/source.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-command.f
require src/lib/task.f
require src/lib/engine-candidate.f
require src/lib/json.f
require src/habu/verify-source.f

package LSP-CHECK
using SOURCE-ROOT

TYPED-VARIABLE CHK-BYTES-A ^u8
variable CHK-BYTES-U


: CHK-ARG+ ( str -- )
   PROC-ARGV+ ;


: CHK-FS-RC? ( n -- bool )
   {: rc:n :}
   rc E-FS-FIRST <= rc E-FS-LAST >= and ;

\ The E-FS code that ended Q, a read of a source, and the errno, else 0 0; any
\ other failure of the read goes on.
: CHK-READ-REJECTED ( [ -- ] -- n n ) {: q :}
   q catch {: rc:n :}
   rc CHK-FS-RC? if rc FS:ERRNO exit then
   rc 0<> if rc throw then
   0 0 ;

\ The loader's record of PATH, whose read ended with E-FS code RC and errno E,
\ placed at T.
: CHK-READ-RECORD ( str str n n -- )
   {: t:str path:str rc:n e:n :}
   rc E-FS-CHANGED = if
      t path SOURCE-ROOT:CHANGED-SOURCE
   then
   t path e SOURCE-ROOT:UNREADABLE-SOURCE ;

\ PATH, which the caller named, will not open or read: the loader's rejection
\ of a read that ended with code RC and errno E, which no source places.
: CHK-UNREADABLE-NAMED ( str n n -- ) {: path:str rc:n e:n :}
   NULL-PTR 0 >str path rc e CHK-READ-RECORD ;

\ PATH, found for the request T, is no regular file: the loader's rejection.
: CHK-IRREGULAR ( str str -- ) {: t:str path:str :}
   path FS:DIR? if
      t path FS:EISDIR SOURCE-ROOT:UNREADABLE-SOURCE
   then
   t path SOURCE-ROOT:UNSTAMPED-SOURCE ;

public

\ Only verified and refused judge PATH, refused for any file of its closure;
\ deferred leaves its warnings' tokens to the run; held: verifier only.
ENUM verdict
   VARIANT verified ;VARIANT
   VARIANT refused ;VARIANT
   VARIANT engine-provided ;VARIANT
   VARIANT held ;VARIANT
   VARIANT incomplete FIELD status PROCESS:ending ;VARIANT
   VARIANT deferred ;VARIANT
;ENUM

private

$40000 constant VFY-ERR-CAP
$0A constant VFY-LF

\ The child's answer, from its result line.
0 constant VFY-NONE
1 constant VFY-VERIFIED
2 constant VFY-REFUSED
3 constant VFY-HELD
4 constant VFY-MISSING
5 constant VFY-DEFERRED
6 constant VFY-UNREAD
7 constant VFY-FULL
8 constant VFY-STOPPED

DYNAMIC-BUFFER VFY-OUT u8               \ the child's stdout, then VERIFY-OUT$
DYNAMIC-BUFFER VFY-LOG u8
DYNAMIC-BUFFER VFY-REC u8               \ VFY-OUT's records alone
DYNAMIC-BUFFER VFY-DEFS u8
DYNAMIC-BUFFER VFY-FILES u8
DYNAMIC-BUFFER VFY-USES u8
DYNAMIC-BUFFER VFY-LOADS u8
DYNAMIC-BUFFER VFY-CANDS u8
DYNAMIC-BUFFER VFY-INDEX u8
DYNAMIC-BUFFER VFY-IN u8
variable VFY-IN-U
variable VFY-REC-U
variable VFY-DEFS-U
variable VFY-FILES-U
variable VFY-USES-U
variable VFY-LOADS-U
variable VFY-CANDS-U
variable VFY-INDEX-U
variable VFY-AT                         \ the child's completion cursor, -1 for none
variable VFY-OUT-U
variable VFY-LOG-U
variable VFY-ANSWER
TYPED-VARIABLE VFY-DEADLINE ms
create VFY-PATH PATH-CAP allot
variable VFY-PATH-U
create VFY-CHILD PATH-CAP allot
variable VFY-CHILD-U

\ While this file loads, SOURCE-ROOT:ACTIVE-SOURCE's path is its own. The child
\ beside it is fixed here before the working directory can name another tree.
SOURCE-ROOT:ACTIVE-SOURCE drop drop SOURCE-ROOT:DIRNAME
s" check-verify-child.f" VFY-CHILD FS:JOIN-PATH VFY-CHILD-U !

TYPED-VARIABLE VFY-PREPASS bool         \ the child runs check.f's pre-pass ...
TYPED-VARIABLE VFY-INDEX? bool          \ standalone candidate scan
TYPED-VARIABLE VFY-JSON bool            \ the child writes packets, false for prose records
TYPED-VARIABLE VFY-LABEL-A ^u8       \ ... naming the subject by this label
variable VFY-LABEL-U
TYPED-VARIABLE VFY-ENTRY bool           \ ... or on entry text, which no label names
TYPED-VARIABLE VFY-OF-A ^u8          \ the tail whose uses the child writes in every file,
variable VFY-OF-U                       \ 0 for none
create VFY-TARGET PATH-CAP allot        \ a missing or unread answer's file, empty for none
variable VFY-TARGET-U


: VFY-CHILD$ ( -- str )
   VFY-CHILD VFY-CHILD-U @ >str ;


: VFY-RESET ( -- )
   0 VFY-OUT-U !
   0 VFY-LOG-U !
   0 VFY-DEFS-U !
   0 VFY-FILES-U !
   0 VFY-USES-U !
   0 VFY-LOADS-U !
   0 VFY-CANDS-U !
   0 VFY-INDEX-U !
   0 VFY-IN-U !
   -1 VFY-AT !
   0 VFY-OF-U !
   VFY-NONE VFY-ANSWER !
   0 VFY-TARGET-U !
   false VFY-PREPASS !
   false VFY-ENTRY !
   false VFY-INDEX? !
   true VFY-JSON ! ;


\ PATH's canonical absolute spelling, a relative PATH read from the working
\ directory: the identity the loader keys it by and the name its packets carry,
\ whether or not a file is there yet.
: VFY-PATH! ( str -- ) {: s:str :}
   s STR:LEN 0= if E-FS-PATH throw then
   s CANONICAL drop {: c:str :}
   c STR:LEN PATH-CAP > if E-FS-CAPACITY throw then
   c STR:DATA VFY-PATH c STR:LEN BYTE-COPY
   c STR:LEN VFY-PATH-U ! ;


: VFY-PATH$ ( -- str )
   VFY-PATH VFY-PATH-U @ >str ;


: VFY-OUT+ ( str -- )
   {: s:str :}
   VFY-OUT-U @ s STR:LEN + VFY-OUT-RESERVE
   s STR:DATA VFY-OUT-U @ VFY-OUT s STR:LEN BYTE-COPY
   VFY-OUT-U @ s STR:LEN + VFY-OUT-U ! ;


: VFY-IN+ ( str -- )
   {: s:str :}
   s STR:LEN 0= if exit then
   VFY-IN-U @ s STR:LEN + VFY-IN-RESERVE
   s STR:DATA VFY-IN-U @ VFY-IN s STR:LEN BYTE-COPY
   VFY-IN-U @ s STR:LEN + VFY-IN-U ! ;


\ An open document after VFY-IN: the byte counts of its canonical path and of
\ its text on a line, then both.
: VFY-OPEN+ ( str str -- )
   {: p:str t:str :}
   SB-RESET p STR:LEN FMT:SB-U SB$ VFY-IN+
   s"  " VFY-IN+
   SB-RESET t STR:LEN FMT:SB-U SB$ VFY-IN+
   s\" \n" VFY-IN+
   p VFY-IN+
   t VFY-IN+ ;


\ The child's stdin: the open documents OPEN gives, an empty line, then the
\ subject's bytes, CHK-BYTES-A and CHK-BYTES-U.
: VFY-IN! ( [ [ str str -- ] -- ] -- )
   {: open :}
   [: VFY-OPEN+ ;] open execute
   s\" \n" VFY-IN+
   CHK-BYTES-A @ CHK-BYTES-U @ >str VFY-IN+ ;


: CHK-ENGINE$ ( -- str )
   ENGINE-CANDIDATE:PATH$ ;


\ The child's executable, the engine CHK-ENGINE$ answers, loading this tree's
\ tools/check-verify-child.f, named absolutely, in this process's working
\ directory, so neither this process's words nor an earlier check's can stand
\ in for a word of the subject. hb carries the verifier, so the child loads
\ only its body.
create VFY-EXE PATH-CAP allot
variable VFY-EXE-U

: VFY-EXE! ( -- )
   CHK-ENGINE$ {: p:str :}
   p STR:LEN PATH-CAP > if E-PROC-PATH throw then
   p STR:DATA VFY-EXE p STR:LEN BYTE-COPY
   p STR:LEN VFY-EXE-U ! ;

: VFY-EXE$ ( -- str )
   VFY-EXE VFY-EXE-U @ >str ;

: VFY-LOAD-ARGS ( -- )
   s" --load" CHK-ARG+ VFY-CHILD$ CHK-ARG+ ;

: VFY-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   VFY-LOAD-ARGS
   s" --" CHK-ARG+
   VFY-PATH$ CHK-ARG+
   VFY-INDEX? @ if s" --index" CHK-ARG+ then
   VFY-PREPASS @ VFY-ENTRY @ and if s" --entry" CHK-ARG+ then
   VFY-PREPASS @ VFY-ENTRY @ 0= and if
      s" --label" CHK-ARG+  VFY-LABEL-A @ VFY-LABEL-U @ >str CHK-ARG+
   then
   VFY-JSON @ 0= if s" --prose" CHK-ARG+ then
   VFY-AT @ 0 >= if s" --at" CHK-ARG+  SB-RESET VFY-AT @ FMT:SB-U SB$ CHK-ARG+ then
   VFY-OF-U @ 0 > if
      s" --of" CHK-ARG+  VFY-OF-A @ VFY-OF-U @ >str CHK-ARG+
   then
   PROC-ENV-INHERIT-MISSING ;


: VFY-OUT-ROOM ( n -- ^u8 )
   VFY-OUT-RESERVE 0 VFY-OUT ;


\ The child's run on VFY-IN, the open documents and the subject's bytes: its
\ stdout into VFY-OUT, which grows to what the child writes, and its stderr
\ into VFY-LOG, their lengths and its end left where PROCESS:CAPTURE-ENDING@
\ reads them. The end is read there, so the one returned here is dropped
\ through the conversion that reads a deadline as a status rather than throwing it.
: VFY-CAPTURE ( -- )
   VFY-ARGV
   VFY-ERR-CAP VFY-LOG-RESERVE
   VFY-EXE$ 0 VFY-IN VFY-IN-U @ >str
   [: VFY-OUT-ROOM ;] 0 VFY-LOG VFY-ERR-CAP >LEN
   VFY-DEADLINE @ RUN-ARGV-ENV-STDIN-GROWING-CAPTURE-OUTCOME
   PROCESS:ENDING>DEADLINE-RC drop 2drop ;


: VFY-DEFINITION$ ( -- str )
   s" check-verify: definition " ;

: VFY-FILE$ ( -- str )
   s" check-verify: file " ;

: VFY-USE$ ( -- str )
   s" check-verify: use " ;

: VFY-LOAD$ ( -- str )
   s" check-verify: load " ;

: VFY-CAND$ ( -- str )
   s" check-verify: candidate " ;

: VFY-INDEX$ ( -- str )
   s" check-verify: index " ;


\ A result line grants a verdict only after a clean child exit. A complete
\ result line is still framing, even when the process ends uncleanly.
: VFY-CLEAN-EXIT? ( PROCESS:ending -- bool )
   MATCH PROCESS:ending
      exited OF 0= ENDOF
      signaled OF drop false ENDOF
      timeout OF false ENDOF
   ;MATCH ;


\ ANSWER when the line A U is TAG alone, or TAG, a space and TARGET, which is
\ kept in VFY-TARGET; VFY-NONE for any other line or a TARGET longer than
\ PATH-CAP.
: VFY-TARGET-ANSWER ( str str n -- n )
   {: s:str tag:str answer:n :}
   s tag STR:STARTS-WITH? 0= if VFY-NONE exit then
   s STR:LEN tag STR:LEN = if 0 VFY-TARGET-U ! answer exit then
   s STR:DATA tag STR:LEN + c@ $20 <> if VFY-NONE exit then
   s STR:LEN tag STR:LEN - 1- {: tu:n :}
   tu PATH-CAP > if VFY-NONE exit then
   s STR:DATA tag STR:LEN + 1+ VFY-TARGET tu BYTE-COPY
   tu VFY-TARGET-U !
   answer ;


\ The answer the final line from AT to END in VFY-OUT gives, its line feed
\ excluded.
: VFY-ANSWER-AT ( n n -- n ) {: at:n end:n :}
   at VFY-OUT end at - >str {: s:str :}
   s s" check-verify: verified" STR:= if VFY-VERIFIED exit then
   s s" check-verify: refused" STR:= if VFY-REFUSED exit then
   s s" check-verify: full" STR:= if VFY-FULL exit then
   s s" check-verify: deferred" STR:= if VFY-DEFERRED exit then
   s s" check-verify: stopped" STR:= if VFY-STOPPED exit then
   s s" check-verify: held" STR:= if VFY-HELD exit then
   s s" check-verify: missing" VFY-MISSING VFY-TARGET-ANSWER {: m:n :}
   m VFY-NONE <> if m exit then
   s s" check-verify: unread" VFY-UNREAD VFY-TARGET-ANSWER ;


: VFY-LINE-START ( n -- n )
   begin dup 0 > while
      dup 1- VFY-OUT c@ VFY-LF = if exit then
      1-
   repeat ;


\ Keep the records in the child's OUTU bytes and the answer that ends them,
\ the answer out of VFY-OUT; a cut line is dropped.
: VFY-TAKE ( n -- )
   {: outu:n :}
   outu VFY-LINE-START {: end:n :}
   end VFY-OUT-U !
   end 0= if exit then
   end 1- VFY-LINE-START {: at:n :}
   at end 1- VFY-ANSWER-AT VFY-ANSWER !
   VFY-ANSWER @ VFY-NONE <> if at VFY-OUT-U ! then ;


: VFY-TARGET$ ( -- str )
   VFY-TARGET VFY-TARGET-U @ >str ;


: VFY-REC+ ( str -- )
   {: s:str :}
   s STR:LEN 0= if exit then
   VFY-REC-U @ s STR:LEN + VFY-REC-RESERVE
   s STR:DATA VFY-REC-U @ VFY-REC s STR:LEN BYTE-COPY
   VFY-REC-U @ s STR:LEN + VFY-REC-U ! ;


: VFY-DEFS+ ( str -- )
   {: s:str :}
   s STR:LEN 0= if exit then
   VFY-DEFS-U @ s STR:LEN + VFY-DEFS-RESERVE
   s STR:DATA VFY-DEFS-U @ VFY-DEFS s STR:LEN BYTE-COPY
   VFY-DEFS-U @ s STR:LEN + VFY-DEFS-U ! ;


: VFY-DEF-LINE ( n n -- )
   {: at:n end:n :}
   at VFY-DEFINITION$ STR:LEN +
   {: obj:n :}
   obj VFY-OUT end obj - >str VFY-DEFS+
   s\" \n" VFY-DEFS+ ;


: VFY-FILES+ ( str -- )
   {: s:str :}
   s STR:LEN 0= if exit then
   VFY-FILES-U @ s STR:LEN + VFY-FILES-RESERVE
   s STR:DATA VFY-FILES-U @ VFY-FILES s STR:LEN BYTE-COPY
   VFY-FILES-U @ s STR:LEN + VFY-FILES-U ! ;


: VFY-FILE-LINE ( n n -- )
   {: at:n end:n :}
   at VFY-FILE$ STR:LEN +
   {: obj:n :}
   obj VFY-OUT end obj - >str VFY-FILES+
   s\" \n" VFY-FILES+ ;


: VFY-USES+ ( str -- )
   {: s:str :}
   s STR:LEN 0= if exit then
   VFY-USES-U @ s STR:LEN + VFY-USES-RESERVE
   s STR:DATA VFY-USES-U @ VFY-USES s STR:LEN BYTE-COPY
   VFY-USES-U @ s STR:LEN + VFY-USES-U ! ;


: VFY-USE-LINE ( n n -- )
   {: at:n end:n :}
   at VFY-USE$ STR:LEN +
   {: obj:n :}
   obj VFY-OUT end obj - >str VFY-USES+
   s\" \n" VFY-USES+ ;


: VFY-LOADS+ ( str -- )
   {: s:str :}
   s STR:LEN 0= if exit then
   VFY-LOADS-U @ s STR:LEN + VFY-LOADS-RESERVE
   s STR:DATA VFY-LOADS-U @ VFY-LOADS s STR:LEN BYTE-COPY
   VFY-LOADS-U @ s STR:LEN + VFY-LOADS-U ! ;


: VFY-LOAD-LINE ( n n -- )
   {: at:n end:n :}
   at VFY-LOAD$ STR:LEN +
   {: obj:n :}
   obj VFY-OUT end obj - >str VFY-LOADS+
   s\" \n" VFY-LOADS+ ;


: VFY-CANDS+ ( str -- )
   {: s:str :}
   s STR:LEN 0= if exit then
   VFY-CANDS-U @ s STR:LEN + VFY-CANDS-RESERVE
   s STR:DATA VFY-CANDS-U @ VFY-CANDS s STR:LEN BYTE-COPY
   VFY-CANDS-U @ s STR:LEN + VFY-CANDS-U ! ;


: VFY-CAND-LINE ( n n -- )
   {: at:n end:n :}
   at VFY-CAND$ STR:LEN +
   {: obj:n :}
   obj VFY-OUT end obj - >str VFY-CANDS+
   s\" \n" VFY-CANDS+ ;

: VFY-INDEX-LINE ( n n -- )
   {: at:n end:n :}
   at VFY-INDEX$ STR:LEN + {: obj:n :}
   end obj - VFY-INDEX-U @ + VFY-INDEX-RESERVE
   obj VFY-OUT VFY-INDEX-U @ VFY-INDEX end obj - BYTE-COPY
   end obj - VFY-INDEX-U +!
   VFY-INDEX-U @ 1+ VFY-INDEX-RESERVE
   VFY-LF VFY-INDEX-U @ VFY-INDEX c!
   1 VFY-INDEX-U +! ;


: VFY-LINE-END ( n -- n )
   begin
      dup VFY-OUT-U @ < if dup VFY-OUT c@ VFY-LF <> else false then
   while 1+ repeat ;


\ The line of VFY-OUT that starts at AT, to the buffer its tag names, a record
\ to VFY-REC: where the next line starts.
: VFY-REC-LINE ( n -- n )
   {: at:n :}
   at VFY-LINE-END
   {: end:n :}
   at VFY-OUT end at - >str {: line:str :}
   line VFY-FILE$ STR:STARTS-WITH? if
      at end VFY-FILE-LINE
      end 1+ exit
   then
   line VFY-DEFINITION$ STR:STARTS-WITH? if
      at end VFY-DEF-LINE
      end 1+ exit
   then
   line VFY-USE$ STR:STARTS-WITH? if
      at end VFY-USE-LINE
      end 1+ exit
   then
   line VFY-LOAD$ STR:STARTS-WITH? if
      at end VFY-LOAD-LINE
      end 1+ exit
   then
   line VFY-CAND$ STR:STARTS-WITH? if
      at end VFY-CAND-LINE
      end 1+ exit
   then
   line VFY-INDEX$ STR:STARTS-WITH? if
      at end VFY-INDEX-LINE
      end 1+ exit
   then
   line VFY-REC+
   s\" \n" VFY-REC+
   end 1+ ;


\ VFY-OUT's records alone, each tagged line moved to its buffer. The pre-pass
\ writes records alone.
: VFY-RECORDS ( -- )
   VFY-PREPASS @ if exit then
   VFY-OUT-U @ 0= if exit then
   0 VFY-REC-U !
   0 begin dup VFY-OUT-U @ < while VFY-REC-LINE repeat drop
   VFY-REC-U @ VFY-OUT-RESERVE
   \ Tagged lines alone leave no record, and VFY-REC no storage to copy.
   VFY-REC-U @ 0<> if 0 VFY-REC 0 VFY-OUT VFY-REC-U @ BYTE-COPY then
   VFY-REC-U @ VFY-OUT-U ! ;


\ How the child ended. More stderr than the capture holds kills it, and its
\ E-PROC-STDERR-CAPACITY is thrown on once what was captured is kept. The
\ executable is resolved before the capture, so a throw from the engine's
\ resolver is the resolver's, never read as the child's overflow.
: VFY-RUN ( -- PROCESS:ending )
   VFY-EXE!
   [: VFY-CAPTURE ;] catch {: rc:n :}
   rc 0<> rc E-PROC-STDERR-CAPACITY <> and if rc throw then
   PROCESS:CAPTURE-ENDING@ {: outu:len erru:len o :}
   erru LEN>N VFY-LOG-U !
   outu LEN>N VFY-TAKE
   VFY-RECORDS
   rc 0<> if rc throw then
   o ;


: VFY-VERDICT ( PROCESS:ending -- verdict ) {: o :}
   o VFY-CLEAN-EXIT? if
      VFY-ANSWER @ VFY-VERIFIED = if LSP--CHECK-VERDICT:VERIFIED exit then
      VFY-ANSWER @ VFY-REFUSED = if LSP--CHECK-VERDICT:REFUSED exit then
      VFY-ANSWER @ VFY-MISSING = if LSP--CHECK-VERDICT:REFUSED exit then
      VFY-ANSWER @ VFY-UNREAD = if LSP--CHECK-VERDICT:REFUSED exit then
      VFY-ANSWER @ VFY-FULL = if LSP--CHECK-VERDICT:REFUSED exit then
      VFY-ANSWER @ VFY-DEFERRED = if LSP--CHECK-VERDICT:DEFERRED exit then
      VFY-ANSWER @ VFY-STOPPED = if LSP--CHECK-VERDICT:DEFERRED exit then
      VFY-ANSWER @ VFY-HELD = if LSP--CHECK-VERDICT:HELD exit then
   then
   o LSP--CHECK-VERDICT:INCOMPLETE ;


: VFY-PREVERDICT ( PROCESS:ending -- result<n,PROCESS:ending> ) {: o :}
   o VFY-CLEAN-EXIT? if
      VFY-ANSWER @ VFY-VERIFIED = if 0 RESULT:OK exit then
      VFY-ANSWER @ VFY-REFUSED = if EXIT-STATUS:REJECTED RESULT:OK exit then
      VFY-ANSWER @ VFY-MISSING = if EXIT-STATUS:REJECTED RESULT:OK exit then
      VFY-ANSWER @ VFY-UNREAD = if EXIT-STATUS:IOERR RESULT:OK exit then
      VFY-ANSWER @ VFY-FULL = if EXIT-STATUS:FULL RESULT:OK exit then
   then
   o RESULT:ERR ;


\ The check of VERIFY-BYTES-AT and VERIFY-BYTES-OF, once VFY-RESET has run and
\ the cursor or the tail is set.
: VFY-BYTES ( str str [ [ str str -- ] -- ] ms -- verdict )
   {: src:str path:str open deadline :}
   path VFY-PATH!
   VFY-INDEX? @ 0= if
      VFY-PATH$ REQUIRE-REG:ENGINE-PROVIDES? if LSP--CHECK-VERDICT:ENGINE-PROVIDED exit then
   then
   src STR:DATA CHK-BYTES-A !
   src STR:LEN CHK-BYTES-U !
   open VFY-IN!
   deadline VFY-DEADLINE !
   VFY-RUN VFY-VERDICT ;

public

\ The records of the last VERIFY-BYTES or PREVERIFY-BYTES: the checker's
\ schema-1 packets, one JSON object per line, or prose records when the call's
\ JSON is false. A packet names the subject PATH's canonical absolute path,
\ LABEL after PREVERIFY-BYTES, and counts positions in the bytes, a dependency
\ its file and counts there. A stop's record is the last. Every VERIFY-*$
\ holds until the next call.
: VERIFY-OUT$ ( -- str )
   VFY-OUT-U @ 0= if NULL$ exit then
   0 VFY-OUT VFY-OUT-U @ >str ;

\ The child's stderr from the last VERIFY-BYTES or PREVERIFY-BYTES.
: VERIFY-LOG$ ( -- str )
   VFY-LOG-U @ 0= if NULL$ exit then
   0 VFY-LOG VFY-LOG-U @ >str ;

\ The definitions the last VERIFY-BYTES retained, one JSON object per line, in
\ the order the verifier read them.
: VERIFY-DEFS$ ( -- str )
   VFY-DEFS-U @ 0= if NULL$ exit then
   0 VFY-DEFS VFY-DEFS-U @ >str ;

\ The file visits the last VERIFY-BYTES read, one JSON object per visit, in
\ acquisition order.
: VERIFY-FILES$ ( -- str )
   VFY-FILES-U @ 0= if NULL$ exit then
   0 VFY-FILES VFY-FILES-U @ >str ;

\ The uses the last VERIFY-BYTES's subject bound to source targets or identified
\ as engine primitives, and for VERIFY-BYTES-OF those of its tail in the files
\ the subject loads, one JSON object per line, in the order the checker
\ published them.
: VERIFY-USES$ ( -- str )
   VFY-USES-U @ 0= if NULL$ exit then
   0 VFY-USES VFY-USES-U @ >str ;

\ The top-level loaders of the last VERIFY-BYTES's subject that reached a file,
\ one JSON object per line, in the subject's order; none for PREVERIFY-BYTES.
: VERIFY-LOADS$ ( -- str )
   VFY-LOADS-U @ 0= if NULL$ exit then
   0 VFY-LOADS VFY-LOADS-U @ >str ;

\ The spellings the last VERIFY-BYTES-AT's cursor offered, one JSON object per
\ line, in the order the checker offered them; none for VERIFY-BYTES.
: VERIFY-CANDIDATES$ ( -- str )
   VFY-CANDS-U @ 0= if NULL$ exit then
   0 VFY-CANDS VFY-CANDS-U @ >str ;

\ Surviving declarations from a standalone scan of the last INDEX-BYTES,
\ one JSON object per line. The caller uses them only after a complete child.
: VERIFY-INDEX$ ( -- str )
   VFY-INDEX-U @ 0= if NULL$ exit then
   0 VFY-INDEX VFY-INDEX-U @ >str ;

\ Check the bytes as the file at PATH, the child given DEADLINE, with the
\ documents open that OPEN gives: OPEN is given a word taking an open
\ document's canonical path and text, and calls it once for each. The records
\ are packets when JSON is true and prose when false. A throw that ends the
\ verification after its record refuses it: the record ends VERIFY-OUT$, after
\ every record made before it, and VERIFY-STATUS says with what status. A
\ loader's fault is such a stop: a loader form the composition rejects, a
\ string or a locals group a file never closes, or a file a loader word names
\ that is not there or cannot be read, which VERIFY-UNREAD$ names. An empty
\ PATH is E-FS-PATH; an engine src/lib/engine-candidate.f refuses (E-FS-OPEN)
\ or a failed spawn throws as well. stdout is kept whole; more than 256 KiB of
\ the child's stderr is E-PROC-STDERR-CAPACITY, VERIFY-OUT$ then holding every
\ complete record received before it.
: VERIFY-BYTES ( str str [ [ str str -- ] -- ] ms bool -- verdict )
   {: src:str path:str open deadline json:bool :}
   VFY-RESET
   json VFY-JSON !
   src path open deadline VFY-BYTES ;

\ VERIFY-BYTES with packets, the child offering as well the spellings that
\ would bind at byte AT of the bytes, VERIFY-CANDIDATES$; a negative AT is no
\ cursor.
: VERIFY-BYTES-AT ( str str n [ [ str str -- ] -- ] ms -- verdict )
   {: src:str path:str at:n open deadline :}
   VFY-RESET
   at VFY-AT !
   src path open deadline VFY-BYTES ;

\ Scan current source in the verifier child. It bypasses the engine-provided
\ path skip only for this mode; incomplete children have no usable index.
: INDEX-BYTES ( str str ms -- verdict )
   {: src:str path:str deadline :}
   VFY-RESET
   true VFY-INDEX? !
   src path [: drop ;] deadline VFY-BYTES ;

\ VERIFY-BYTES with packets and VERIFY-USES$ holding as well every use the check binds to
\ a located declaration in a file other than the subject when the declaration's
\ canonical tail is TAIL, byte for byte: a use of the word wherever the
\ composition reads it. An empty TAIL names no word.
: VERIFY-BYTES-OF ( str str str [ [ str str -- ] -- ] ms -- verdict )
   {: src:str path:str tail:str open deadline :}
   VFY-RESET
   tail STR:DATA VFY-OF-A !
   tail STR:LEN VFY-OF-U !
   src path open deadline VFY-BYTES ;


private

\ The framed verifier keeps input offsets, since VFY-IN can move as subjects
\ are added. Each frame records its path and source.
4 constant VFY-BATCH-CELLS
0 constant B-PATH
1 constant B-PATH-U
2 constant B-SRC
3 constant B-SRC-U
DYNAMIC-BUFFER VFY-BATCH-META n
DYNAMIC-BUFFER VFY-BATCH-OUT u8
DYNAMIC-BUFFER VFY-BATCH-TAIL u8
variable VFY-BATCH-N
TYPED-VARIABLE VFY-REF-BATCH bool
variable VFY-BATCH-OUT-U
variable VFY-BATCH-POS
variable VFY-BATCH-BROKEN
variable VFY-BATCH-COMPLETE
TYPED-VARIABLE VFY-BATCH-OUTCOME PROCESS:ending

: BATCH@ ( n n -- n )  swap VFY-BATCH-CELLS * + VFY-BATCH-META @ ;
: BATCH! ( n n n -- )  swap VFY-BATCH-CELLS * + VFY-BATCH-META ! ;

: VFY-BATCH-ADD ( str str [ [ str str -- ] -- ] -- )
   {: src:str path:str open :}
   path VFY-PATH!
   VFY-BATCH-N @ {: k:n :}
   k 1+ VFY-BATCH-CELLS * VFY-BATCH-META-RESERVE
   SB-RESET VFY-PATH-U @ FMT:SB-U s"  " SB-APPEND
   src STR:LEN FMT:SB-U s\" \n" SB-APPEND SB$ VFY-IN+
   VFY-IN-U @ k B-PATH BATCH!
   VFY-PATH-U @ k B-PATH-U BATCH!
   VFY-PATH$ VFY-IN+
   VFY-IN-U @ k B-SRC BATCH!
   src STR:LEN k B-SRC-U BATCH!
   src VFY-IN+
   [: VFY-OPEN+ ;] open execute
   s\" \n" VFY-IN+
   k 1+ VFY-BATCH-N ! ;

: VFY-BATCH-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   VFY-LOAD-ARGS
   s" --" CHK-ARG+
   VFY-REF-BATCH @ if s" --batch-ref" else s" --batch" then CHK-ARG+
   VFY-OF-A @ VFY-OF-U @ >str CHK-ARG+
   PROC-ENV-INHERIT-MISSING ;

: VFY-BATCH-ROOM ( n -- ^u8 )
   VFY-BATCH-OUT-RESERVE 0 VFY-BATCH-OUT ;

: VFY-BATCH-CAPTURE ( -- )
   VFY-BATCH-ARGV
   VFY-ERR-CAP VFY-BATCH-N @ * {: errcap:n :}
   errcap VFY-LOG-RESERVE
   VFY-EXE$ 0 VFY-IN VFY-IN-U @ >str
   [: VFY-BATCH-ROOM ;] 0 VFY-LOG errcap >LEN
   VFY-DEADLINE @ RUN-ARGV-ENV-STDIN-GROWING-CAPTURE-OUTCOME
   PROCESS:ENDING>DEADLINE-RC drop 2drop ;

: VFY-BATCH-RESET-RESULT ( -- )
   0 VFY-OUT-U ! 0 VFY-DEFS-U ! 0 VFY-FILES-U !
   0 VFY-USES-U ! 0 VFY-LOADS-U ! 0 VFY-CANDS-U !
   VFY-NONE VFY-ANSWER !
   0 VFY-TARGET-U ! ;

: VFY-BATCH-SUBJECT ( n -- )
   {: k:n :}
   k B-PATH BATCH@ VFY-IN k B-PATH-U BATCH@ >str VFY-PATH!
   k B-SRC BATCH@ VFY-IN CHK-BYTES-A !
   k B-SRC-U BATCH@ CHK-BYTES-U ! ;

: VFY-BATCH-MARK$ ( n -- str )
   SB-RESET s" check-verify: batch-end " SB-APPEND
   FMT:SB-U s\" \n" SB-APPEND SB$ ;

\ The first complete marker from the current cursor, or -1 if the child did
\ not finish that frame. The marker cannot be part of a JSON packet.
: VFY-BATCH-MARK ( n -- n )
   {: k:n :}
   VFY-BATCH-POS @ {: at:n :}
   at VFY-BATCH-OUT VFY-BATCH-OUT-U @ at - >str
   k VFY-BATCH-MARK$ STR:FIND-SUB
   MATCH option
      some OF IDX>N at + ENDOF
      none OF -1 ENDOF
   ;MATCH ;

: VFY-BATCH-RESULT ( n -- verdict )
   {: k:n :}
   k VFY-BATCH-N @ >= if E-TBL-BOUNDS throw then
   VFY-BATCH-RESET-RESULT
   k VFY-BATCH-SUBJECT
   false VFY-BATCH-COMPLETE !
   VFY-BATCH-BROKEN @ if
      VFY-PATH$ REQUIRE-REG:ENGINE-PROVIDES? if LSP--CHECK-VERDICT:ENGINE-PROVIDED exit then
      VFY-BATCH-OUTCOME @ VFY-VERDICT exit
   then
   k VFY-BATCH-MARK {: end:n :}
   end 0 >= if
      VFY-BATCH-POS @ {: at:n :}
      end at - {: u:n :}
      u VFY-OUT-RESERVE
      at VFY-BATCH-OUT 0 VFY-OUT u BYTE-COPY
      u VFY-TAKE
      VFY-ANSWER @ VFY-NONE = if true VFY-BATCH-BROKEN ! then
      VFY-RECORDS
      VFY-BATCH-BROKEN @ 0= VFY-BATCH-OUTCOME @ VFY-CLEAN-EXIT? and if
         true VFY-BATCH-COMPLETE !
      then
      end k VFY-BATCH-MARK$ STR:LEN + VFY-BATCH-POS !
      k VFY-BATCH-N @ 1- = VFY-BATCH-POS @ VFY-BATCH-OUT-U @ <> and
      if true VFY-BATCH-BROKEN ! false VFY-BATCH-COMPLETE ! then
   else true VFY-BATCH-BROKEN ! then
   VFY-PATH$ REQUIRE-REG:ENGINE-PROVIDES? if LSP--CHECK-VERDICT:ENGINE-PROVIDED exit then
   VFY-BATCH-BROKEN @ if VFY-NONE VFY-ANSWER ! then
   VFY-BATCH-OUTCOME @ VFY-VERDICT ;

12 constant VFY-LANES
DYNAMIC-BUFFER VFY-LANE-IN u8
DYNAMIC-BUFFER VFY-LANE-META n
DYNAMIC-BUFFER VFY-LANE-OUT0 u8
DYNAMIC-BUFFER VFY-LANE-OUT1 u8
DYNAMIC-BUFFER VFY-LANE-OUT2 u8
DYNAMIC-BUFFER VFY-LANE-OUT3 u8
DYNAMIC-BUFFER VFY-LANE-OUT4 u8
DYNAMIC-BUFFER VFY-LANE-OUT5 u8
DYNAMIC-BUFFER VFY-LANE-OUT6 u8
DYNAMIC-BUFFER VFY-LANE-OUT7 u8
DYNAMIC-BUFFER VFY-LANE-OUT8 u8
DYNAMIC-BUFFER VFY-LANE-OUT9 u8
DYNAMIC-BUFFER VFY-LANE-OUT10 u8
DYNAMIC-BUFFER VFY-LANE-OUT11 u8
DYNAMIC-BUFFER VFY-LANE-ERR0 u8
DYNAMIC-BUFFER VFY-LANE-ERR1 u8
DYNAMIC-BUFFER VFY-LANE-ERR2 u8
DYNAMIC-BUFFER VFY-LANE-ERR3 u8
DYNAMIC-BUFFER VFY-LANE-ERR4 u8
DYNAMIC-BUFFER VFY-LANE-ERR5 u8
DYNAMIC-BUFFER VFY-LANE-ERR6 u8
DYNAMIC-BUFFER VFY-LANE-ERR7 u8
DYNAMIC-BUFFER VFY-LANE-ERR8 u8
DYNAMIC-BUFFER VFY-LANE-ERR9 u8
DYNAMIC-BUFFER VFY-LANE-ERR10 u8
DYNAMIC-BUFFER VFY-LANE-ERR11 u8
VFY-LANES TYPED-BUFFER VFY-LANE-IN-AT n
VFY-LANES TYPED-BUFFER VFY-LANE-IN-U n
VFY-LANES TYPED-BUFFER VFY-LANE-META-AT n
VFY-LANES TYPED-BUFFER VFY-LANE-N n
VFY-LANES TYPED-BUFFER VFY-LANE-OUT-U n
VFY-LANES TYPED-BUFFER VFY-LANE-ERR-U n
VFY-LANES TYPED-BUFFER VFY-LANE-OF-AT n
VFY-LANES TYPED-BUFFER VFY-LANE-OF-U n
VFY-LANES TYPED-BUFFER VFY-LANE-REF bool
VFY-LANES TYPED-BUFFER VFY-LANE-CODE n
VFY-LANES TYPED-BUFFER VFY-LANE-STARTED n
VFY-LANES TYPED-BUFFER VFY-LANE-END PROCESS:ending
variable VFY-LANE-IN-TOP
variable VFY-LANE-META-TOP
create VFY-LANE-ENGINE PATH-CAP allot
variable VFY-LANE-ENGINE-U

TASK:MIN-STACK TASK:TASK VFY-TASK0
TASK:MIN-STACK TASK:TASK VFY-TASK1
TASK:MIN-STACK TASK:TASK VFY-TASK2
TASK:MIN-STACK TASK:TASK VFY-TASK3
TASK:MIN-STACK TASK:TASK VFY-TASK4
TASK:MIN-STACK TASK:TASK VFY-TASK5
TASK:MIN-STACK TASK:TASK VFY-TASK6
TASK:MIN-STACK TASK:TASK VFY-TASK7
TASK:MIN-STACK TASK:TASK VFY-TASK8
TASK:MIN-STACK TASK:TASK VFY-TASK9
TASK:MIN-STACK TASK:TASK VFY-TASK10
TASK:MIN-STACK TASK:TASK VFY-TASK11

: VFY-LANE-TASK ( n -- ^n )
   dup 0= if drop VFY-TASK0 exit then
   dup 1 = if drop VFY-TASK1 exit then
   dup 2 = if drop VFY-TASK2 exit then
   dup 3 = if drop VFY-TASK3 exit then
   dup 4 = if drop VFY-TASK4 exit then
   dup 5 = if drop VFY-TASK5 exit then
   dup 6 = if drop VFY-TASK6 exit then
   dup 7 = if drop VFY-TASK7 exit then
   dup 8 = if drop VFY-TASK8 exit then
   dup 9 = if drop VFY-TASK9 exit then
   dup 10 = if drop VFY-TASK10 exit then
   dup 11 = if drop VFY-TASK11 exit then
   E-TBL-BOUNDS throw ;

: VFY-LANE-OUT-ROOM ( n n -- ^u8 )
   {: k:n need:n :}
   k 0= if need VFY-LANE-OUT0-RESERVE 0 VFY-LANE-OUT0 exit then
   k 1 = if need VFY-LANE-OUT1-RESERVE 0 VFY-LANE-OUT1 exit then
   k 2 = if need VFY-LANE-OUT2-RESERVE 0 VFY-LANE-OUT2 exit then
   k 3 = if need VFY-LANE-OUT3-RESERVE 0 VFY-LANE-OUT3 exit then
   k 4 = if need VFY-LANE-OUT4-RESERVE 0 VFY-LANE-OUT4 exit then
   k 5 = if need VFY-LANE-OUT5-RESERVE 0 VFY-LANE-OUT5 exit then
   k 6 = if need VFY-LANE-OUT6-RESERVE 0 VFY-LANE-OUT6 exit then
   k 7 = if need VFY-LANE-OUT7-RESERVE 0 VFY-LANE-OUT7 exit then
   k 8 = if need VFY-LANE-OUT8-RESERVE 0 VFY-LANE-OUT8 exit then
   k 9 = if need VFY-LANE-OUT9-RESERVE 0 VFY-LANE-OUT9 exit then
   k 10 = if need VFY-LANE-OUT10-RESERVE 0 VFY-LANE-OUT10 exit then
   k 11 = if need VFY-LANE-OUT11-RESERVE 0 VFY-LANE-OUT11 exit then
   E-TBL-BOUNDS throw ;

: VFY-LANE-ERR-ROOM ( n n -- ^u8 )
   {: k:n need:n :}
   k 0= if need VFY-LANE-ERR0-RESERVE 0 VFY-LANE-ERR0 exit then
   k 1 = if need VFY-LANE-ERR1-RESERVE 0 VFY-LANE-ERR1 exit then
   k 2 = if need VFY-LANE-ERR2-RESERVE 0 VFY-LANE-ERR2 exit then
   k 3 = if need VFY-LANE-ERR3-RESERVE 0 VFY-LANE-ERR3 exit then
   k 4 = if need VFY-LANE-ERR4-RESERVE 0 VFY-LANE-ERR4 exit then
   k 5 = if need VFY-LANE-ERR5-RESERVE 0 VFY-LANE-ERR5 exit then
   k 6 = if need VFY-LANE-ERR6-RESERVE 0 VFY-LANE-ERR6 exit then
   k 7 = if need VFY-LANE-ERR7-RESERVE 0 VFY-LANE-ERR7 exit then
   k 8 = if need VFY-LANE-ERR8-RESERVE 0 VFY-LANE-ERR8 exit then
   k 9 = if need VFY-LANE-ERR9-RESERVE 0 VFY-LANE-ERR9 exit then
   k 10 = if need VFY-LANE-ERR10-RESERVE 0 VFY-LANE-ERR10 exit then
   k 11 = if need VFY-LANE-ERR11-RESERVE 0 VFY-LANE-ERR11 exit then
   E-TBL-BOUNDS throw ;

: VFY-LANE-OUT$ ( n -- str )
   {: k:n :}
   k VFY-LANE-OUT-U @ 0= if NULL$ exit then
   k k VFY-LANE-OUT-U @ VFY-LANE-OUT-ROOM k VFY-LANE-OUT-U @ >str ;

: VFY-LANE-ERR$ ( n -- str )
   {: k:n :}
   k VFY-LANE-ERR-U @ 0= if NULL$ exit then
   k k VFY-LANE-ERR-U @ VFY-LANE-ERR-ROOM k VFY-LANE-ERR-U @ >str ;

: VFY-LANE-SAVE ( n -- )
   {: k:n :}
   s" 0 0" VFY-IN+ s\" \n" VFY-IN+
   VFY-BATCH-N @ k VFY-LANE-N !
   VFY-IN-U @ k VFY-LANE-IN-U !
   VFY-LANE-IN-TOP @ k VFY-LANE-IN-AT !
   VFY-LANE-IN-TOP @ VFY-IN-U @ + VFY-LANE-IN-RESERVE
   0 VFY-IN VFY-LANE-IN-TOP @ VFY-LANE-IN VFY-IN-U @ BYTE-COPY
   VFY-LANE-IN-TOP @ VFY-IN-U @ + VFY-LANE-IN-TOP !
   VFY-LANE-IN-TOP @ k VFY-LANE-OF-AT !
   VFY-OF-U @ k VFY-LANE-OF-U !
   VFY-LANE-IN-TOP @ VFY-OF-U @ + VFY-LANE-IN-RESERVE
   VFY-OF-A @ VFY-LANE-IN-TOP @ VFY-LANE-IN VFY-OF-U @ BYTE-COPY
   VFY-LANE-IN-TOP @ VFY-OF-U @ + VFY-LANE-IN-TOP !
   VFY-REF-BATCH @ k VFY-LANE-REF !
   VFY-LANE-META-TOP @ k VFY-LANE-META-AT !
   VFY-LANE-META-TOP @ VFY-BATCH-N @ VFY-BATCH-CELLS * + VFY-LANE-META-RESERVE
   VFY-BATCH-N @ VFY-BATCH-CELLS * 0 ?do i VFY-BATCH-META @ VFY-LANE-META-TOP @ i + VFY-LANE-META ! loop
   VFY-LANE-META-TOP @ VFY-BATCH-N @ VFY-BATCH-CELLS * + VFY-LANE-META-TOP !
   0 k VFY-LANE-OUT-U ! 0 k VFY-LANE-ERR-U ! 0 k VFY-LANE-CODE ! ;

: VFY-LANE-ARGS ( CMD:command n -- CMD:command )
   {: k:n :}
   s" --load" CMD:ARG+ VFY-CHILD$ CMD:ARG+
   s" --" CMD:ARG+
   k VFY-LANE-REF @ if s" --batch-ref" else s" --batch" then CMD:ARG+
   k VFY-LANE-OF-AT @ VFY-LANE-IN k VFY-LANE-OF-U @ >str CMD:ARG+ ;

\ Run lane k's child on its snapshot and copy both captures into the lane's
\ buffers before the scope releases them.
: VFY-LANE-RUN ( n CMD:command -- CMD:command )
   swap {: k:n :}
   k VFY-LANE-ARGS
   k VFY-LANE-IN-AT @ VFY-LANE-IN k VFY-LANE-IN-U @ >str CMD:IN!
   VFY-LANE-ENGINE VFY-LANE-ENGINE-U @ >str VFY-DEADLINE @ CMD:RUN-OUTCOME
   k VFY-LANE-END !
   CMD:OUT$ {: out:str :}
   out STR:LEN {: outu:n :}
   outu 0 > if out STR:DATA k outu VFY-LANE-OUT-ROOM outu BYTE-COPY then
   outu k VFY-LANE-OUT-U !
   CMD:ERR$ {: err:str :}
   err STR:LEN {: erru:n :}
   erru 0 > if err STR:DATA k erru VFY-LANE-ERR-ROOM erru BYTE-COPY then
   erru k VFY-LANE-ERR-U ! ;

: VFY-LANE-CAPTURE ( n -- ) [: VFY-LANE-RUN ;] CMD:WITH-COMMAND ;

: VFY-LANE-CAPTURE-KEEP ( n -- n ) dup VFY-LANE-CAPTURE ;

: VFY-LANE-WORK ( n -- )
   {: k:n :}
   k [: VFY-LANE-CAPTURE-KEEP ;] catch {: code:n :} drop
   code 0<> if code k VFY-LANE-CODE ! then
   0 TASK:RETURN ;

: VFY-WORK0 ( -- ) 0 VFY-LANE-WORK ;
: VFY-WORK1 ( -- ) 1 VFY-LANE-WORK ;
: VFY-WORK2 ( -- ) 2 VFY-LANE-WORK ;
: VFY-WORK3 ( -- ) 3 VFY-LANE-WORK ;
: VFY-WORK4 ( -- ) 4 VFY-LANE-WORK ;
: VFY-WORK5 ( -- ) 5 VFY-LANE-WORK ;
: VFY-WORK6 ( -- ) 6 VFY-LANE-WORK ;
: VFY-WORK7 ( -- ) 7 VFY-LANE-WORK ;
: VFY-WORK8 ( -- ) 8 VFY-LANE-WORK ;
: VFY-WORK9 ( -- ) 9 VFY-LANE-WORK ;
: VFY-WORK10 ( -- ) 10 VFY-LANE-WORK ;
: VFY-WORK11 ( -- ) 11 VFY-LANE-WORK ;

: VFY-LANE-WORD ( n -- [ -- ] )
   dup 0= if drop ['] VFY-WORK0 exit then
   dup 1 = if drop ['] VFY-WORK1 exit then
   dup 2 = if drop ['] VFY-WORK2 exit then
   dup 3 = if drop ['] VFY-WORK3 exit then
   dup 4 = if drop ['] VFY-WORK4 exit then
   dup 5 = if drop ['] VFY-WORK5 exit then
   dup 6 = if drop ['] VFY-WORK6 exit then
   dup 7 = if drop ['] VFY-WORK7 exit then
   dup 8 = if drop ['] VFY-WORK8 exit then
   dup 9 = if drop ['] VFY-WORK9 exit then
   dup 10 = if drop ['] VFY-WORK10 exit then
   dup 11 = if drop ['] VFY-WORK11 exit then
   E-TBL-BOUNDS throw ;

: VFY-LANE-START ( n -- )
   {: k:n :}
   k VFY-LANE-N @ 0= if exit then
   k VFY-LANE-WORD k VFY-LANE-TASK TASK:ACTIVATE
   1 k VFY-LANE-STARTED ! ;

: VFY-LANE-START-KEEP ( n -- n ) dup VFY-LANE-START ;

: VFY-LANE-JOIN ( -- )
   VFY-LANES 0 ?do
      i VFY-LANE-STARTED @ 0<> if
         i VFY-LANE-TASK TASK:JOIN MATCH result
            ok OF drop ENDOF
            err OF i VFY-LANE-CODE ! ENDOF
         ;MATCH
         0 i VFY-LANE-STARTED !
      then
   loop ;

: VFY-LANE-RESTORE ( n -- )
   {: k:n :}
   VFY-RESET
   k VFY-LANE-N @ VFY-BATCH-N !
   k VFY-LANE-IN-U @ dup VFY-IN-U ! VFY-IN-RESERVE
   k VFY-LANE-IN-AT @ VFY-LANE-IN 0 VFY-IN VFY-IN-U @ BYTE-COPY
   VFY-BATCH-N @ VFY-BATCH-CELLS * VFY-BATCH-META-RESERVE
   VFY-BATCH-N @ VFY-BATCH-CELLS * 0 ?do k VFY-LANE-META-AT @ i + VFY-LANE-META @ i VFY-BATCH-META ! loop
   k VFY-LANE-OUT-U @ dup VFY-BATCH-OUT-U ! VFY-BATCH-OUT-RESERVE
   VFY-BATCH-OUT-U @ 0 > if
      k VFY-LANE-OUT$ STR:DATA 0 VFY-BATCH-OUT VFY-BATCH-OUT-U @ BYTE-COPY
   then
   k VFY-LANE-ERR-U @ dup VFY-LOG-U ! VFY-LOG-RESERVE
   VFY-LOG-U @ 0 > if
      k VFY-LANE-ERR$ STR:DATA 0 VFY-LOG VFY-LOG-U @ BYTE-COPY
   then
   k VFY-LANE-END @ VFY-BATCH-OUTCOME !
   0 VFY-BATCH-POS ! false VFY-BATCH-BROKEN ! ;

public

: VERIFY-BATCH-WAVE-BEGIN ( -- )
   0 VFY-LANE-IN-TOP !
   0 VFY-LANE-META-TOP !
   VFY-LANES 0 ?do 0 i VFY-LANE-STARTED ! loop
   VFY-EXE! VFY-EXE$ {: p:str :}
   p STR:LEN PATH-CAP > if E-PROC-PATH throw then
   p STR:DATA VFY-LANE-ENGINE p STR:LEN BYTE-COPY
   p STR:LEN VFY-LANE-ENGINE-U ! ;


: VERIFY-BATCH-WAVE-ADD ( n -- ) VFY-LANE-SAVE ;

: VERIFY-BATCH-WAVE-RUN ( n ms -- )
   {: count:n deadline:ms :}
   count 0 < count VFY-LANES > or if E-TBL-BOUNDS throw then
   deadline VFY-DEADLINE !
   count 0 ?do i [: VFY-LANE-START-KEEP ;] catch {: code:n :} drop code 0<> if
      VFY-LANE-JOIN code throw
   then loop
   VFY-LANE-JOIN ;

: VERIFY-BATCH-WAVE-LOAD ( n -- n )
   {: k:n :}
   k VFY-LANE-RESTORE
   k VFY-LANE-CODE @ ;

\ Build a bounded set of independent checks for one tail, then consume their
\ results in add order. Each ADD receives the same open-document callback a
\ one-shot check would; each result exposes the usual VERIFY-* accessors.
: VERIFY-BATCH-BEGIN ( str -- )
   {: tail:str :}
   VFY-RESET
   false VFY-REF-BATCH !
   tail STR:LEN 1 max VFY-BATCH-TAIL-RESERVE
   tail STR:DATA 0 VFY-BATCH-TAIL tail STR:LEN BYTE-COPY
   0 VFY-BATCH-TAIL VFY-OF-A ! tail STR:LEN VFY-OF-U !
   0 VFY-BATCH-N ! ;

: VERIFY-REF-BATCH-BEGIN ( str -- )
   VERIFY-BATCH-BEGIN
   true VFY-REF-BATCH ! ;

: VERIFY-BATCH-ADD ( str str [ [ str str -- ] -- ] -- )
   VFY-BATCH-ADD ;

: VERIFY-BATCH-RUN ( ms -- )
   VFY-DEADLINE !
   s" 0 0" VFY-IN+ s\" \n" VFY-IN+
   VFY-EXE!
   [: VFY-BATCH-CAPTURE ;] catch {: rc:n :}
   rc 0<> rc E-PROC-STDERR-CAPACITY <> and if rc throw then
   PROCESS:CAPTURE-ENDING@ {: outu:len erru:len o :}
   outu LEN>N VFY-BATCH-OUT-U !
   erru LEN>N VFY-LOG-U !
   o VFY-BATCH-OUTCOME !
   0 VFY-BATCH-POS !
   false VFY-BATCH-BROKEN !
   rc 0<> if rc throw then ;

: VERIFY-BATCH-N ( -- n )  VFY-BATCH-N @ ;
: VERIFY-BATCH-RESULT ( n -- verdict )  VFY-BATCH-RESULT ;
\ An abnormal child makes every frame subject to independent retry.
: VERIFY-BATCH-CLEAN? ( -- bool )
   VFY-BATCH-OUTCOME @ VFY-CLEAN-EXIT? ;
: VERIFY-BATCH-COMPLETE? ( -- bool )  VFY-BATCH-COMPLETE @ ;

private

: VFY-PREPASS-RUN ( str str ms bool -- result<n,PROCESS:ending> )
   {: src:str path:str deadline json:bool :}
   json VFY-JSON !
   path VFY-PATH!
   true VFY-PREPASS !
   src STR:DATA CHK-BYTES-A !
   src STR:LEN CHK-BYTES-U !
   [: drop ;] VFY-IN!
   deadline VFY-DEADLINE !
   VFY-RUN VFY-PREVERDICT ;

public

\ check.f's pre-pass in the verifier child, which resolves words as the run does,
\ its rejections packets when JSON is true and prose records when false: ok 0,
\ ok the code it stopped with, or err how a child with no answer ended.
: PREVERIFY-BYTES ( str str str ms bool -- result<n,PROCESS:ending> )
   {: src:str path:str label:str deadline json:bool :}
   VFY-RESET
   label STR:DATA VFY-LABEL-A !
   label STR:LEN VFY-LABEL-U !
   src path deadline json VFY-PREPASS-RUN ;

\ PREVERIFY-BYTES of entry text, which no label names.
: PREVERIFY-ENTRY ( str str ms bool -- result<n,PROCESS:ending> )
   {: src:str path:str deadline json:bool :}
   VFY-RESET
   true VFY-ENTRY !
   src path deadline json VFY-PREPASS-RUN ;

\ What the last VERIFY-BYTES or PREVERIFY-BYTES failed with: 70 refused or a
\ source missing, 74 one unreadable, 90 a full dictionary, 1 a stop, else 0.
: VERIFY-STATUS ( -- n )
   VFY-ANSWER @ VFY-STOPPED = if EXIT-STATUS:FAILURE exit then
   VFY-ANSWER @ VFY-UNREAD = if EXIT-STATUS:IOERR exit then
   VFY-ANSWER @ VFY-FULL = if EXIT-STATUS:FULL exit then
   VFY-ANSWER @ VFY-REFUSED =  VFY-ANSWER @ VFY-MISSING =  or if EXIT-STATUS:REJECTED exit then
   0 ;

\ The file a loader of the last verification named that is not there or could
\ not be read, as it was resolved, else empty. The next verification replaces
\ these bytes; a consumer keeps its own copy.
: VERIFY-UNREAD$ ( -- str )
   VFY-ANSWER @ VFY-MISSING =  VFY-ANSWER @ VFY-UNREAD =  or
   if VFY-TARGET$ else NULL$ then ;

;using
;package
