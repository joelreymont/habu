\ chain-plan.f - decide whether a revision needs a new engine.
\
\ The decision belongs in checked code.  A caller supplies the newline-separated
\ paths changed by one revision; this file answers `engine` when a changed path
\ can reach the native product and `focused` otherwise.  Unknown source inputs
\ fail closed: `src/` and the bootstrap prefix are engine inputs, and the
\ transitive closure of tools/native-build.f covers the remaining libraries.
\ Tests and documentation are therefore allowed to reuse the previous engine.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/source.f
require lib/process-command.f
require tools/event-closure-lib.f

package CHAIN-PLAN
using SOURCE-ROOT

0 constant MODE-FOCUSED
1 constant MODE-ENGINE

private

variable CP-READY
create CP-ENTRY FS-PATH-CAP allot
variable CP-ENTRY-U
create CP-CANON FS-PATH-CAP allot
variable CP-CANON-U
create CP-JJ-PATH FS-PATH-CAP allot
variable CP-JJ-U
65536 constant CP-FILE-CAP
create CP-FILE CP-FILE-CAP allot

: CP-TRUE ( -- bool ) 0 0= ;
: CP-FALSE ( -- bool ) CP-TRUE 0= ;

: CP-ENSURE ( -- )
   CP-READY @ if exit then
   CWD$ s" tools/native-build.f" JOIN {: a:ptr u:n :}
   a CP-ENTRY u BYTE-COPY u CP-ENTRY-U !
   CP-ENTRY CP-ENTRY-U @ EC:BUILD
   CP-TRUE CP-READY ! ;

: CP-PATH! ( ptr u8 n -- )
   CWD$ 2swap JOIN CANONICAL drop {: a:ptr u:n :}
   a CP-CANON u BYTE-COPY u CP-CANON-U ! ;

: CP-CLOSURE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   CP-ENSURE
   a u CP-PATH!
   0 begin dup EC:COUNT < while
      dup EC:PATH$ CP-CANON CP-CANON-U @ STR= if drop CP-TRUE exit then
      1+
   repeat drop CP-FALSE ;

: CP-PREFIX? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" src/" STARTS-WITH? if CP-TRUE exit then
   a u s" bootstrap/" STARTS-WITH? if CP-TRUE exit then
   CP-FALSE ;

public

: ENGINE? ( ptr u8 n -- bool )
   2dup CP-PREFIX? if 2drop CP-TRUE exit then
   2dup s" tools/chain-plan.f" STR= if 2drop CP-FALSE exit then
   2dup CP-CLOSURE? if 2drop CP-TRUE exit then
   2drop CP-FALSE ;

: PLAN ( ptr u8 n -- n )
   ENGINE? if MODE-ENGINE else MODE-FOCUSED then ;

private

: CP-LINES-ENGINE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup u < while
      {: start:n :}
      a u 10 start SPLIT-NEXT {: line:ptr lineu:n next:n more:bool :}
      lineu 0 > if line lineu ENGINE? if CP-TRUE exit then then
      next dup u >= if drop CP-FALSE exit then
   repeat drop CP-FALSE ;

public

: TEXT-PLAN ( ptr u8 n -- n )
   CP-LINES-ENGINE? if MODE-ENGINE else MODE-FOCUSED then ;

: FILE-PLAN ( ptr u8 n -- n )
   {: path:ptr pathu:n :}
   path pathu FILE? 0= if E-FS-OPEN throw then
   path pathu CP-FILE CP-FILE-CAP READ-ALL {: u:n :}
   CP-FILE u TEXT-PLAN ;

: REV-PLAN ( ptr u8 n ptr u8 n -- n )
   {: rev:ptr revu:n root:ptr rootu:n :}
   PROC-CMD:RESET
   s" --ignore-working-copy" >LEN PROC-CMD:ARG+
   s" -R" >LEN PROC-CMD:ARG+
   root rootu >LEN PROC-CMD:ARG+
   s" diff" >LEN PROC-CMD:ARG+
   s" -r" >LEN PROC-CMD:ARG+
   rev revu >LEN PROC-CMD:ARG+
   s" --name-only" >LEN PROC-CMD:ARG+
   root rootu >LEN PROC-CMD:CWD!
   s" jj" >LEN CP-JJ-PATH RESOLVE-EXECUTABLE CP-JJ-U !
   CP-JJ-PATH CP-JJ-U @ 30000 >MS PROC-CMD:RUN-RC MATCH result
      ok OF drop ENDOF
      err OF drop s" chain-plan: jj stderr: " type
         PROC-CMD:ERR$ type cr E-BUILD-STATUS throw ENDOF
   ;MATCH
   PROC-CMD:OUT$ TEXT-PLAN ;

;package
