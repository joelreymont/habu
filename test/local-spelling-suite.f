\ local-spelling-suite.f - a local answers to its DECLARED SPELLING, and a word
\ written in its own case is that word.
\
\     bin/hb --load test/local-spelling-suite.f
\
\ WHAT IS UNDER TEST. A reference binds a live local only when its spelling
\ matches the declaration byte for byte. Word lookup stays case-insensitive, so a
\ body that declares `text` and writes `TEXT` names the word TEXT, while `text`
\ still names the local. Two places decide this and they must agree: the checker
\ (src/core/checker.f - LOCNB holds the raw declaration, LOC-REF? compares it to
\ the raw token) and the engine's own compiled lookup (src/habu/habu2.f
\ EMIT-LOC-FIND, which the frame carve and every reference go through). A file
\ that loads at all has already proved they agree: a checker that read the word
\ where the engine read the local would refuse these bodies on their arithmetic.
\
\ WHY IT HAS A SUITE. Three bodies in this tree lost a word to a local of the
\ same letters, and each read as a stack error somewhere else:
\
\   create TEXT ... {: text size:n :} ... TEXT i + c!   \ wrote the local's source
\   : KEY ( ptr u8 n -- ) ... {: key:ptr keyu:n :} key keyu KEY   \ called the local
\   variable REV  ... {: rev:n :} REV @                 \ fetched through an int
\
\ The json-write one (dot habu-warn-when-a-8c4d889a) was fixed by renaming the
\ local to `kp`; the other two by renaming as well. All three are written here in
\ their original spelling and must now mean what they say.
\
\ WHAT WOULD BREAK EACH SECTION. Section 1 answers the word's result, so a
\ resolver that folded case answers the local's and reds. Section 2 keeps the 883
\ same-spelling shadows of the engine build path honest: a resolver that compared
\ raw bytes but stored a folded name, or that dropped local-first, loses `count`
\ and the loop-index `i` that the ij-locals landing (41b13bdd) put in both
\ compilers. Section 3 asks the question of the TOKENS: the local's spelling
\ inside a comment, inside a string literal and inside a longer name must not
\ move anything. Section 4 is the refusal - a differently-cased mention with no
\ word of that name is undefined, by name, not a silent local. Section 5 runs
\ section 1's body through the OTHER compiler: tier 1 elaborates from the same
\ front-end resolution, and a tier that resolved names for itself would answer
\ tier 0's old answer here.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/test.f
require lib/test/subject.f
require src/compiler/native/compiler.f

\ ---- section 1: the three incidents, compiled by tier 0 (the cold JIT) -------
package LSP-JIT

create TEXT 64 allot           \ the buffer the incident lost
variable REV                   \ the variable the incident fetched through

create OUT 64 allot
variable OUT-N

: PUTC ( n -- ) {: c:n :}
   c OUT OUT-N @ + c!
   OUT-N @ 1 + OUT-N ! ;

\ The json-write emitter, reduced: it writes the key bytes and the colon.
: KEY ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do a i + c@ PUTC loop
   58 PUTC ;

public

\ lib/json-write.f FIELD-S, with the local the incident had to rename to `kp`:
\ `key keyu KEY` is the local, the local, and then the WORD.
: FIELD-S ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu:n v:ptr vu:n :}
   0 OUT-N !
   key keyu KEY
   vu 0 ?do v i + c@ PUTC loop ;

: OUT$ ( -- ptr u8 n )
   OUT OUT-N @ ;

\ The buffer incident: `text` is the source span, `TEXT` the buffer it fills.
: PUT ( ptr u8 n -- ) {: text:ptr size:n :}
   size 0 ?do  text i + c@  TEXT i + c!  loop ;

: TEXT$ ( n -- ptr u8 n ) {: u:n :}
   TEXT u ;

\ The variable incident: `rev` is the addend, `REV` the variable.
: REV! ( n -- )
   REV ! ;

: BUMP ( n -- n ) {: rev:n :}
   REV @ rev + ;

;package

\ ---- section 2: same-spelling shadowing is untouched -------------------------
package LSP-SHADOW
public

\ `count` is an engine prim ( ptr u8 -- ptr u8 n ). Referenced in its declared
\ spelling it is the local, and the body is arithmetic on one number.
: SCALE ( n -- n ) {: count:n :}
   count 3 * ;

\ The ij-locals case (dot sharpening, 2026-08-13): a local named `i` declared
\ before a counted loop answers three turns of the LOCAL, not the loop index.
: TURNS ( n -- n ) {: i:n :}
   0 3 0 ?do i + loop ;

\ The same body without the declaration answers the loop index: 0 + 1 + 2.
: INDEX ( -- n )
   0 3 0 ?do i + loop ;

;package

\ ---- section 3: it is the TOKEN that resolves, not the text ------------------
package LSP-TOKENS
public

\ A longer name that contains the local's letters.
: TEXTURE ( n -- n )
   5 * ;

\ Neither the comment below nor the string body holds a token, and TEXTURE is one
\ name, not TEXT and URE: only the bare `text` here is the local.
: SPELLED ( n -- n ) {: text:n :}
   s" TEXT is a word and text is a local" 2drop   \ TEXT, text ( TEXT text )
   text TEXTURE ;

;package

\ ---- section 4: a differently-cased mention with no word of that name --------
\ The engine writes this one to fd 2 and dies, which an in-process case cannot
\ host, so it runs in a child: the exit status AND the message a user sees.
package LSP-CHILD
public

$400 constant CAP
10000 constant CHILD-MS
70 constant UNDEF-RC

create OUT CAP allot
create ERR CAP allot
variable ERR-U

: RUN ( ptr u8 n -- n )   \ source -> child exit status (-1 = signal or timeout)
   OUT CAP >LEN ERR CAP >LEN CHILD-MS >MS SUBJECT:RUN
   MATCH outcome
     exited OF ENDOF
     signaled OF drop -1 ENDOF
     timeout OF -1 ENDOF
   ;MATCH
   {: rc:n :}
   LEN>N ERR-U !
   LEN>N drop
   rc ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

;package

\ ---- section 5: the same incident through the other compiler -----------------
1 set-tier

package LSP-NCOMP

create TEXT 64 allot

public

: PUT ( ptr u8 n -- ) {: text:ptr size:n :}
   size 0 ?do  text i + c@  TEXT i + c!  loop ;

: TEXT$ ( n -- ptr u8 n ) {: u:n :}
   TEXT u ;

;package

0 set-tier

\ ---- the cases ---------------------------------------------------------------
package LSP-TEST
private

: INCIDENT-CASE ( -- )
   s" a buffer named TEXT is the buffer under a local named text" T-LABEL
   s" hello" LSP-JIT:PUT
   5 LSP-JIT:TEXT$ s" hello" T$=

   s" a package word KEY is the emitter under a local named key" T-LABEL
   s" id" s" 7" LSP-JIT:FIELD-S
   LSP-JIT:OUT$ s" id:7" T$=

   s" a variable REV is the variable under a local named rev" T-LABEL
   5 LSP-JIT:REV!
   3 LSP-JIT:BUMP 8 T= ;

: SHADOW-CASE ( -- )
   s" a local spelled like a word still shadows it" T-LABEL
   4 LSP-SHADOW:SCALE 12 T=
   s" a local named i shadows the loop index" T-LABEL
   5 LSP-SHADOW:TURNS 15 T=
   LSP-SHADOW:INDEX 3 T= ;

: TOKEN-CASE ( -- )
   s" the local's spelling in a comment, a string or a longer name moves nothing" T-LABEL
   3 LSP-TOKENS:SPELLED 15 T= ;

: UNDEFINED-CASE ( -- )
   s" a differently-cased mention with no word of that name is undefined by name" T-LABEL
   s" : LSP-BAD ( n -- n ) {: zork:n :} ZORK ;" LSP-CHILD:RUN LSP-CHILD:UNDEF-RC T=
   LSP-CHILD:ERR$ s" E-UNDEFINED: ZORK" CONTAINS? TTRUE ;

: NCOMP-CASE ( -- )
   s" the tier-1 compiler reads the same two names the same way" T-LABEL
   s" world" LSP-NCOMP:PUT
   5 LSP-NCOMP:TEXT$ s" world" T$= ;

public

: RUN ( -- )
   T-RESET
   INCIDENT-CASE
   SHADOW-CASE
   TOKEN-CASE
   UNDEFINED-CASE
   NCOMP-CASE
   T-REPORT ;

;package

LSP-TEST:RUN
