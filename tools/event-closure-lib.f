\ event-closure-lib.f - ordered transitive source-composition closure list.
\
\ Builds the deduplicated, ordered list of files reachable from an entry file
\ through the shared ordered event log (src/core/include.f). Discovery gathers
\ the accepted loading edges in actual load order; BUILD publishes their
\ breadth-first closure for content keys, while LOAD-ORDER publishes postorder
\ for scope replay.
\
\ Only files that actually load contribute: `included` (replays every
\ occurrence) and a fresh (first-seen) `required` are followed; a `required`
\ already known through an earlier `provided`/`require` is not re-loaded, and a
\ bare `provided` registers a path without loading it, so neither adds content.
\ Distinct entries are deduplicated by canonical absolute pathname. Discovery
\ itself rejects fail-closed (a top-level dynamic path or unsupported opener,
\ or a loader or family opener made ambiguous before use); this file
\ propagates that so a broken closure cannot be keyed. A loading event whose
\ path is not a file joins the list like any other, and the walk refuses it
\ where it reads the file (DISCOVER:RUN-IN names it on fd 2 and rethrows
\ E-FS-STAT), as BUILD-WITH's reader refuses it: a closure that left it out
\ would key fewer files than the build reads.
\
\ This file only produces the ordered list (BUILD / COUNT / PATH$). Content
\ hashing and package-scope replay live in the consumers.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/source.f
require tools/source-discovery.f

package EC
using SOURCE-ROOT

\ One closure's entries and its path pool - each entry stores its own path and
\ the root it resolved under, each at most PATH-CAP bytes, so the pool holds
\ EC-MAX of both and a walk is refused for its entry count alone. The largest
\ walk any consumer keys is tools/native-build.f, the engine's own prefix, well
\ under EC-MAX.
$400 constant EC-MAX
EC-MAX 2 * PATH-CAP * constant EC-POOL-CAP

create EC-POOL EC-POOL-CAP allot
PATH-CAP 1+ 2 * constant EC-ENTRY-CAP
create EC-ENTRY EC-ENTRY-CAP allot
create EC-OFF EC-MAX cells allot
create EC-LEN EC-MAX cells allot
create EC-ROOT-OFF EC-MAX cells allot
create EC-ROOT-LEN EC-MAX cells allot
create EC-STATE EC-MAX cells allot
create EC-REG EC-MAX cells allot
create EC-ACTIVE EC-MAX cells allot
create EC-DIR EC-MAX cells allot
create EC-DIR-OFF EC-MAX cells allot
create EC-DIR-KIND EC-MAX cells allot
create EC-DIR-SPEC EC-MAX cells allot
create EC-ORDER EC-MAX cells allot
create EC-OUT EC-MAX cells allot
create EC-QUEUE EC-MAX cells allot
create EC-QUEUED EC-MAX cells allot
DYNAMIC-BUFFER EC-EDGE-FROM n
DYNAMIC-BUFFER EC-EDGE-TO n
variable EC-N
variable EC-POOL-N
variable EC-HEAD
variable EC-I
variable EC-DIR-N
variable EC-ORD-N
variable EC-OUT-N
variable EC-QUEUE-N
variable EC-EDGE-N
variable EC-WITH
variable EC-HARVEST-N
variable EC-REG-N
TYPED-VARIABLE EC-HARVEST-RUN [ -- ]
TYPED-VARIABLE EC-HARVEST-READER [ ptr u8 n ptr u8 n -- ptr u8 n ]
TYPED-VARIABLE EC-CONTEXT-RUN [ n -- ]

: EC-TRUE ( -- bool )   0 0= ;
: EC-FALSE ( -- bool )  EC-TRUE 0= ;

: EC-OFF@ ( n -- n )   cells EC-OFF + @ ;
: EC-LEN@ ( n -- n )   cells EC-LEN + @ ;

: EC-PATH$ ( n -- ptr u8 n ) {: i:n :}
   i EC-OFF@ EC-POOL + i EC-LEN@ ;

: EC-ROOT$ ( n -- ptr u8 n ) {: idx:n :}
   idx cells EC-ROOT-OFF + @ EC-POOL +
   idx cells EC-ROOT-LEN + @ ;

: EC-SEEN-WITH? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n root:ptr rootu:n :}
   0 begin dup EC-N @ < while
      dup EC-PATH$ a u STR= if
         dup EC-ROOT$ root rootu STR= if drop EC-TRUE exit then
      then
      1+
   repeat drop EC-FALSE ;

: EC-ROOM ( n -- ) {: u:n :}
   EC-N @ EC-MAX >= if E-DISC-CAPACITY throw then
   EC-POOL-N @ u + EC-POOL-CAP > if E-DISC-CAPACITY throw then ;

: EC-ADD-ROW ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n root:ptr rootu:n :}
   u rootu + EC-ROOM
   EC-POOL-N @ {: off:n :}
   a off EC-POOL + u BYTE-COPY
   off EC-N @ cells EC-OFF + !
   u   EC-N @ cells EC-LEN + !
   root off u + EC-POOL + rootu BYTE-COPY
   off u + EC-N @ cells EC-ROOT-OFF + !
   rootu EC-N @ cells EC-ROOT-LEN + !
   off u + rootu + EC-POOL-N !
   EC-N @ 1+ EC-N ! ;

: EC-ADD-WITH ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n root:ptr rootu:n :}
   a u root rootu EC-SEEN-WITH? if exit then
   a u root rootu EC-ADD-ROW ;

: EC-SPEC-ONLY? ( n -- bool ) {: ix:n :}
   ix EVENT-PATH@ {: a:ptr u:n :}
   false
   ix 0 ?do
      i EVENT-KIND@ EV-INCLUDED <> if
         i EVENT-PATH@ a u STR= if
            i DISCOVER:EVENT-SPEC@ 0= if drop false unloop exit then
            drop true
         then
      then
   loop
   ;

: EC-LOADS? ( n -- bool ) {: i:n :}
   i EVENT-KIND@ {: k:n :}
   k EV-INCLUDED = if EC-TRUE exit then
   k EV-REQUIRED = if
      i EVENT-STATE@ EV-STATE-FRESH = if EC-TRUE exit then
      i EC-SPEC-ONLY? if EC-TRUE exit then
   then
   EC-FALSE ;

: EC-SCAN-EVENTS ( -- )
   0 EC-I !
   begin EC-I @ EVENT-COUNT < while
      EC-I @ EC-LOADS? if
         EC-I @ EVENT-PATH@ EC-I @ SOURCE-EVENT:ROOT@ EC-ADD-WITH
      then
      EC-I @ 1+ EC-I !
   repeat ;

\ LOAD-ORDER is depth-first postorder. BUILD traverses the same accepted edges
\ breadth-first; both expose the same files without rescanning source under a
\ fresh, contradictory loader registry.

: EC-CLEAR ( -- )
   0 EC-N !  0 EC-POOL-N !  0 EC-HEAD ! ;

: EC-FIND-WITH ( ptr u8 n ptr u8 n -- n )
   {: a:ptr u:n root:ptr rootu:n :}
   0 begin dup EC-N @ < while
      dup EC-PATH$ a u STR= if
         dup EC-ROOT$ root rootu STR= if exit then
      then
      1+
   repeat drop -1 ;

: EC-STATE@ ( n -- n )   cells EC-STATE + @ ;
: EC-STATE! ( n n -- ) {: v:n id:n :}   v id cells EC-STATE + ! ;

: EC-INTERN ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n root:ptr rootu:n :}
   a u root rootu EC-FIND-WITH
   dup 0 >= if exit then
   drop
   EC-N @ {: id:n :}
   a u root rootu EC-ADD-WITH
   0 id EC-STATE!
   0 id cells EC-ACTIVE + !
   0 id cells EC-QUEUED + !
   id ;

: EC-DIR-PUSH ( n n n bool -- ) {: id:n off:n kind:n spec:bool :}
   EC-DIR-N @ EC-MAX >= if E-DISC-CAPACITY throw then
   id EC-DIR EC-DIR-N @ cells + !
   off EC-DIR-OFF EC-DIR-N @ cells + !
   kind EC-DIR-KIND EC-DIR-N @ cells + !
   spec EC-DIR-SPEC EC-DIR-N @ cells + !
   EC-DIR-N @ 1+ EC-DIR-N ! ;

: EC-ORDER-FIND ( n -- n ) {: id:n :}
   id EC-PATH$ {: a:ptr u:n :}
   0 begin dup EC-ORD-N @ < while
      dup cells EC-ORDER + @ EC-PATH$ a u STR= if exit then
      1+
   repeat drop -1 ;

: EC-ORDER-PUSH ( n -- ) {: id:n :}
   id EC-ORDER-FIND dup 0 >= if
      begin dup EC-ORD-N @ 1- < while
         dup 1+ cells EC-ORDER + @ over cells EC-ORDER + !
         1+
      repeat drop
      EC-ORD-N @ 1- EC-ORD-N !
   else drop then
   EC-ORD-N @ EC-MAX >= if E-DISC-CAPACITY throw then
   id EC-ORDER EC-ORD-N @ cells + !
   EC-ORD-N @ 1+ EC-ORD-N ! ;

: EC-CONTEXT-COLLECT ( -- )
   EVENT-COUNT 0 ?do
      i EVENT-PATH@ i SOURCE-EVENT:ROOT@ EC-INTERN
      i EVENT-TOK@ drop i EVENT-KIND@ i DISCOVER:EVENT-SPEC@ EC-DIR-PUSH
   loop ;

: EC-EDGE+ ( n n -- ) {: from:n to:n :}
   EC-EDGE-N @ 1+ dup EC-EDGE-FROM-RESERVE EC-EDGE-TO-RESERVE
   from EC-EDGE-N @ EC-EDGE-FROM !
   to EC-EDGE-N @ EC-EDGE-TO !
   EC-EDGE-N @ 1+ EC-EDGE-N ! ;

\ A speculative loader owns the registrations made by its recursive load.
\ Restoring the registry at its return keeps an uncalled body's provided from
\ suppressing a later load, including when the body sits inside another body.
: EC-PATH-KNOWN? ( n -- bool ) {: id:n :}
   id EC-PATH$ {: a:ptr u:n :}
   EC-REG-N @ 0 ?do
      i cells EC-REG + @ EC-PATH$ a u STR= if true unloop exit then
   loop
   false ;

: EC-KNOW ( n -- ) {: id:n :}
   id EC-PATH-KNOWN? if exit then
   EC-REG-N @ EC-MAX >= if E-DISC-CAPACITY throw then
   id EC-REG-N @ cells EC-REG + !
   1 EC-REG-N +! ;

: EC-ORD-RESET ( -- )
   EC-CLEAR
   0 EC-DIR-N !
   0 EC-ORD-N !
   0 EC-OUT-N !
   0 EC-QUEUE-N !
   0 EC-EDGE-N !
   0 EC-REG-N ! ;

\ Harvest the same closure traversal before the final walk. A dependency can
\ define or retire a family opener that the entry later calls, and rows learned
\ in one pass can change which operands the next pass reads as source.
: EC-HARVEST-ACT ( -- )
   EC-HEAD @ EC-PATH$ EC-HEAD @ EC-ROOT$ DISCOVER:RUN-IN ;

: EC-HARVEST-WITH-ACT ( -- )
   EC-HEAD @ EC-PATH$ EC-HEAD @ EC-ROOT$
   EC-HARVEST-READER @ execute {: bytes:ptr size:n :}
   EC-HEAD @ EC-PATH$ EC-HEAD @ EC-ROOT$ bytes size DISCOVER:RUN-BYTES ;

: EC-HARVEST-PASS ( ptr u8 n -- ) {: a:ptr u:n :}
   EC-CLEAR
   a u ENTRY-RESOLVE drop RESOLVED-ROOT$ EC-ADD-WITH
   DISCOVER:ROWS-PASS-BEGIN
   begin EC-HEAD @ EC-N @ < while
      EC-HARVEST-RUN @ DISCOVER:HARVEST
      EC-SCAN-EVENTS
      EC-HEAD @ 1+ EC-HEAD !
   repeat
   DISCOVER:ROWS-PASS-END ;

: EC-HARVEST-LOOP ( ptr u8 n -- ) {: a:ptr u:n :}
   DISCOVER:ROWS-RESET
   -1 EC-HARVEST-N !
   begin
      DISCOVER:ROWS-VERSION {: ver:n :}
      a u EC-HARVEST-PASS
      EC-N @ EC-HARVEST-N @ =
      DISCOVER:ROWS-VERSION ver = and
      DISCOVER:ROWS-PASS-STABLE? and
      EC-N @ EC-HARVEST-N !
   until ;

: EC-HARVEST ( ptr u8 n -- )
   [: EC-HARVEST-ACT ;] EC-HARVEST-RUN !
   EC-HARVEST-LOOP ;

: EC-HARVEST-WITH ( ptr u8 n [ ptr u8 n ptr u8 n -- ptr u8 n ] -- )
   {: a:ptr u:n reader :}
   reader EC-HARVEST-READER !
   [: EC-HARVEST-WITH-ACT ;] EC-HARVEST-RUN !
   a u EC-HARVEST-LOOP ;

\ Replay harvested rows at their actual load sites. A later sibling must not
\ shadow an earlier family, while a prior sibling can change the opener before
\ this file reaches it. The file scan itself records the ordered load sites.
: EC-CONTEXT-ACT ( n -- ) {: id:n :}
   id EC-PATH$ id EC-ROOT$ DISCOVER:RUN-IN ;

: EC-CONTEXT-WITH-ACT ( n -- ) {: id:n :}
   id EC-PATH$ id EC-ROOT$ EC-HARVEST-READER @ execute
   {: bytes:ptr size:n :}
   id EC-PATH$ id EC-ROOT$ bytes size DISCOVER:RUN-BYTES ;

: EC-CONTEXT-EXPAND ( n bool bool -- )
   {: id:n speculative:bool required:bool :}
   id cells EC-ACTIVE + @ 0<> required 0= and if E-DISC-CAPACITY throw then
   EC-REG-N @ {: reg-mark:n :}
   required if id EC-KNOW then
   id EC-STATE@ 0= {: first:bool :}
   1 id cells EC-ACTIVE + +!
   EC-DIR-N @ {: mark:n :}
   id EC-CONTEXT-RUN @ execute
   EC-CONTEXT-COLLECT
   EC-DIR-N @ {: end:n :}
   mark begin dup end < while
      dup mark = if 0 else dup 1- cells EC-DIR-OFF + @ 1+ then
      over cells EC-DIR-OFF + @
      id EC-PATH$ DISCOVER:CONTEXT-BETWEEN
      dup cells EC-DIR-SPEC + @ {: child-spec:bool :}
      dup cells EC-DIR-KIND + @ EV-PROVIDED = if
         child-spec 0= if dup cells EC-DIR + @ EC-KNOW then
      else
         dup cells EC-DIR-KIND + @ EV-INCLUDED = if
            id over cells EC-DIR + @ EC-EDGE+
            dup cells EC-DIR + @ child-spec false RECURSE
         else
            dup cells EC-DIR + @ {: child:n :}
            child EC-PATH-KNOWN? 0= if
               id child EC-EDGE+
               child child-spec true RECURSE
            then
         then
      then
      1+
   repeat
   mark end = if 0 else end 1- cells EC-DIR-OFF + @ 1+ then
   $7fffffffffffffff id EC-PATH$ DISCOVER:CONTEXT-BETWEEN
   drop
   mark EC-DIR-N !
   first if id EC-ORDER-PUSH then
   1 id EC-STATE!
   -1 id cells EC-ACTIVE + +!
   speculative if reg-mark EC-REG-N ! then ;

: EC-CONTEXT-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   EC-ORD-RESET
   DISCOVER:CONTEXT-BEGIN
   a u ENTRY-RESOLVE drop RESOLVED-ROOT$ EC-INTERN
   false true EC-CONTEXT-EXPAND ;

: EC-OUT-HAS-PATH? ( n -- bool ) {: id:n :}
   id EC-PATH$ {: a:ptr u:n :}
   EC-OUT-N @ 0 ?do
      i cells EC-OUT + @ EC-PATH$ a u STR= if true unloop exit then
   loop
   false ;

: EC-QUEUE+ ( n -- ) {: id:n :}
   id cells EC-QUEUED + @ 0<> if exit then
   EC-QUEUE-N @ EC-MAX >= if E-DISC-CAPACITY throw then
   id EC-QUEUE EC-QUEUE-N @ cells + !
   EC-QUEUE-N @ 1+ EC-QUEUE-N !
   1 id cells EC-QUEUED + !
   EC-WITH @ 0= id EC-OUT-HAS-PATH? and if exit then
   EC-OUT-N @ EC-MAX >= if E-DISC-CAPACITY throw then
   id EC-OUT EC-OUT-N @ cells + !
   EC-OUT-N @ 1+ EC-OUT-N ! ;

: EC-PUBLISH ( -- )
   0 EC-HEAD !
   0 EC-QUEUE+
   begin EC-HEAD @ EC-QUEUE-N @ < while
      EC-HEAD @ cells EC-QUEUE + @ {: from:n :}
      EC-EDGE-N @ 0 ?do
         i EC-EDGE-FROM @ from = if i EC-EDGE-TO @ EC-QUEUE+ then
      loop
      EC-HEAD @ 1+ EC-HEAD !
   repeat ;

public

: RESET ( -- )
   EC-ORD-RESET ;

: COUNT ( -- n )   EC-OUT-N @ ;

: PATH$ ( n -- ptr u8 n )   cells EC-OUT + @ EC-PATH$ ;

\ Identity within the root that resolved this member; PATH$ still names the
\ actual file to read, including a dependency found through a fallback root.
: NAME$ ( n -- ptr u8 n ) {: i:n :}
   i cells EC-OUT + @ {: id:n :}
   id EC-PATH$ id EC-ROOT$ RELATIVE ;

\ ENTRY-RESOLVE and later dependency scans reuse the engine's JOIN buffer.
\ Keep a caller-owned entry there before either of the two closure passes.
: EC-STABLE-ENTRY ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 0 <= u EC-ENTRY-CAP >= or if E-PATH-RANGE throw then
   a EC-ENTRY u BYTE-COPY
   EC-ENTRY u ;

: BUILD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u EC-STABLE-ENTRY {: entry:ptr entryu:n :}
   entry entryu EC-HARVEST
   [: EC-CONTEXT-ACT ;] EC-CONTEXT-RUN !
   0 EC-WITH !
   entry entryu EC-CONTEXT-CHECK
   EC-PUBLISH
   DISCOVER:ROWS-RESET ;

\ A source owner reads every loading event from its own byte table. A missing
\ dependency is a refusal at that read, never an omitted closure member.
: BUILD-WITH ( ptr u8 n [ ptr u8 n ptr u8 n -- ptr u8 n ] -- )
   {: a:ptr u:n reader :}
   a u EC-STABLE-ENTRY {: entry:ptr entryu:n :}
   entry entryu reader EC-HARVEST-WITH
   [: EC-CONTEXT-WITH-ACT ;] EC-CONTEXT-RUN !
   1 EC-WITH !
   entry entryu EC-CONTEXT-CHECK
   EC-PUBLISH
   DISCOVER:ROWS-RESET ;

\ Depth-first, post-order closure: each file is preceded by its own transitive
\ require/include closure, so the last entry is always the entry file itself.
\ Consumers that replay residual package scope walk indices 0..ORDER-COUNT-2
\ (the deps in load order) and scan the entry file separately.
: LOAD-ORDER ( ptr u8 n -- ) BUILD ;

: ORDER-COUNT ( -- n )   EC-ORD-N @ ;

: ORDER-PATH$ ( n -- ptr u8 n )   cells EC-ORDER + @ EC-PATH$ ;

;using
;package
