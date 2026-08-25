\ event-closure-lib.f - ordered transitive source-composition closure list.
\
\ Builds the deduplicated, ordered list of files reachable from an entry file
\ through the shared ordered event log (src/core/include.f): it replays the
\ restricted discovery pass (tools/source-discovery.f) breadth-first, following
\ every `require`/`included` loader event to the files it loads, so a consumer
\ can key or pre-scan the whole closure instead of only the top-level entry.
\
\ Only files that actually load contribute: `included` (replays every
\ occurrence) and a fresh (first-seen) `required` are followed; a `required`
\ already known through an earlier `provided`/`require` is not re-loaded, and a
\ bare `provided` registers a path without loading it, so neither adds content.
\ Distinct entries are deduplicated by exact path string. Discovery itself
\ rejects fail-closed (shadowed/undefined loader word, dynamic path, unsupported
\ opener); this file propagates that so a broken closure cannot be keyed.
\
\ This file only produces the ordered list (BUILD / COUNT / PATH$). Content
\ hashing and package-scope replay live in the consumers.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, lib/source.f,
\ and tools/source-discovery.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/source.f
require tools/source-discovery.f

package EC

$400 constant EC-MAX
$40000 constant EC-POOL-CAP

create EC-POOL EC-POOL-CAP allot
create EC-OFF EC-MAX cells allot
create EC-LEN EC-MAX cells allot
create EC-STATE EC-MAX cells allot
create EC-DIR EC-MAX cells allot
create EC-ORDER EC-MAX cells allot
variable EC-N
variable EC-POOL-N
variable EC-HEAD
variable EC-I
variable EC-DIR-N
variable EC-ORD-N

: EC-TRUE ( -- bool )   0 0= ;
: EC-FALSE ( -- bool )  EC-TRUE 0= ;

: EC-OFF@ ( i -- n )   cells EC-OFF + @ ;
: EC-LEN@ ( i -- n )   cells EC-LEN + @ ;

: EC-PATH$ ( i -- ptr u8 n ) {: i:n :}
   i EC-OFF@ EC-POOL + i EC-LEN@ ;

: EC-SEEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup EC-N @ < while
      dup EC-PATH$ a u STR= if drop EC-TRUE exit then
      1+
   repeat drop EC-FALSE ;

: EC-ROOM ( n -- ) {: u:n :}
   EC-N @ EC-MAX >= if E-DISC-CAPACITY throw then
   EC-POOL-N @ u + EC-POOL-CAP > if E-DISC-CAPACITY throw then ;

: EC-ADD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u EC-SEEN? if exit then
   u EC-ROOM
   EC-POOL-N @ {: off:n :}
   a off EC-POOL + u BYTE-COPY
   off EC-N @ cells EC-OFF + !
   u   EC-N @ cells EC-LEN + !
   off u + EC-POOL-N !
   EC-N @ 1+ EC-N ! ;

: EC-LOADS? ( i -- bool ) {: i:n :}
   i EVENT-KIND@ {: k:n :}
   k EV-INCLUDED = if EC-TRUE exit then
   k EV-REQUIRED = i EVENT-STATE@ EV-STATE-FRESH = and if EC-TRUE exit then
   EC-FALSE ;

: EC-ENQUEUE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FILE? 0= if exit then
   a u EC-ADD ;

: EC-SCAN-EVENTS ( -- )
   0 EC-I !
   begin EC-I @ EVENT-COUNT < while
      EC-I @ EC-LOADS? if EC-I @ EVENT-PATH@ EC-ENQUEUE then
      EC-I @ 1+ EC-I !
   repeat ;

\ Load-faithful (depth-first, post-order) closure. BUILD produces the same set
\ breadth-first, which is right for content keying but not for replaying package
\ open/public/private/;package scope that stays UNCLOSED across nested deps:
\ that residual must be threaded file-by-file in true load order. This DFS mirrors
\ tools/check-core.f CHK-EXPAND-ID - each file's own require/include closure is
\ fully expanded (post-order pushed) before the file itself, so a dep's scope
\ tokens replay exactly where its content would load. Only loading events
\ (EC-LOADS?) contribute, so the ordered list is a permutation of BUILD's set.

: EC-CLEAR ( -- )
   0 EC-N !  0 EC-POOL-N !  0 EC-HEAD ! ;

: EC-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup EC-N @ < while
      dup EC-PATH$ a u STR= if exit then
      1+
   repeat drop -1 ;

: EC-STATE@ ( n -- n )   cells EC-STATE + @ ;
: EC-STATE! ( n n -- ) {: v:n id:n :}   v id cells EC-STATE + ! ;

: EC-INTERN ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u EC-FIND dup 0 >= if exit then
   drop
   EC-N @ {: id:n :}
   a u EC-ADD
   0 id EC-STATE!
   id ;

: EC-DIR-PUSH ( n -- ) {: id:n :}
   EC-DIR-N @ EC-MAX >= if E-DISC-CAPACITY throw then
   id EC-DIR EC-DIR-N @ cells + !
   EC-DIR-N @ 1+ EC-DIR-N ! ;

: EC-ORDER-PUSH ( n -- ) {: id:n :}
   EC-ORD-N @ EC-MAX >= if E-DISC-CAPACITY throw then
   id EC-ORDER EC-ORD-N @ cells + !
   EC-ORD-N @ 1+ EC-ORD-N ! ;

: EC-DIR-QUEUE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FILE? 0= if exit then
   a u EC-INTERN EC-DIR-PUSH ;

: EC-EVENT-DIR+ ( n -- ) {: ix:n :}
   ix EC-LOADS? 0= if exit then
   ix EVENT-PATH@ EC-DIR-QUEUE ;

: EC-COLLECT-DEPS ( -- )
   0 begin dup EVENT-COUNT < while
      dup EC-EVENT-DIR+
      1+
   repeat drop ;

: EC-EXPAND ( n -- ) {: id:n :}
   id EC-STATE@ 0= 0= if exit then
   1 id EC-STATE!
   EC-DIR-N @ {: mark:n :}
   id EC-PATH$ DISCOVER:RUN
   EC-COLLECT-DEPS
   mark begin dup EC-DIR-N @ < while
      dup cells EC-DIR + @ RECURSE
      1+
   repeat drop
   mark EC-DIR-N !
   id EC-ORDER-PUSH
   2 id EC-STATE! ;

: EC-ORD-RESET ( -- )
   EC-CLEAR
   0 EC-DIR-N !
   0 EC-ORD-N ! ;

public

: RESET ( -- )
   EC-CLEAR ;

: COUNT ( -- n )   EC-N @ ;

: PATH$ ( i -- ptr u8 n )   EC-PATH$ ;

: BUILD ( ptr u8 n -- ) {: a:ptr u:n :}
   RESET
   a u EC-ADD
   begin EC-HEAD @ EC-N @ < while
      EC-HEAD @ EC-PATH$ DISCOVER:RUN
      EC-SCAN-EVENTS
      EC-HEAD @ 1+ EC-HEAD !
   repeat ;

\ Depth-first, post-order closure: each file is preceded by its own transitive
\ require/include closure, so the last entry is always the entry file itself.
\ Consumers that replay residual package scope walk indices 0..ORDER-COUNT-2
\ (the deps in load order) and scan the entry file separately.
: LOAD-ORDER ( ptr u8 n -- ) {: a:ptr u:n :}
   EC-ORD-RESET
   a u EC-INTERN EC-EXPAND ;

: ORDER-COUNT ( -- n )   EC-ORD-N @ ;

: ORDER-PATH$ ( i -- ptr u8 n )   cells EC-ORDER + @ EC-PATH$ ;

;package
