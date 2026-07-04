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
variable EC-N
variable EC-POOL-N
variable EC-HEAD
variable EC-I

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

public

: RESET ( -- )
   0 EC-N !  0 EC-POOL-N !  0 EC-HEAD ! ;

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

end-package
