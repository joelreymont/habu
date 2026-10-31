\ c2.f - scoped UTF-8 XML pull reader (package XML-C2) over a borrowed C2 view.
\ STORAGE CLASS. CALLER-OWNED: the view and the cursor storage are the caller's;
\ the frame, attribute and namespace tables the reader grows are released when
\ WITH-READER's callback returns or throws. TASK-LOCAL: the chain of the task's
\ live reader records is one TASK:+USER cell.
require src/lib/xml.f
require src/lib/c2-memory.f
require src/lib/task.f

package XML-C2
public

\ An XML:cursor record (src/lib/xml/state.f), field for field, then the link to
\ the record of the enclosing WITH-READER in this task. The cursor storage is
\ XML:CURSOR-BYTES plus one cell.
STRUCTURE reader-state<a,b>
   FIELD source read-view<a,b,u8>
   FIELD position n
   FIELD frame-cap n
   FIELD open-depth n
   FIELD ns-count n
   FIELD attr-count n
   FIELD attr-index n
   FIELD raw-off n
   FIELD raw-len n
   FIELD token-kind n
   FIELD name-off n
   FIELD name-len n
   FIELD value-off n
   FIELD value-len n
   FIELD uri-off n
   FIELD uri-len n
   FIELD pending n
   FIELD root-count n
   FIELD failed n
   FIELD attr-cap n
   FIELD source-mode n
   FIELD wire-original ptr u8
   FIELD wire-original-len n
   FIELD wire-storage ptr u8
   FIELD wire-storage-len n
   FIELD ns-cap n
   FIELD frames ptr n
   FIELD attrs ptr n
   FIELD namespaces ptr n
   FIELD hash-k0 n
   FIELD hash-k1 n
   FIELD sip0 n
   FIELD sip1 n
   FIELD sip2 n
   FIELD sip3 n
   FIELD sip4 n
   FIELD sip5 n
   FIELD link ptr u8
;STRUCTURE

private

\ The cells of reader-state above; the cursor part must be XML:cursor's.
39 constant READER-CELLS
XML:CURSOR-BYTES constant LINK-OFF
: LAYOUT-CHECK ( -- )
   READER-CELLS 1- cells LINK-OFF <> if s" xml-c2: reader layout" XML:E-STATE die then ;
LAYOUT-CHECK

\ The task's innermost live reader record, or null. A reader links its record
\ only once WITH-INIT has opened it, so inner readers unwind first, also under
\ a throw, and the release always finds its own record at the head.
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and CELL TASK:+USER READER-HEAD drop

\ The load-time marker is retired after it seals the representation leaves.
TRUSTED: HIDE ( n -- ) int-mark ;
ndict@ 1- constant HIDE-ID

\ WITH-INIT copies a record that owns nothing into the cursor storage;
\ OPEN-READER parses it there.
: INIT-HEADER ( mut-view<b,l,a,u8> read-view<p,q,u8> -- mut-view<b,l,a,u8> reader-state<p,q> )
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   NULL-PTR 0 NULL-PTR 0 0 NULL-PTR NULL-PTR NULL-PTR
   0 0 0 0 0 0 0 0 NULL-PTR XML--C2-READER--STATE:MAKE ;
ndict@ 1- constant INIT-HEADER-ID

\ The parser header constructor and destructor are not caller capabilities.
s" XML--C2-READER--STATE:MAKE" XREF-FIND-INDEX HIDE
s" XML--C2-READER--STATE:UNMAKE" XREF-FIND-INDEX HIDE

\ Each unpack changes one logical view into its two ABI cells. CURSOR-BASE
\ and DEST-SPAN below are their only callers.
TRUSTED: CURSOR-UNPACK ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- ptr u8 n ) ;
ndict@ 1- constant CURSOR-ID

TRUSTED: DEST-UNPACK ( mut-view<p,q,a,u8> -- ptr u8 n ) ;
ndict@ 1- constant DEST-ID

\ The cursor view stays live beside its record address; an unpack consumes it.
TRUSTED: CURSOR-BASE ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> ptr u8 )
   CURSOR-UNPACK 2dup drop ;
ndict@ 1- constant CURSOR-BASE-ID

\ The destination view stays live beside its raw cells; an unpack consumes it.
TRUSTED: DEST-SPAN ( mut-view<x,y,z,u8> -- mut-view<x,y,z,u8> ptr u8 n )
   DEST-UNPACK 2dup ;
ndict@ 1- constant DEST-SPAN-ID

\ A slice of the source keeps its lifetime: an XML:INIT reader spans only its
\ source (XML's SPAN$), proved by "a projected name lives after each cursor".
TRUSTED: SOURCE-SLICE ( mut-view<b,i,a,init<i,reader-state<p,q>>> ptr u8 n -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> ) ;
ndict@ 1- constant SOURCE-SLICE-ID

CAST: >HEAD-CELL ( ptr n -- ptr ptr u8 )
ndict@ 1- constant HEAD-CELL-ID
CAST: >ADDR-CELL ( ptr u8 -- ptr ptr u8 )
ndict@ 1- constant ADDR-CELL-ID
CAST: >LEN-CELL ( ptr u8 -- ptr n )
ndict@ 1- constant LEN-CELL-ID

\ A live reader's token is its record's address. Each accessor re-enters the
\ reader from the cursor storage and parks it there again; only WITH-READER's
\ release ends it.
TRUSTED: REENTER ( ptr u8 -- XML:reader ) ;
ndict@ 1- constant REENTER-ID

TRUSTED: PARK ( XML:reader -- ) drop ;
ndict@ 1- constant PARK-ID

: OPEN-READER ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> )
   CURSOR-BASE {: record:ptr :}
   record >ADDR-CELL @ record cell+ >LEN-CELL @ {: source size:n :}
   record XML:CURSOR-BYTES XML:EMPTY-CURSOR source size XML:INIT PARK ;
ndict@ 1- constant OPEN-ID

: LINK ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> )
   CURSOR-BASE {: record:ptr :}
   READER-HEAD >HEAD-CELL @ record LINK-OFF + >ADDR-CELL !
   record READER-HEAD >HEAD-CELL ! ;
ndict@ 1- constant LINK-ID

: RELEASE ( -- )
   READER-HEAD >HEAD-CELL @ {: record:ptr :}
   record LINK-OFF + >ADDR-CELL @ READER-HEAD >HEAD-CELL !
   record REENTER XML:CLOSE ;
ndict@ 1- constant RELEASE-ID

\ GUARD links the record before the guarded body parses it, so a failed INIT,
\ a throw from the callback and a normal return all release its tables, with
\ the primary error preserved. Parsing starts only after WITH-INIT accepted
\ the storage, so a refused scope maps nothing.
: READ-BODY ( R [ R mut-view<b,i,a,init<i,reader-state<p,q>>> -- S mut-view<b,i,a,init<i,reader-state<p,q>>> | U -- U ] mut-view<b,i,a,init<i,reader-state<p,q>>> | U -- S mut-view<b,i,a,init<i,reader-state<p,q>>> | U )
   OPEN-READER swap execute ;

: GUARD ( R [ R mut-view<b,i,a,init<i,reader-state<p,q>>> -- S mut-view<b,i,a,init<i,reader-state<p,q>>> | U -- U ] mut-view<b,i,a,init<i,reader-state<p,q>>> | U -- S mut-view<b,i,a,init<i,reader-state<p,q>>> | U )
   LINK [: READ-BODY ;] [: RELEASE ;] finally ;

public

: WITH-READER ( R mut-view<b,l,a,u8> read-view<p,q,u8> forall<i inside [l,reader-state<p,q>], [ R mut-view<b,i,a,init<i,reader-state<p,q>>> -- S mut-view<b,i,a,init<i,reader-state<p,q>>> | U -- U ]> | U -- S mut-view<b,l,a,u8> | U )
   {: source cb :} cb swap source INIT-HEADER [: GUARD ;] C2-MEM:WITH-INIT ;

INIT-HEADER-ID HIDE
OPEN-ID HIDE
LINK-ID HIDE
RELEASE-ID HIDE

: NEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> XML:kind )
   CURSOR-BASE REENTER XML:NEXT swap PARK ;

: KIND ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> XML:kind )
   CURSOR-BASE REENTER XML:KIND swap PARK ;

: NAME$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-BASE REENTER XML:NAME$ rot PARK SOURCE-SLICE ;

: LOCAL$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-BASE REENTER XML:LOCAL$ rot PARK SOURCE-SLICE ;

: RAW$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-BASE REENTER XML:RAW$ rot PARK SOURCE-SLICE ;

: RAW ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> off len )
   CURSOR-BASE REENTER XML:RAW rot PARK ;

: CONTENT$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-BASE {: state:ptr :}
   state REENTER XML:CONTENT rot PARK {: offset:off count:len :}
   state >ADDR-CELL @ offset OFF>N + count LEN>N SOURCE-SLICE ;

: CONTENT ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> off len )
   CURSOR-BASE REENTER XML:CONTENT rot PARK ;

: SOURCE$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-BASE {: record:ptr :}
   record >ADDR-CELL @ record cell+ >LEN-CELL @ SOURCE-SLICE ;

: DEPTH ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> n )
   CURSOR-BASE REENTER XML:DEPTH swap PARK ;

: ATTR-RESET ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> )
   CURSOR-BASE REENTER XML:ATTR-RESET PARK ;

: ATTR-NEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> bool )
   CURSOR-BASE REENTER XML:ATTR-NEXT swap PARK ;

: ATTR-NAME$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-BASE REENTER XML:ATTR-NAME$ rot PARK SOURCE-SLICE ;

: ATTR-LOCAL$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-BASE REENTER XML:ATTR-LOCAL$ rot PARK SOURCE-SLICE ;

: ATTR-VALUE$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-BASE REENTER XML:ATTR-VALUE$ rot PARK SOURCE-SLICE ;

: ATTR-RAW$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-BASE {: state:ptr :}
   state REENTER XML:ATTR-RAW rot PARK {: offset:off count:len :}
   state >ADDR-CELL @ offset OFF>N + count LEN>N SOURCE-SLICE ;

: ATTR-RAW ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> off len )
   CURSOR-BASE REENTER XML:ATTR-RAW rot PARK ;

: TEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> -- mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> n )
   DEST-SPAN {: destination:ptr cap:n :} swap
   CURSOR-BASE REENTER destination cap XML:TEXT swap PARK {: count:n :}
   swap count ;

: URI ( mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> -- mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> n )
   DEST-SPAN {: destination:ptr cap:n :} swap
   CURSOR-BASE REENTER destination cap XML:URI swap PARK {: count:n :}
   swap count ;

: ATTR-TEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> -- mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> n )
   DEST-SPAN {: destination:ptr cap:n :} swap
   CURSOR-BASE REENTER destination cap XML:ATTR-TEXT swap PARK {: count:n :}
   swap count ;

: ATTR-URI ( mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> -- mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> n )
   DEST-SPAN {: destination:ptr cap:n :} swap
   CURSOR-BASE REENTER destination cap XML:ATTR-URI swap PARK {: count:n :}
   swap count ;

\ The representation leaves, field casts and reader crossings are retired only
\ after every accessor above is compiled; past here they have no public or
\ interpret path.
CURSOR-ID HIDE
DEST-ID HIDE
CURSOR-BASE-ID HIDE
DEST-SPAN-ID HIDE
SOURCE-SLICE-ID HIDE
HEAD-CELL-ID HIDE
ADDR-CELL-ID HIDE
LEN-CELL-ID HIDE
REENTER-ID HIDE
PARK-ID HIDE
HIDE-ID HIDE

;package
