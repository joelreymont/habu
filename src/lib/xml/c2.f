\ c2.f - scoped UTF-8 XML pull reader (package XML-C2) over a borrowed C2 view.
\
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

\ The parser header constructor and destructor are not caller capabilities.
s" XML--C2-READER--STATE:MAKE" XREF-FIND-INDEX HIDE
s" XML--C2-READER--STATE:UNMAKE" XREF-FIND-INDEX HIDE

\ WITH-INIT copies a record that owns nothing into the cursor storage: the
\ source view, then zero cells in bounded groups. OPEN-READER parses it there.
TRUSTED: ZEROS ( -- n n n n n n n n n n n n )
   0 0 0 0 0 0 0 0 0 0 0 0 ;
ndict@ 1- constant ZEROS-ID

TRUSTED: INIT-HEADER ( mut-view<b,l,a,u8> read-view<p,q,u8> -- mut-view<b,l,a,u8> reader-state<p,q> )
   ZEROS ZEROS ZEROS 0 ;
ndict@ 1- constant INIT-HEADER-ID
ZEROS-ID HIDE

\ Each unpack changes one logical initialized cursor into its two ABI cells.
\ Its only clients below are private adapters to the established XML parser.
TRUSTED: CURSOR-UNPACK ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- ptr u8 n ) ;
ndict@ 1- constant CURSOR-ID

TRUSTED: DEST-UNPACK ( mut-view<p,q,a,u8> -- ptr u8 n ) ;
ndict@ 1- constant DEST-ID

\ A live reader's token is its record's address. Each accessor re-enters the
\ reader from the cursor storage and parks it there again; only WITH-READER's
\ release ends it.
TRUSTED: REENTER ( ptr u8 -- XML:reader ) ;
ndict@ 1- constant REENTER-ID

TRUSTED: PARK ( XML:reader -- ) drop ;
ndict@ 1- constant PARK-ID

TRUSTED: OPEN-READER ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> )
   CURSOR-UNPACK over {: record:ptr :}
   record record @ record cell+ @ XML:INIT PARK ;
ndict@ 1- constant OPEN-ID

TRUSTED: LINK ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> )
   CURSOR-UNPACK over {: record:ptr :}
   READER-HEAD @ record LINK-OFF + !
   record READER-HEAD ! ;
ndict@ 1- constant LINK-ID

\ Unlink the innermost record, then release what its reader owns.
TRUSTED: RELEASE ( -- )
   READER-HEAD @ {: record:ptr :}
   record LINK-OFF + @ READER-HEAD !
   record XML:DISPOSE ;
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

private
INIT-HEADER-ID HIDE
OPEN-ID HIDE
LINK-ID HIDE
RELEASE-ID HIDE

TRUSTED: C-NEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> XML:kind )
   CURSOR-UNPACK 2dup drop REENTER XML:NEXT swap PARK ;
ndict@ 1- constant NEXT-ID
public
: NEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> XML:kind )
   C-NEXT ;
private
NEXT-ID HIDE

TRUSTED: C-KIND ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> XML:kind )
   CURSOR-UNPACK 2dup drop REENTER XML:KIND swap PARK ;
ndict@ 1- constant KIND-ID
public
: KIND ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> XML:kind )
   C-KIND ;
private
KIND-ID HIDE

TRUSTED: C-NAME$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop REENTER XML:NAME$ rot PARK ;
ndict@ 1- constant NAME-ID
public
: NAME$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-NAME$ ;
private
NAME-ID HIDE

TRUSTED: C-LOCAL$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop REENTER XML:LOCAL$ rot PARK ;
ndict@ 1- constant LOCAL-ID
public
: LOCAL$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-LOCAL$ ;
private
LOCAL-ID HIDE

TRUSTED: C-RAW$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop REENTER XML:RAW$ rot PARK ;
ndict@ 1- constant RAW-ID
public
: RAW$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-RAW$ ;
private
RAW-ID HIDE

TRUSTED: C-RAW ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> off len )
   CURSOR-UNPACK 2dup drop REENTER XML:RAW rot PARK ;
ndict@ 1- constant RAW-SPAN-ID
public
: RAW ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> off len )
   C-RAW ;
private
RAW-SPAN-ID HIDE

TRUSTED: C-CONTENT$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop {: state:ptr :}
   state REENTER XML:CONTENT {: raw:ptr offset:n count:n :}
   raw PARK
   state @ offset + count ;
ndict@ 1- constant CONTENT-ID
public
: CONTENT$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-CONTENT$ ;
private
CONTENT-ID HIDE

TRUSTED: C-CONTENT ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> off len )
   CURSOR-UNPACK 2dup drop REENTER XML:CONTENT rot PARK ;
ndict@ 1- constant CONTENT-SPAN-ID
public
: CONTENT ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> off len )
   C-CONTENT ;
private
CONTENT-SPAN-ID HIDE

TRUSTED: C-SOURCE$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop dup @ swap cell+ @ ;
ndict@ 1- constant SOURCE-ID
public
: SOURCE$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-SOURCE$ ;
private
SOURCE-ID HIDE

TRUSTED: C-DEPTH ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> n )
   CURSOR-UNPACK 2dup drop REENTER XML:DEPTH swap PARK ;
ndict@ 1- constant DEPTH-ID
public
: DEPTH ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> n )
   C-DEPTH ;
private
DEPTH-ID HIDE

TRUSTED: C-ATTR-RESET ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> )
   CURSOR-UNPACK 2dup drop REENTER XML:ATTR-RESET PARK ;
ndict@ 1- constant ATTR-RESET-ID
public
: ATTR-RESET ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> )
   C-ATTR-RESET ;
private
ATTR-RESET-ID HIDE

TRUSTED: C-ATTR-NEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> bool )
   CURSOR-UNPACK 2dup drop REENTER XML:ATTR-NEXT swap PARK ;
ndict@ 1- constant ATTR-NEXT-ID
public
: ATTR-NEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> bool )
   C-ATTR-NEXT ;
private
ATTR-NEXT-ID HIDE

TRUSTED: C-ATTR-NAME$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop REENTER XML:ATTR-NAME$ rot PARK ;
ndict@ 1- constant ATTR-NAME-ID
public
: ATTR-NAME$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-ATTR-NAME$ ;
private
ATTR-NAME-ID HIDE

TRUSTED: C-ATTR-LOCAL$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop REENTER XML:ATTR-LOCAL$ rot PARK ;
ndict@ 1- constant ATTR-LOCAL-ID
public
: ATTR-LOCAL$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-ATTR-LOCAL$ ;
private
ATTR-LOCAL-ID HIDE

TRUSTED: C-ATTR-VALUE$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop REENTER XML:ATTR-VALUE$ rot PARK ;
ndict@ 1- constant ATTR-VALUE-ID
public
: ATTR-VALUE$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-ATTR-VALUE$ ;
private
ATTR-VALUE-ID HIDE

TRUSTED: C-ATTR-RAW$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop {: state:ptr :}
   state REENTER XML:ATTR-RAW {: raw:ptr offset:n count:n :}
   raw PARK
   state @ offset + count ;
ndict@ 1- constant ATTR-RAW-ID
public
: ATTR-RAW$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-ATTR-RAW$ ;
private
ATTR-RAW-ID HIDE

TRUSTED: C-ATTR-RAW ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> off len )
   CURSOR-UNPACK 2dup drop REENTER XML:ATTR-RAW rot PARK ;
ndict@ 1- constant ATTR-RAW-SPAN-ID
public
: ATTR-RAW ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> off len )
   C-ATTR-RAW ;
private
ATTR-RAW-SPAN-ID HIDE

TRUSTED: C-TEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> -- mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> n )
   DEST-UNPACK {: destination:ptr cap:n :}
   CURSOR-UNPACK {: state:ptr bound:n :}
   state REENTER destination cap XML:TEXT swap PARK {: count:n :}
   state bound destination cap count ;
ndict@ 1- constant TEXT-ID
public
: TEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> -- mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> n )
   C-TEXT ;
private
TEXT-ID HIDE

TRUSTED: C-URI ( mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> -- mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> n )
   DEST-UNPACK {: destination:ptr cap:n :}
   CURSOR-UNPACK {: state:ptr bound:n :}
   state REENTER destination cap XML:URI swap PARK {: count:n :}
   state bound destination cap count ;
ndict@ 1- constant URI-ID
public
: URI ( mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> -- mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> n )
   C-URI ;
private
URI-ID HIDE

TRUSTED: C-ATTR-TEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> -- mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> n )
   DEST-UNPACK {: destination:ptr cap:n :}
   CURSOR-UNPACK {: state:ptr bound:n :}
   state REENTER destination cap XML:ATTR-TEXT swap PARK {: count:n :}
   state bound destination cap count ;
ndict@ 1- constant ATTR-TEXT-ID
public
: ATTR-TEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> -- mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> n )
   C-ATTR-TEXT ;
private
ATTR-TEXT-ID HIDE

TRUSTED: C-ATTR-URI ( mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> -- mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> n )
   DEST-UNPACK {: destination:ptr cap:n :}
   CURSOR-UNPACK {: state:ptr bound:n :}
   state REENTER destination cap XML:ATTR-URI swap PARK {: count:n :}
   state bound destination cap count ;
ndict@ 1- constant ATTR-URI-ID
public
: ATTR-URI ( mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> -- mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> n )
   C-ATTR-URI ;
private
ATTR-URI-ID HIDE

\ The representation leaves and reader crossings cannot be retired until all
\ adapters above are compiled. They now have no public or interpret path.
CURSOR-ID HIDE
DEST-ID HIDE
REENTER-ID HIDE
PARK-ID HIDE
HIDE-ID HIDE

;package
