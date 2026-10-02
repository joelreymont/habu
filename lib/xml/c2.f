\ Scoped UTF-8 XML pull reader. The lexical source is a borrowed C2 view.
require lib/xml.f
require lib/c2-memory.f

package XML-C2
public

\ XML/state.f's 26-cell header. The two wire pointers are null for INIT's
\ caller-owned UTF-8 source; INIT-SOURCE's converted storage is excluded here.
STRUCTURE reader-state 2
   FIELD source read-view<a,b,u8>
   FIELD position n
   FIELD capacity n
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
   FIELD storage-len n
   FIELD source-mode n
   FIELD wire-original ptr u8
   FIELD wire-original-len n
   FIELD wire-storage ptr u8
   FIELD wire-storage-len n
;STRUCTURE

private

\ The load-time marker is retired after it seals the representation leaves.
TRUSTED: HIDE ( n -- ) int-mark ;
ndict@ 1- constant HIDE-ID

\ The parser header constructor and destructor are not caller capabilities.
s" XML--C2-READER--STATE:MAKE" XREF-FIND-INDEX HIDE
s" XML--C2-READER--STATE:UNMAKE" XREF-FIND-INDEX HIDE

TRUSTED: SOURCE-UNPACK ( read-view<p,q,u8> -- ptr u8 n ) ;
ndict@ 1- constant SOURCE-UNPACK-ID

TRUSTED: BYTE-UNPACK ( mut-view<b,l,a,u8> -- ptr n n ) ;
ndict@ 1- constant BYTE-UNPACK-ID

\ XML:INIT validates extent, aliasing, BOM and capacity, then writes exactly
\ XML/state.f's 26-cell header. WITH-INIT copies that header into the same
\ storage before the caller sees a typed cursor.
\ The native allocator reads the fixed header in two bounded groups.
TRUSTED: HEADER-LOW ( ptr n -- n n n n n n n n n n n n n ) {: state:ptr :}
   state 0 cells + @
   state 1 cells + @
   state 2 cells + @
   state 3 cells + @
   state 4 cells + @
   state 5 cells + @
   state 6 cells + @
   state 7 cells + @
   state 8 cells + @
   state 9 cells + @
   state 10 cells + @
   state 11 cells + @
   state 12 cells + @ ;
ndict@ 1- constant HEADER-LOW-ID

TRUSTED: HEADER-HIGH ( ptr n -- n n n n n n n n n n n n n ) {: state:ptr :}
   state 13 cells + @
   state 14 cells + @
   state 15 cells + @
   state 16 cells + @
   state 17 cells + @
   state 18 cells + @
   state 19 cells + @
   state 20 cells + @
   state 21 cells + @
   state 22 cells + @
   state 23 cells + @
   state 24 cells + @
   state 25 cells + @ ;
ndict@ 1- constant HEADER-HIGH-ID

TRUSTED: INIT-HEADER ( mut-view<b,l,a,u8> read-view<p,q,u8> -- mut-view<b,l,a,u8> reader-state<p,q> )
   SOURCE-UNPACK {: source:ptr size:n :}
   BYTE-UNPACK {: state:ptr cap:n :}
   state cap source size XML:INIT XML:CLOSE
   state cap
   state HEADER-LOW
   state HEADER-HIGH ;
ndict@ 1- constant INIT-HEADER-ID
SOURCE-UNPACK-ID HIDE
BYTE-UNPACK-ID HIDE

\ Each unpack changes one logical initialized cursor into its two ABI cells.
\ Its only clients below are private adapters to the established XML parser.
TRUSTED: CURSOR-UNPACK ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- ptr u8 n ) ;
ndict@ 1- constant CURSOR-ID

TRUSTED: DEST-UNPACK ( mut-view<p,q,a,u8> -- ptr u8 n ) ;
ndict@ 1- constant DEST-ID

public

: WITH-READER ( R mut-view<b,l,a,u8> read-view<p,q,u8> forall<i inside [l,reader-state<p,q>], [ R mut-view<b,i,a,init<i,reader-state<p,q>>> -- S mut-view<b,i,a,init<i,reader-state<p,q>>> | U -- U ]> | U -- S mut-view<b,l,a,u8> | U )
   {: cb :} INIT-HEADER cb C2-MEM:WITH-INIT ;

private
INIT-HEADER-ID HIDE
HEADER-LOW-ID HIDE
HEADER-HIGH-ID HIDE

TRUSTED: C-NEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> XML:kind )
   CURSOR-UNPACK 2dup drop XML:NEXT swap XML:CLOSE ;
ndict@ 1- constant NEXT-ID
public
: NEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> XML:kind )
   C-NEXT ;
private
NEXT-ID HIDE

TRUSTED: C-KIND ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> XML:kind )
   CURSOR-UNPACK 2dup drop XML:KIND swap XML:CLOSE ;
ndict@ 1- constant KIND-ID
public
: KIND ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> XML:kind )
   C-KIND ;
private
KIND-ID HIDE

TRUSTED: C-NAME$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop XML:NAME$ rot XML:CLOSE ;
ndict@ 1- constant NAME-ID
public
: NAME$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-NAME$ ;
private
NAME-ID HIDE

TRUSTED: C-LOCAL$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop XML:LOCAL$ rot XML:CLOSE ;
ndict@ 1- constant LOCAL-ID
public
: LOCAL$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-LOCAL$ ;
private
LOCAL-ID HIDE

TRUSTED: C-RAW$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop XML:RAW$ rot XML:CLOSE ;
ndict@ 1- constant RAW-ID
public
: RAW$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-RAW$ ;
private
RAW-ID HIDE

TRUSTED: C-RAW ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> off len )
   CURSOR-UNPACK 2dup drop XML:RAW rot XML:CLOSE ;
ndict@ 1- constant RAW-SPAN-ID
public
: RAW ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> off len )
   C-RAW ;
private
RAW-SPAN-ID HIDE

TRUSTED: C-CONTENT$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop {: state:ptr :}
   state XML:CONTENT {: raw:ptr offset:n count:n :}
   raw XML:CLOSE
   state @ offset + count ;
ndict@ 1- constant CONTENT-ID
public
: CONTENT$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-CONTENT$ ;
private
CONTENT-ID HIDE

TRUSTED: C-CONTENT ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> off len )
   CURSOR-UNPACK 2dup drop XML:CONTENT rot XML:CLOSE ;
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
   CURSOR-UNPACK 2dup drop XML:DEPTH swap XML:CLOSE ;
ndict@ 1- constant DEPTH-ID
public
: DEPTH ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> n )
   C-DEPTH ;
private
DEPTH-ID HIDE

TRUSTED: C-ATTR-RESET ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> )
   CURSOR-UNPACK 2dup drop XML:ATTR-RESET XML:CLOSE ;
ndict@ 1- constant ATTR-RESET-ID
public
: ATTR-RESET ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> )
   C-ATTR-RESET ;
private
ATTR-RESET-ID HIDE

TRUSTED: C-ATTR-NEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> bool )
   CURSOR-UNPACK 2dup drop XML:ATTR-NEXT swap XML:CLOSE ;
ndict@ 1- constant ATTR-NEXT-ID
public
: ATTR-NEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> bool )
   C-ATTR-NEXT ;
private
ATTR-NEXT-ID HIDE

TRUSTED: C-ATTR-NAME$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop XML:ATTR-NAME$ rot XML:CLOSE ;
ndict@ 1- constant ATTR-NAME-ID
public
: ATTR-NAME$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-ATTR-NAME$ ;
private
ATTR-NAME-ID HIDE

TRUSTED: C-ATTR-LOCAL$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop XML:ATTR-LOCAL$ rot XML:CLOSE ;
ndict@ 1- constant ATTR-LOCAL-ID
public
: ATTR-LOCAL$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-ATTR-LOCAL$ ;
private
ATTR-LOCAL-ID HIDE

TRUSTED: C-ATTR-VALUE$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop XML:ATTR-VALUE$ rot XML:CLOSE ;
ndict@ 1- constant ATTR-VALUE-ID
public
: ATTR-VALUE$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-ATTR-VALUE$ ;
private
ATTR-VALUE-ID HIDE

TRUSTED: C-ATTR-RAW$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   CURSOR-UNPACK 2dup drop {: state:ptr :}
   state XML:ATTR-RAW {: raw:ptr offset:n count:n :}
   raw XML:CLOSE
   state @ offset + count ;
ndict@ 1- constant ATTR-RAW-ID
public
: ATTR-RAW$ ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> read-view<p,q,u8> )
   C-ATTR-RAW$ ;
private
ATTR-RAW-ID HIDE

TRUSTED: C-ATTR-RAW ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> off len )
   CURSOR-UNPACK 2dup drop XML:ATTR-RAW rot XML:CLOSE ;
ndict@ 1- constant ATTR-RAW-SPAN-ID
public
: ATTR-RAW ( mut-view<b,i,a,init<i,reader-state<p,q>>> -- mut-view<b,i,a,init<i,reader-state<p,q>>> off len )
   C-ATTR-RAW ;
private
ATTR-RAW-SPAN-ID HIDE

TRUSTED: C-TEXT ( mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> -- mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> n )
   DEST-UNPACK {: destination:ptr cap:n :}
   CURSOR-UNPACK {: state:ptr bound:n :}
   state destination cap XML:TEXT swap XML:CLOSE {: count:n :}
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
   state destination cap XML:URI swap XML:CLOSE {: count:n :}
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
   state destination cap XML:ATTR-TEXT swap XML:CLOSE {: count:n :}
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
   state destination cap XML:ATTR-URI swap XML:CLOSE {: count:n :}
   state bound destination cap count ;
ndict@ 1- constant ATTR-URI-ID
public
: ATTR-URI ( mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> -- mut-view<b,i,a,init<i,reader-state<p,q>>> mut-view<x,y,z,u8> n )
   C-ATTR-URI ;
private
ATTR-URI-ID HIDE

\ The last two representation leaves cannot be retired until all adapters above
\ are compiled. They now have no public or interpret path.
CURSOR-ID HIDE
DEST-ID HIDE
HIDE-ID HIDE

;package
