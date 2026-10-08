\ lsp-workspace.f - the files of the workspace whose loads reach a file.
\
\ The subjects are the `.f` files under the folders ROOT+ adds, as WALK-FILES
\ finds them (lib/fs.f, which passes over .jj, .jj-ws, .git and .dots), and
\ the documents open in the server. REACHING takes a file F and a document D,
\ each by its canonical path, and the open documents as a word that hands the
\ word it is given each open document's canonical path and text, and gives
\ every subject whose closure holds F, and D as a subject whatever its closure
\ holds: F first when it is a subject, then the others in the byte order of
\ their paths.
\
\ A closure is every file a check of its subject could read. A load resolves
\ against the root that resolved the file holding it (docs/forth-card.md
\ section 7), so a file is followed once under each root a load reaches it
\ under: a node is a path and a root, and a subject's node is rooted at the
\ directory holding it, as the checker roots a document it checks
\ (tools/check-verify-core.f CHK-EXPAND-BYTES). A node's loads are the events
\ discovery (tools/source-discovery.f) records in the text of the first open
\ document at its path, which every check reads in place of the file, else in
\ its file on disk, when it is there. A file outside the workspace is followed
\ like any other and never given. A load of a file that is not there is followed
\ no further.
\
\ A file whose loads cannot be found, one discovery refuses (an E-DISC-*
\ code) or one that is there and will not be read (an E-FS-* code), loads
\ nothing and the walk goes on: UNREADABLE and UNREADABLE$ name it and why. A
\ root whose walk fails (an E-FS-* code) refuses the request, which then gives
\ no subject: REFUSED? answers true and BLOCKED$ names the root. Any other
\ throw goes on.
\
\ Nothing but the roots is kept from one request to the next.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the roots and the graph of the request being
\ answered belong to the server's one task.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/sort.f
require tools/source-discovery.f
require tools/lsp-text.f

package LSP-WORKSPACE

private

\ A node: its path's and its root's offsets and lengths in POOL, the hash of
\ both, 1 when it is a subject, 1 once it reaches F, and the code of the
\ throw that kept its loads from being found, else 0.
0 constant N-PATH
1 constant N-PATH-U
2 constant N-ROOT
3 constant N-ROOT-U
4 constant N-HASH
5 constant N-SUBJECT
6 constant N-REACH
7 constant N-FAULT
8 constant NODE-CELLS

\ A load: the node whose file loads, and the node it loads.
0 constant EDGE-FROM
1 constant EDGE-TO
2 constant EDGE-CELLS

\ An open document: its node, and its text's offset and length in POOL.
0 constant O-NODE
1 constant O-TEXT
2 constant O-TEXT-U
3 constant OPEN-CELLS

\ A root: its path's offset and length in ROOT-BYTES.
0 constant R-PATH
1 constant R-PATH-U
2 constant ROOT-CELLS

1024 constant FIRST-SLOTS                \ the node index's first capacity, a power of two

$CBF29CE484222325 constant FNV-BASIS     \ FNV-1a, 64 bits
$100000001B3 constant FNV-PRIME

DYNAMIC-BUFFER POOL u8                   \ the request's paths and open texts,
variable POOL-U
DYNAMIC-BUFFER NODES n                   \ its nodes,
variable NODE-N
DYNAMIC-BUFFER SLOTS n                   \ an index of them by hash, a node + 1 or 0,
variable SLOT-CAP
DYNAMIC-BUFFER EDGES n                   \ its loads,
variable EDGE-N
DYNAMIC-BUFFER OPENS n                   \ its open documents,
variable OPEN-N
DYNAMIC-BUFFER HITS n                    \ the subjects it gives, in order,
variable HIT-N
DYNAMIC-BUFFER BAD n                     \ and a node of each file it could not read
variable BAD-N
variable F-AT                            \ F's offset and length in POOL,
variable F-U
variable D-NODE                          \ D's node
TYPED-VARIABLE BLOCKED bool              \ whether a root refused it,
DYNAMIC-BUFFER BLOCK-B u8                \ and which
variable BLOCK-U
DYNAMIC-BUFFER ROOT-BYTES u8             \ the roots, kept between requests
variable ROOT-BYTES-U
DYNAMIC-BUFFER ROOTS n
variable ROOT-N
variable CUR                             \ the node being followed,
variable CUR-OPEN                        \ the open document whose text is read,
variable CUR-ROOT                        \ and the root being walked
\ POOL holds a byte past those it names, which POOL+ reserves with its own:
\ its accessor refuses an index at its capacity, and a string at the end of
\ what it names, as an empty one is, still needs the address of that index.

: NODE@ ( n n -- n )  swap NODE-CELLS * + NODES @ ;
: NODE! ( n n n -- )  swap NODE-CELLS * + NODES ! ;
: EDGE@ ( n n -- n )  swap EDGE-CELLS * + EDGES @ ;
: EDGE! ( n n n -- )  swap EDGE-CELLS * + EDGES ! ;
: OPEN@ ( n n -- n )  swap OPEN-CELLS * + OPENS @ ;
: OPEN! ( n n n -- )  swap OPEN-CELLS * + OPENS ! ;
: ROOT@ ( n n -- n )  swap ROOT-CELLS * + ROOTS @ ;
: ROOT! ( n n n -- )  swap ROOT-CELLS * + ROOTS ! ;

: POOL$ ( n n -- ptr u8 n )
   {: at:n u:n :}
   at POOL u ;

: PATH$ ( n -- ptr u8 n )  dup N-PATH NODE@ swap N-PATH-U NODE@ POOL$ ;
: DIR$ ( n -- ptr u8 n )  dup N-ROOT NODE@ swap N-ROOT-U NODE@ POOL$ ;
: F$ ( -- ptr u8 n )  F-AT @ F-U @ POOL$ ;

: ROOT-PATH$ ( n -- ptr u8 n )
   {: r:n :}
   r R-PATH ROOT@ ROOT-BYTES r R-PATH-U ROOT@ ;

\ The bytes appended to POOL: their offset there. They never lie in POOL,
\ which the append may move.
: POOL+ ( ptr u8 n -- n )
   {: a:ptr u:n :}
   POOL-U @ {: at:n :}
   at u + 1+ POOL-RESERVE
   a at POOL u BYTE-COPY
   u POOL-U +!
   at ;

\ Why a throw of this code kept a file's loads from being found, as
\ lib/errors.f names a discovery's code, and whether it is such a throw.
: FAULT$ ( n -- ptr u8 n bool )
   {: rc:n :}
   rc LSP-TEXT:FS-RC? if s" cannot be read" true exit then
   rc E-DISC-SHADOW = if s" E-DISC-SHADOW" true exit then
   rc E-DISC-DYNAMIC = if s" E-DISC-DYNAMIC" true exit then
   rc E-DISC-OPENER = if s" E-DISC-OPENER" true exit then
   rc E-DISC-UNTERM = if s" E-DISC-UNTERM" true exit then
   rc E-DISC-CAPACITY = if s" E-DISC-CAPACITY" true exit then
   rc E-DISC-RETIRE = if s" E-DISC-RETIRE" true exit then
   s" " false ;

\ The request refused, naming this root.
: REFUSE ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 1+ BLOCK-B-RESERVE
   a 0 BLOCK-B u BYTE-COPY
   u BLOCK-U !
   true BLOCKED ! ;

\ ---- the nodes, indexed by path and root ----------------------------------------

: MIX ( n ptr u8 n -- n )
   {: h:n a:ptr u:n :}
   h u 0 ?do a i + c@ xor FNV-PRIME * loop
   u xor FNV-PRIME * ;

: KEY ( ptr u8 n ptr u8 n -- n )
   {: a:ptr u:n r:ptr ru:n :}
   FNV-BASIS a u MIX r ru MIX ;

: SAME? ( n ptr u8 n ptr u8 n n -- bool )
   {: id:n a:ptr u:n r:ptr ru:n h:n :}
   id N-HASH NODE@ h <> if false exit then
   id PATH$ a u STR= id DIR$ r ru STR= and ;

\ The slot holding the node of this path, root and hash, else the empty slot
\ it would take.
: SLOT ( ptr u8 n ptr u8 n n -- n )
   {: a:ptr u:n r:ptr ru:n h:n :}
   SLOT-CAP @ 1- {: mask:n :}
   h mask and
   begin dup SLOTS @ dup 0<> if 1- a u r ru h SAME? 0= else drop false then while
      1+ mask and
   repeat ;

: FREE-SLOT ( n -- n )
   {: h:n :}
   SLOT-CAP @ 1- {: mask:n :}
   h mask and
   begin dup SLOTS @ 0<> while 1+ mask and repeat ;

\ The index at twice its capacity, or at its first, holding every node again.
: GROW ( -- )
   SLOT-CAP @ 2 * FIRST-SLOTS max SLOT-CAP !
   SLOT-CAP @ SLOTS-RESERVE
   SLOT-CAP @ 0 ?do 0 i SLOTS ! loop
   NODE-N @ 0 ?do i 1+ i N-HASH NODE@ FREE-SLOT SLOTS ! loop ;

\ The node of this path under this root, made when there is none. Neither
\ string lies in POOL.
: NODE-OF ( ptr u8 n ptr u8 n -- n )
   {: a:ptr u:n r:ptr ru:n :}
   NODE-N @ 1+ 2 * SLOT-CAP @ > if GROW then
   a u r ru KEY {: h:n :}
   a u r ru h SLOT {: s:n :}
   s SLOTS @ 0<> if s SLOTS @ 1- exit then
   NODE-N @ {: id:n :}
   id 1+ NODE-CELLS * NODES-RESERVE
   a u POOL+ id N-PATH NODE!
   u id N-PATH-U NODE!
   r ru POOL+ id N-ROOT NODE!
   ru id N-ROOT-U NODE!
   h id N-HASH NODE!
   0 id N-SUBJECT NODE!
   0 id N-REACH NODE!
   0 id N-FAULT NODE!
   id 1+ s SLOTS !
   id 1+ NODE-N !
   id ;

: EDGE+ ( n n -- )
   {: from:n to:n :}
   EDGE-N @ {: e:n :}
   e 1+ EDGE-CELLS * EDGES-RESERVE
   from e EDGE-FROM EDGE!
   to e EDGE-TO EDGE!
   e 1+ EDGE-N ! ;

\ ---- the subjects ---------------------------------------------------------------

\ The subject at this canonical path, rooted at its directory.
: SUBJECT ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u a u SOURCE-ROOT:DIRNAME NODE-OF {: id:n :}
   1 id N-SUBJECT NODE!
   id ;

\ An open document, by its canonical path and its text.
: OPEN+ ( ptr u8 n ptr u8 n -- )
   {: p:ptr pu:n t:ptr tu:n :}
   p pu SUBJECT {: id:n :}
   OPEN-N @ {: o:n :}
   o 1+ OPEN-CELLS * OPENS-RESERVE
   id o O-NODE OPEN!
   t tu POOL+ o O-TEXT OPEN!
   tu o O-TEXT-U OPEN!
   o 1+ OPEN-N ! ;

\ A file the walk finds, a subject when it is a `.f` file.
: FOUND ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u s" .f" ENDS-WITH? 0= if exit then
   a u SOURCE-ROOT:CANONICAL drop SUBJECT drop ;

: WALK ( -- )
   CUR-ROOT @ ROOT-PATH$ [: FOUND ;] WALK-FILES ;

\ The subjects under root R; a walk that fails refuses, naming R.
: WALK-ROOT ( n -- )
   {: r:n :}
   r CUR-ROOT !
   [: WALK ;] catch {: rc:n :}
   rc 0= if exit then
   rc LSP-TEXT:FS-RC? if r ROOT-PATH$ REFUSE exit then
   rc throw ;

: WALK-ROOTS ( -- )
   0 begin dup ROOT-N @ < BLOCKED @ 0= and while
      dup WALK-ROOT 1+
   repeat drop ;

\ ---- the loads ------------------------------------------------------------------

: DISK-TEXT ( -- )
   CUR @ PATH$ CUR @ DIR$ DISCOVER:READ-IN
   DISCOVER:RUN-READ ;

: OPEN-TEXT ( -- )
   CUR-OPEN @ {: o:n :}
   CUR @ PATH$ CUR @ DIR$
   o O-TEXT OPEN@ o O-TEXT-U OPEN@ POOL$
   DISCOVER:RUN-BYTES ;

\ A load of each file the last discovery's events name, by node K.
: LOADS ( n -- )
   {: k:n :}
   EVENT-COUNT 0 ?do
      k i EVENT-PATH@ i SOURCE-EVENT:ROOT@ NODE-OF EDGE+
   loop ;

\ Whether a node of this path's file is already named as one not read.
: NAMED? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   BAD-N @ 0 ?do
      i BAD @ PATH$ a u STR= if true unloop exit then
   loop
   false ;

\ Node K's loads were kept from being found by a throw of code RC.
: FAULT ( n n -- )
   {: k:n rc:n :}
   rc k N-FAULT NODE!
   k PATH$ NAMED? if exit then
   BAD-N @ {: b:n :}
   b 1+ BAD-RESERVE
   k b BAD !
   b 1+ BAD-N ! ;

: FAULT? ( n -- bool )  N-FAULT NODE@ 0<> ;

\ The loads discovery finds in Q's reading of the node being followed; a
\ file whose loads it cannot find is named.
: FOLLOW ( [ -- ] -- )
   {: q :}
   q catch {: rc:n :}
   rc 0= if CUR @ LOADS exit then
   rc FAULT$ nip nip if CUR @ rc FAULT exit then
   rc throw ;

\ The first open document at the path of node K, -1 for none.
: OPEN-OF ( n -- n )
   {: k:n :}
   OPEN-N @ 0 ?do
      i O-NODE OPEN@ PATH$ k PATH$ STR= if i unloop exit then
   loop
   -1 ;

\ Node K's loads: those of the text of the first open document at its path,
\ which a check reads in place of the file, else of its file on disk, when it
\ is there; none when they cannot be found.
: EXPAND ( n -- )
   {: k:n :}
   k CUR !
   k OPEN-OF {: o:n :}
   o 0 >= if o CUR-OPEN ! [: OPEN-TEXT ;] FOLLOW exit then
   k PATH$ FILE? if [: DISK-TEXT ;] FOLLOW then ;

\ Every node the subjects reach, each followed once: a node a load makes is
\ followed in its turn.
: EXPAND-ALL ( -- )
   0 begin dup NODE-N @ < while
      dup EXPAND 1+
   repeat drop ;

\ ---- what reaches F -------------------------------------------------------------

: REACHES? ( n -- bool )  N-REACH NODE@ 0<> ;

\ Each node of F's path reaches F.
: MARK ( -- )
   NODE-N @ 0 ?do
      i PATH$ F$ STR= if 1 i N-REACH NODE! then
   loop ;

\ Whether load E's loading node is found to reach F, through the node it
\ loads.
: LIFT ( n -- bool )
   {: e:n :}
   e EDGE-FROM EDGE@ {: from:n :}
   e EDGE-TO EDGE@ REACHES? from REACHES? 0= and
   dup if 1 from N-REACH NODE! then ;

\ One pass over the loads, the last first: whether it found a node.
: PASS ( -- bool )
   false
   EDGE-N @ 0 ?do EDGE-N @ 1- i - LIFT or loop ;

: SPREAD ( -- )  begin PASS 0= until ;

: HIT+ ( n -- )
   {: id:n :}
   HIT-N @ {: h:n :}
   h 1+ HITS-RESERVE
   id h HITS !
   h 1+ HIT-N ! ;

\ Whether node N is given: D, or a subject that reaches F.
: GIVEN? ( n -- bool )
   {: id:n :}
   id D-NODE @ = if true exit then
   id N-SUBJECT NODE@ 0<> id REACHES? and ;

: GIVE ( -- )
   NODE-N @ 0 ?do i GIVEN? if i HIT+ then loop ;

\ Whether path A comes before path B in byte order.
: PATH< ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n b:ptr v:n :}
   u v min 0 ?do
      a i + c@ b i + c@ <> if a i + c@ b i + c@ < unloop exit then
   loop
   u v < ;

\ Whether subject A comes before subject B: F first, then by path.
: BEFORE? ( n n -- bool )
   {: a:n b:n :}
   a PATH$ F$ STR= if true exit then
   b PATH$ F$ STR= if false exit then
   a PATH$ b PATH$ PATH< ;

: ORDER ( -- )
   HIT-N @ 2 < if exit then
   0 HITS HIT-N @ [: BEFORE? ;] SORT:SORT! ;

: BAD< ( n n -- bool )
   {: a:n b:n :}
   a PATH$ b PATH$ PATH< ;

: ORDER-BAD ( -- )
   BAD-N @ 2 < if exit then
   0 BAD BAD-N @ [: BAD< ;] SORT:SORT! ;

: RESET ( -- )
   0 POOL-U !
   0 NODE-N !
   0 SLOT-CAP !
   0 EDGE-N !
   0 OPEN-N !
   0 HIT-N !
   0 BAD-N !
   0 BLOCK-U !
   1 BLOCK-B-RESERVE
   false BLOCKED ! ;

public

\ Adds a workspace folder, by its path.
: ROOT+ ( ptr u8 n -- )
   SOURCE-ROOT:CANONICAL drop {: a:ptr u:n :}
   ROOT-BYTES-U @ {: at:n :}
   at u + 1+ ROOT-BYTES-RESERVE
   a at ROOT-BYTES u BYTE-COPY
   u ROOT-BYTES-U +!
   ROOT-N @ {: r:n :}
   r 1+ ROOT-CELLS * ROOTS-RESERVE
   at r R-PATH ROOT!
   u r R-PATH-U ROOT!
   r 1+ ROOT-N ! ;

\ Finds the subjects whose closures hold the file at canonical path F, and
\ gives the document at canonical path D among them; both paths are taken
\ before EACH runs. EACH is given a word taking an open document's canonical
\ path and text, and calls it once for each open document.
: REACHING ( ptr u8 n ptr u8 n [ [ ptr u8 n ptr u8 n -- ] -- ] -- )
   {: f:ptr fu:n d:ptr du:n each :}
   RESET
   f fu POOL+ F-AT !
   fu F-U !
   d du SUBJECT D-NODE !
   [: OPEN+ ;] each execute
   WALK-ROOTS
   BLOCKED @ if exit then
   EXPAND-ALL
   MARK
   SPREAD
   GIVE
   ORDER
   ORDER-BAD ;

\ Whether the last REACHING refused, and the root it named.
: REFUSED? ( -- bool )  BLOCKED @ ;
: BLOCKED$ ( -- ptr u8 n )  0 BLOCK-B BLOCK-U @ ;

\ The subjects the last REACHING gave, in its order.
: SUBJECTS ( -- n )  HIT-N @ ;
: SUBJECT$ ( n -- ptr u8 n )  HITS @ PATH$ ;

\ The files whose loads the last REACHING could not find, in the byte order
\ of their paths: each one's path, and why, a discovery's code as
\ lib/errors.f names it or "cannot be read".
: UNREADABLE ( -- n )  BAD-N @ ;
: UNREADABLE$ ( n -- ptr u8 n ptr u8 n )
   BAD @ {: k:n :}
   k PATH$ k N-FAULT NODE@ FAULT$ drop ;

;package
