\ maki/bpe-full.f - FULL runtime GPT-2 vocab loader: parse the fetched vocab.bpe end to end
\ and install all 50000 merges into the rank-lookup engine (dot habu-bpe-full-50k-a598ba57;
\ follow-up to the committed-subset loader maki/bpe-real.f / bpe-real-data.f).
\
\ vocab.bpe is the canonical GPT-2 merge table: a "#version" header line, then 50000 lines,
\ each two space-separated tokens in priority (rank) order. Every token is a string over the
\ GPT-2 bytes_to_unicode alphabet (each of the 256 bytes maps to one codepoint in [33,323],
\ UTF-8 encoded in the file). This module reads the file, resolves each merge line's two
\ child tokens to the engine's INTERNAL ids (byte b -> id b; merge of rank r -> id 256+r),
\ and hands (child a, child b, merged real id) for all 50000 merges to BPR-INSTALL.
\
\ mid[r] = 256 + r for the full table (measured: the merged token of vocab.bpe line r is
\ exactly encoder.json id 256+r, verified across all 50000 lines), so encoder.json is NOT
\ needed - the byte-id table is the committed BPR-D-BYTEID and the merged ids are 256+rank.
\ Child resolution needs a token-string -> internal-id map, built incrementally: seeded with
\ the 256 byte tokens (their bytes_to_unicode strings), then each line's merged string is
\ inserted at id 256+rank before later lines can reference it. The map is an open-addressing
\ hashtable with string-equality probing (FNV-1a keys), turning the O(N^2) it would take to
\ resolve 100000 child references by scan into O(N).
\
\ Loader discipline mirrors maki/bpe-real.f: the string map is built in scratch, then the
\ resolved merges pass through BPR-INSTALL, which fully re-validates (ids in range, injective)
\ and only then commits - a corrupt vocab.bpe (a child referenced before it is defined, a
\ duplicate token, an over-capacity table) throws a named E-code before any state is committed,
\ leaving a previously loaded vocab intact. maki -> habu only. bpe-full owns -5307..-5309.

require lib/fs.f
require lib/hashmap.f
require maki/examples/nanogpt/bpe-real-data.f

-5307 constant E-BPF-CAP     \ a table/arena/string-map bound exceeded (vocab larger than provisioned)
-5308 constant E-BPF-VOCAB   \ malformed vocab.bpe: a line without a separator, a child token referenced before it is defined, or a duplicate token string
-5309 constant E-BPF-IO      \ vocab.bpe absent or unreadable at the given path

package MAKI
private

256    constant BPF-BYTE-N       \ base byte tokens (internal ids 0..255); mid = BPF-BYTE-N + rank
50000  constant BPF-MERGE-CAP     \ full GPT-2 merge count
524288 constant BPF-BUF-CAP       \ vocab.bpe read buffer (file is ~446 KB)
400000 constant BPF-ARENA-CAP     \ token-string arena (256 byte tokens + 50000 merged strings, ~360 KB)
131072 constant BPF-SCAP          \ string-map slots: power of two > 1.4 * (256+50000)

$CBF29CE484222325 constant BPF-FNV-OFF   \ FNV-1a 64-bit offset basis
$100000001B3       constant BPF-FNV-PRIME \ FNV-1a 64-bit prime

create BPF-BUF   BPF-BUF-CAP   allot          \ raw vocab.bpe bytes
create BPF-ARENA BPF-ARENA-CAP allot          \ copied token strings (byte tokens, then merged tokens)
create BPF-MA    BPF-MERGE-CAP cells allot    \ rank -> child a internal id
create BPF-MB    BPF-MERGE-CAP cells allot    \ rank -> child b internal id
create BPF-MID   BPF-MERGE-CAP cells allot    \ rank -> merged token real id (256+rank)
create BPF-SUSED BPF-SCAP cells allot          \ string-map slot used flag (0 = empty)
create BPF-SOFF  BPF-SCAP cells allot          \ string-map slot -> arena offset
create BPF-SLEN  BPF-SCAP cells allot          \ string-map slot -> string length
create BPF-SVAL  BPF-SCAP cells allot          \ string-map slot -> internal id

variable BPF-U      \ vocab.bpe length in bytes
variable BPF-P      \ parse cursor into BPF-BUF
variable BPF-RN     \ merges resolved so far
variable BPF-AN     \ bytes used in BPF-ARENA
variable BPF-NP     \ bytes_to_unicode non-printable counter
variable BPF-H      \ FNV hash accumulator
variable BPF-SLOT   \ string-map probe cursor
variable BPF-DONE   \ string-map probe loop control

: BPF-FALSE ( -- bool )  0 0= 0= ;
: BPF-TRUE  ( -- bool )  0 0= ;

\ ---- bytes_to_unicode: byte -> codepoint, then UTF-8 into the arena --------------
\ printable bytes map to themselves; the rest map to 256,257,... in byte order (GPT-2 rule).
: BPF-PRINTABLE? ( n -- bool )  {: b:n :}
   b 33 >= b 126 <= and  b 161 >= b 172 <= and  or  b 174 >= b 255 <= and  or ;
\ codepoint for byte b; stateful over BPF-NP, so callers MUST iterate b = 0..255 in order
: BPF-CP ( n -- n )  {: b:n :}
   b BPF-PRINTABLE? if b exit then
   256 BPF-NP @ +  BPF-NP @ 1+ BPF-NP ! ;
\ UTF-8 encode codepoint cp (<2048 here) at dst; return byte count (1 or 2)
: BPF-UTF8 ( n ptr u8 -- n )  {: cp:n d:ptr :}
   cp 128 < if cp d c! 1 exit then
   cp 6 rshift 192 or  d c!
   cp 63 and 128 or    d 1+ c!
   2 ;

\ ---- string map: token string -> internal id (open addressing, string-equality) --
: BPF-SHASH ( ptr u8 n -- n )  {: a:ptr n:n :}
   BPF-FNV-OFF BPF-H !
   n 0 ?do  BPF-H @ a i + c@ xor BPF-FNV-PRIME * BPF-H !  loop
   BPF-H @ ;
\ does query (a,n) equal the arena string at (off,len)?
: BPF-SEQ? ( ptr u8 n n n -- bool )  {: a:ptr n:n off:n len:n :}
   n len <> if BPF-FALSE exit then
   n 0 ?do
      a i + c@  BPF-ARENA off i + + c@  <> if BPF-FALSE unloop exit then
   loop  BPF-TRUE ;
\ slot where the string lives, or the first empty slot for insertion
: BPF-FIND ( ptr u8 n -- n )  {: a:ptr n:n :}
   a n BPF-SHASH BPF-SCAP 1- and BPF-SLOT !
   0 BPF-DONE !
   begin BPF-DONE @ 0= while
      BPF-SUSED BPF-SLOT @ cells + @ 0= if -1 BPF-DONE ! else
         a n  BPF-SOFF BPF-SLOT @ cells + @  BPF-SLEN BPF-SLOT @ cells + @  BPF-SEQ?
         if -1 BPF-DONE ! else
            BPF-SLOT @ 1+ BPF-SCAP 1- and BPF-SLOT !
         then
      then
   repeat  BPF-SLOT @ ;
\ insert arena[off,len) -> id; a repeated token string is a corrupt vocab (fail closed)
: BPF-SPUT ( n n n -- )  {: off:n len:n id:n :}
   BPF-ARENA off + len BPF-FIND {: s:n :}
   BPF-SUSED s cells + @ 0= 0= if E-BPF-VOCAB throw then
   1   BPF-SUSED s cells + !
   off BPF-SOFF  s cells + !
   len BPF-SLEN  s cells + !
   id  BPF-SVAL  s cells + ! ;
\ internal id of an existing token; a token referenced before it is defined is corrupt
: BPF-SGET ( ptr u8 n -- n )  {: a:ptr n:n :}
   a n BPF-FIND {: s:n :}
   BPF-SUSED s cells + @ 0= if E-BPF-VOCAB throw then
   BPF-SVAL s cells + @ ;

\ seed the map with the 256 byte tokens (their bytes_to_unicode UTF-8 strings, id = byte)
: BPF-SEED ( -- )
   0 BPF-NP !  0 BPF-AN !
   BPF-BYTE-N 0 ?do
      i BPF-CP  BPF-ARENA BPF-AN @ +  BPF-UTF8 {: len:n :}
      BPF-AN @  len  i  BPF-SPUT
      BPF-AN @ len + BPF-AN !
   loop ;

\ ---- vocab.bpe scanning ----------------------------------------------------------
: BPF-NL ( n -- n )  {: from:n :}   \ first index >= from holding \n, else BPF-U
   from begin dup BPF-U @ < if dup BPF-BUF + c@ 10 <> else BPF-FALSE then while 1+ repeat ;
: BPF-SP ( n n -- n )  {: ls:n le:n :}   \ first space in [ls,le), else le
   ls begin dup le < if dup BPF-BUF + c@ 32 <> else BPF-FALSE then while 1+ repeat ;

\ resolve one merge line [ls,le): split on the separator space, record (a,b,mid), and
\ insert the merged string so later lines can reference it
: BPF-LINE ( n n -- )  {: ls:n le:n :}
   ls le BPF-SP {: sp:n :}
   sp le >= if E-BPF-VOCAB throw then
   BPF-BUF ls +  sp ls -        BPF-SGET {: ia:n :}
   BPF-BUF sp 1+ +  le sp 1+ -  BPF-SGET {: ib:n :}
   BPF-RN @ {: r:n :}
   r BPF-MERGE-CAP >= if E-BPF-CAP throw then
   ia BPF-MA r cells + !
   ib BPF-MB r cells + !
   BPF-BYTE-N r + BPF-MID r cells + !
   sp ls - {: la:n :}
   le sp 1+ - {: lb:n :}
   BPF-AN @ la + lb + BPF-ARENA-CAP > if E-BPF-CAP throw then
   BPF-AN @ {: moff:n :}
   la 0 ?do  BPF-BUF ls + i + c@       BPF-ARENA moff i + + c!  loop
   lb 0 ?do  BPF-BUF sp 1+ + i + c@    BPF-ARENA moff la + i + + c!  loop
   moff la lb +  BPF-BYTE-N r +  BPF-SPUT
   moff la + lb + BPF-AN !
   r 1+ BPF-RN ! ;

\ parse every merge line after the header
: BPF-PARSE ( -- )
   0 BPF-RN !
   0 BPF-NL 1+ BPF-P !                       \ skip the #version header line
   begin BPF-P @ BPF-U @ < while
      BPF-P @ BPF-NL {: le:n :}
      le BPF-P @ > if  BPF-P @ le BPF-LINE  then   \ non-empty line
      le 1+ BPF-P !
   repeat ;

public

\ true iff a readable vocab.bpe exists at the path (the full-load test presence-gates on this)
: BPF-PRESENT? ( ptr u8 n -- bool )  {: pa:ptr pu:n :}
   pa pu EXISTS? 0= if BPF-FALSE exit then  pa pu FILE? ;

\ Read + parse vocab.bpe at the path and install the full merge table through BPR-INSTALL
\ (which re-validates and commits). Returns the merge count loaded (50000 for the real file).
: BPF-LOAD ( ptr u8 n -- n )  {: pa:ptr pu:n :}
   pa pu BPF-PRESENT? 0= if E-BPF-IO throw then
   pa pu BPF-BUF BPF-BUF-CAP READ-ALL BPF-U !
   BPF-SUSED BPF-SCAP HM:CLEAR
   BPF-SEED
   BPF-PARSE
   BPR-D-BYTEID  BPF-MA BPF-MB BPF-MID  BPF-RN @  BPR-INSTALL
   BPF-RN @ ;

;package
