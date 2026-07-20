\ nanogpt-inventory-lint-core.f - keep docs/nanogpt-inventory.md's owner links
\ honest against live dot status and on-disk module owners.
\
\ The inventory classifies every nanoGPT capability across six dimensions
\ (prototype golden, host production, trainable AD, batched semantics, device
\ lowering, measured performance) and links each INCOMPLETE dimension to its
\ exact live owner. A dot that closes or reopens, or an owner module that is
\ moved/renamed, silently rots those prose links: a closed dot keeps being
\ cited as a live owner, or a live owner is buried under a stale "landed" note.
\
\ Policy is DERIVED, not transcribed. The doc carries ONE machine-readable
\ owner manifest (a fenced ```owners block); each row is `<kind> <identifier>`
\ where kind is open | closed | module. This lint:
\   (a) resolves every open/closed dot id against the real .dots tree (archive/
\       => closed; else the frontmatter `status:` line, open|active => open,
\       closed|done => closed) and reds STATUS-MISMATCH when polarity disagrees,
\       UNKNOWN when the id exists in no dot file, BADSTATUS otherwise;
\   (b) reds UNKNOWN-MODULE when a module owner path does not exist on disk;
\   (c) reds DUPLICATE when one identifier is manifested twice;
\   (d) binds prose to the manifest: every habu-<8hex> id written in the doc
\       body (outside the block) must be a manifest row (PROSE-UNLISTED), and
\       every manifest identifier must be referenced in the body (DEAD-ROW) -
\       so an owner link cannot be added or removed in prose without the
\       manifest, and the manifest cannot silently outlive its row.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/lib.f.
\ Run: bin/hb --load tools/nanogpt-inventory-lint.f

package NANOGPT-INV

$40000 constant NGI-DOC-CAP                     \ inventory doc (headroom past ~20KB)
$8000  constant NGI-DOTF-CAP                    \ one dot file at a time
256    constant NGI-MAX                         \ manifest rows (owners)
$4000  constant NGI-IDS-CAP                     \ interned identifier bytes

0 constant NGI-K-OPEN
1 constant NGI-K-CLOSED
2 constant NGI-K-MODULE

-1 constant NGI-S-UNRES                         \ id resolved to no dot file yet
0  constant NGI-S-OPEN
1  constant NGI-S-CLOSED
3  constant NGI-S-BAD                            \ unparseable frontmatter / conflicting files

create NGI-DOC  NGI-DOC-CAP  allot   variable NGI-DOC-U
create NGI-DOTF NGI-DOTF-CAP allot   variable NGI-DOTF-U
create NGI-IDS  NGI-IDS-CAP  allot   variable NGI-IDS-U
create NGI-SRCH 128 allot            variable NGI-SRCH-U
create NGI-CB 1 allot
32 constant NGI-NUM-CAP
create NGI-NUM NGI-NUM-CAP allot   variable NGI-NUM-L

create NGI-KIND  NGI-MAX cells allot            \ row kind
create NGI-IDOFF NGI-MAX cells allot            \ id offset into NGI-IDS
create NGI-IDLEN NGI-MAX cells allot            \ id length
create NGI-STAT  NGI-MAX cells allot            \ resolved dot-status class
create NGI-SEEN  NGI-MAX cells allot            \ prose-reference count
variable NGI-N                                  \ manifest row count
variable NGI-BAD                                \ finding count

variable NGI-BLK-S                              \ manifest block body start byte
variable NGI-BLK-E                              \ manifest block body end byte (at closing fence)
variable NGI-HAVE-BLK
variable NGI-LI                                 \ line-scan cursor (SPLIT-LINES index)
variable NGI-TC                                 \ token-scan cursor within a line
variable NGI-SC                                 \ id-scan cursor (NGI-ID-AT?)
variable NGI-SC2                                \ prose-scan cursor (NGI-SCAN-PROSE-IDS)

\ ---- output (buffer-aware, so tests can capture) ---------------------------
: NGI-OUT ( ptr u8 n -- )   1 -rot LINT-OUT-WRITE ;
: NGI-OUT-C ( n -- )   NGI-CB c!  NGI-CB 1 NGI-OUT ;
: NGI-NL ( -- )   10 NGI-OUT-C ;
: NGI-U. ( n -- )
   0 NGI-NUM-L !
   dup 0= if drop 48 NGI-OUT-C exit then
   begin dup 0 > while
      dup 10 mod 48 + NGI-NUM NGI-NUM-L @ + c!
      10 /  NGI-NUM-L @ 1+ NGI-NUM-L !
   repeat drop
   begin NGI-NUM-L @ 0 > while
      NGI-NUM-L @ 1- NGI-NUM-L !
      NGI-NUM NGI-NUM-L @ + c@ NGI-OUT-C
   repeat ;
: NGI-BAD+ ( -- )   NGI-BAD @ 1+ NGI-BAD ! ;

\ ---- char classes ----------------------------------------------------------
: NGI-LOWER? ( n -- bool )   dup 97 >= swap 122 <= and ;
: NGI-DIGIT? ( n -- bool )   dup 48 >= swap 57 <= and ;
: NGI-IDCH? ( n -- bool )                       \ [a-z0-9-] : the dot-id alphabet
   dup NGI-LOWER? if drop LINT-TRUE exit then
   dup NGI-DIGIT? if drop LINT-TRUE exit then
   45 = ;
: NGI-HEX? ( n -- bool )
   dup NGI-DIGIT? if drop LINT-TRUE exit then
   dup 97 >= swap 102 <= and ;

\ ---- interned identifier store ---------------------------------------------
: NGI-SRCH! ( ptr u8 n -- ) {: a:ptr u:n :}   a NGI-SRCH u BYTE-COPY  u NGI-SRCH-U ! ;
: NGI-SRCH$ ( -- ptr u8 n )   NGI-SRCH NGI-SRCH-U @ ;
: NGI-ID$ ( n -- ptr u8 n ) {: k :}
   NGI-IDS NGI-IDOFF k cells + @ +   NGI-IDLEN k cells + @ ;
: NGI-KIND-AT ( n -- n )   cells NGI-KIND + @ ;
: NGI-ID= ( ptr u8 n n -- bool ) {: a:ptr u:n k :}
   a u k NGI-ID$ LINT-STR= ;
: NGI-ROW-MATCH? ( n -- bool )   NGI-SRCH$ rot NGI-ID= ;   \ row k vs the stored search key
: NGI-FIND ( ptr u8 n -- n )                    \ first manifest row with this id, or -1
   NGI-SRCH!
   0 begin dup NGI-N @ < while
      dup NGI-ROW-MATCH? if exit then
      1+
   repeat drop -1 ;
: NGI-INTERN ( ptr u8 n -- n ) {: a:ptr u:n :}  \ store id bytes, return its offset
   NGI-IDS-U @ u + NGI-IDS-CAP > if s" nanogpt-inventory-lint: id store full" 1 die then
   a NGI-IDS NGI-IDS-U @ + u BYTE-COPY
   NGI-IDS-U @  dup u + NGI-IDS-U ! ;
: NGI-ROW+ ( ptr u8 n n -- ) {: a:ptr u:n kind :}
   NGI-N @ NGI-MAX >= if s" nanogpt-inventory-lint: too many owner rows" 1 die then
   a u NGI-INTERN NGI-IDOFF NGI-N @ cells + !
   u NGI-IDLEN NGI-N @ cells + !
   kind NGI-KIND NGI-N @ cells + !
   NGI-S-UNRES NGI-STAT NGI-N @ cells + !
   0 NGI-SEEN NGI-N @ cells + !
   NGI-N @ 1+ NGI-N ! ;

\ ---- byte access into the loaded doc (NGI-DOC is a typed create pointer) ----
: NGI-DOC$ ( -- ptr u8 n )   NGI-DOC NGI-DOC-U @ ;
: NGI-DOC-C@ ( n -- n )   NGI-DOC + c@ ;
: NGI-DOTF$ ( -- ptr u8 n )   NGI-DOTF NGI-DOTF-U @ ;
: NGI-DOC-LOAD ( ptr u8 n -- )   NGI-DOC NGI-DOC-CAP READ-FILE NGI-DOC-U ! drop ;

\ ---- single-line token split (cursor + local base ptr; type-safe) ----------
: NGI-AT ( ptr u8 n -- n ) {: a:ptr i:n :}   a i + c@ ;   \ byte at a+i, coerced u8 -> n
: NGI-WS-AT? ( ptr u8 n -- bool )   NGI-AT LINT-WS? ;
: NGI-TOK ( ptr u8 n n -- ptr u8 n n )          \ (line-a line-u start -- tok-a tok-u next); tok-u=0 => none
   {: a:ptr u:n s:n :}
   s NGI-TC !
   begin NGI-TC @ u < a NGI-TC @ NGI-WS-AT? and while NGI-TC @ 1+ NGI-TC ! repeat
   NGI-TC @ u >= if a 0 u exit then
   a NGI-TC @ +  {: tb:ptr :}
   begin NGI-TC @ u < a NGI-TC @ NGI-WS-AT? 0= and while NGI-TC @ 1+ NGI-TC ! repeat
   tb  a NGI-TC @ + tb -  NGI-TC @ ;

\ ---- manifest kind parse ---------------------------------------------------
: NGI-KIND-OF ( ptr u8 n -- n )                 \ -1 when not a valid kind
   2dup s" open"   LINT-STR= if 2drop NGI-K-OPEN   exit then
   2dup s" closed" LINT-STR= if 2drop NGI-K-CLOSED exit then
   2dup s" module" LINT-STR= if 2drop NGI-K-MODULE exit then
   2drop -1 ;
: NGI-ROW-MALFORMED ( ptr u8 n -- )
   s" NGI-ROW-MALFORMED nanogpt-inventory-lint: bad owner row `" NGI-OUT NGI-OUT s" `" NGI-OUT NGI-NL
   NGI-BAD+ ;
: NGI-PARSE-ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LINT-TRIM nip 0= if exit then            \ blank line inside the block
   a u 0 NGI-TOK {: ka:ptr ku:n s1:n :}
   ku 0= if a u NGI-ROW-MALFORMED exit then
   ka ku NGI-KIND-OF dup 0 < if drop a u NGI-ROW-MALFORMED exit then {: kind :}
   a u s1 NGI-TOK {: ia:ptr iu:n s2:n :}
   iu 0= if a u NGI-ROW-MALFORMED exit then
   a u s2 NGI-TOK drop nip 0 > if a u NGI-ROW-MALFORMED exit then   \ stray third token
   ia iu kind NGI-ROW+ ;

\ ---- pass A: locate + parse the manifest block -----------------------------
: NGI-FENCE-OPEN?  ( ptr u8 n -- bool )  LINT-TRIM s" ```owners" LINT-STR= ;
: NGI-FENCE-CLOSE? ( ptr u8 n -- bool )  LINT-TRIM s" ```" LINT-STR= ;
: NGI-LINE-OFF ( n -- n )   S@ drop NGI-DOC - ;  \ byte offset of split-line i in the doc

: NGI-BLOCK-BODY ( n -- )                        \ i = opening-fence line index
   dup 1+ dup SN# @ < if NGI-LINE-OFF else drop NGI-DOC-U @ then NGI-BLK-S !
   1+
   begin dup SN# @ < while
      dup S@ NGI-FENCE-CLOSE? if dup NGI-LINE-OFF NGI-BLK-E ! drop exit then
      dup S@ NGI-PARSE-ROW
      1+
   repeat
   drop NGI-DOC-U @ NGI-BLK-E ! ;

: NGI-PARSE-MANIFEST ( -- )
   LINT-FALSE NGI-HAVE-BLK !  0 NGI-BLK-S !  0 NGI-BLK-E !
   NGI-DOC$ SPLIT-LINES
   0 begin dup SN# @ < while
      dup S@ NGI-FENCE-OPEN? if
         LINT-TRUE NGI-HAVE-BLK !
         NGI-BLOCK-BODY exit
      then
      1+
   repeat drop ;

\ ---- prose region helpers (doc body outside the manifest block) ------------
: NGI-IN-BLOCK? ( n -- bool )                   \ byte offset lies inside the fenced rows
   NGI-HAVE-BLK @ 0= if drop LINT-FALSE exit then
   dup NGI-BLK-S @ >= swap NGI-BLK-E @ < and ;
: NGI-BODY-CONTAINS? ( ptr u8 n -- bool )       \ id appears in the doc OUTSIDE the block
   {: a:ptr u:n :}
   NGI-HAVE-BLK @ 0= if NGI-DOC$ a u LINT-CONTAINS? exit then
   NGI-DOC NGI-BLK-S @  a u LINT-CONTAINS? if LINT-TRUE exit then
   NGI-DOC NGI-BLK-E @ +  NGI-DOC-U @ NGI-BLK-E @ -  a u LINT-CONTAINS? ;

\ ---- prose id scan: every habu-<8hex> outside the block must be manifested --
: NGI-HEX8? ( n -- bool ) {: p :}               \ 8 hex digits at doc offset p
   p 8 + NGI-DOC-U @ > if LINT-FALSE exit then
   p begin dup p 8 + < while
      dup NGI-DOC-C@ NGI-HEX? 0= if drop LINT-FALSE exit then 1+
   repeat drop LINT-TRUE ;
: NGI-ID-AT? ( n -- n ) {: off :}                \ length of a valid dot id at off, else 0
   off 0 > if off 1- NGI-DOC-C@ NGI-IDCH? if 0 exit then then    \ left boundary
   NGI-DOC off +  NGI-DOC-U @ off -  s" habu-" LINT-STARTS-WITH? 0= if 0 exit then
   off NGI-SC !
   begin NGI-SC @ NGI-DOC-U @ < NGI-SC @ NGI-DOC-C@ NGI-IDCH? and while
      NGI-SC @ 1+ NGI-SC ! repeat
   NGI-SC @ off - {: len :}
   len 14 < if 0 exit then                       \ habu- + >=1 word char + '-' + 8hex minimum
   off len + 9 - NGI-DOC-C@ 45 <> if 0 exit then \ '-' before the hex tail
   off len + 8 - NGI-HEX8? 0= if 0 exit then
   len ;
: NGI-PROSE-UNLISTED ( ptr u8 n -- )
   s" NGI-PROSE-UNLISTED nanogpt-inventory-lint: `" NGI-OUT NGI-OUT
   s" ` cited in prose but not in the owner manifest" NGI-OUT NGI-NL
   NGI-BAD+ ;
: NGI-NOTE-PROSE-ID ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NGI-FIND dup 0 < if
      drop a u NGI-PROSE-UNLISTED
   else
      NGI-SEEN swap cells + dup @ 1+ swap !
   then ;
: NGI-SCAN-PROSE-IDS ( -- )
   0 NGI-SC2 !
   begin NGI-SC2 @ NGI-DOC-U @ < while
      NGI-SC2 @ NGI-IN-BLOCK? if
         NGI-BLK-E @ NGI-SC2 !
      else
         NGI-SC2 @ NGI-ID-AT? dup 0 > if
            NGI-DOC NGI-SC2 @ + over NGI-NOTE-PROSE-ID
            NGI-SC2 @ + NGI-SC2 !
         else drop NGI-SC2 @ 1+ NGI-SC2 ! then
      then
   repeat ;

\ ---- dot-status resolution over the .dots tree -----------------------------
: NGI-MD? ( ptr u8 n -- bool )   s" .md" LINT-ENDS-WITH? ;
: NGI-ARCHIVE? ( ptr u8 n -- bool )   s" /archive/" LINT-CONTAINS? ;
: NGI-PATH-ID$ ( ptr u8 n -- ptr u8 n )   BASENAME  3 - ;   \ basename minus ".md"

: NGI-SKIP ( ptr u8 n n -- ptr u8 n ) {: a:ptr u:n k:n :}   a k +  u k - ;
: NGI-STATUS-VALUE ( ptr u8 n -- n )            \ classify a trimmed status: value
   2dup s" open"   LINT-STR= if 2drop NGI-S-OPEN   exit then
   2dup s" active" LINT-STR= if 2drop NGI-S-OPEN   exit then
   2dup s" closed" LINT-STR= if 2drop NGI-S-CLOSED exit then
   2dup s" done"   LINT-STR= if 2drop NGI-S-CLOSED exit then
   2drop NGI-S-BAD ;
: NGI-DOTF-STATUS ( -- n )                       \ first frontmatter status: line wins
   NGI-DOTF$ SPLIT-LINES
   0 NGI-LI !
   begin NGI-LI @ SN# @ < while
      NGI-LI @ S@ LINT-TRIM 2dup s" status:" LINT-STARTS-WITH? if
         7 NGI-SKIP LINT-TRIM NGI-STATUS-VALUE exit
      then
      2drop
      NGI-LI @ 1+ NGI-LI !
   repeat
   NGI-S-BAD ;
: NGI-SET-ROW-STATUS ( n n -- ) {: idx class :}
   NGI-STAT idx cells + {: cell:ptr :}
   cell @ NGI-S-UNRES = if class cell ! exit then
   cell @ class <> if NGI-S-BAD cell ! then ;    \ two dot files, disagreeing status
: NGI-RESOLVE-DOT ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu NGI-MD? 0= if exit then
   pa pu NGI-PATH-ID$ NGI-FIND 0 < if exit then  \ id not manifested (also arms NGI-SRCH = this id)
   pa pu NGI-ARCHIVE? if
      NGI-S-CLOSED
   else
      pa pu NGI-DOTF NGI-DOTF-CAP READ-FILE NGI-DOTF-U ! drop
      NGI-DOTF-STATUS
   then  {: cls :}
   0 begin dup NGI-N @ < while                    \ apply to every open/closed row with this id
      dup NGI-KIND-AT NGI-K-MODULE <> if
         dup NGI-ROW-MATCH? if dup cls NGI-SET-ROW-STATUS then
      then
      1+
   repeat drop ;

\ ---- findings + per-row checks ---------------------------------------------
: NGI-DUP ( n -- )
   s" NGI-DUPLICATE nanogpt-inventory-lint: `" NGI-OUT NGI-ID$ NGI-OUT
   s" ` listed more than once in the owner manifest" NGI-OUT NGI-NL  NGI-BAD+ ;
: NGI-DEAD ( n -- )
   s" NGI-DEAD-ROW nanogpt-inventory-lint: `" NGI-OUT NGI-ID$ NGI-OUT
   s" ` manifested but never referenced in the inventory body" NGI-OUT NGI-NL  NGI-BAD+ ;
: NGI-UNKNOWN ( n -- )
   s" NGI-UNKNOWN nanogpt-inventory-lint: dot `" NGI-OUT NGI-ID$ NGI-OUT
   s" ` exists in no .dots file" NGI-OUT NGI-NL  NGI-BAD+ ;
: NGI-UNKNOWN-MODULE ( n -- )
   s" NGI-UNKNOWN-MODULE nanogpt-inventory-lint: module `" NGI-OUT NGI-ID$ NGI-OUT
   s" ` does not exist on disk" NGI-OUT NGI-NL  NGI-BAD+ ;
: NGI-MISMATCH ( n n -- ) {: idx want :}
   s" NGI-STATUS-MISMATCH nanogpt-inventory-lint: `" NGI-OUT idx NGI-ID$ NGI-OUT
   s" ` manifested as " NGI-OUT
   want NGI-K-OPEN = if s" a live owner (open) but the dot is closed" else
      s" landed (closed) but the dot is still open" then NGI-OUT NGI-NL  NGI-BAD+ ;
: NGI-BADSTAT ( n -- )
   s" NGI-BADSTATUS nanogpt-inventory-lint: `" NGI-OUT NGI-ID$ NGI-OUT
   s" ` has no parseable / a conflicting dot status" NGI-OUT NGI-NL  NGI-BAD+ ;

: NGI-DUP? ( n -- bool ) {: idx :}               \ an earlier row already carries this id
   0 begin dup idx < while
      dup NGI-ID$ idx NGI-ID= if drop LINT-TRUE exit then
      1+
   repeat drop LINT-FALSE ;
: NGI-CHECK-DOT ( n n -- ) {: idx want :}
   NGI-STAT idx cells + @ {: st :}
   st NGI-S-UNRES = if idx NGI-UNKNOWN exit then
   st NGI-S-BAD   = if idx NGI-BADSTAT exit then
   st want <> if idx want NGI-MISMATCH then ;
: NGI-CHECK-MODULE ( n -- ) {: idx :}
   idx NGI-ID$ EXISTS? 0= if idx NGI-UNKNOWN-MODULE then ;
: NGI-CHECK-ROW ( n -- ) {: idx :}
   idx NGI-DUP? if idx NGI-DUP exit then          \ report the id once, at its later row
   idx NGI-KIND-AT {: kind :}
   kind NGI-K-MODULE = if idx NGI-CHECK-MODULE exit then
   idx kind NGI-CHECK-DOT ;
: NGI-CHECK-DEAD ( n -- ) {: idx :}
   NGI-SEEN idx cells + @ 0= if                   \ module rows: a body mention also counts
      idx NGI-ID$ NGI-BODY-CONTAINS? 0= if idx NGI-DEAD then
   then ;
: NGI-CHECK-ROWS ( -- )
   0 begin dup NGI-N @ < while dup NGI-CHECK-ROW  dup NGI-CHECK-DEAD  1+ repeat drop ;

\ ---- driver ----------------------------------------------------------------
: NGI-RESET ( -- )   0 NGI-N !  0 NGI-BAD !  0 NGI-IDS-U ! ;
: NGI-NO-MANIFEST ( -- )
   s" NGI-MANIFEST-MISSING nanogpt-inventory-lint: no ```owners manifest block in the inventory" NGI-OUT NGI-NL
   NGI-BAD+ ;
: NGI-SUMMARY ( -- )
   s" nanogpt-inventory-lint: " NGI-OUT NGI-N @ NGI-U. s"  owner(s), " NGI-OUT
   NGI-BAD @ NGI-U. s"  finding(s)" NGI-OUT NGI-NL ;

public

\ Parameterized entry (tests point it at fixture docs / the real .dots tree).
: NGI-LINT-AT ( ptr u8 n ptr u8 n -- ) {: da:ptr du:n ra:ptr ru:n :}
   NGI-RESET
   da du NGI-DOC-LOAD
   NGI-PARSE-MANIFEST
   NGI-HAVE-BLK @ 0= if NGI-NO-MANIFEST NGI-SUMMARY 1 throw then
   NGI-SCAN-PROSE-IDS
   ra ru [: NGI-RESOLVE-DOT ;] WALK-FILES
   NGI-CHECK-ROWS
   NGI-SUMMARY
   NGI-BAD @ 0 > if 1 throw then ;

: NGI-LINT ( -- )
   s" docs/nanogpt-inventory.md" s" .dots/" NGI-LINT-AT ;

;package

: NANOGPT-INVENTORY-LINT ( -- )   NANOGPT-INV:NGI-LINT ;
