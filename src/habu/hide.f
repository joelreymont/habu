\ hide.f - refresh prelude dictionary truncation.
\
\ Loaded before the native refresh source reloads common engine files. The words
\ intentionally use a BFR prefix so they can be defined in old engines that lack
\ xref.f, then hide themselves by truncating back to the requested marker.
\
\ WHAT IS NOT MIRRORED HERE ANY MORE. tools/bootstrap.sh's prologue resets the
\ signature store before it truncates (BOOT-USIGS-RESET); this file carried the
\ BFR-* twin of that reset, and nothing in the tree ever called it - the native
\ refresh rewinds the checker through its own boundary instead
\ (src/habu/prefix-rewind.f). Its only body was a TRUSTED: row naming `USIGS`,
\ a signature-less colon word of the checker's that the seal marks DNAME-INT,
\ so the uncalled words were also the reason a product image that keeps only
\ the names the checker knows could not compile this payload. They are gone
\ rather than given a declared surface, because the seal is not wrongly
\ covering `USIGS`: no consumer needs it (dot habu-give-the-build-4b825045).

0 constant BFR-START-SLOT
2 constant BFR-FLAGS-SLOT
3 constant BFR-NAME-SLOT

-1 constant BFR-NOT-FOUND
24 constant BFR-INLINE-OFF

\ Refresh casts expose mixed dictionary records and inline/long names.
\ Retirement: habu-builder-trust-rows-c5d41af6.
TRUSTED: BFR-N>REC ( n -- ptr n ) ;
TRUSTED: BFR-A>U8 ( ptr n -- ptr u8 ) ;
TRUSTED: BFR-N>U8 ( n -- ptr u8 ) ;
\ THE LOWERING SEAM, and `seed-ndict!` rather than public `ndict!` because the
\ host this prelude runs on has its seal floor armed: the watermark is set
\ before any entry (habu2.f EM-SEAL-SEEDED-RUNTIME) and BNDSET refuses every
\ count below it with a silent exit 83, so the payload used to die with no
\ diagnostic at all. `seed-ndict!` is the engine's one authorized lowering: it
\ refuses a raise, guards the record span, rebuilds the name index and clears
\ the floor, and its checker row is admitted only inside a TRUSTED: boundary -
\ which is exactly what this row is, and this file is payload-only.
\ src/habu/prefix-rewind.f drives the same seam for the other rewind.
\
\ tools/bootstrap.sh's BOOT-* twin of these words lowers through `seed-ndict!`
\ as well: the launcher feeds one prologue to the gforth stage0 and to the
\ sealed native stages after it, and the stage0 prim table
\ (bootstrap/cg/forth.fs) answers the name with its unsealed BNDSET.
TRUSTED: BFR-NDICT! ( n -- ) seed-ndict! ;
\ Named refresh-prelude boundary (staged fixpoint source checking,
\ habu-staged-fixpoint-src-0b5fc6e6): the stage compile loads the checker-boot
\ region with the hook silenced, but the blocking pre-pass (tools/
\ build-fixpoint.f BF-CERTIFY-*) statically checks THROUGH the window, so the
\ window's only effect is stage-compile hook silence.
\ Retirement: habu-builder-trust-rows-c5d41af6.
TRUSTED: BFR-CHECK-OFF ( -- ) 0 set-check ;

: BFR-REC-ADDR ( n -- n )
   DREC * dbase@ + ;

: BFR-REC ( n -- ptr n )
   BFR-REC-ADDR BFR-N>REC ;

: BFR-CELL@ ( ptr n n -- n )
   cells + @ ;

: BFR-PTR@ ( ptr n n -- ptr u8 )
   BFR-CELL@ BFR-N>U8 ;

: BFR-START ( ptr n -- n )
   BFR-START-SLOT BFR-CELL@ ;

: BFR-FLAGS ( ptr n -- n )
   BFR-FLAGS-SLOT BFR-CELL@ ;

: BFR-NAME-LEN ( ptr n -- n )
   BFR-FLAGS DNAME-LEN-MASK and ;

: BFR-EXT? ( ptr n -- bool )
   BFR-FLAGS DNAME-EXT and 0= 0= ;

: BFR-INLINE-NAME ( ptr n -- ptr u8 )
   BFR-INLINE-OFF + BFR-A>U8 ;

: BFR-NAME-A ( ptr n -- ptr u8 )
   dup BFR-EXT? if BFR-NAME-SLOT BFR-PTR@ exit then
   BFR-INLINE-NAME ;

: BFR-NAME$ ( ptr n -- ptr u8 n )
   dup BFR-NAME-A swap BFR-NAME-LEN ;

: BFR-FOLD-C ( n -- n )
   dup $41 < if exit then
   dup $5A > if exit then
   $20 or ;

PTR-VARIABLE BFR-A
PTR-VARIABLE BFR-B
variable BFR-U
variable BFR-V
PTR-VARIABLE BFR-SN
variable BFR-SU

: BFR-BYTE@ ( ptr u8 n -- u8 )
   + c@ ;
s" BFR-BYTE@" s" ptr u8 n -- u8" TRUST

\ The name scanner reads and writes three byte pointers through generic scratch
\ cells. Retirement: habu-builder-trust-rows-c5d41af6.
TRUSTED: BFR-A@ ( -- ptr u8 )
   BFR-A @ ;

TRUSTED: BFR-B@ ( -- ptr u8 )
   BFR-B @ ;

TRUSTED: BFR-SN@ ( -- ptr u8 )
   BFR-SN @ ;

TRUSTED: BFR-A! ( ptr u8 -- )
   BFR-A ! ;

TRUSTED: BFR-B! ( ptr u8 -- )
   BFR-B ! ;

TRUSTED: BFR-SN! ( ptr u8 -- )
   BFR-SN ! ;

: BFR-STR=CI ( ptr u8 n ptr u8 n -- bool )
   BFR-V ! BFR-B! BFR-U ! BFR-A!
   BFR-U @ BFR-V @ <> if 0 0= 0= exit then
   0 begin dup BFR-U @ < while
      dup BFR-A@ swap BFR-BYTE@ BFR-FOLD-C
      over BFR-B@ swap BFR-BYTE@ BFR-FOLD-C <> if drop 0 0= 0= exit then
      1+
   repeat drop
   0 0= ;

: BFR-MATCH? ( ptr n ptr u8 n -- bool )
   BFR-U ! BFR-A!
   BFR-NAME$ BFR-A@ BFR-U @ BFR-STR=CI ;

: BFR-FIND-FIRST-INDEX ( ptr u8 n -- n )
   BFR-SU ! BFR-SN!
   0 begin dup ndict@ < while
      dup BFR-REC BFR-SN@ BFR-SU @ BFR-MATCH? if exit then
      1+
   repeat drop
   BFR-NOT-FOUND ;

: BFR-REQUIRE-INDEX ( n -- n )
   dup 0 >= if exit then
   s" build-fixpoint: hide word not found" 76 die ;

: BFR-MIN-FOUND ( n n -- n ) {: a:n b:n :}
   a BFR-NOT-FOUND = if b exit then
   b BFR-NOT-FOUND = if a exit then
   a b < if a else b then ;

: BFR-MARKER-INDEX ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n b:ptr v:n :}
   a u BFR-FIND-FIRST-INDEX
   b v BFR-FIND-FIRST-INDEX
   BFR-MIN-FOUND BFR-REQUIRE-INDEX ;

\ THE EARLIEST-MARKER REWIND. Truncates to whichever named marker sits earlier,
\ which is util.f's first record - the start of the whole core prefix. The
\ native builder no longer emits it (src/habu/prefix-rewind.f rewinds to the
\ prefix END instead), and it stays because tools/bootstrap.sh's recovery
\ prelude emits its own BOOT-* twin of exactly these words: this file is the
\ executable spec that tools/bootstrap-codegen-test.f drives to keep the two
\ sides honest.
: BFR-HIDE-DICT-FROM-EARLIEST ( ptr u8 n ptr u8 n -- )
   BFR-MARKER-INDEX BFR-NDICT! ;
