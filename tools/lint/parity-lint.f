\ parity-lint.f — token-diff paired emitter definitions (boot vs port) by NAME.
\ Self-hosted parity lint. For each (boot, port) file pair, compare the
\ filtered token stream of every def present in BOTH (minus ALLOW); a difference is
\ a silent divergence (the bug class the byte-goldens only flag as "bytes differ").
\ Filtering drops \ and ( ) comments (the tokenizer strips them), LBL, {: .. :}
\ label spans + label references, and case-folds — so only the real instruction
\ stream is compared. Run: cat tools/lint/lib.f tools/lint/parity-lint.f | bin/hb

create FB 131072 allot

\ ALLOW: words whose two impls legitimately differ (gforth vs subset idioms);
\ their OUTPUT is still byte-golden-checked. emit-prims = the native-only prim
\ registration table (the bootstrap builder is frozen).
: ALLOW?  {: a u :}  ( -- f )
   a u s" emit-forth"          STR=CI IF -1 exit THEN
   a u s" fprim"               STR=CI IF -1 exit THEN
   a u s" fprim-l"             STR=CI IF -1 exit THEN
   a u s" keep?"               STR=CI IF -1 exit THEN
   a u s" shk-tok="            STR=CI IF -1 exit THEN
   a u s" c-emitw"             STR=CI IF -1 exit THEN
   a u s" cf-entry"            STR=CI IF -1 exit THEN
   a u s" cfn-entry"           STR=CI IF -1 exit THEN
   a u s" emit-dict"           STR=CI IF -1 exit THEN
   a u s" emit-main"           STR=CI IF -1 exit THEN
   a u s" reg-prim"            STR=CI IF -1 exit THEN
   a u s" (sigact)"            STR=CI IF -1 exit THEN
   a u s" emit-crash-handler"  STR=CI IF -1 exit THEN
   a u s" emit-source"         STR=CI IF -1 exit THEN
   a u s" crh-init"            STR=CI IF -1 exit THEN
   a u s" emit-prims"          STR=CI IF -1 exit THEN  0 ;

\ ---- closing ';' (last on its line) after the colon at index j ----
variable DEI  variable DEDONE
: DEF-END  {: dj :}  ( -- e )   \ NB: 'i'/'j' are loop-index keywords, never local names
   dj 2 + DEI !  0 DEDONE !
   begin DEI @ TN# @ <  DEDONE @ 0=  and while
      DEI @ TOK s" ;" STR=  DEI @ TEOL? and IF -1 DEDONE ! ELSE DEI @ 1+ DEI ! THEN
   repeat  DEI @ ;

\ ---- per-def label set: words inside {: .. :} (minus --) ----
$80 constant LMAX
create LOFF LMAX cells allot   create LLEN LMAX cells allot   variable LN#
variable PI
: LABEL+  {: a u :}  a LOFF LN# @ cells + !  u LLEN LN# @ cells + !  LN# @ 1+ LN# ! ;
: LABEL?  {: a u :}  ( -- f )
   0 begin dup LN# @ < while
      dup cells LOFF + @  over cells LLEN + @  a u STR= IF drop -1 exit THEN  1+
   repeat  drop 0 ;
: COLLECT-LABELS  {: lo hi :}
   0 LN# !  lo PI !
   begin PI @ hi < while
      PI @ TOK s" {:" STR= IF
         PI @ 1+ PI !
         begin PI @ hi < PI @ TOK s" :}" STR= 0= and while
            PI @ TOK s" --" STR= 0= IF PI @ TOK LABEL+ THEN  PI @ 1+ PI !
         repeat
      THEN  PI @ 1+ PI !
   repeat ;

\ ---- filtered, space-joined, folded instruction string of body [lo,hi) ----
$8000 constant FSCR-CAP
create FSCR FSCR-CAP allot   variable FSL   variable FJ
: F+  {: a u :}
   FSL @ u + 1+ FSCR-CAP > IF s" parity-lint: filtered def too large" type cr 1 die THEN
   a u FSCR FSL @ + FOLD-TO  FSL @ u + FSL !  32 FSCR FSL @ + c!  FSL @ 1+ FSL ! ;
: FILTER  {: lo hi :}  ( -- a u )
   lo hi COLLECT-LABELS
   0 FSL !  lo FJ !
   begin FJ @ hi < while
      FJ @ TOK s" {:" STR= IF
         begin FJ @ hi < FJ @ TOK s" :}" STR= 0= and while FJ @ 1+ FJ ! repeat
      ELSE FJ @ TOK s" LBL" STR= IF
      ELSE FJ @ TOK LABEL? IF
      ELSE FJ @ TOK F+ THEN THEN THEN
      FJ @ 1+ FJ !
   repeat
   FSCR FSL @ ;

\ ---- boot def store (name + filtered body), rebuilt per pair ----
create DBUF 131072 allot   variable DEND
$200 constant DMAX
create DNOFF DMAX cells allot   create DNLEN DMAX cells allot
create DBOFF DMAX cells allot   create DBLEN DMAX cells allot   variable DN#
: D-ADD  {: na nu ba bu :}
   DN# @ DMAX >= IF s" parity-lint: too many boot defs" type cr 1 die THEN
   DEND @ nu + bu + 131072 > IF s" parity-lint: boot def store full" type cr 1 die THEN
   na DBUF DEND @ + nu BMOVE  DBUF DEND @ + DNOFF DN# @ cells + !  nu DNLEN DN# @ cells + !  DEND @ nu + DEND !
   ba DBUF DEND @ + bu BMOVE  DBUF DEND @ + DBOFF DN# @ cells + !  bu DBLEN DN# @ cells + !  DEND @ bu + DEND !
   DN# @ 1+ DN# ! ;
: D-FIND  {: na nu :}  ( -- idx|-1 )
   0 begin dup DN# @ < while
      dup cells DNOFF + @  over cells DNLEN + @  na nu STR=CI IF exit THEN  1+
   repeat  drop -1 ;
variable WI  variable DE
: WALK-BOOT  ( -- )   \ FB tokenized; store every non-ALLOW def's name + filtered body
   0 DEND !  0 DN# !  0 WI !
   begin WI @ TN# @ 1- < while
      WI @ TOK s" :" STR=  WI @ TOK0? and IF
         WI @ DEF-END DE !
         WI @ 1+ TOK ALLOW? 0= IF
            WI @ 1+ TOK   WI @ 2 + DE @ FILTER   D-ADD
         THEN
         DE @ WI !
      THEN
      WI @ 1+ WI !
   repeat ;
variable PIX  variable PE  variable BAD
: WALK-PORT  {: pf pu :}   \ compare each shared, non-ALLOW port def vs the boot store
   pf pu FB 131072 READ-FILE  TOKENIZE
   0 PIX !
   begin PIX @ TN# @ 1- < while
      PIX @ TOK s" :" STR=  PIX @ TOK0? and IF
         PIX @ DEF-END PE !
         PIX @ 1+ TOK ALLOW? 0= IF
            PIX @ 1+ TOK D-FIND  dup 0 >= IF
               >R
               PIX @ 2 + PE @ FILTER                 \ ( pa pu )
               R@ cells DBOFF + @   R> cells DBLEN + @   \ ( pa pu ba bu )
               STR= 0= IF
                  s" DIVERGE " type  PIX @ 1+ TOK type  s"  (" type pf pu type s" )" type cr
                  BAD @ 1+ BAD !
               THEN
            ELSE drop THEN
         THEN
         PE @ PIX !
      THEN
      PIX @ 1+ PIX !
   repeat ;
: BOOT  {: bf bu :}  bf bu FB 131072 READ-FILE  TOKENIZE  WALK-BOOT ;
: PARITY-LINT
   0 BAD !  -1 PARENS? !                              \ strip ( .. ) comments
   s" bootstrap/cg/forth.fs"    BOOT
      s" src/habu/habu1.f"  WALK-PORT   s" src/habu/habu2.f" WALK-PORT
   s" bootstrap/cg/regalloc.fs" BOOT   s" src/habu/regalloc.f" WALK-PORT
   s" bootstrap/cg/jit.fs"      BOOT   s" src/habu/jit.f"      WALK-PORT
   s" bootstrap/cg/prof.fs"     BOOT   s" src/habu/prof.f"     WALK-PORT
   s" bootstrap/cg/rt.fs"       BOOT   s" src/habu/rt.f"       WALK-PORT
   s" bootstrap/cg/crash.fs"    BOOT   s" src/habu/crash.f"    WALK-PORT
   BAD @ 0 > IF  s" parity-lint: " type BAD @ . s"  divergence(s)" type cr  1 die
   ELSE  s" parity-lint: 0 divergence(s)" type cr  THEN ;
PARITY-LINT
