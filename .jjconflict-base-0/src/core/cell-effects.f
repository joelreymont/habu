\ cell-effects.f - checker effects for the pre-checker foundation.
\
\ CELL, the selected target layout, the shared engine layout and pointer
\ storage intentionally load before checker.f. Their definitions stay with
\ their owning sources; this one post-checker seam publishes their effects.
\ Retirement: habu-primitive-effect-axiom-1119f176.

s" CELL" s" -- n" TRUST
s" CELL-WIDTH-CHECK" s" --" TRUST
s" CHECKER-CAPTURE-PREPARE" s" --" TRUST
s" HB-TARGET-LINUX?" s" -- bool" TRUST
s" HB-TARGET-MACOS?" s" -- bool" TRUST
s" HB-TARGET-KNOWN?" s" -- bool" TRUST

package CELL-EFFECTS
private

TRUSTED: PUBLISH-TARGET ( -- )
   HB-TARGET-LINUX? if
      s" IMAGE-TEXT-SIZE-OFF" s" -- n" TRUST
      s" IMAGE-TEXT-CONTENT-ADJ" s" -- n" TRUST
      s" IMAGE-TEXT-TRAILER-ADJ" s" -- n" TRUST
      s" DATA-VA" s" -- va" TRUST
      s" DATA-SIZE" s" -- n" TRUST
      s" CODE-OFF" s" -- n" TRUST
      s" LINUX-DLOPEN-SLOT-OFF" s" -- n" TRUST
      s" LINUX-DLSYM-SLOT-OFF" s" -- n" TRUST
      s" LINUX-IMAGE-BASE" s" -- n" TRUST
      s" LINUX-TEXT-CELL" s" -- ptr n" TRUST
      s" LINUX-TEXT-SIZE" s" -- n" TRUST
      s" LINUX-RW-VA" s" -- va" TRUST
      s" DLOPEN-SLOT-VA" s" -- va" TRUST
      s" DLSYM-SLOT-VA" s" -- va" TRUST
      s" DLOPEN-SLOT" s" -- ptr n" TRUST
      s" DLSYM-SLOT" s" -- ptr n" TRUST
   else HB-TARGET-MACOS? if
      s" IMAGE-TEXT-SIZE-OFF" s" -- n" TRUST
      s" IMAGE-TEXT-CONTENT-ADJ" s" -- n" TRUST
      s" IMAGE-TEXT-TRAILER-ADJ" s" -- n" TRUST
      s" DATA-VA" s" -- va" TRUST
      s" DATA-SIZE" s" -- n" TRUST
      s" CODE-OFF" s" -- n" TRUST
      s" DLOPEN-SLOT" s" -- ptr n" TRUST
      s" DLSYM-SLOT" s" -- ptr n" TRUST
   else
      s" cell-effects: unsupported target" 76 die
   then then ;

' PUBLISH-TARGET
;package
execute

s" ARGC-CELL" s" -- n" TRUST
s" ARGV-CELL" s" -- n" TRUST
s" ENVP-CELL" s" -- n" TRUST
s" TKA-CELL" s" -- n" TRUST
s" TKL-CELL" s" -- n" TRUST
s" DREC" s" -- n" TRUST
s" BODYBUF-CAP" s" -- n" TRUST
s" BPTAB-OFF" s" -- n" TRUST
s" BPWBASE-CELL" s" -- n" TRUST
s" BPWN-CELL" s" -- n" TRUST
s" DEFER-MAGIC" s" -- n" TRUST
s" DICT-WL:NAMESPACE" s" -- n" TRUST
s" DICT-WL:RETIRED" s" -- n" TRUST
s" DICT-CAP" s" -- n" TRUST
s" DNAME-EXT" s" -- n" TRUST
s" DNAME-IMM" s" -- n" TRUST
s" DNAME-INT" s" -- n" TRUST
s" DNAME-LEN-MASK" s" -- n" TRUST
s" DNAME-WIDE" s" -- n" TRUST
s" DOESB-CELL" s" -- n" TRUST
s" DP-CELL" s" -- n" TRUST
s" DKIND:ADDR" s" -- n" TRUST
s" DKIND:VAL" s" -- n" TRUST
s" ENGINE-GPR:DSTACK" s" -- n" TRUST
s" ENGINE-GPR:MASK" s" -- n" TRUST
s" INE-CELL" s" -- n" TRUST
s" INP-CELL" s" -- n" TRUST
s" NCOMP-DISPATCH:XT-CELL" s" -- n" TRUST
s" PKG-PRI-CELL" s" -- n" TRUST
s" PKG-PUB-CELL" s" -- n" TRUST
s" PKG-REC-CELL" s" -- n" TRUST
s" REGION" s" -- n" TRUST
s" REPLH-CELL" s" -- n" TRUST
s" SEAL-NDICT-CELL" s" -- n" TRUST
s" TCSIG-A-CELL" s" -- n" TRUST
s" TCSIG-U-CELL" s" -- n" TRUST
s" TRUSTED-CELL" s" -- n" TRUST
s" TSIG-A-CELL" s" -- n" TRUST
s" TSIG-U-CELL" s" -- n" TRUST
s" USE-DEPTH-CELL" s" -- n" TRUST
s" USE-WIDS-OFF" s" -- n" TRUST
s" WIDN-CELL" s" -- n" TRUST
s" DICT-SIZE" s" -- n" TRUST

s" PTR-VARIABLE" s" --" TRUST
s" PERSISTED-PTR-VARIABLE" s" --" TRUST
s" NULL-PTR" s" -- ptr a" TRUST
s" LBUF-CAPTURE-PREPARE" s" --" TRUST
