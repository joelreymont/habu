\ t-sh-sign.fs — the standalone SELF-SIGNS a Mach-O with zero gforth and zero
\ external codesign: it builds an exit(42) binary (macho-min.fs) and applies the
\ ad-hoc CodeDirectory post-pass (sign.fs, SHA-256 page hashes from sha256.fs),
\ writing /tmp/se-signed. We then assert Apple's own codesign validates it AND it
\ runs (exit 42). Run: gforth test/t-sh-sign.fs -e bye
require nf.fs
require tester.fs
create SBUF 65536 allot   variable SLN
: S+ {: a u -- }  a  SBUF SLN @ +  u move  u SLN +! ;
: BUILD-SIGNED-STANDALONE ( -- )
   0 SLN !
   s" src/core/sha256.f"   slurp-file S+   s"  " S+
   s" src/arch/arm64/asm.f"      slurp-file S+   s"  " S+
   s" src/arch/arm64/icode.f"    slurp-file S+   s"  " S+
   s" src/arch/arm64/mnem.f"     slurp-file S+   s"  " S+
   s" src/core/util.f"     slurp-file S+   s"  " S+
   s" src/os/macos/macho.f"    slurp-file S+   s"  " S+
   s" src/os/macos/sign2.f"    slurp-file S+   s"  " S+
   s" test/demos/sign2-demo.f"  slurp-file S+
   SBUF SLN @ NF-RUN ;
BUILD-SIGNED-STANDALONE
\ codesign valid?  (exit 0 = valid)
: CS-OK? ( -- f )  s" codesign -v /tmp/se-signed 2>/dev/null" system $? 0= ;
\ runs and exits 42?
: RC ( -- n )  s" /tmp/se-signed; echo $? > /tmp/se-sig-rc" system
   s" /tmp/se-sig-rc" slurp-file  s>number? 2drop ;
T{ CS-OK? -> true }T
T{ RC -> 42 }T
