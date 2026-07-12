\ owner-wid-emit-seal.f - erase sealed-owner image-construction authority.
\ Loaded after habu2 compiles every exact caller and xref installs undefine.

package OWNER-WID-EMIT
public
undefine LABELS
undefine ADD-LABEL@
undefine PREFLIGHT-LABEL@
undefine PUBLIC-LABEL@
undefine PRIVATE-LABEL@
undefine ANY-LABEL@
undefine COLD-HOOK!
undefine COLD-HOOK
undefine PROOF-HOOK!
undefine PROOF-HOOK
undefine COLD-RESET
undefine COLD-LABEL@
undefine PRIMS
undefine ROUTINES
private
undefine LPUBQ
undefine LPRIQ
undefine LANYQ
undefine LPREF
undefine LADD
undefine LCOLD
undefine COLD-XT
undefine PROOF-XT
undefine W-COUNT-LDAR
undefine W-COUNT-STLR
undefine COUNT@,
undefine COUNT!,
undefine SCAN-INIT
undefine SCAN-NEXT
undefine ROLE-ROW
undefine ROLE
undefine ANY-ROW
undefine ANY-DONE
undefine ANY
undefine PREFLIGHT-ARGS
undefine PREFLIGHT-ROW
undefine PREFLIGHT
undefine STORE-PAIR
undefine ADD-BODY
undefine ADD-ROUTINE
undefine BPRE?
undefine BPUB?
undefine BPRI?
undefine BANY?
;package

: OWNER-WID-SEAL-ABSENT ( ptr u8 n n -- ) {: a:ptr u:n wid:n :}
   a u wid search-wl 0 <> if
      s" owner-WID emitter authority survived seal: " type a u type cr
      s" owner-WID emitter authority survived seal" 70 die
   then ;

package OWNER-WID-EMIT
public
s" LABELS" get-current OWNER-WID-SEAL-ABSENT
s" ADD-LABEL@" get-current OWNER-WID-SEAL-ABSENT
s" PREFLIGHT-LABEL@" get-current OWNER-WID-SEAL-ABSENT
s" PUBLIC-LABEL@" get-current OWNER-WID-SEAL-ABSENT
s" PRIVATE-LABEL@" get-current OWNER-WID-SEAL-ABSENT
s" ANY-LABEL@" get-current OWNER-WID-SEAL-ABSENT
s" COLD-HOOK!" get-current OWNER-WID-SEAL-ABSENT
s" COLD-HOOK" get-current OWNER-WID-SEAL-ABSENT
s" PROOF-HOOK!" get-current OWNER-WID-SEAL-ABSENT
s" PROOF-HOOK" get-current OWNER-WID-SEAL-ABSENT
s" COLD-RESET" get-current OWNER-WID-SEAL-ABSENT
s" COLD-LABEL@" get-current OWNER-WID-SEAL-ABSENT
s" PRIMS" get-current OWNER-WID-SEAL-ABSENT
s" ROUTINES" get-current OWNER-WID-SEAL-ABSENT
private
s" LPUBQ" get-current OWNER-WID-SEAL-ABSENT
s" LPRIQ" get-current OWNER-WID-SEAL-ABSENT
s" LANYQ" get-current OWNER-WID-SEAL-ABSENT
s" LPREF" get-current OWNER-WID-SEAL-ABSENT
s" LADD" get-current OWNER-WID-SEAL-ABSENT
s" LCOLD" get-current OWNER-WID-SEAL-ABSENT
s" COLD-XT" get-current OWNER-WID-SEAL-ABSENT
s" PROOF-XT" get-current OWNER-WID-SEAL-ABSENT
s" W-COUNT-LDAR" get-current OWNER-WID-SEAL-ABSENT
s" W-COUNT-STLR" get-current OWNER-WID-SEAL-ABSENT
s" COUNT@," get-current OWNER-WID-SEAL-ABSENT
s" COUNT!," get-current OWNER-WID-SEAL-ABSENT
s" SCAN-INIT" get-current OWNER-WID-SEAL-ABSENT
s" SCAN-NEXT" get-current OWNER-WID-SEAL-ABSENT
s" ROLE-ROW" get-current OWNER-WID-SEAL-ABSENT
s" ROLE" get-current OWNER-WID-SEAL-ABSENT
s" ANY-ROW" get-current OWNER-WID-SEAL-ABSENT
s" ANY-DONE" get-current OWNER-WID-SEAL-ABSENT
s" ANY" get-current OWNER-WID-SEAL-ABSENT
s" PREFLIGHT-ARGS" get-current OWNER-WID-SEAL-ABSENT
s" PREFLIGHT-ROW" get-current OWNER-WID-SEAL-ABSENT
s" PREFLIGHT" get-current OWNER-WID-SEAL-ABSENT
s" STORE-PAIR" get-current OWNER-WID-SEAL-ABSENT
s" ADD-BODY" get-current OWNER-WID-SEAL-ABSENT
s" ADD-ROUTINE" get-current OWNER-WID-SEAL-ABSENT
s" BPRE?" get-current OWNER-WID-SEAL-ABSENT
s" BPUB?" get-current OWNER-WID-SEAL-ABSENT
s" BPRI?" get-current OWNER-WID-SEAL-ABSENT
s" BANY?" get-current OWNER-WID-SEAL-ABSENT
;package

undefine OWNER-WID-SEAL-ABSENT
