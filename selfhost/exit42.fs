\ exit42.fs — write the unsigned exit(42) image (macho-min.fs's BUILD) to /tmp/se-out.
\ Used by the drift guard (t-sh-drift.fs) to assert byte-identity with caf's macho.fs.
: SAVE s" /tmp/se-out" PSET BUILD PB 1537 493 open {: fd :} fd MSTART @ MOFF write drop fd close ;
SAVE
