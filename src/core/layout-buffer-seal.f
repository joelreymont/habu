\ layout-buffer-seal.f — erase the allocator's one-shot checker capability.
\ Loaded after xref installs `undefine`; compiled calls retain their direct xts.

undefine LBUF-PEND!
undefine LBUF-PEND-CLEAR
undefine LBUF-PEND-MATCH?
undefine LBUF-PEND-A
undefine LBUF-PEND-U
