\ maki/examples/nanogpt/data-loader.f - transactional v0 text corpus loader for the
\ char pipeline (dot habu-tiny-shakespeare-char-125d9684; made transactional by
\ habu-make-corpus-load-d6ce6c05). Reads a corpus file with the Habu-native checked
\ reader (lib/fs READ-ALL - no FFI), builds the char vocab, and encodes the whole
\ corpus into a token-id cell buffer (maki/examples/nanogpt/tokenizer.f).
\
\ Transactional publication: TOK-BUILD replaces the process-wide vocabulary, so it
\ is the single commit point. Every operation that can throw - the file read
\ (E-FS-OPEN / E-FS-CAPACITY), the empty-corpus reject (E-DL-EMPTY), and the ids
\ capacity preflight (E-TOK-CAP) - runs BEFORE TOK-BUILD. Because the vocab is built
\ from the same bytes it then encodes and the id capacity is already proven to hold
\ them, the trailing TOK-ENCODE is total (its readiness, capacity, and range checks
\ cannot fire), so nothing after the commit point can fail. Any file, empty,
\ capacity, construction, or encoding error therefore leaves the prior tokenizer
\ state and the caller's already-loaded corpus bit-identical; the only mutation a
\ failed load performs is filling the caller's raw-text buffer via READ-ALL, which
\ is input capture, not a published dataset.
\
\ get_batch (the seeded (B,T) window draw) is maki/examples/nanogpt/batch-loader.f
\ BL-LOAD, REUSED unchanged: it already draws contiguous windows into the B*T-row
\ BL-IDS/BL-TGT buffers with the target shifted one token (y = x >> 1), B outermost
\ (the RxC = B x T row layout the model consumes). This file supplies the TEXT->ids
\ half that BL-LOAD reads; it does not re-implement the window draw (which would
\ duplicate the seeded LCG windowing). After LOAD-CORPUS, the training loop draws a batch
\ with  <ids-buffer> <ntok> B T dim seed BL-LOAD .
\
\ Buffers are CALLER-owned (like BL-LOAD's corpus argument): the module keeps no
\ large corpus storage, so the real tiny-shakespeare corpus (~1.1 MB, kept OUTSIDE
\ the repo) loads by sizing the caller's text/ids buffers, while tests pass small
\ ones. maki -> habu only. data-loader owns -5155.
\
\ NOTE: returning a STRUCTURE corpus (immutable vocab + text/token spans + count)
\ instead of relying on the ambient process-wide tokenizer waits for
\ habu-own-tokenizer-state-d5db1943, which owns the tokenizer state/STRUCTURE rework.

require lib/fs.f
require maki/examples/nanogpt/tokenizer.f

package DATA-LOADER

public

-5155 constant E-DL-EMPTY     \ corpus file is empty (zero bytes -> no tokens)

private

\ Ids capacity preflight: one char token is one byte is one id cell, so a corpus of
\ NTOK tokens needs NTOK id cells. Reject a negative capacity and a destination too
\ small for the whole corpus - E-TOK-CAP, the same code TOK-ENCODE raises - BEFORE
\ TOK-BUILD publishes, so an undersized ids buffer can never replace the vocabulary.
: DL-FIT ( n n -- ) {: ntok:n icap:n :}
   icap 0 <      if E-TOK-CAP throw then
   ntok icap >   if E-TOK-CAP throw then ;

public

\ Read a corpus file, build the char vocab from it, and encode it into a token-id
\ cell buffer, publishing the process-wide tokenizer exactly once at the end. tb/tcap
\ is the caller's byte buffer for the raw text; ib/icap the caller's float-cell buffer
\ for the ids. Returns the corpus length in tokens (== bytes). Fails closed, leaving
\ prior tokenizer state unchanged: missing/unreadable file -> E-FS-OPEN, file over
\ tcap -> E-FS-CAPACITY (lib/fs), empty file -> E-DL-EMPTY, ids buffer smaller than
\ the corpus -> E-TOK-CAP - every one raised before TOK-BUILD.
\
\ Named LOAD-CORPUS, not a bare LOAD. A consumer imports this package public with
\ "using DATA-LOADER" and then calls it as a bareword, but a bare LOAD collides with
\ the global PTX kernel-DSL word LOAD (lib/ptx/tile.f, effect span gridctx -- tile).
\ Globals resolve before "using" publics (docs/forth.md Packages), so once any
\ GPU/eval suite has loaded the kernel DSL into the shared image a bare LOAD would
\ silently bind the kernel word and be rejected by the checker. A distinctive public
\ tail keeps the import unambiguous in every co-loaded image; do not shorten it back.
: LOAD-CORPUS ( ptr u8 n ptr u8 n ptr r n -- n ) {: pa:ptr pu:n tb:ptr tcap:n ib:ptr icap:n :}
   pa pu tb tcap READ-ALL {: nbytes:n :}   \ input capture into tb (not a published dataset)
   nbytes 0 <= if E-DL-EMPTY throw then
   nbytes icap DL-FIT                       \ preflight ids capacity before any publish
   tb nbytes MAKI:TOK-BUILD                 \ single commit point: publish the vocabulary
   tb nbytes ib icap MAKI:TOK-ENCODE ;      \ total after a good build + preflight

;package
