\ maki/data-loader.f - v0 text corpus loader for the char pipeline (dot
\ habu-tiny-shakespeare-char-125d9684). Reads a corpus file with the Habu-native
\ checked reader (lib/fs READ-ALL - no FFI), builds the char vocab and encodes the
\ whole corpus into a token-id cell buffer (maki/tokenizer.f).
\
\ get_batch (the seeded (B,T) window draw) is maki/batch-loader.f BL-LOAD, REUSED
\ unchanged: it already draws contiguous windows into the B*T-row BL-IDS/BL-TGT
\ buffers with the target shifted one token (y = x >> 1), B outermost (the RxC =
\ B x T row layout the model consumes). This file supplies the TEXT->ids half that
\ BL-LOAD reads; it does not re-implement the window draw (which would duplicate the
\ seeded LCG windowing). After DL-LOAD-CORPUS, the training loop draws a batch with
\   <ids-buffer> <ntok> B T dim seed BL-LOAD .
\
\ Buffers are CALLER-owned (like BL-LOAD's corpus argument): the module keeps no
\ large corpus storage, so the real tiny-shakespeare corpus (~1.1 MB, kept OUTSIDE
\ the repo) loads by sizing the caller's text/ids buffers, while tests pass small
\ ones. maki -> habu only. data-loader owns -5155.

require lib/fs.f
require maki/tokenizer.f

-5155 constant E-DL-EMPTY     \ corpus file is empty (zero bytes -> no tokens)

package MAKI
public

\ Read a corpus file, build the char vocab from it, and encode it into a token-id
\ cell buffer. tb/tcap is the caller's byte buffer for the raw text; ib/icap the
\ caller's float-cell buffer for the ids. Returns the corpus length in tokens
\ (== bytes). Fails closed: missing/unreadable file -> E-FS-OPEN (lib/fs), file over
\ tcap -> E-FS-CAPACITY (lib/fs), empty file -> E-DL-EMPTY, ids buffer smaller than
\ the corpus -> E-TOK-CAP (maki/tokenizer.f).
: DL-LOAD-CORPUS ( ptr u8 n ptr u8 n ptr a n -- n ) {: pa:ptr pu:n tb:ptr tcap:n ib:ptr icap:n :}
   pa pu tb tcap READ-ALL {: nbytes:n :}
   nbytes 0 <= if E-DL-EMPTY throw then
   tb nbytes TOK-BUILD
   tb nbytes ib icap TOK-ENCODE ;

;package
