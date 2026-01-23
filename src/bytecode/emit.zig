//! Bytecode emitter - compiles IR to bytecode
//!
//! Walks the IR tree and emits stack-based bytecode.
//! Handles:
//! - Forward jump patching
//! - Constant pool management
//! - Closure capture indices

const std = @import("std");
const ir = @import("../compiler/ir.zig");
const Ir = ir.Ir;
const opcodes = @import("opcodes.zig");
const Op = opcodes.Op;
const runtime = @import("../runtime/runtime.zig");
const Chunk = runtime.Chunk;
const Value = runtime.Value;
const Heap = runtime.Heap;

pub const Error = error{
    OutOfMemory,
    TooManyConstants,
    TooManyLocals,
    JumpTooLong,
    InvalidIr,
    NoHeap,
};

/// Block info for tracking named exits
const BlockInfo = struct {
    name: []const u8,
    /// Pending jump locations to patch when block ends
    pending_exits: std.ArrayList(usize),
};

/// Pending go jump entry
const PendingGoJump = struct {
    tag_idx: usize,
    jump_loc: usize,
};

/// Unified control stack entry (for blocks, unwind-protects, tagbodies)
const ControlEntry = union(enum) {
    block: struct {
        name: []const u8,
        pending_exits: std.ArrayList(usize),
    },
    unwind_protect: struct {
        cleanup: *const Ir,
    },
    tagbody: struct {
        /// Tag names
        tags: []const []const u8,
        /// Bytecode offset of each tag (populated during emission)
        tag_offsets: []usize,
        /// Pending go jumps that need patching
        pending_jumps: std.ArrayList(PendingGoJump),
    },
};

/// Bytecode emitter
pub const Emitter = struct {
    allocator: std.mem.Allocator,
    /// Bytecode buffer
    code: std.ArrayList(u8),
    /// Constant pool (raw u64 values)
    constants: std.ArrayList(u64),
    /// Constant deduplication map: value -> index (O(1) lookup)
    constant_map: std.AutoHashMap(u64, u16),
    /// Child chunks (for lambdas) - GC-managed
    child_chunks: std.ArrayList(Value),
    /// Number of local variables (set for lambdas, computed for REPL expressions)
    num_locals: u8,
    /// Maximum local index used (for computing num_locals in REPL)
    max_local_idx: u8,
    /// Function arity (required params)
    arity: u8,
    /// Number of optional parameters
    optional_count: u8,
    /// Number of keyword parameters
    key_count: u8,
    /// Whether function accepts rest parameter
    has_rest: bool,
    /// Function name
    name: []const u8,
    /// Captured variables (for inner lambdas)
    captures: []const Ir.Capture,
    /// Heap for symbol interning (optional)
    heap: ?*Heap,
    /// Unified control stack for blocks and unwind-protects
    control_stack: std.ArrayList(ControlEntry),

    pub fn init(allocator: std.mem.Allocator) Emitter {
        return .{
            .allocator = allocator,
            .code = std.ArrayList(u8){},
            .constants = std.ArrayList(u64){},
            .constant_map = std.AutoHashMap(u64, u16).init(allocator),
            .child_chunks = std.ArrayList(Value){},
            .num_locals = 0,
            .max_local_idx = 0,
            .arity = 0,
            .optional_count = 0,
            .key_count = 0,
            .has_rest = false,
            .name = "",
            .captures = &[_]Ir.Capture{},
            .heap = null,
            .control_stack = std.ArrayList(ControlEntry){},
        };
    }

    pub fn initWithHeap(allocator: std.mem.Allocator, heap: *Heap) Emitter {
        return .{
            .allocator = allocator,
            .code = std.ArrayList(u8){},
            .constants = std.ArrayList(u64){},
            .constant_map = std.AutoHashMap(u64, u16).init(allocator),
            .child_chunks = std.ArrayList(Value){},
            .num_locals = 0,
            .max_local_idx = 0,
            .arity = 0,
            .optional_count = 0,
            .key_count = 0,
            .has_rest = false,
            .name = "",
            .captures = &[_]Ir.Capture{},
            .heap = heap,
            .control_stack = std.ArrayList(ControlEntry){},
        };
    }

    pub fn deinit(self: *Emitter) void {
        self.code.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        self.constant_map.deinit();
        // Child chunks are GC-managed, no manual free needed
        self.child_chunks.deinit(self.allocator);
        // Free control stack
        for (self.control_stack.items) |*entry| {
            switch (entry.*) {
                .block => |*b| b.pending_exits.deinit(self.allocator),
                .unwind_protect => {},
                .tagbody => |*tb| {
                    self.allocator.free(tb.tag_offsets);
                    tb.pending_jumps.deinit(self.allocator);
                },
            }
        }
        self.control_stack.deinit(self.allocator);
    }

    /// Compute the maximum local index used in an IR tree
    /// Returns 0 if no locals are used
    fn computeMaxLocalIndex(node: *const Ir) u8 {
        return computeMaxLocalIndexImpl(node, 0);
    }

    fn computeMaxLocalIndexImpl(node: *const Ir, current_max: u8) u8 {
        var max_idx = current_max;

        switch (node.*) {
            // Let bindings - the key case
            .let => |l| {
                for (l.bindings) |b| {
                    const idx: u8 = @intCast(b.index);
                    max_idx = @max(max_idx, idx + 1);
                    max_idx = computeMaxLocalIndexImpl(b.value, max_idx);
                }
                max_idx = computeMaxLocalIndexImpl(l.body, max_idx);
            },

            // Control flow with multiple branches
            .@"if" => |i| {
                max_idx = computeMaxLocalIndexImpl(i.cond, max_idx);
                max_idx = computeMaxLocalIndexImpl(i.then_branch, max_idx);
                max_idx = computeMaxLocalIndexImpl(i.else_branch, max_idx);
            },
            .progn => |exprs| {
                for (exprs) |e| max_idx = computeMaxLocalIndexImpl(e, max_idx);
            },
            .loop => |l| {
                max_idx = computeMaxLocalIndexImpl(l.cond, max_idx);
                max_idx = computeMaxLocalIndexImpl(l.body, max_idx);
            },
            .block => |b| {
                max_idx = computeMaxLocalIndexImpl(b.body, max_idx);
            },
            .return_from => |r| {
                max_idx = computeMaxLocalIndexImpl(r.value, max_idx);
            },
            .unwind_protect => |u| {
                max_idx = computeMaxLocalIndexImpl(u.protected, max_idx);
                max_idx = computeMaxLocalIndexImpl(u.cleanup, max_idx);
            },
            .@"catch" => |c| {
                max_idx = computeMaxLocalIndexImpl(c.tag, max_idx);
                max_idx = computeMaxLocalIndexImpl(c.body, max_idx);
            },
            .throw => |t| {
                max_idx = computeMaxLocalIndexImpl(t.tag, max_idx);
                max_idx = computeMaxLocalIndexImpl(t.value, max_idx);
            },
            .handler_case => |hc| {
                max_idx = computeMaxLocalIndexImpl(hc.tag, max_idx);
                max_idx = computeMaxLocalIndexImpl(hc.body, max_idx);
                max_idx = computeMaxLocalIndexImpl(hc.handler, max_idx);
            },
            .restart_case => |rc| {
                max_idx = computeMaxLocalIndexImpl(rc.body, max_idx);
                for (rc.restarts) |r| {
                    max_idx = computeMaxLocalIndexImpl(r.name, max_idx);
                    max_idx = computeMaxLocalIndexImpl(r.handler, max_idx);
                }
            },
            .invoke_restart => |inv| {
                max_idx = computeMaxLocalIndexImpl(inv.name, max_idx);
                max_idx = computeMaxLocalIndexImpl(inv.value, max_idx);
            },
            .find_restart => |fr| max_idx = computeMaxLocalIndexImpl(fr.operand, max_idx),
            .tagbody => |tb| {
                for (tb.segments) |seg| max_idx = computeMaxLocalIndexImpl(seg, max_idx);
            },
            .go => {},
            .mv_bind => |m| {
                // Reserve slots for all vars (they start at index 0)
                const var_count: u8 = @intCast(m.vars.len);
                max_idx = @max(max_idx, var_count);
                max_idx = computeMaxLocalIndexImpl(m.expr, max_idx);
                max_idx = computeMaxLocalIndexImpl(m.body, max_idx);
            },
            .values => |v| {
                for (v) |e| max_idx = computeMaxLocalIndexImpl(e, max_idx);
            },
            .values_list => |op| {
                max_idx = computeMaxLocalIndexImpl(op.operand, max_idx);
            },
            .mv_list => |m| {
                max_idx = computeMaxLocalIndexImpl(m.expr, max_idx);
            },
            .mv_call => |m| {
                max_idx = computeMaxLocalIndexImpl(m.func, max_idx);
                for (m.forms) |f| max_idx = computeMaxLocalIndexImpl(f, max_idx);
            },

            // Function calls
            .call => |c| {
                max_idx = computeMaxLocalIndexImpl(c.func, max_idx);
                for (c.args) |a| max_idx = computeMaxLocalIndexImpl(a, max_idx);
            },
            .tailcall => |c| {
                max_idx = computeMaxLocalIndexImpl(c.func, max_idx);
                for (c.args) |a| max_idx = computeMaxLocalIndexImpl(a, max_idx);
            },
            .apply => |a| {
                max_idx = computeMaxLocalIndexImpl(a.func, max_idx);
                max_idx = computeMaxLocalIndexImpl(a.args, max_idx);
            },

            // Set has a nested value
            .set => |s| max_idx = computeMaxLocalIndexImpl(s.value, max_idx),
            .define => |d| max_idx = computeMaxLocalIndexImpl(d.value, max_idx),

            // Lambda creates its own frame, don't recurse
            .lambda => {},

            // BinaryOp cases - all have left/right
            .add,
            .sub,
            .mul,
            .div,
            .mod,
            .eq,
            .num_eq,
            .lt,
            .gt,
            .le,
            .ge,
            .equal,
            .eql,
            .cons,
            .append,
            .assoc,
            .assoc_eql,
            .assoc_equal,
            .rplaca,
            .rplacd,
            .nth,
            .nthcdr,
            .member,
            .member_eql,
            .member_equal,
            .find,
            .find_eq,
            .find_equal,
            .position,
            .position_eq,
            .position_equal,
            .count,
            .count_eq,
            .count_equal,
            .remove,
            .remove_eq,
            .remove_equal,
            .char_eq,
            .char_lt,
            .char_gt,
            .str_eq,
            .str_concat,
            .logand,
            .logior,
            .logxor,
            .ash,
            .vec_ref,
            .box_set,
            .str_ref,
            .typep,
            .make_string,
            .write_file,
            => |op| {
                max_idx = computeMaxLocalIndexImpl(op.left, max_idx);
                max_idx = computeMaxLocalIndexImpl(op.right, max_idx);
            },

            // UnaryOp cases - all have operand
            .not,
            .car,
            .cdr,
            .consp,
            .symbolp,
            .numberp,
            .stringp,
            .vectorp,
            .listp,
            .atom,
            .closurep,
            .keywordp,
            .nilp,
            .characterp,
            .floatp,
            .boundp,
            .fboundp,
            .zerop,
            .plusp,
            .minusp,
            .evenp,
            .oddp,
            .abs,
            .length,
            .reverse,
            .last,
            .sym_name,
            .char_code,
            .code_char,
            .char_upcase,
            .char_downcase,
            .digit_char_p,
            .alpha_char_p,
            .print,
            .random,
            .intern,
            .type_of,
            .error_user,
            .read_from_string,
            .load,
            .unread_char,
            .eval,
            .macroexpand,
            .princ,
            .write_char,
            .parse_integer,
            .write_to_string,
            .lognot,
            .read_file,
            .list_to_string,
            .string_upcase,
            .string_downcase,
            .symbol_value,
            .symbol_function,
            .vec_len,
            .make_box,
            .box_ref,
            .str_len,
            .assert_fixnum,
            .assert_cons,
            .assert_symbol,
            .assert_string,
            .assert_vector,
            .assert_closure,
            .assert_non_nil,
            .assert_list,
            => |op| {
                max_idx = computeMaxLocalIndexImpl(op.operand, max_idx);
            },

            // These have their own struct with operand field
            .hash_count => |h| max_idx = computeMaxLocalIndexImpl(h.operand, max_idx),
            .hash_capacity => |h| max_idx = computeMaxLocalIndexImpl(h.operand, max_idx),
            .hashtablep => |h| max_idx = computeMaxLocalIndexImpl(h.operand, max_idx),
            .rationalp => |r| max_idx = computeMaxLocalIndexImpl(r.operand, max_idx),
            .complexp => |c| max_idx = computeMaxLocalIndexImpl(c.operand, max_idx),
            .make_complex => |mc| {
                max_idx = computeMaxLocalIndexImpl(mc.left, max_idx);
                max_idx = computeMaxLocalIndexImpl(mc.right, max_idx);
            },
            .real_part => |rp| max_idx = computeMaxLocalIndexImpl(rp.operand, max_idx),
            .imag_part => |ip| max_idx = computeMaxLocalIndexImpl(ip.operand, max_idx),
            .numerator => |n| max_idx = computeMaxLocalIndexImpl(n.operand, max_idx),
            .denominator => |d| max_idx = computeMaxLocalIndexImpl(d.operand, max_idx),
            .rationalize => |r| max_idx = computeMaxLocalIndexImpl(r.operand, max_idx),
            .rational => |r| max_idx = computeMaxLocalIndexImpl(r.operand, max_idx),
            .struct_p => |sp| max_idx = computeMaxLocalIndexImpl(sp.operand, max_idx),
            .streamp => |s| max_idx = computeMaxLocalIndexImpl(s.operand, max_idx),
            .input_stream_p => |s| max_idx = computeMaxLocalIndexImpl(s.operand, max_idx),
            .output_stream_p => |s| max_idx = computeMaxLocalIndexImpl(s.operand, max_idx),
            .make_string_input_stream => |s| max_idx = computeMaxLocalIndexImpl(s.operand, max_idx),
            .make_string_output_stream => {}, // No operands
            .get_output_stream_string => |s| max_idx = computeMaxLocalIndexImpl(s.operand, max_idx),
            .write_to_stream => |w| {
                max_idx = computeMaxLocalIndexImpl(w.left, max_idx);
                max_idx = computeMaxLocalIndexImpl(w.right, max_idx);
            },

            // List/array nodes - recurse into all elements
            .list, .vec => |elements| {
                for (elements) |elem| max_idx = computeMaxLocalIndexImpl(elem, max_idx);
            },
            .format => |f| {
                max_idx = computeMaxLocalIndexImpl(f.dest, max_idx);
                max_idx = computeMaxLocalIndexImpl(f.control, max_idx);
                for (f.args) |arg| max_idx = computeMaxLocalIndexImpl(arg, max_idx);
            },
            .hash_get => |h| {
                max_idx = computeMaxLocalIndexImpl(h.table, max_idx);
                max_idx = computeMaxLocalIndexImpl(h.key, max_idx);
                if (h.default) |def| max_idx = computeMaxLocalIndexImpl(def, max_idx);
            },
            .hash_set => |h| {
                max_idx = computeMaxLocalIndexImpl(h.table, max_idx);
                max_idx = computeMaxLocalIndexImpl(h.key, max_idx);
                max_idx = computeMaxLocalIndexImpl(h.value, max_idx);
            },
            .hash_rem => |h| {
                max_idx = computeMaxLocalIndexImpl(h.table, max_idx);
                max_idx = computeMaxLocalIndexImpl(h.key, max_idx);
            },
            .vec_new => |v| {
                max_idx = computeMaxLocalIndexImpl(v.size, max_idx);
                if (v.init) |init_val| max_idx = computeMaxLocalIndexImpl(init_val, max_idx);
            },
            .arr_new => |a| {
                for (a.dimensions) |dim| {
                    max_idx = computeMaxLocalIndexImpl(dim, max_idx);
                }
                if (a.init) |init_val| max_idx = computeMaxLocalIndexImpl(init_val, max_idx);
            },
            .arr_ref => |a| {
                max_idx = computeMaxLocalIndexImpl(a.array, max_idx);
                for (a.subscripts) |sub| {
                    max_idx = computeMaxLocalIndexImpl(sub, max_idx);
                }
            },
            .arr_set => |a| {
                max_idx = computeMaxLocalIndexImpl(a.array, max_idx);
                for (a.subscripts) |sub| {
                    max_idx = computeMaxLocalIndexImpl(sub, max_idx);
                }
                max_idx = computeMaxLocalIndexImpl(a.value, max_idx);
            },
            .vec_set => |v| {
                max_idx = computeMaxLocalIndexImpl(v.vec, max_idx);
                max_idx = computeMaxLocalIndexImpl(v.index, max_idx);
                max_idx = computeMaxLocalIndexImpl(v.value, max_idx);
            },
            .str_set => |s| {
                max_idx = computeMaxLocalIndexImpl(s.str, max_idx);
                max_idx = computeMaxLocalIndexImpl(s.index, max_idx);
                max_idx = computeMaxLocalIndexImpl(s.value, max_idx);
            },
            .substring => |s| {
                max_idx = computeMaxLocalIndexImpl(s.str, max_idx);
                max_idx = computeMaxLocalIndexImpl(s.start, max_idx);
                max_idx = computeMaxLocalIndexImpl(s.end, max_idx);
            },

            // Other nodes don't contain nested let bindings or we don't need to recurse
            else => {},
        }

        return max_idx;
    }

    /// Emit bytecode for an IR node
    pub fn emit(self: *Emitter, node: *const Ir) Error!void {
        // Compute max local index on first call (when code is empty and not yet computed)
        // Check max_local_idx == 0 to avoid recomputing on recursive emit calls
        if (self.code.items.len == 0 and self.num_locals == 0 and self.max_local_idx == 0) {
            self.max_local_idx = computeMaxLocalIndex(node);
        }
        switch (node.*) {
            .lit => |v| try self.emitLiteral(v),
            .quote_sym => |name| try self.emitQuoteSym(name),
            .quote => |inner| try self.emitQuote(inner),
            .@"var" => |v| try self.emitVar(v.depth, v.index),
            .set => |s| try self.emitSet(s),
            .global_ref => |g| {
                const idx = g.index;
                try self.emitGlobalRef(idx);
            },
            .define => |d| try self.emitDefine(d),
            .let => |l| try self.emitLet(l),
            .lambda => |lam| try self.emitLambda(lam),
            .@"if" => |i| try self.emitIf(i),
            .progn => |exprs| try self.emitProgn(exprs),
            .loop => |l| try self.emitLoop(l),
            .block => |b| try self.emitBlock(b),
            .return_from => |r| try self.emitReturnFrom(r),
            .unwind_protect => |u| try self.emitUnwindProtect(u),
            .@"catch" => |c| try self.emitCatch(c),
            .handler_case => |hc| try self.emitHandlerCase(hc),
            .throw => |t| try self.emitThrow(t),
            .handler_bind => |hb| try self.emitHandlerBind(hb),
            .restart_case => |rc| try self.emitRestartCase(rc),
            .invoke_restart => |inv| try self.emitInvokeRestart(inv),
            .find_restart => |fr| try self.emitUnaryOp(fr.operand, .find_restart),
            .tagbody => |tb| try self.emitTagbody(tb),
            .go => |g| try self.emitGo(g),
            .progv => |p| try self.emitProgv(p),
            .values => |v| try self.emitValues(v),
            .values_list => |op| try self.emitUnaryOp(op.operand, .values_list),
            .mv_bind => |m| try self.emitMvBind(m),
            .mv_list => |m| try self.emitMvList(m),
            .mv_call => |m| try self.emitMvCall(m),
            .format => |f| try self.emitFormat(f),
            .make_hash => |h| try self.emitMakeHash(h),
            .hash_get => |h| try self.emitHashGet(h),
            .hash_set => |h| try self.emitHashSet(h),
            .hash_rem => |h| try self.emitHashRem(h),
            .hash_count => |h| try self.emitUnaryOp(h.operand, .hash_count),
            .hash_capacity => |h| try self.emitUnaryOp(h.operand, .hash_capacity),
            .hash_clear => |h| try self.emitUnaryOp(h.operand, .hash_clear),
            .hash_test => |h| try self.emitUnaryOp(h.operand, .hash_test),
            .hash_keys => |h| try self.emitUnaryOp(h.operand, .hash_keys),
            .hash_alist => |h| try self.emitUnaryOp(h.operand, .hash_alist),
            .sxhash => |h| try self.emitUnaryOp(h.operand, .sxhash),
            .hashtablep => |h| try self.emitUnaryOp(h.operand, .hashtablep),
            .rationalp => |r| try self.emitUnaryOp(r.operand, .rationalp),
            .complexp => |c| try self.emitUnaryOp(c.operand, .complexp),
            .make_complex => |mc| try self.emitBinaryOp(mc, .make_complex),
            .real_part => |rp| try self.emitUnaryOp(rp.operand, .real_part),
            .imag_part => |ip| try self.emitUnaryOp(ip.operand, .imag_part),
            .numerator => |n| try self.emitUnaryOp(n.operand, .numerator),
            .denominator => |d| try self.emitUnaryOp(d.operand, .denominator),
            .rationalize => |r| try self.emitUnaryOp(r.operand, .rational),
            .rational => |r| try self.emitUnaryOp(r.operand, .rational),
            .streamp => |s| try self.emitUnaryOp(s.operand, .streamp),
            .input_stream_p => |s| try self.emitUnaryOp(s.operand, .input_stream_p),
            .output_stream_p => |s| try self.emitUnaryOp(s.operand, .output_stream_p),
            .make_string_input_stream => |s| try self.emitUnaryOp(s.operand, .make_string_input_stream),
            .make_string_output_stream => try self.emitOp(.make_string_output_stream),
            .get_output_stream_string => |s| try self.emitUnaryOp(s.operand, .get_output_stream_string),
            .write_to_stream => |w| try self.emitBinaryOp(w, .write_to_stream),
            .pathname_host => |p| try self.emitUnaryOp(p.operand, .pathname_host),
            .pathname_device => |p| try self.emitUnaryOp(p.operand, .pathname_device),
            .pathname_directory => |p| try self.emitUnaryOp(p.operand, .pathname_directory),
            .pathname_name => |p| try self.emitUnaryOp(p.operand, .pathname_name),
            .pathname_type => |p| try self.emitUnaryOp(p.operand, .pathname_type),
            .pathname_version => |p| try self.emitUnaryOp(p.operand, .pathname_version),
            .package_symbols_table => |p| try self.emitUnaryOp(p.operand, .package_symbols_table),
            .package_exports_table => |p| try self.emitUnaryOp(p.operand, .package_exports_table),
            .call => |c| try self.emitCall(c, false),
            .tailcall => |c| try self.emitCall(c, true),
            .apply => |a| try self.emitApply(a),

            // Arithmetic
            .add => |op| try self.emitBinaryOp(op, .add),
            .sub => |op| try self.emitBinaryOp(op, .sub),
            .mul => |op| try self.emitBinaryOp(op, .mul),
            .div => |op| try self.emitBinaryOp(op, .div),
            .mod => |op| try self.emitBinaryOp(op, .mod),
            .quot => |op| try self.emitBinaryOp(op, .quot),
            .rem => |op| try self.emitBinaryOp(op, .rem),

            // Comparison
            .eq => |op| try self.emitBinaryOp(op, .eq),
            .lt => |op| try self.emitBinaryOp(op, .lt),
            .gt => |op| try self.emitBinaryOp(op, .gt),
            .le => |op| try self.emitBinaryOp(op, .le),
            .ge => |op| try self.emitBinaryOp(op, .ge),
            .num_eq => |op| try self.emitBinaryOp(op, .num_eq),
            .equal => |op| try self.emitBinaryOp(op, .equal),
            .eql => |op| try self.emitBinaryOp(op, .eql),

            // Logic
            .not => |op| try self.emitUnaryOp(op.operand, .not),

            // List operations
            .cons => |op| try self.emitBinaryOp(op, .cons),
            .car => |op| try self.emitUnaryOp(op.operand, .car),
            .cdr => |op| try self.emitUnaryOp(op.operand, .cdr),
            .list => |elements| try self.emitList(elements),
            .list_star => |elements| try self.emitListStar(elements),
            .append => |op| try self.emitBinaryOp(op, .append_lists),
            .length => |op| try self.emitUnaryOp(op.operand, .list_length),
            .reverse => |op| try self.emitUnaryOp(op.operand, .list_reverse),
            .nth => |op| try self.emitBinaryOp(op, .list_nth),
            .nthcdr => |op| try self.emitBinaryOp(op, .list_nthcdr),
            .last => |op| try self.emitUnaryOp(op.operand, .list_last),
            .member => |op| try self.emitBinaryOp(op, .list_member),
            .member_eql => |op| try self.emitBinaryOp(op, .list_member_eql),
            .member_equal => |op| try self.emitBinaryOp(op, .list_member_equal),
            .assoc => |op| try self.emitBinaryOp(op, .assoc),
            .assoc_eql => |op| try self.emitBinaryOp(op, .assoc_eql),
            .assoc_equal => |op| try self.emitBinaryOp(op, .assoc_equal),
            .find => |op| try self.emitBinaryOp(op, .list_find),
            .find_eq => |op| try self.emitBinaryOp(op, .list_find_eq),
            .find_equal => |op| try self.emitBinaryOp(op, .list_find_equal),
            .position, .position_eq, .position_equal => |op| try self.emitBinaryOp(op, .list_position),
            .count => |op| try self.emitBinaryOp(op, .list_count),
            .count_eq => |op| try self.emitBinaryOp(op, .list_count_eq),
            .count_equal => |op| try self.emitBinaryOp(op, .list_count_equal),
            .remove => |op| try self.emitBinaryOp(op, .list_remove),
            .remove_eq => |op| try self.emitBinaryOp(op, .list_remove_eq),
            .remove_equal => |op| try self.emitBinaryOp(op, .list_remove_equal),
            .rplaca => |op| try self.emitBinaryOp(op, .rplaca),
            .rplacd => |op| try self.emitBinaryOp(op, .rplacd),

            // Type predicates
            .consp => |op| try self.emitUnaryOp(op.operand, .consp),
            .symbolp => |op| try self.emitUnaryOp(op.operand, .symbolp),
            .numberp => |op| try self.emitUnaryOp(op.operand, .numberp),
            .stringp => |op| try self.emitUnaryOp(op.operand, .stringp),
            .vectorp => |op| try self.emitUnaryOp(op.operand, .vectorp),
            .closurep => |op| try self.emitUnaryOp(op.operand, .closurep),
            .keywordp => |op| try self.emitUnaryOp(op.operand, .keywordp),
            .method_qualifiers => |op| try self.emitUnaryOp(op.operand, .method_qualifiers),
            .method_specializers => |op| try self.emitUnaryOp(op.operand, .method_specializers),
            .method_function => |op| try self.emitUnaryOp(op.operand, .method_function),
            .generic_function_methods => |op| try self.emitUnaryOp(op.operand, .generic_function_methods),
            .generic_function_lambda_list => |op| try self.emitUnaryOp(op.operand, .generic_function_lambda_list),
            .generic_function_name => |op| try self.emitUnaryOp(op.operand, .generic_function_name),
            .nilp => |op| try self.emitUnaryOp(op.operand, .nilp),
            .characterp => |op| try self.emitUnaryOp(op.operand, .characterp),
            .floatp => |op| try self.emitUnaryOp(op.operand, .floatp),
            .listp => |op| try self.emitUnaryOp(op.operand, .listp),
            .atom => |op| try self.emitUnaryOp(op.operand, .atom),
            .struct_p => |sp| try self.emitStructP(sp.operand, sp.struct_name),

            // Character operations
            .char_code => |op| try self.emitUnaryOp(op.operand, .char_code),
            .code_char => |op| try self.emitUnaryOp(op.operand, .code_char),
            .char_eq => |op| try self.emitBinaryOp(op, .char_eq),
            .char_lt => |op| try self.emitBinaryOp(op, .char_lt),
            .char_gt => |op| try self.emitBinaryOp(op, .char_gt),
            .read_char => try self.emitOp(.read_char),
            .peek_char => try self.emitOp(.peek_char),
            .unread_char => |op| try self.emitUnaryOp(op.operand, .unread_char),
            .read => try self.emitOp(.read),
            .read_from_string => |op| try self.emitUnaryOp(op.operand, .read_from_string),
            .load => |op| try self.emitUnaryOp(op.operand, .load),
            .eval => |op| try self.emitUnaryOp(op.operand, .eval),
            .gensym => |op| {
                if (op.operand) |operand| {
                    try self.emit(operand);
                } else {
                    try self.emitOp(.push_nil);
                }
                try self.emitOp(.gensym);
            },
            .macroexpand => |op| try self.emitUnaryOp(op.operand, .macroexpand),
            .princ => |op| try self.emitUnaryOp(op.operand, .princ),
            .terpri => try self.emitOp(.terpri),
            .write_char => |op| try self.emitUnaryOp(op.operand, .write_char),
            .char_upcase => |op| try self.emitUnaryOp(op.operand, .char_upcase),
            .char_downcase => |op| try self.emitUnaryOp(op.operand, .char_downcase),
            .digit_char_p => |op| try self.emitUnaryOp(op.operand, .digit_char_p),
            .alpha_char_p => |op| try self.emitUnaryOp(op.operand, .alpha_char_p),
            .parse_integer => |op| try self.emitUnaryOp(op.operand, .parse_integer),
            .write_to_string => |op| try self.emitUnaryOp(op.operand, .write_to_string),
            .logand => |op| try self.emitBinaryOp(op, .logand),
            .logior => |op| try self.emitBinaryOp(op, .logior),
            .logxor => |op| try self.emitBinaryOp(op, .logxor),
            .lognot => |op| try self.emitUnaryOp(op.operand, .lognot),
            .ash => |op| try self.emitBinaryOp(op, .ash),
            .lognand => |op| try self.emitBinaryOp(op, .lognand),
            .lognor => |op| try self.emitBinaryOp(op, .lognor),
            .logandc1 => |op| try self.emitBinaryOp(op, .logandc1),
            .logandc2 => |op| try self.emitBinaryOp(op, .logandc2),
            .logeqv => |op| try self.emitBinaryOp(op, .logeqv),
            .logbitp => |op| try self.emitBinaryOp(op, .logbitp),
            .logcount => |op| try self.emitUnaryOp(op.operand, .logcount),
            .integer_length => |op| try self.emitUnaryOp(op.operand, .integer_length),
            .read_file => |op| try self.emitUnaryOp(op.operand, .read_file),
            .write_file => |op| try self.emitBinaryOp(op, .write_file),
            .make_string => |op| try self.emitBinaryOp(op, .make_string),
            .list_to_string => |op| try self.emitUnaryOp(op.operand, .list_to_string),
            .string_upcase => |op| try self.emitUnaryOp(op.operand, .string_upcase),
            .string_downcase => |op| try self.emitUnaryOp(op.operand, .string_downcase),
            .boundp => |op| try self.emitUnaryOp(op.operand, .boundp),
            .fboundp => |op| try self.emitUnaryOp(op.operand, .fboundp),
            .symbol_value => |op| try self.emitUnaryOp(op.operand, .symbol_value),
            .symbol_function => |op| try self.emitUnaryOp(op.operand, .symbol_function),
            .typep => |op| try self.emitBinaryOp(op, .typep),

            // Numeric predicates
            .abs => |op| try self.emitUnaryOp(op.operand, .abs),
            .zerop => |op| try self.emitUnaryOp(op.operand, .zerop),
            .plusp => |op| try self.emitUnaryOp(op.operand, .plusp),
            .minusp => |op| try self.emitUnaryOp(op.operand, .minusp),
            .evenp => |op| try self.emitUnaryOp(op.operand, .evenp),
            .oddp => |op| try self.emitUnaryOp(op.operand, .oddp),

            // Math functions
            .sqrt => |op| try self.emitUnaryOp(op.operand, .sqrt),
            .sin => |op| try self.emitUnaryOp(op.operand, .sin),
            .cos => |op| try self.emitUnaryOp(op.operand, .cos),
            .tan => |op| try self.emitUnaryOp(op.operand, .tan),
            .asin => |op| try self.emitMathExt(op.operand, .asin),
            .acos => |op| try self.emitMathExt(op.operand, .acos),
            .atan => |op| try self.emitMathExt(op.operand, .atan),
            .atan2 => |op| try self.emitMathExt2(op, .atan2),
            .sinh => |op| try self.emitMathExt(op.operand, .sinh),
            .cosh => |op| try self.emitMathExt(op.operand, .cosh),
            .tanh => |op| try self.emitMathExt(op.operand, .tanh),
            .asinh => |op| try self.emitMathExt(op.operand, .asinh),
            .acosh => |op| try self.emitMathExt(op.operand, .acosh),
            .atanh => |op| try self.emitMathExt(op.operand, .atanh),
            .exp => |op| try self.emitUnaryOp(op.operand, .exp),
            .log => |op| try self.emitUnaryOp(op.operand, .log),
            .floor => |op| try self.emitUnaryOp(op.operand, .floor),
            .ceiling => |op| try self.emitUnaryOp(op.operand, .ceiling),
            .round => |op| try self.emitUnaryOp(op.operand, .round),

            // Vector operations
            .vec_new => |v| try self.emitVecNew(v),
            .vec => |elements| try self.emitVec(elements),
            .vec_ref => |op| try self.emitBinaryOp(op, .vec_ref),
            .vec_set => |v| try self.emitVecSet(v),
            .vec_len => |op| try self.emitUnaryOp(op.operand, .vec_len),
            .vec_fill_ptr => |op| try self.emitUnaryOp(op.operand, .vec_fill_ptr),
            .vec_push => |op| try self.emitBinaryOp(op, .vec_push),
            .vec_push_ext => |op| try self.emitTernaryOp(op, .vec_push_ext),
            .vec_pop => |op| try self.emitUnaryOp(op.operand, .vec_pop),

            // Array operations
            .arr_new => |a| try self.emitArrNew(a),
            .arr_ref => |a| try self.emitArrRef(a),
            .arr_set => |a| try self.emitArrSet(a),

            // CLOS operations
            .slot_value => |op| try self.emitBinaryOp(op, .slot_value),
            .set_slot_value => |op| try self.emitTernaryOp(op, .set_slot_value),
            .class_of => |op| try self.emitUnaryOp(op.operand, .class_of),
            .find_class => |op| try self.emitUnaryOp(op.operand, .find_class),
            .class_name => |op| try self.emitUnaryOp(op.operand, .class_name),
            .make_generic_function => |op| try self.emitBinaryOp(op, .make_generic_function),
            .make_unbound => try self.emitOp(.make_unbound),
            .make_method => |op| try self.emitQuaternaryOp(op, .make_method),
            .set_gf_dispatcher => |op| try self.emitBinaryOp(op, .set_gf_dispatcher),

            // Box operations (mutable cells)
            .make_box => |op| try self.emitUnaryOp(op.operand, .make_box),
            .box_ref => |op| try self.emitUnaryOp(op.operand, .box_ref),
            .box_set => |op| try self.emitBinaryOp(op, .box_set),

            // String operations
            .str_ref => |op| try self.emitBinaryOp(op, .str_ref),
            .str_len => |op| try self.emitUnaryOp(op.operand, .str_len),
            .str_set => |s| try self.emitStrSet(s),
            .str_concat => |op| try self.emitBinaryOp(op, .str_concat),
            .str_eq => |op| try self.emitBinaryOp(op, .str_eq),
            .str_lt => |op| try self.emitBinaryOp(op, .str_lt),
            .str_gt => |op| try self.emitBinaryOp(op, .str_gt),
            .str_le => |op| try self.emitBinaryOp(op, .str_le),
            .str_ge => |op| try self.emitBinaryOp(op, .str_ge),
            .substring => |op| {
                try self.emit(op.str);
                try self.emit(op.start);
                try self.emit(op.end);
                try self.emitOp(.substring);
            },

            // I/O
            .write => |op| try self.emitUnaryOp(op.operand, .write),
            .print => |op| try self.emitUnaryOp(op.operand, .print),
            .random => |op| try self.emitUnaryOp(op.operand, .random),
            .random_seed => |op| try self.emitUnaryOp(op.operand, .random_seed),
            .intern => |op| try self.emitUnaryOp(op.operand, .intern),
            .unintern => |op| try self.emitBinaryOp(op, .unintern),
            .find_symbol => |op| try self.emitBinaryOp(op, .find_symbol),
            .sym_name => |op| try self.emitUnaryOp(op.operand, .sym_name),
            .get => |op| try self.emitBinaryOp(op, .get),
            .put => |op| try self.emitTernaryOp(op, .put),
            .remprop => |op| try self.emitBinaryOp(op, .remprop),
            .type_of => |op| try self.emitUnaryOp(op.operand, .type_of),

            // Stream I/O
            .open => |op| try self.emitBinaryOp(op, .open),
            .close => |op| try self.emitUnaryOp(op.operand, .close),
            .read_line => |op| try self.emitUnaryOp(op.operand, .read_line),
            .write_line => |op| try self.emitBinaryOp(op, .write_line),
            .read_byte => |op| try self.emitUnaryOp(op.operand, .read_byte),
            .write_byte => |op| try self.emitBinaryOp(op, .write_byte),
            .file_position => |op| try self.emitUnaryOp(op.operand, .file_position),
            .file_length => |op| try self.emitUnaryOp(op.operand, .file_length),
            .finish_output => |op| try self.emitUnaryOp(op.operand, .finish_output),
            .force_output => |op| try self.emitUnaryOp(op.operand, .force_output),

            // Reader macros
            .set_macro_character => |op| try self.emitTernaryOp(op, .set_macro_character),
            .get_macro_character => |op| try self.emitUnaryOp(op.operand, .get_macro_character),
            .set_dispatch_macro_character => |op| try self.emitTernaryOp(op, .set_dispatch_macro_character),
            .get_dispatch_macro_character => |op| try self.emitBinaryOp(op, .get_dispatch_macro_character),
            .error_user => |op| try self.emitUnaryOp(op.operand, .error_user),

            // Type assertions
            .assert_fixnum => |op| try self.emitUnaryOp(op.operand, .check_fixnum),
            .assert_cons => |op| try self.emitUnaryOp(op.operand, .check_cons),
            .assert_symbol => |op| try self.emitUnaryOp(op.operand, .check_symbol),
            .assert_string => |op| try self.emitUnaryOp(op.operand, .check_string),
            .assert_vector => |op| try self.emitUnaryOp(op.operand, .check_vector),
            .assert_closure => |op| try self.emitUnaryOp(op.operand, .check_closure),
            .assert_non_nil => |op| try self.emitUnaryOp(op.operand, .check_non_nil),
            .assert_list => |op| try self.emitUnaryOp(op.operand, .check_list),
            .assert_or => |ao| try self.emitAssertOr(ao),

            // Dependent type operations
            .assert_refine => |ar| try self.emitAssertRefine(ar),
            .dpair => |dp| try self.emitDpair(dp),
            .dfst => |df| try self.emitDfst(df),
            .dsnd => |ds| try self.emitDsnd(ds),
        }
    }

    /// Finalize and return the chunk as a GC-managed Value
    pub fn finalize(self: *Emitter) Error!Value {
        // Add implicit return if not present
        if (self.code.items.len == 0) {
            // Empty function returns nil
            try self.emitOp(.push_nil);
            try self.emitOp(.ret);
        } else if (self.code.items[self.code.items.len - 1] != @intFromEnum(Op.ret)) {
            // Non-empty function: just add ret (value already on stack)
            try self.emitOp(.ret);
        }

        // Use computed max_local_idx for REPL expressions (when num_locals is 0)
        const final_num_locals = if (self.num_locals > 0)
            self.num_locals
        else
            self.max_local_idx;

        const heap = self.heap orelse return error.NoHeap;

        // Convert constant pool from u64 to Value
        const const_values = try self.allocator.alloc(Value, self.constants.items.len);
        defer self.allocator.free(const_values);
        for (self.constants.items, 0..) |c, i| {
            const_values[i] = Value{ .raw = c };
        }

        return heap.allocChunk(
            self.code.items,
            const_values,
            self.arity,
            self.optional_count,
            self.key_count,
            self.has_rest,
            final_num_locals,
        );
    }

    /// Get child chunks as GC Values (caller takes ownership via duped slice)
    pub fn getChildChunks(self: *Emitter) ![]Value {
        return self.allocator.dupe(Value, self.child_chunks.items);
    }

    // ========================================================================
    // Emission helpers
    // ========================================================================

    fn emitOp(self: *Emitter, op: Op) Error!void {
        const opcode: u16 = @intFromEnum(op);
        try self.code.append(self.allocator, @truncate(opcode & 0xFF));
        try self.code.append(self.allocator, @truncate(opcode >> 8));
    }

    fn emitU8(self: *Emitter, val: u8) Error!void {
        try self.code.append(self.allocator, val);
    }

    fn emitU16(self: *Emitter, val: u16) Error!void {
        try self.code.append(self.allocator, @truncate(val));
        try self.code.append(self.allocator, @truncate(val >> 8));
    }

    fn emitI16(self: *Emitter, val: i16) Error!void {
        try self.emitU16(@bitCast(val));
    }

    fn emitI32(self: *Emitter, val: i32) Error!void {
        const u: u32 = @bitCast(val);
        const bytes = [_]u8{ @truncate(u), @truncate(u >> 8), @truncate(u >> 16), @truncate(u >> 24) };
        try self.code.appendSlice(self.allocator, &bytes);
    }

    /// Add constant to pool, return index (O(1) deduplication via hash map)
    fn addConstant(self: *Emitter, val: u64) Error!u16 {
        // O(1) lookup in hash map
        if (self.constant_map.get(val)) |idx| {
            return idx;
        }

        if (self.constants.items.len >= 65535) {
            return error.TooManyConstants;
        }

        const idx: u16 = @intCast(self.constants.items.len);
        self.constants.append(self.allocator, val) catch return error.OutOfMemory;
        self.constant_map.put(val, idx) catch return error.OutOfMemory;
        return idx;
    }

    /// Get current code offset
    fn currentOffset(self: *const Emitter) usize {
        return self.code.items.len;
    }

    /// Emit placeholder jump, return offset to patch
    fn emitJump(self: *Emitter, op: Op) Error!usize {
        try self.emitOp(op);
        const offset = self.currentOffset();
        try self.emitI16(0); // Placeholder
        return offset;
    }

    /// Write a 16-bit displacement at a jump location
    /// Core helper used by all jump patching functions
    fn writeJumpDisplacement(self: *Emitter, jump_loc: usize, target: usize) Error!void {
        const distance = @as(i32, @intCast(target)) - @as(i32, @intCast(jump_loc + 2));
        if (distance > 32767 or distance < -32768) {
            return error.JumpTooLong;
        }
        const val: i16 = @intCast(distance);
        const u: u16 = @bitCast(val);
        self.code.items[jump_loc] = @truncate(u);
        self.code.items[jump_loc + 1] = @truncate(u >> 8);
    }

    /// Patch jump at offset to current position
    fn patchJump(self: *Emitter, offset: usize) Error!void {
        try self.writeJumpDisplacement(offset, self.currentOffset());
    }

    // ========================================================================
    // IR emission
    // ========================================================================

    fn emitLiteral(self: *Emitter, val: Value) Error!void {
        if (val.isNil()) {
            try self.emitOp(.push_nil);
        } else if (val.eq(Value.t)) {
            try self.emitOp(.push_t);
        } else if (val.isFixnum()) {
            const n = val.toFixnum();
            if (n >= std.math.minInt(i32) and n <= std.math.maxInt(i32)) {
                try self.emitOp(.push_i32);
                try self.emitI32(@intCast(n));
            } else {
                // Large fixnum - use constant pool
                const idx = try self.addConstant(val.raw);
                try self.emitOp(.push_const);
                try self.emitU16(idx);
            }
        } else {
            // Other values go in constant pool
            const idx = try self.addConstant(val.raw);
            try self.emitOp(.push_const);
            try self.emitU16(idx);
        }
    }

    fn emitQuoteSym(self: *Emitter, name: []const u8) Error!void {
        if (self.heap) |heap| {
            // Intern symbol and add to constant pool
            const sym = try heap.intern(name);
            const idx = try self.addConstant(sym.raw);
            try self.emitOp(.push_const);
            try self.emitU16(idx);
        } else {
            // No heap available - push nil as fallback
            try self.emitOp(.push_nil);
        }
    }

    fn emitQuote(self: *Emitter, inner: *const Ir) Error!void {
        // Quote just emits the literal value
        try self.emit(inner);
    }

    fn emitVar(self: *Emitter, depth: u16, index: u16) Error!void {
        if (depth == 0) {
            // Local variable
            if (index > 255) return error.TooManyLocals;
            try self.emitOp(.load_local);
            try self.emitU8(@intCast(index));
        } else {
            // Check if this is a captured variable
            // Captures are stored with depth from enclosing function's perspective
            // IR nodes have depth from this function's perspective (so +1)
            for (self.captures, 0..) |cap, i| {
                if (cap.depth + 1 == depth and cap.index == index) {
                    // Load from capture array
                    try self.emitOp(.load_capture);
                    try self.emitU8(@intCast(i));
                    return;
                }
            }
            // Upvalue (nested closure case)
            if (depth > 255 or index > 255) return error.TooManyLocals;
            try self.emitOp(.load_upvalue);
            try self.emitU8(@intCast(depth));
            try self.emitU8(@intCast(index));
        }
    }

    fn emitGlobalRef(self: *Emitter, index: u16) Error!void {
        try self.emitOp(.load_global);
        try self.emitU16(index);
    }

    fn emitGlobalRefNode(self: *Emitter, node: anytype) Error!void {
        try self.emitGlobalRef(node.index);
    }

    fn emitDefine(self: *Emitter, d: anytype) Error!void {
        // Emit value
        try self.emit(d.value);
        // Store to global
        try self.emitOp(.store_global);
        try self.emitU16(d.index);
        // Define leaves value on stack (for REPL)
        try self.emitOp(.load_global);
        try self.emitU16(d.index);
    }

    fn emitSet(self: *Emitter, s: anytype) Error!void {
        // Emit value first
        try self.emit(s.value);

        // Dup value so setq returns it (store will pop one copy)
        try self.emitOp(.dup);

        // Then store
        if (s.depth == 0) {
            if (s.index > 255) return error.TooManyLocals;
            try self.emitOp(.store_local);
            try self.emitU8(@intCast(s.index));
        } else {
            if (s.depth > 255 or s.index > 255) return error.TooManyLocals;
            try self.emitOp(.store_upvalue);
            try self.emitU8(@intCast(s.depth));
            try self.emitU8(@intCast(s.index));
        }
    }

    fn emitLet(self: *Emitter, l: anytype) Error!void {
        // Local slots are pre-allocated by vm.run() based on num_locals.
        // Just store each binding at its absolute slot.
        for (l.bindings) |b| {
            try self.emit(b.value);
            try self.emitOp(.store_local);
            try self.emitU8(@intCast(b.index)); // Use IR's absolute index
        }

        // Emit body - result will be on top of stack
        try self.emit(l.body);
        // No cleanup needed - slots remain allocated for the function's lifetime
    }

    fn emitLambda(self: *Emitter, lam: anytype) Error!void {
        // Create nested emitter for lambda body - inherit heap for symbol interning
        var lambda_emitter = if (self.heap) |h|
            Emitter.initWithHeap(self.allocator, h)
        else
            Emitter.init(self.allocator);

        const arity: u8 = @intCast(lam.params.len);
        const optional_count: u8 = @intCast(lam.optional_params.len);
        const key_count: u8 = @intCast(lam.key_params.len);
        lambda_emitter.arity = arity;
        lambda_emitter.optional_count = optional_count;
        lambda_emitter.key_count = key_count;
        lambda_emitter.has_rest = lam.rest_param != null;
        // Layout: [positional] [key params] [keyword pairs] [rest] [let bindings]
        // num_locals = max of (param slots, max local index used in body + 1)
        const rest_count: u8 = if (lam.rest_param != null) 1 else 0;
        const key_pair_slots: u8 = key_count * 2; // max space for keyword-value pairs
        const param_slots: u8 = arity + optional_count + key_count + key_pair_slots + rest_count;
        // Compute max local index used in the body (for let bindings)
        const body_max_idx = computeMaxLocalIndex(lam.body);
        // num_locals must be at least param_slots, but also cover any let bindings
        lambda_emitter.num_locals = @max(param_slots, body_max_idx);
        // Pass captures so emitVar knows which variables to load from capture array
        lambda_emitter.captures = lam.captures;

        // Emit preamble for optional parameters: check argc and fill defaults
        for (lam.optional_params, 0..) |opt_param, i| {
            const slot_index: u8 = arity + @as(u8, @intCast(i));

            // Check if argument was provided: argc > slot_index
            try lambda_emitter.emitOp(.load_argc);
            try lambda_emitter.emitOp(.push_i32);
            try lambda_emitter.emitI32(@as(i32, slot_index));
            try lambda_emitter.emitOp(.gt);

            // Jump over default assignment if arg was provided
            const skip_jump = try lambda_emitter.emitJump(.jmp_not_nil);

            // Emit default value and store to slot
            if (opt_param.default) |default_ir| {
                lambda_emitter.emit(default_ir) catch {
                    lambda_emitter.deinit();
                    return error.InvalidIr;
                };
            } else {
                try lambda_emitter.emitOp(.push_nil);
            }
            try lambda_emitter.emitOp(.store_local);
            try lambda_emitter.emitU8(slot_index);

            // Patch jump to skip default
            try lambda_emitter.patchJump(skip_jump);
        }

        // Emit preamble for keyword parameters: find keyword in args and fill defaults
        for (lam.key_params, 0..) |key_param, i| {
            const slot_index: u8 = arity + optional_count + @as(u8, @intCast(i));

            // Add keyword to constant pool for find_key opcode
            if (lambda_emitter.heap) |heap| {
                const kw = heap.internKeyword(key_param.keyword) catch {
                    lambda_emitter.deinit();
                    return error.OutOfMemory;
                };
                const kw_idx = lambda_emitter.addConstant(kw.raw) catch {
                    lambda_emitter.deinit();
                    return error.TooManyConstants;
                };

                // Emit find_key which pushes (found_flag, value)
                try lambda_emitter.emitOp(.find_key);
                try lambda_emitter.emitU16(kw_idx);

                // Stack: ..., found_flag, value
                // Swap so value is below, found_flag on top for jmp_nil
                try lambda_emitter.emitOp(.swap);
                // Stack: ..., value, found_flag

                // Jump to use_default if found_flag is nil
                const use_default_jump = try lambda_emitter.emitJump(.jmp_nil);

                // Keyword was found - value is on stack, store it
                try lambda_emitter.emitOp(.store_local);
                try lambda_emitter.emitU8(slot_index);

                // Jump past default handling
                const done_jump = try lambda_emitter.emitJump(.jmp);

                // use_default: pop the nil value, emit default
                try lambda_emitter.patchJump(use_default_jump);
                try lambda_emitter.emitOp(.pop); // discard nil value

                // Emit default value and store to slot
                if (key_param.default) |default_ir| {
                    lambda_emitter.emit(default_ir) catch {
                        lambda_emitter.deinit();
                        return error.InvalidIr;
                    };
                } else {
                    try lambda_emitter.emitOp(.push_nil);
                }
                try lambda_emitter.emitOp(.store_local);
                try lambda_emitter.emitU8(slot_index);

                // done:
                try lambda_emitter.patchJump(done_jump);
            } else {
                // No heap - just initialize to nil or default
                if (key_param.default) |default_ir| {
                    lambda_emitter.emit(default_ir) catch {
                        lambda_emitter.deinit();
                        return error.InvalidIr;
                    };
                } else {
                    try lambda_emitter.emitOp(.push_nil);
                }
                try lambda_emitter.emitOp(.store_local);
                try lambda_emitter.emitU8(slot_index);
            }
        }

        // Emit body
        lambda_emitter.emit(lam.body) catch {
            lambda_emitter.deinit();
            return error.InvalidIr;
        };

        // Finalize lambda chunk (GC Value)
        const chunk_val = lambda_emitter.finalize() catch {
            lambda_emitter.deinit();
            return error.OutOfMemory;
        };
        const chunk = chunk_val.toPtr(Chunk);

        // Patch lambda's make_closure indices to account for parent's existing child_chunks
        // The lambda's code uses indices relative to its own child_chunks array (starting at 0).
        // But when we copy those child_chunks into the parent, they'll be at offset = parent's current len.
        // So we need to add this offset to all make_closure indices in the lambda's code.
        const child_base: u16 = @intCast(self.child_chunks.items.len);
        if (child_base > 0) {
            patchMakeClosureIndicesOffset(chunk.getCode(), child_base);
        }

        // Collect any child chunks from the lambda
        try self.child_chunks.appendSlice(self.allocator, lambda_emitter.child_chunks.items);
        lambda_emitter.deinit();

        // Store chunk Value in child_chunks, get its index
        const chunk_idx: u16 = @intCast(self.child_chunks.items.len);
        self.child_chunks.append(self.allocator, chunk_val) catch return error.OutOfMemory;

        // Emit captures (if any)
        for (lam.captures) |cap| {
            // Load the captured value from upvalue
            if (cap.depth == 0) {
                try self.emitOp(.load_local);
                try self.emitU8(@intCast(cap.index));
            } else {
                try self.emitOp(.load_upvalue);
                try self.emitU8(@intCast(cap.depth - 1));
                try self.emitU8(@intCast(cap.index));
            }
        }

        // Emit make_closure: u16 chunk_index, u8 num_captures
        try self.emitOp(.make_closure);
        try self.emitU16(chunk_idx);
        try self.emitU8(@intCast(lam.captures.len));
    }

    fn emitIf(self: *Emitter, i: anytype) Error!void {
        // Emit test
        try self.emit(i.cond);

        // Jump to else if nil
        const else_jump = try self.emitJump(.jmp_nil);

        // Emit then branch
        try self.emit(i.then_branch);

        // Jump over else
        const end_jump = try self.emitJump(.jmp);

        // Patch else jump
        try self.patchJump(else_jump);

        // Emit else branch
        try self.emit(i.else_branch);

        // Patch end jump
        try self.patchJump(end_jump);
    }

    fn emitProgn(self: *Emitter, exprs: []const *const Ir) Error!void {
        if (exprs.len == 0) {
            try self.emitOp(.push_nil);
            return;
        }

        for (exprs, 0..) |expr, i| {
            try self.emit(expr);
            // Pop intermediate values, keep last
            if (i < exprs.len - 1) {
                try self.emitOp(.pop);
            }
        }
    }

    fn emitLoop(self: *Emitter, l: anytype) Error!void {
        // Loop start
        const loop_start = self.currentOffset();

        // Emit test
        try self.emit(l.cond);

        // Jump out if nil
        const exit_jump = try self.emitJump(.jmp_nil);

        // Emit body
        try self.emit(l.body);
        try self.emitOp(.pop); // Discard body result

        // Jump back to start
        const back_distance = @as(i32, @intCast(loop_start)) -
            @as(i32, @intCast(self.currentOffset() + 4)); // +4 for jmp instruction (2-byte opcode + 2-byte i16)
        if (back_distance > 32767 or back_distance < -32768) {
            return error.JumpTooLong;
        }
        try self.emitOp(.jmp);
        try self.emitI16(@intCast(back_distance));

        // Patch exit
        try self.patchJump(exit_jump);

        // Push nil as loop result
        try self.emitOp(.push_nil);
    }

    /// Emit struct predicate check: (if (vectorp obj) (eq (vec-ref obj 0) 'name) nil)
    fn emitStructP(self: *Emitter, operand: *const Ir, struct_name: []const u8) Error!void {
        // Emit the operand
        try self.emit(operand);

        // Duplicate for vectorp check (need value again for vec-ref)
        try self.emitOp(.dup);

        // Check if it's a vector
        try self.emitOp(.vectorp);

        // Jump to nil if not a vector
        const nil_jump = try self.emitJump(.jmp_nil);

        // It's a vector - check the type tag at index 0
        // Stack: [obj] - duplicate for vec-ref
        try self.emitOp(.dup);

        // Push index 0
        try self.emitOp(.push_i32);
        try self.emitI32(0);

        // Get element at index 0
        try self.emitOp(.vec_ref);

        // Push struct name symbol as constant
        const heap = self.heap orelse return error.NoHeap;
        const name_sym = try heap.intern(struct_name);
        const const_idx = try self.addConstant(name_sym.raw);
        try self.emitOp(.push_const);
        try self.emitU16(const_idx);

        // Compare with eq
        try self.emitOp(.eq);

        // Swap and pop the extra obj copy (stack: [obj, result] -> [result])
        try self.emitOp(.swap);
        try self.emitOp(.pop);

        // Jump to end
        const end_jump = try self.emitJump(.jmp);

        // Nil case: pop the extra copy and push nil
        try self.patchJump(nil_jump);
        try self.emitOp(.pop); // Remove obj copy
        try self.emitOp(.push_nil);

        // End
        try self.patchJump(end_jump);
    }

    fn emitBlock(self: *Emitter, b: anytype) Error!void {
        // Intern block name and add to constant pool
        const heap = self.heap orelse return error.OutOfMemory;
        const name_sym = try heap.intern(b.name);
        const name_idx = try self.addConstant(name_sym.raw);

        // Emit push_block with placeholder offset and name index
        try self.emitOp(.push_block);
        const exit_offset_loc = self.code.items.len;
        try self.code.appendSlice(self.allocator, &[_]u8{ 0, 0 }); // i16 exit offset placeholder
        try self.emitU16(name_idx);

        // Still track on control stack for unwind-protect handling
        const entry = ControlEntry{
            .block = .{
                .name = b.name,
                .pending_exits = std.ArrayList(usize){},
            },
        };
        self.control_stack.append(self.allocator, entry) catch return error.OutOfMemory;

        // Emit body
        try self.emit(b.body);

        // Emit pop_block for normal exit
        try self.emitOp(.pop_block);

        // Patch the exit offset to point here (after pop_block)
        const current_ip = self.code.items.len;
        const exit_offset_ip = exit_offset_loc + 2; // After the offset bytes
        const offset: i16 = @intCast(@as(isize, @intCast(current_ip)) - @as(isize, @intCast(exit_offset_ip)));
        self.code.items[exit_offset_loc] = @truncate(@as(u16, @bitCast(offset)));
        self.code.items[exit_offset_loc + 1] = @truncate(@as(u16, @bitCast(offset)) >> 8);

        // Pop block from control stack and patch any compile-time exits (for same-chunk return-from)
        var popped = self.control_stack.pop().?;
        switch (popped) {
            .block => |*blk| {
                for (blk.pending_exits.items) |jump_loc| {
                    try self.patchJumpAt(jump_loc);
                }
                blk.pending_exits.deinit(self.allocator);
            },
            .unwind_protect, .tagbody => unreachable,
        }
    }

    fn emitReturnFrom(self: *Emitter, r: anytype) Error!void {
        // Emit the return value
        try self.emit(r.value);

        // First check if block is in current chunk - if so, use compile-time jump
        var i = self.control_stack.items.len;
        while (i > 0) {
            i -= 1;
            switch (self.control_stack.items[i]) {
                .block => |*blk| {
                    // String comparison needed: block names are raw strings from IR, not interned symbols
                    if (std.mem.eql(u8, blk.name, r.name)) {
                        // Found target block in same chunk - use compile-time jump
                        const jump_loc = try self.emitJump(.jmp);
                        blk.pending_exits.append(self.allocator, jump_loc) catch
                            return error.OutOfMemory;
                        return;
                    }
                    // Not our target block, keep searching
                },
                .unwind_protect => |up| {
                    // Crossing an unwind-protect - emit cleanup
                    try self.emit(up.cleanup);
                    try self.emitOp(.pop);
                },
                .tagbody => {
                    // Crossing a tagbody - no cleanup, just continue
                },
            }
        }

        // Block not in current chunk - emit runtime return_from opcode
        // This handles cross-closure return-from
        const heap = self.heap orelse return error.OutOfMemory;
        const name_sym = try heap.intern(r.name);
        const name_idx = try self.addConstant(name_sym.raw);
        try self.emitOp(.return_from);
        try self.emitU16(name_idx);
    }

    fn emitUnwindProtect(self: *Emitter, u: anytype) Error!void {
        // Bytecode layout:
        // push_unwind -> cleanup_addr   ; saves cleanup IP for throw case
        // <protected code>              ; leaves result on stack (or throws)
        // cleanup_addr:                 ; falls through on normal exit
        // <cleanup code>
        // pop                           ; discard cleanup result
        // pop_unwind                    ; remove frame, continue (or re-throw)

        // Push unwind-protect onto control stack (for return-from handling)
        const entry = ControlEntry{
            .unwind_protect = .{ .cleanup = u.cleanup },
        };
        self.control_stack.append(self.allocator, entry) catch return error.OutOfMemory;

        // Emit push_unwind with forward jump to cleanup code
        const unwind_jump = try self.emitJump(.push_unwind);

        // Emit protected form
        try self.emit(u.protected);

        // Pop from control stack (compile-time only)
        _ = self.control_stack.pop();

        // Patch push_unwind to point here (cleanup start)
        try self.patchJump(unwind_jump);

        // Emit cleanup code (falls through for normal exit, jumped to for throw)
        try self.emit(u.cleanup);
        try self.emitOp(.pop); // Discard cleanup result

        // pop_unwind signals end of cleanup region
        // The operand is unused (0) - VM tracks state internally
        try self.emitOp(.pop_unwind);
        try self.code.appendSlice(self.allocator, &[_]u8{ 0, 0 });
    }

    fn emitCatch(self: *Emitter, c: anytype) Error!void {
        // Emit tag expression (will be on stack for push_catch)
        try self.emit(c.tag);

        // Emit push_catch with forward jump to handler/end
        // push_catch pops tag and saves catch frame
        const catch_jump = try self.emitJump(.push_catch);

        // Emit body
        try self.emit(c.body);

        // Normal exit: pop catch frame
        try self.emitOp(.pop_catch);

        // Jump over the throw handler (body completed normally)
        const end_jump = try self.emitJump(.jmp);

        // Patch catch_jump to point here (throw lands here)
        try self.patchJumpAt(catch_jump);

        // When throw happens, thrown value is on stack (pushed by throw opcode)
        // Nothing more needed - value is already the result

        // Patch end_jump
        try self.patchJumpAt(end_jump);
    }

    fn emitProgv(self: *Emitter, p: anytype) Error!void {
        // Emit symbols and values expressions
        try self.emit(p.symbols);
        try self.emit(p.values);

        // Push progv frame (pops symbols and values, establishes bindings)
        try self.emitOp(.push_progv);

        // Emit body
        try self.emit(p.body);

        // Pop progv frame (restore previous bindings)
        try self.emitOp(.pop_progv);
    }

    fn emitHandlerCase(self: *Emitter, hc: anytype) Error!void {
        // handler-case is like catch but uses let-style binding for the caught value
        // The caught value is on the stack when the handler runs

        // Emit tag expression (will be on stack for push_catch)
        try self.emit(hc.tag);

        // Emit push_catch with forward jump to handler
        const catch_jump = try self.emitJump(.push_catch);

        // Emit protected body
        try self.emit(hc.body);

        // Normal exit: pop catch frame
        try self.emitOp(.pop_catch);

        // Jump over the handler (body completed normally)
        const end_jump = try self.emitJump(.jmp);

        // Patch catch_jump to point here (throw lands here)
        try self.patchJumpAt(catch_jump);

        // When throw happens, thrown value is on operand stack
        // Store it to the local slot where the handler code expects it
        try self.emitOp(.store_local);
        try self.emitU8(@intCast(hc.cond_idx));

        // Now emit handler dispatch code (which reads from local slot)
        try self.emit(hc.handler);

        // Handler result is on stack - that's our return value
        // (caught value is in local slot, not on stack)

        // Patch end_jump
        try self.patchJumpAt(end_jump);
    }

    fn emitThrow(self: *Emitter, t: anytype) Error!void {
        // Emit tag and value
        try self.emit(t.tag);
        try self.emit(t.value);

        // Emit throw opcode - VM will unwind to matching catch
        try self.emitOp(.throw);
    }

    fn emitHandlerBind(self: *Emitter, hb: anytype) Error!void {
        // Build handlers alist at compile time and push as literal
        // Start with nil
        try self.emitOp(.push_nil);

        // Cons each handler pair onto list (reverse order for proper list)
        var i = hb.handlers.len;
        while (i > 0) {
            i -= 1;
            const h = hb.handlers[i];

            // Push handler function (should be lambda/closure)
            try self.emit(h.handler_fn);
            // Push condition type
            try self.emit(h.condition_type);
            // Cons them: (condition_type . handler_fn)
            try self.emitOp(.cons);
            // Cons onto list
            try self.emitOp(.cons);
        }

        // Emit body (should be lambda/closure)
        try self.emit(hb.body);

        // Swap so stack is: body-fn handlers-alist
        try self.emitOp(.swap);

        // Call handler_bind opcode
        try self.emitOp(.handler_bind);
    }

    fn emitRestartCase(self: *Emitter, rc: anytype) Error!void {
        // Layout:
        // For each restart: push_restart name -> handler_addr
        // <body code>
        // pop_restarts N
        // jmp -> end
        // handler_1: <handler code>, jmp -> end
        // handler_2: <handler code>, jmp -> end
        // ...
        // end:

        const restart_count = rc.restarts.len;

        // Track where each restart handler will be
        const handler_jumps = self.allocator.alloc(usize, restart_count) catch return error.OutOfMemory;
        defer self.allocator.free(handler_jumps);

        // Emit push_restart for each restart (name on stack, then push_restart offset)
        for (rc.restarts, 0..) |r, i| {
            try self.emit(r.name); // Push restart name symbol
            try self.emitOp(.push_restart);
            handler_jumps[i] = self.code.items.len;
            try self.emitI16(0); // Placeholder offset to handler
        }

        // Emit body
        try self.emit(rc.body);

        // Pop all restarts on normal exit
        try self.emitOp(.pop_restarts);
        try self.emitU8(@intCast(restart_count));

        // Jump to end (skip handlers)
        try self.emitOp(.jmp);
        const end_jump = self.code.items.len;
        try self.emitI16(0);

        // Track jumps from handlers to end
        const end_jumps = self.allocator.alloc(usize, restart_count) catch return error.OutOfMemory;
        defer self.allocator.free(end_jumps);

        // Emit each handler
        for (rc.restarts, 0..) |r, i| {
            // Patch the push_restart offset
            const handler_addr = self.code.items.len;
            const displacement = @as(i16, @intCast(@as(isize, @intCast(handler_addr)) - @as(isize, @intCast(handler_jumps[i])) - 2));
            const disp_u16: u16 = @bitCast(displacement);
            self.code.items[handler_jumps[i]] = @truncate(disp_u16);
            self.code.items[handler_jumps[i] + 1] = @truncate(disp_u16 >> 8);

            // Handler is a lambda - call it with the invoked value (already on stack)
            // Stack: [value]
            // Check if handler has parameters
            const has_params = if (r.handler.* == .lambda)
                r.handler.lambda.params.len > 0
            else
                true; // Default to passing value for non-lambda handlers

            if (has_params) {
                try self.emit(r.handler); // Push lambda
                // Stack: [value, lambda]
                try self.emitOp(.swap); // Swap so lambda is TOS-1, value is TOS
                // Stack: [lambda, value]
                try self.emitOp(.call);
                try self.emitU8(1); // 1 argument
            } else {
                // Handler takes no args - pop the value and call with 0 args
                try self.emitOp(.pop); // Discard value
                try self.emit(r.handler); // Push lambda
                try self.emitOp(.call);
                try self.emitU8(0); // 0 arguments
            }

            // Jump to end
            try self.emitOp(.jmp);
            end_jumps[i] = self.code.items.len;
            try self.emitI16(0);
        }

        // Patch all end jumps
        const end_addr = self.code.items.len;
        // Patch the main end_jump
        {
            const displacement = @as(i16, @intCast(@as(isize, @intCast(end_addr)) - @as(isize, @intCast(end_jump)) - 2));
            const disp_u16: u16 = @bitCast(displacement);
            self.code.items[end_jump] = @truncate(disp_u16);
            self.code.items[end_jump + 1] = @truncate(disp_u16 >> 8);
        }
        // Patch handler end jumps
        for (end_jumps) |jump_addr| {
            const displacement = @as(i16, @intCast(@as(isize, @intCast(end_addr)) - @as(isize, @intCast(jump_addr)) - 2));
            const disp_u16: u16 = @bitCast(displacement);
            self.code.items[jump_addr] = @truncate(disp_u16);
            self.code.items[jump_addr + 1] = @truncate(disp_u16 >> 8);
        }
    }

    fn emitInvokeRestart(self: *Emitter, inv: anytype) Error!void {
        // Emit name and value, then invoke_restart opcode
        try self.emit(inv.name);
        try self.emit(inv.value);
        try self.emitOp(.invoke_restart);
    }

    fn emitTagbody(self: *Emitter, tb: anytype) Error!void {
        // Allocate offset array for tags
        const tag_offsets = self.allocator.alloc(usize, tb.tags.len) catch return error.OutOfMemory;
        @memset(tag_offsets, 0);

        // Push tagbody entry onto control stack
        const entry = ControlEntry{
            .tagbody = .{
                .tags = tb.tags,
                .tag_offsets = tag_offsets,
                .pending_jumps = std.ArrayList(PendingGoJump){},
            },
        };
        self.control_stack.append(self.allocator, entry) catch return error.OutOfMemory;

        // Emit segments, recording tag positions
        // segments[0] = code before first tag
        // segments[i] = code after tags[i-1]
        for (tb.segments, 0..) |segment, i| {
            if (i > 0) {
                // Record position of this tag (tags[i-1])
                const tag_idx = i - 1;
                // Get the tagbody entry (should be at top of control stack)
                const top_idx = self.control_stack.items.len - 1;
                self.control_stack.items[top_idx].tagbody.tag_offsets[tag_idx] = self.currentOffset();
            }

            try self.emit(segment);
            // Pop result of each segment (except the last one is the tagbody result)
            if (i < tb.segments.len - 1) {
                try self.emitOp(.pop);
            }
        }

        // Pop tagbody and patch pending jumps
        var popped = self.control_stack.pop().?;
        switch (popped) {
            .tagbody => |*tbe| {
                // Patch all pending go jumps
                for (tbe.pending_jumps.items) |pending| {
                    const target = tbe.tag_offsets[pending.tag_idx];
                    try self.patchJumpTo(pending.jump_loc, target);
                }
                // Free resources
                self.allocator.free(tbe.tag_offsets);
                tbe.pending_jumps.deinit(self.allocator);
            },
            else => unreachable,
        }

        // tagbody always returns nil
        try self.emitOp(.pop);
        try self.emitOp(.push_nil);
    }

    fn emitGo(self: *Emitter, g: anytype) Error!void {
        // Find enclosing tagbody with matching tag
        var i = self.control_stack.items.len;
        while (i > 0) {
            i -= 1;
            switch (self.control_stack.items[i]) {
                .tagbody => |*tbe| {
                    // String comparison needed: tag names are raw strings from IR, not interned symbols
                    // Search for tag
                    for (tbe.tags, 0..) |tag, tag_idx| {
                        if (std.mem.eql(u8, tag, g.tag)) {
                            // Found! Check if tag position is known (forward vs backward jump)
                            if (tbe.tag_offsets[tag_idx] != 0) {
                                // Backward jump - target is known
                                const target = tbe.tag_offsets[tag_idx];
                                const jump_loc = try self.emitJump(.jmp);
                                try self.patchJumpTo(jump_loc, target);
                            } else {
                                // Forward jump - record for later patching
                                const jump_loc = try self.emitJump(.jmp);
                                tbe.pending_jumps.append(self.allocator, .{
                                    .tag_idx = tag_idx,
                                    .jump_loc = jump_loc,
                                }) catch return error.OutOfMemory;
                            }
                            return;
                        }
                    }
                },
                else => {},
            }
        }
        // Tag not found
        return error.InvalidIr;
    }

    fn emitValues(self: *Emitter, v: anytype) Error!void {
        // Emit each value expression
        for (v) |val| {
            try self.emit(val);
        }

        // Emit values opcode with count
        if (v.len > 255) return error.TooManyLocals;
        try self.emitOp(.values);
        try self.emitU8(@intCast(v.len));
    }

    fn emitMvBind(self: *Emitter, m: anytype) Error!void {
        // Emit the expression that produces multiple values
        // This may produce a `values` opcode that stores secondary values
        try self.emit(m.expr);

        // Emit mv_bind opcode with count
        // This pops primary, stores all values to bp+0..bp+count-1
        if (m.vars.len > 255) return error.TooManyLocals;
        try self.emitOp(.mv_bind);
        try self.emitU8(@intCast(m.vars.len));

        // Emit body - variables are now bound to locals by VM
        try self.emit(m.body);
    }

    fn emitMvList(self: *Emitter, m: anytype) Error!void {
        // Evaluate the expression (leaves primary on stack, secondaries in buffer)
        try self.emit(m.expr);
        // Emit mv_list opcode - gathers all values into a list
        try self.emitOp(.mv_list);
    }

    fn emitMvCall(self: *Emitter, m: anytype) Error!void {
        // (multiple-value-call fn form1 form2 ...)
        // Compiles to: (apply fn (append (mv-list form1) (mv-list form2) ...))

        // First, emit each form wrapped in mv_list
        for (m.forms) |form| {
            try self.emit(form);
            try self.emitOp(.mv_list);
        }

        // Append all the lists together
        // (append l1 l2 l3) = (append (append l1 l2) l3)
        if (m.forms.len > 1) {
            var i: usize = 1;
            while (i < m.forms.len) : (i += 1) {
                try self.emitOp(.append_lists);
            }
        } else if (m.forms.len == 0) {
            // No forms - pass nil as args
            try self.emitOp(.push_nil);
        }

        // Now we have the combined args list on stack
        // Emit function
        try self.emit(m.func);
        // Swap so args are on top (apply expects func, args)
        try self.emitOp(.swap);
        // Apply
        try self.emitOp(.apply);
    }

    fn emitFormat(self: *Emitter, f: anytype) Error!void {
        // Emit destination
        try self.emit(f.dest);
        // Emit control string
        try self.emit(f.control);
        // Emit arguments
        for (f.args) |arg| {
            try self.emit(arg);
        }
        // Emit format opcode with argument count
        if (f.args.len > 255) return error.TooManyLocals;
        try self.emitOp(.format);
        try self.emitU8(@intCast(f.args.len));
    }

    fn emitMakeHash(self: *Emitter, h: anytype) Error!void {
        try self.emitOp(.make_hash);
        try self.emitU16(h.capacity);
        try self.emitU8(@intFromEnum(h.test_type));
    }
    fn emitHashGet(self: *Emitter, h: anytype) Error!void {
        try self.emit(h.table);
        try self.emit(h.key);
        try self.emitOp(.hash_get);
        if (h.default) |def| {
            // Pattern: result = (hash-get ht key) or default
            // Compile as: dup result, if not nil skip default
            try self.emitOp(.dup);
            const skip_default = try self.emitJump(.jmp_not_nil);
            try self.emitOp(.pop); // pop nil
            try self.emit(def);
            try self.patchJump(skip_default);
        }
    }

    fn emitHashSet(self: *Emitter, h: anytype) Error!void {
        try self.emit(h.table);
        try self.emit(h.key);
        try self.emit(h.value);
        try self.emitOp(.hash_set);
    }

    fn emitHashRem(self: *Emitter, h: anytype) Error!void {
        try self.emit(h.table);
        try self.emit(h.key);
        try self.emitOp(.hash_rem);
    }

    /// Patch a jump to a specific target offset
    fn patchJumpTo(self: *Emitter, jump_loc: usize, target: usize) Error!void {
        try self.writeJumpDisplacement(jump_loc, target);
    }

    /// Patch a jump at a specific location to jump to current offset
    fn patchJumpAt(self: *Emitter, jump_loc: usize) Error!void {
        try self.writeJumpDisplacement(jump_loc, self.currentOffset());
    }

    fn emitCall(self: *Emitter, c: anytype, tail: bool) Error!void {
        // Emit function
        try self.emit(c.func);

        // Emit arguments
        for (c.args) |arg| {
            try self.emit(arg);
        }

        // Emit call
        if (c.args.len > 255) return error.TooManyLocals;
        try self.emitOp(if (tail) .tail_call else .call);
        try self.emitU8(@intCast(c.args.len));
    }

    fn emitApply(self: *Emitter, a: anytype) Error!void {
        // Emit function then args list
        try self.emit(a.func);
        try self.emit(a.args);
        try self.emitOp(.apply);
    }

    fn emitBinaryOp(self: *Emitter, op: Ir.BinaryOp, opcode: Op) Error!void {
        try self.emit(op.left);
        try self.emit(op.right);
        try self.emitOp(opcode);
    }

    fn emitTernaryOp(self: *Emitter, op: Ir.TernaryOp, opcode: Op) Error!void {
        try self.emit(op.first);
        try self.emit(op.second);
        try self.emit(op.third);
        try self.emitOp(opcode);
    }

    fn emitQuaternaryOp(self: *Emitter, op: Ir.QuaternaryOp, opcode: Op) Error!void {
        try self.emit(op.first);
        try self.emit(op.second);
        try self.emit(op.third);
        try self.emit(op.fourth);
        try self.emitOp(opcode);
    }

    fn emitUnaryOp(self: *Emitter, operand: *const Ir, opcode: Op) Error!void {
        try self.emit(operand);
        try self.emitOp(opcode);
    }

    fn emitMathExt(self: *Emitter, operand: *const Ir, sub_op: opcodes.MathExtOp) Error!void {
        try self.emit(operand);
        try self.emitOp(.math_ext);
        try self.emitU8(@intFromEnum(sub_op));
    }

    fn emitMathExt2(self: *Emitter, op: Ir.BinaryOp, sub_op: opcodes.MathExtOp) Error!void {
        try self.emit(op.left);
        try self.emit(op.right);
        try self.emitOp(.math_ext);
        try self.emitU8(@intFromEnum(sub_op));
    }

    fn emitList(self: *Emitter, elements: []const *const Ir) Error!void {
        // Emit elements in order
        for (elements) |elem| {
            try self.emit(elem);
        }

        // Make list
        if (elements.len > 255) return error.TooManyLocals;
        try self.emitOp(.make_list);
        try self.emitU8(@intCast(elements.len));
    }

    fn emitListStar(self: *Emitter, elements: []const *const Ir) Error!void {
        // (list* a b c tail) => (cons a (cons b (cons c tail)))
        // Build from back to front: emit tail, then cons each element
        if (elements.len == 0) return;

        // Emit last element (the tail)
        try self.emit(elements[elements.len - 1]);

        // Cons each preceding element from right to left
        var i: usize = elements.len - 1;
        while (i > 0) {
            i -= 1;
            try self.emit(elements[i]);
            try self.emitOp(.swap); // swap so we have (elem tail)
            try self.emitOp(.cons);
        }
    }

    fn emitVecNew(self: *Emitter, v: anytype) Error!void {
        try self.emit(v.size);
        if (v.init) |init_val| {
            try self.emit(init_val);
        } else {
            try self.emitOp(.push_nil);
        }
        try self.emitOp(.make_vec);
        try self.emitU16(0); // Size from stack (u16 unused)
    }

    fn emitVec(self: *Emitter, elements: []const *const Ir) Error!void {
        // Emit elements in order
        for (elements) |elem| {
            try self.emit(elem);
        }

        // Make vector from N elements
        if (elements.len > 255) return error.TooManyLocals;
        try self.emitOp(.make_vec_n);
        try self.emitU8(@intCast(elements.len));
    }

    fn emitVecSet(self: *Emitter, v: anytype) Error!void {
        try self.emit(v.vec);
        try self.emit(v.index);
        try self.emit(v.value);
        try self.emitOp(.vec_set);
    }

    fn emitStrSet(self: *Emitter, s: anytype) Error!void {
        try self.emit(s.str);
        try self.emit(s.index);
        try self.emit(s.value);
        try self.emitOp(.str_set);
    }

    fn emitArrNew(self: *Emitter, a: anytype) Error!void {
        // Emit dimensions in order (stack: dim1 dim2 ... dimN)
        for (a.dimensions) |dim| {
            try self.emit(dim);
        }

        // Emit optional initial element
        const has_init: u8 = if (a.init != null) 1 else 0;
        if (a.init) |init_val| {
            try self.emit(init_val);
        }

        // Encode: [7:1] = dim count, [0] = has initial-element
        const dim_count: u8 = @intCast(a.dimensions.len);
        const operand = (dim_count << 1) | has_init;
        try self.emitOp(.make_array);
        try self.emitU8(operand);
    }

    fn emitArrRef(self: *Emitter, a: anytype) Error!void {
        // Emit array and subscripts (stack: array sub1 sub2 ... subN)
        try self.emit(a.array);
        for (a.subscripts) |sub| {
            try self.emit(sub);
        }

        // Emit aref opcode with subscript count
        const sub_count: u8 = @intCast(a.subscripts.len);
        try self.emitOp(.aref);
        try self.emitU8(sub_count);
    }

    fn emitArrSet(self: *Emitter, a: anytype) Error!void {
        // Emit array, subscripts, and value (stack: array sub1 sub2 ... subN value)
        try self.emit(a.array);
        for (a.subscripts) |sub| {
            try self.emit(sub);
        }
        try self.emit(a.value);

        // Emit aset opcode with subscript count
        const sub_count: u8 = @intCast(a.subscripts.len);
        try self.emitOp(.aset);
        try self.emitU8(sub_count);
    }

    // ========================================================================
    // Dependent type operations
    // ========================================================================

    /// Emit refinement type assertion: check predicate holds for value
    fn emitAssertRefine(self: *Emitter, ar: anytype) Error!void {
        // 1. Emit the operand (value to check)
        try self.emit(ar.operand);

        // 2. If we have a base type, emit that check first
        // (base_type is optional but we don't use it for bytecode yet)
        _ = ar.base_type;

        // 3. Duplicate value for predicate application
        try self.emitOp(.dup);

        // 4. Emit the predicate (a lambda)
        try self.emit(ar.predicate);

        // 5. Swap so predicate is below value: [value, predicate, value] -> [value, value, predicate]
        try self.emitOp(.swap);

        // 6. Call predicate with value: (predicate value)
        try self.emitOp(.call);
        try self.emitU8(1); // 1 argument

        // 7. Check result is truthy, error if not
        try self.emitOp(.check_refine);
        // Stack now has original value (from dup before predicate call)
    }

    /// Emit or-type assertion: check value matches one of multiple types
    fn emitAssertOr(self: *Emitter, ao: anytype) Error!void {
        // 1. Emit the operand (value to check)
        try self.emit(ao.operand);

        // 2. Build a list from the type_symbols and add to constant pool
        const heap = self.heap orelse return error.NoHeap;
        var type_list = Value.nil;
        var i = ao.type_symbols.len;
        while (i > 0) {
            i -= 1;
            type_list = try heap.allocCons(ao.type_symbols[i], type_list);
        }

        // 3. Add to constant pool and emit check_or with the index
        const idx = try self.addConstant(type_list.raw);
        try self.emitOp(.check_or);
        try self.emitU16(idx);
    }

    /// Emit dependent pair creation
    fn emitDpair(self: *Emitter, dp: anytype) Error!void {
        // At runtime, a dependent pair is just a cons cell
        // The type info is for compile-time checking only
        _ = dp.sigma_type;
        try self.emit(dp.first);
        try self.emit(dp.second);
        try self.emitOp(.cons);
    }

    /// Emit dependent pair first projection
    fn emitDfst(self: *Emitter, df: anytype) Error!void {
        // At runtime, just car - type info is for compile-time
        _ = df.type_info;
        try self.emit(df.pair);
        try self.emitOp(.car);
    }

    /// Emit dependent pair second projection
    fn emitDsnd(self: *Emitter, ds: anytype) Error!void {
        // At runtime, just cdr - type info is for compile-time
        _ = ds.type_info;
        try self.emit(ds.pair);
        try self.emitOp(.cdr);
    }
};

/// Patch make_closure instructions by adding an offset to their chunk indices.
/// Used when collecting child chunks from nested emitters - the nested code's
/// indices are relative to ITS child_chunks array, but they need to be adjusted
/// to be relative to the parent's array.
fn patchMakeClosureIndicesOffset(code: []u8, offset: u16) void {
    var i: usize = 0;
    while (i + 1 < code.len) {
        // Read opcode (2 bytes, little-endian)
        const low: u16 = code[i];
        const high: u16 = code[i + 1];
        const opcode = low | (high << 8);
        const op: Op = @enumFromInt(opcode);
        const size = op.operandSize();

        if (op == .make_closure) {
            // make_closure has: u16 chunk_index, u8 num_captures
            // Operand starts at i+2 (after 2-byte opcode)
            // Add offset to the u16 index at code[i+2..i+4]
            if (i + 2 + 2 <= code.len) {
                const rel_idx = std.mem.readInt(u16, code[i + 2 ..][0..2], .little);
                const new_idx = rel_idx + offset;
                std.mem.writeInt(u16, code[i + 2 ..][0..2], new_idx, .little);
            }
        }

        // Move to next instruction: 2 bytes for opcode + operand size
        i += 2 + size;
    }
}

// ============================================================================
// Tests
// ============================================================================

test "emit literal nil" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var emitter = Emitter.init(allocator);
    defer emitter.deinit();

    const builder = ir.IrBuilder.init(allocator);
    const node = try builder.lit(Value.nil);
    defer allocator.destroy(node);

    try emitter.emit(node);
    try testing.expectEqual(@as(u8, @intFromEnum(Op.push_nil)), emitter.code.items[0]);
}

test "emit literal fixnum" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var emitter = Emitter.init(allocator);
    defer emitter.deinit();

    const builder = ir.IrBuilder.init(allocator);
    const node = try builder.lit(Value.makeFixnum(42));
    defer allocator.destroy(node);

    try emitter.emit(node);
    // Opcode is now 2 bytes (little-endian u16)
    const push_i32_opcode: u16 = @intFromEnum(Op.push_i32);
    try testing.expectEqual(@as(u8, @truncate(push_i32_opcode & 0xFF)), emitter.code.items[0]);
    try testing.expectEqual(@as(u8, @truncate(push_i32_opcode >> 8)), emitter.code.items[1]);
    // 42 in little-endian i32 starts at index 2
    try testing.expectEqual(@as(u8, 42), emitter.code.items[2]);
}

test "emit add" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var emitter = Emitter.init(allocator);
    defer emitter.deinit();

    const builder = ir.IrBuilder.init(allocator);
    // Use 10 and 20 to avoid push_t optimization (fixnum 1 == t)
    const left = try builder.lit(Value.makeFixnum(10));
    const right = try builder.lit(Value.makeFixnum(20));
    const node = try builder.add(left, right);
    defer {
        allocator.destroy(left);
        allocator.destroy(right);
        allocator.destroy(node);
    }

    try emitter.emit(node);

    // push_i32(2+4) + push_i32(2+4) + add(2) = 14 bytes
    try testing.expectEqual(@as(usize, 14), emitter.code.items.len);
    // Last 2 bytes should be add opcode (little-endian u16)
    const add_opcode: u16 = @intFromEnum(Op.add);
    try testing.expectEqual(@as(u8, @truncate(add_opcode & 0xFF)), emitter.code.items[emitter.code.items.len - 2]);
    try testing.expectEqual(@as(u8, @truncate(add_opcode >> 8)), emitter.code.items[emitter.code.items.len - 1]);
}

test "emit if" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var emitter = Emitter.init(allocator);
    defer emitter.deinit();

    const builder = ir.IrBuilder.init(allocator);
    const cond = try builder.lit(Value.t);
    const then_branch = try builder.lit(Value.makeFixnum(1));
    const else_branch = try builder.lit(Value.makeFixnum(0));
    const node = try builder.ifExpr(cond, then_branch, else_branch);
    defer {
        allocator.destroy(cond);
        allocator.destroy(then_branch);
        allocator.destroy(else_branch);
        allocator.destroy(node);
    }

    try emitter.emit(node);

    // Should contain jmp_nil and jmp opcodes
    var has_jmp_nil = false;
    var has_jmp = false;
    for (emitter.code.items) |byte| {
        if (byte == @intFromEnum(Op.jmp_nil)) has_jmp_nil = true;
        if (byte == @intFromEnum(Op.jmp)) has_jmp = true;
    }
    try testing.expect(has_jmp_nil);
    try testing.expect(has_jmp);
}

test "finalize adds return" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();

    var emitter = Emitter.initWithHeap(allocator, &heap);
    defer emitter.deinit();

    const builder = ir.IrBuilder.init(allocator);
    const node = try builder.lit(Value.makeFixnum(42));
    defer allocator.destroy(node);

    try emitter.emit(node);
    const chunk_val = try emitter.finalize();
    const chunk = chunk_val.toPtr(Chunk);

    // Last instruction should be ret (2 bytes, little-endian)
    const code = chunk.getCode();
    const ret_opcode: u16 = @intFromEnum(Op.ret);
    try testing.expectEqual(@as(u8, @truncate(ret_opcode & 0xFF)), code[code.len - 2]);
    try testing.expectEqual(@as(u8, @truncate(ret_opcode >> 8)), code[code.len - 1]);
}
