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
const Chunk = opcodes.Chunk;
const Value = @import("../runtime/value.zig").Value;
const Heap = @import("../runtime/heap.zig").Heap;

pub const EmitError = error{
    OutOfMemory,
    TooManyConstants,
    TooManyLocals,
    JumpTooLong,
    InvalidIr,
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
    /// Child chunks (for lambdas)
    child_chunks: std.ArrayList(Chunk),
    /// Number of local variables
    num_locals: u8,
    /// Function arity (required params)
    arity: u8,
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
            .child_chunks = std.ArrayList(Chunk){},
            .num_locals = 0,
            .arity = 0,
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
            .child_chunks = std.ArrayList(Chunk){},
            .num_locals = 0,
            .arity = 0,
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
        // Free child chunks
        for (self.child_chunks.items) |chunk| {
            self.allocator.free(chunk.code);
            self.allocator.free(chunk.constants);
        }
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

    /// Emit bytecode for an IR node
    pub fn emit(self: *Emitter, node: *const Ir) EmitError!void {
        switch (node.*) {
            .lit => |v| try self.emitLiteral(v),
            .quote_sym => |name| try self.emitQuoteSym(name),
            .quote => |inner| try self.emitQuote(inner),
            .@"var" => |v| try self.emitVar(v.depth, v.index),
            .set => |s| try self.emitSet(s),
            .global_ref => |g| try self.emitGlobalRef(g.index),
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
            .throw => |t| try self.emitThrow(t),
            .tagbody => |tb| try self.emitTagbody(tb),
            .go => |g| try self.emitGo(g),
            .values => |v| try self.emitValues(v),
            .mv_bind => |m| try self.emitMvBind(m),
            .mv_list => |m| try self.emitMvList(m),
            .mv_call => |m| try self.emitMvCall(m),
            .format => |f| try self.emitFormat(f),
            .make_hash => |h| try self.emitMakeHash(h),
            .hash_get => |h| try self.emitHashGet(h),
            .hash_set => |h| try self.emitHashSet(h),
            .hash_rem => |h| try self.emitHashRem(h),
            .hash_count => |h| try self.emitUnaryOp(h.operand, .hash_count),
            .hashtablep => |h| try self.emitUnaryOp(h.operand, .hashtablep),
            .call => |c| try self.emitCall(c, false),
            .tailcall => |c| try self.emitCall(c, true),
            .apply => |a| try self.emitApply(a),

            // Arithmetic
            .add => |op| try self.emitBinaryOp(op, .add),
            .sub => |op| try self.emitBinaryOp(op, .sub),
            .mul => |op| try self.emitBinaryOp(op, .mul),
            .div => |op| try self.emitBinaryOp(op, .div),
            .mod => |op| try self.emitBinaryOp(op, .mod),

            // Comparison
            .eq => |op| try self.emitBinaryOp(op, .eq),
            .lt => |op| try self.emitBinaryOp(op, .lt),
            .gt => |op| try self.emitBinaryOp(op, .gt),
            .le => |op| try self.emitBinaryOp(op, .le),
            .ge => |op| try self.emitBinaryOp(op, .ge),
            .num_eq => |op| try self.emitBinaryOp(op, .num_eq),

            // Logic
            .not => |op| try self.emitUnaryOp(op.operand, .not),

            // List operations
            .cons => |op| try self.emitBinaryOp(op, .cons),
            .car => |op| try self.emitUnaryOp(op.operand, .car),
            .cdr => |op| try self.emitUnaryOp(op.operand, .cdr),
            .list => |elements| try self.emitList(elements),
            .append => |op| try self.emitBinaryOp(op, .append_lists),
            .length => |op| try self.emitUnaryOp(op.operand, .list_length),
            .reverse => |op| try self.emitUnaryOp(op.operand, .list_reverse),
            .nth => |op| try self.emitBinaryOp(op, .list_nth),
            .nthcdr => |op| try self.emitBinaryOp(op, .list_nthcdr),
            .last => |op| try self.emitUnaryOp(op.operand, .list_last),
            .member => |op| try self.emitBinaryOp(op, .list_member),

            // Type predicates
            .consp => |op| try self.emitUnaryOp(op.operand, .consp),
            .symbolp => |op| try self.emitUnaryOp(op.operand, .symbolp),
            .numberp => |op| try self.emitUnaryOp(op.operand, .numberp),
            .stringp => |op| try self.emitUnaryOp(op.operand, .stringp),
            .vectorp => |op| try self.emitUnaryOp(op.operand, .vectorp),
            .closurep => |op| try self.emitUnaryOp(op.operand, .closurep),
            .keywordp => |op| try self.emitUnaryOp(op.operand, .keywordp),
            .nilp => |op| try self.emitUnaryOp(op.operand, .nilp),
            .characterp => |op| try self.emitUnaryOp(op.operand, .characterp),
            .floatp => |op| try self.emitUnaryOp(op.operand, .floatp),

            // Character operations
            .char_code => |op| try self.emitUnaryOp(op.operand, .char_code),
            .code_char => |op| try self.emitUnaryOp(op.operand, .code_char),
            .char_eq => |op| try self.emitBinaryOp(op, .char_eq),
            .char_lt => |op| try self.emitBinaryOp(op, .char_lt),
            .char_gt => |op| try self.emitBinaryOp(op, .char_gt),
            .read_char => try self.emitOp(.read_char),
            .peek_char => try self.emitOp(.peek_char),
            .read => try self.emitOp(.read),
            .read_from_string => |op| try self.emitUnaryOp(op.operand, .read_from_string),
            .load => |op| try self.emitUnaryOp(op.operand, .load),
            .unread_char => |op| try self.emitUnaryOp(op.operand, .unread_char),
            .eval => |op| try self.emitUnaryOp(op.operand, .eval),
            .gensym => try self.emitOp(.gensym),
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

            // Vector operations
            .vec_new => |v| try self.emitVecNew(v),
            .vec_ref => |op| try self.emitBinaryOp(op, .vec_ref),
            .vec_set => |v| try self.emitVecSet(v),
            .vec_len => |op| try self.emitUnaryOp(op.operand, .vec_len),

            // Box operations (mutable cells)
            .make_box => |op| try self.emitUnaryOp(op.operand, .make_box),
            .box_ref => |op| try self.emitUnaryOp(op.operand, .box_ref),
            .box_set => |op| try self.emitBinaryOp(op, .box_set),

            // String operations
            .str_ref => |op| try self.emitBinaryOp(op, .str_ref),
            .str_len => |op| try self.emitUnaryOp(op.operand, .str_len),
            .str_concat => |op| try self.emitBinaryOp(op, .str_concat),
            .str_eq => |op| try self.emitBinaryOp(op, .str_eq),
            .substring => |op| {
                try self.emit(op.str);
                try self.emit(op.start);
                try self.emit(op.end);
                try self.emitOp(.substring);
            },

            // I/O
            .print => |op| try self.emitUnaryOp(op.operand, .print),
            .random => |op| try self.emitUnaryOp(op.operand, .random),
            .intern => |op| try self.emitUnaryOp(op.operand, .intern),
            .sym_name => |op| try self.emitUnaryOp(op.operand, .sym_name),
            .type_of => |op| try self.emitUnaryOp(op.operand, .type_of),

            // Type assertions
            .assert_fixnum => |op| try self.emitUnaryOp(op.operand, .check_fixnum),
            .assert_cons => |op| try self.emitUnaryOp(op.operand, .check_cons),
            .assert_symbol => |op| try self.emitUnaryOp(op.operand, .check_symbol),
            .assert_string => |op| try self.emitUnaryOp(op.operand, .check_string),
            .assert_vector => |op| try self.emitUnaryOp(op.operand, .check_vector),
            .assert_closure => |op| try self.emitUnaryOp(op.operand, .check_closure),
            .assert_non_nil => |op| try self.emitUnaryOp(op.operand, .check_non_nil),
            .assert_list => |op| try self.emitUnaryOp(op.operand, .check_list),
        }
    }

    /// Finalize and return the chunk
    pub fn finalize(self: *Emitter) EmitError!Chunk {
        // Add implicit return if not present
        if (self.code.items.len == 0) {
            // Empty function returns nil
            try self.emitOp(.push_nil);
            try self.emitOp(.ret);
        } else if (self.code.items[self.code.items.len - 1] != @intFromEnum(Op.ret)) {
            // Non-empty function: just add ret (value already on stack)
            try self.emitOp(.ret);
        }

        return Chunk{
            .code = try self.allocator.dupe(u8, self.code.items),
            .constants = try self.allocator.dupe(u64, self.constants.items),
            .arity = self.arity,
            .has_rest = self.has_rest,
            .num_locals = self.num_locals,
            .name = self.name,
        };
    }

    /// Get child chunks (caller takes ownership via duped slice)
    pub fn getChildChunks(self: *Emitter) ![]Chunk {
        const chunks = try self.allocator.dupe(Chunk, self.child_chunks.items);
        // Clear the list so deinit doesn't free the chunk contents
        self.child_chunks.items.len = 0;
        return chunks;
    }

    // ========================================================================
    // Emission helpers
    // ========================================================================

    fn emitOp(self: *Emitter, op: Op) EmitError!void {
        self.code.append(self.allocator, @intFromEnum(op)) catch
            return error.OutOfMemory;
    }

    fn emitU8(self: *Emitter, val: u8) EmitError!void {
        self.code.append(self.allocator, val) catch
            return error.OutOfMemory;
    }

    fn emitU16(self: *Emitter, val: u16) EmitError!void {
        self.code.append(self.allocator, @truncate(val)) catch
            return error.OutOfMemory;
        self.code.append(self.allocator, @truncate(val >> 8)) catch
            return error.OutOfMemory;
    }

    fn emitI16(self: *Emitter, val: i16) EmitError!void {
        try self.emitU16(@bitCast(val));
    }

    fn emitI32(self: *Emitter, val: i32) EmitError!void {
        const u: u32 = @bitCast(val);
        self.code.append(self.allocator, @truncate(u)) catch return error.OutOfMemory;
        self.code.append(self.allocator, @truncate(u >> 8)) catch return error.OutOfMemory;
        self.code.append(self.allocator, @truncate(u >> 16)) catch return error.OutOfMemory;
        self.code.append(self.allocator, @truncate(u >> 24)) catch return error.OutOfMemory;
    }

    /// Add constant to pool, return index
    fn addConstant(self: *Emitter, val: u64) EmitError!u16 {
        // Check if already in pool
        for (self.constants.items, 0..) |c, i| {
            if (c == val) return @intCast(i);
        }

        if (self.constants.items.len >= 65535) {
            return error.TooManyConstants;
        }

        const idx: u16 = @intCast(self.constants.items.len);
        self.constants.append(self.allocator, val) catch return error.OutOfMemory;
        return idx;
    }

    /// Get current code offset
    fn currentOffset(self: *const Emitter) usize {
        return self.code.items.len;
    }

    /// Emit placeholder jump, return offset to patch
    fn emitJump(self: *Emitter, op: Op) EmitError!usize {
        try self.emitOp(op);
        const offset = self.currentOffset();
        try self.emitI16(0); // Placeholder
        return offset;
    }

    /// Patch jump at offset to current position
    fn patchJump(self: *Emitter, offset: usize) EmitError!void {
        const target = self.currentOffset();
        const distance = @as(i32, @intCast(target)) - @as(i32, @intCast(offset + 2));

        if (distance > 32767 or distance < -32768) {
            return error.JumpTooLong;
        }

        const val: i16 = @intCast(distance);
        const u: u16 = @bitCast(val);
        self.code.items[offset] = @truncate(u);
        self.code.items[offset + 1] = @truncate(u >> 8);
    }

    // ========================================================================
    // IR emission
    // ========================================================================

    fn emitLiteral(self: *Emitter, val: Value) EmitError!void {
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

    fn emitQuoteSym(self: *Emitter, name: []const u8) EmitError!void {
        if (self.heap) |heap| {
            // Intern symbol and add to constant pool
            const sym = heap.intern(name) orelse return error.OutOfMemory;
            const idx = try self.addConstant(sym.raw);
            try self.emitOp(.push_const);
            try self.emitU16(idx);
        } else {
            // No heap available - push nil as fallback
            try self.emitOp(.push_nil);
        }
    }

    fn emitQuote(self: *Emitter, inner: *const Ir) EmitError!void {
        // Quote just emits the literal value
        try self.emit(inner);
    }

    fn emitVar(self: *Emitter, depth: u16, index: u16) EmitError!void {
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

    fn emitGlobalRef(self: *Emitter, index: u16) EmitError!void {
        try self.emitOp(.load_global);
        try self.emitU16(index);
    }

    fn emitDefine(self: *Emitter, d: anytype) EmitError!void {
        // Emit value
        try self.emit(d.value);
        // Store to global
        try self.emitOp(.store_global);
        try self.emitU16(d.index);
        // Define leaves value on stack (for REPL)
        try self.emitOp(.load_global);
        try self.emitU16(d.index);
    }

    fn emitSet(self: *Emitter, s: anytype) EmitError!void {
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

    fn emitLet(self: *Emitter, l: anytype) EmitError!void {
        // Emit binding values
        for (l.bindings) |b| {
            try self.emit(b.value);
            // Values are now on stack as locals
        }

        // Emit body
        try self.emit(l.body);

        // TODO: Pop locals when let scope ends
    }

    fn emitLambda(self: *Emitter, lam: anytype) EmitError!void {
        // Create nested emitter for lambda body - inherit heap for symbol interning
        var lambda_emitter = if (self.heap) |h|
            Emitter.initWithHeap(self.allocator, h)
        else
            Emitter.init(self.allocator);

        lambda_emitter.arity = @intCast(lam.params.len);
        lambda_emitter.has_rest = lam.rest_param != null;
        // num_locals = required params + optional rest param
        const rest_count: u8 = if (lam.rest_param != null) 1 else 0;
        lambda_emitter.num_locals = @intCast(lam.params.len + rest_count);
        // Pass captures so emitVar knows which variables to load from capture array
        lambda_emitter.captures = lam.captures;

        // Emit body
        lambda_emitter.emit(lam.body) catch {
            lambda_emitter.deinit();
            return error.InvalidIr;
        };

        // Finalize lambda chunk
        const chunk = lambda_emitter.finalize() catch {
            lambda_emitter.deinit();
            return error.OutOfMemory;
        };

        // Collect any child chunks from the lambda
        for (lambda_emitter.child_chunks.items) |child_chunk| {
            self.child_chunks.append(self.allocator, child_chunk) catch {
                lambda_emitter.deinit();
                return error.OutOfMemory;
            };
        }
        lambda_emitter.child_chunks.items.len = 0; // Prevent double-free
        lambda_emitter.deinit();

        // Store chunk in child_chunks, get its index
        const chunk_idx: u16 = @intCast(self.child_chunks.items.len);
        self.child_chunks.append(self.allocator, chunk) catch return error.OutOfMemory;

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

    fn emitIf(self: *Emitter, i: anytype) EmitError!void {
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

    fn emitProgn(self: *Emitter, exprs: []const *const Ir) EmitError!void {
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

    fn emitLoop(self: *Emitter, l: anytype) EmitError!void {
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
            @as(i32, @intCast(self.currentOffset() + 3)); // +3 for jmp instruction
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

    fn emitBlock(self: *Emitter, b: anytype) EmitError!void {
        // Push a new block onto the control stack
        const entry = ControlEntry{
            .block = .{
                .name = b.name,
                .pending_exits = std.ArrayList(usize){},
            },
        };
        self.control_stack.append(self.allocator, entry) catch return error.OutOfMemory;

        // Emit body
        try self.emit(b.body);

        // Pop block and patch all pending exit jumps to here
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

    fn emitReturnFrom(self: *Emitter, r: anytype) EmitError!void {
        // Emit the return value
        try self.emit(r.value);

        // Find the block by name, emitting cleanup for any unwind-protects crossed
        var i = self.control_stack.items.len;
        while (i > 0) {
            i -= 1;
            switch (self.control_stack.items[i]) {
                .block => |*blk| {
                    if (std.mem.eql(u8, blk.name, r.name)) {
                        // Found target block - record jump for patching
                        const jump_loc = try self.emitJump(.jmp);
                        blk.pending_exits.append(self.allocator, jump_loc) catch
                            return error.OutOfMemory;
                        return;
                    }
                    // Not our target block, keep searching
                },
                .unwind_protect => |up| {
                    // Crossing an unwind-protect - emit cleanup
                    // Note: cleanup result is discarded, value is already on stack
                    try self.emit(up.cleanup);
                    try self.emitOp(.pop);
                },
                .tagbody => {
                    // Crossing a tagbody - no cleanup, just continue
                },
            }
        }
        // Block not found - this is a compile error
        return error.InvalidIr;
    }

    fn emitUnwindProtect(self: *Emitter, u: anytype) EmitError!void {
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
        try self.code.append(self.allocator, 0);
        try self.code.append(self.allocator, 0);
    }

    fn emitCatch(self: *Emitter, c: anytype) EmitError!void {
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

    fn emitThrow(self: *Emitter, t: anytype) EmitError!void {
        // Emit tag and value
        try self.emit(t.tag);
        try self.emit(t.value);

        // Emit throw opcode - VM will unwind to matching catch
        try self.emitOp(.throw);
    }

    fn emitTagbody(self: *Emitter, tb: anytype) EmitError!void {
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

    fn emitGo(self: *Emitter, g: anytype) EmitError!void {
        // Find enclosing tagbody with matching tag
        var i = self.control_stack.items.len;
        while (i > 0) {
            i -= 1;
            switch (self.control_stack.items[i]) {
                .tagbody => |*tbe| {
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

    fn emitValues(self: *Emitter, v: anytype) EmitError!void {
        // Emit each value expression
        for (v) |val| {
            try self.emit(val);
        }

        // Emit values opcode with count
        if (v.len > 255) return error.TooManyLocals;
        try self.emitOp(.values);
        try self.emitU8(@intCast(v.len));
    }

    fn emitMvBind(self: *Emitter, m: anytype) EmitError!void {
        // Emit the expression that produces multiple values
        // This may produce a `values` opcode that stores secondary values
        try self.emit(m.expr);

        // Emit mv_bind opcode with count
        // This takes primary from stack, expands secondary values onto stack
        if (m.vars.len > 255) return error.TooManyLocals;
        try self.emitOp(.mv_bind);
        try self.emitU8(@intCast(m.vars.len));

        // Emit body - variables are now on stack as locals
        try self.emit(m.body);
    }

    fn emitMvList(self: *Emitter, m: anytype) EmitError!void {
        // Evaluate the expression (leaves primary on stack, secondaries in buffer)
        try self.emit(m.expr);
        // Emit mv_list opcode - gathers all values into a list
        try self.emitOp(.mv_list);
    }

    fn emitMvCall(self: *Emitter, m: anytype) EmitError!void {
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

    fn emitFormat(self: *Emitter, f: anytype) EmitError!void {
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

    fn emitMakeHash(self: *Emitter, h: anytype) EmitError!void {
        try self.emitOp(.make_hash);
        try self.emitU16(h.capacity);
    }

    fn emitHashGet(self: *Emitter, h: anytype) EmitError!void {
        try self.emit(h.table);
        try self.emit(h.key);
        try self.emitOp(.hash_get);
    }

    fn emitHashSet(self: *Emitter, h: anytype) EmitError!void {
        try self.emit(h.table);
        try self.emit(h.key);
        try self.emit(h.value);
        try self.emitOp(.hash_set);
    }

    fn emitHashRem(self: *Emitter, h: anytype) EmitError!void {
        try self.emit(h.table);
        try self.emit(h.key);
        try self.emitOp(.hash_rem);
    }

    /// Patch a jump to a specific target offset
    fn patchJumpTo(self: *Emitter, jump_loc: usize, target: usize) EmitError!void {
        const offset = @as(i32, @intCast(target)) - @as(i32, @intCast(jump_loc + 2));
        if (offset > 32767 or offset < -32768) {
            return error.JumpTooLong;
        }
        const displacement: i16 = @intCast(offset);
        self.code.items[jump_loc] = @bitCast(@as(u8, @truncate(@as(u16, @bitCast(displacement)))));
        self.code.items[jump_loc + 1] = @bitCast(@as(u8, @truncate(@as(u16, @bitCast(displacement)) >> 8)));
    }

    /// Patch a jump at a specific location to jump to current offset
    fn patchJumpAt(self: *Emitter, jump_loc: usize) EmitError!void {
        const target = self.currentOffset();
        const offset = @as(i32, @intCast(target)) - @as(i32, @intCast(jump_loc + 2));
        if (offset > 32767 or offset < -32768) {
            return error.JumpTooLong;
        }
        const displacement: i16 = @intCast(offset);
        self.code.items[jump_loc] = @bitCast(@as(u8, @truncate(@as(u16, @bitCast(displacement)))));
        self.code.items[jump_loc + 1] = @bitCast(@as(u8, @truncate(@as(u16, @bitCast(displacement)) >> 8)));
    }

    fn emitCall(self: *Emitter, c: anytype, tail: bool) EmitError!void {
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

    fn emitApply(self: *Emitter, a: anytype) EmitError!void {
        // Emit function then args list
        try self.emit(a.func);
        try self.emit(a.args);
        try self.emitOp(.apply);
    }

    fn emitBinaryOp(self: *Emitter, op: Ir.BinaryOp, opcode: Op) EmitError!void {
        try self.emit(op.left);
        try self.emit(op.right);
        try self.emitOp(opcode);
    }

    fn emitUnaryOp(self: *Emitter, operand: *const Ir, opcode: Op) EmitError!void {
        try self.emit(operand);
        try self.emitOp(opcode);
    }

    fn emitList(self: *Emitter, elements: []const *const Ir) EmitError!void {
        // Emit elements in order
        for (elements) |elem| {
            try self.emit(elem);
        }

        // Make list
        if (elements.len > 255) return error.TooManyLocals;
        try self.emitOp(.make_list);
        try self.emitU8(@intCast(elements.len));
    }

    fn emitVecNew(self: *Emitter, v: anytype) EmitError!void {
        try self.emit(v.size);
        if (v.init) |init_val| {
            try self.emit(init_val);
            // TODO: Need vec_new_init opcode
        }
        try self.emitOp(.make_vec);
        try self.emitU16(0); // Size from stack
    }

    fn emitVecSet(self: *Emitter, v: anytype) EmitError!void {
        try self.emit(v.vec);
        try self.emit(v.index);
        try self.emit(v.value);
        try self.emitOp(.vec_set);
    }
};

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
    try testing.expectEqual(@as(u8, @intFromEnum(Op.push_i32)), emitter.code.items[0]);
    // 42 in little-endian i32
    try testing.expectEqual(@as(u8, 42), emitter.code.items[1]);
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

    // push_i32(5) + push_i32(5) + add(1) = 11 bytes
    try testing.expectEqual(@as(usize, 11), emitter.code.items.len);
    // Last byte should be add opcode
    try testing.expectEqual(@as(u8, @intFromEnum(Op.add)), emitter.code.items[emitter.code.items.len - 1]);
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

    var emitter = Emitter.init(allocator);
    defer emitter.deinit();

    const builder = ir.IrBuilder.init(allocator);
    const node = try builder.lit(Value.makeFixnum(42));
    defer allocator.destroy(node);

    try emitter.emit(node);
    const chunk = try emitter.finalize();
    defer allocator.free(chunk.code);
    defer allocator.free(chunk.constants);

    // Last instruction should be ret
    try testing.expectEqual(@as(u8, @intFromEnum(Op.ret)), chunk.code[chunk.code.len - 1]);
}
