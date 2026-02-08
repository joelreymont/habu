//! JIT runtime call helpers

const ctx = @import("ctx.zig");
const runtime = @import("../runtime/runtime.zig");
const arith = @import("../runtime/primitives/arith.zig");
const char_prims = @import("../runtime/primitives/char.zig");
const str_prims = @import("../runtime/primitives/string.zig");
const vec_prims = @import("../runtime/primitives/vector.zig");
const io_prims = @import("../runtime/primitives/io.zig");
const vm_mod = @import("../interp/vm.zig");

const Value = runtime.Value;
const std = @import("std");

const BinaryOp = *const fn (*runtime.Heap, Value, Value) arith.Error!Value;

// C-ABI wrapper generator for JIT call targets.
// Zig's internal ABI for error unions differs between Debug and Release
// (hidden error return trace, sret placement). C calling convention is stable.
fn coerce(comptime T: type, raw: u64) T {
    if (T == Value) return Value{ .raw = raw };
    if (T == u8) return @truncate(raw);
    if (T == u16) return @truncate(raw);
    if (T == *ctx.JitContext) return @ptrFromInt(raw);
    @compileError("unsupported JIT arg type: " ++ @typeName(T));
}

pub fn cWrap1(comptime f: anytype) *const fn (*ctx.JitContext, u64) callconv(.c) u64 {
    const info = @typeInfo(@TypeOf(f)).@"fn";
    const P1 = info.params[1].type.?;
    return &struct {
        fn call(c: *ctx.JitContext, a: u64) callconv(.c) u64 {
            const val = @call(.auto, f, .{ c, coerce(P1, a) }) catch |err| {
                c.ret_buf.err = @intFromError(err);
                return Value.nil.raw;
            };
            return val.raw;
        }
    }.call;
}

pub fn cWrap2(comptime f: anytype) *const fn (*ctx.JitContext, u64, u64) callconv(.c) u64 {
    const info = @typeInfo(@TypeOf(f)).@"fn";
    const P1 = info.params[1].type.?;
    const P2 = info.params[2].type.?;
    return &struct {
        fn call(c: *ctx.JitContext, a: u64, b: u64) callconv(.c) u64 {
            const val = @call(.auto, f, .{ c, coerce(P1, a), coerce(P2, b) }) catch |err| {
                c.ret_buf.err = @intFromError(err);
                return Value.nil.raw;
            };
            return val.raw;
        }
    }.call;
}

fn stackLen(c: *ctx.JitContext) usize {
    const len_bytes = @intFromPtr(c.sp) - @intFromPtr(c.frame_base);
    return @intCast(@divExact(len_bytes, @sizeOf(Value)));
}

fn syncVmSp(c: *ctx.JitContext) void {
    // JIT keeps the authoritative sp in ctx.sp; some VM helpers assume vm.sp is current.
    c.vm.sp = stackLen(c);
}

fn stackCap(c: *ctx.JitContext) usize {
    const cap_bytes = @intFromPtr(c.stack_end) - @intFromPtr(c.frame_base);
    return @intCast(@divExact(cap_bytes, @sizeOf(Value)));
}

fn collectJitGarbage(c: *ctx.JitContext, extra: []Value) !void {
    syncVmSp(c);

    c.vm.setExtRoots(extra);
    defer {
        c.vm.clearExtRoots();
        // const_pool points into a GC-managed Chunk; refresh after relocation.
        c.const_pool = c.vm.chunk.const_pool;
        c.const_count = @intCast(c.vm.chunk.const_count);
    }

    _ = try c.vm.collectGarbage();
}

fn callBinaryWithGc(c: *ctx.JitContext, a: Value, b: Value, func: BinaryOp) arith.Error!Value {
    var args = [_]Value{ a, b };
    return func(c.heap, args[0], args[1]) catch |err| switch (err) {
        error.OutOfMemory => blk: {
            try collectJitGarbage(c, &args);
            break :blk try func(c.heap, args[0], args[1]);
        },
        else => return err,
    };
}

fn allocConsWithGc(c: *ctx.JitContext, car: Value, cdr: Value) vm_mod.Error!Value {
    var args = [_]Value{ car, cdr };
    return c.heap.allocCons(args[0], args[1]) catch |err| switch (err) {
        error.OutOfMemory => blk: {
            try collectJitGarbage(c, &args);
            break :blk try c.heap.allocCons(args[0], args[1]);
        },
        else => return err,
    };
}

pub fn cons(c: *ctx.JitContext, car: Value, cdr: Value) vm_mod.Error!Value {
    return try allocConsWithGc(c, car, cdr);
}

fn allocVectorWithGc(c: *ctx.JitContext, len: usize) vm_mod.Error!Value {
    return c.heap.allocVector(len, len) catch |err| switch (err) {
        error.OutOfMemory => blk: {
            var none: [0]Value = .{};
            try collectJitGarbage(c, none[0..]);
            break :blk try c.heap.allocVector(len, len);
        },
        else => return err,
    };
}

fn allocClosureWithGc(c: *ctx.JitContext, code: Value, arity: u32, captures: []Value) vm_mod.Error!Value {
    return c.heap.allocClosure(code, arity, captures) catch |err| switch (err) {
        error.OutOfMemory => blk: {
            try collectJitGarbage(c, captures);
            break :blk try c.heap.allocClosure(code, arity, captures);
        },
        error.Overflow => return error.Overflow,
        else => return err,
    };
}

pub fn add(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    return try callBinaryWithGc(c, a, b, arith.add);
}

pub fn sub(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    return try callBinaryWithGc(c, a, b, arith.sub);
}

pub fn mul(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    return try callBinaryWithGc(c, a, b, arith.mul);
}

pub fn div(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    return try callBinaryWithGc(c, a, b, arith.div);
}

pub fn mod(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    _ = c;
    return try arith.mod(a, b);
}

pub fn neg(c: *ctx.JitContext, a: Value) arith.Error!Value {
    _ = c;
    return try arith.negate(a);
}

pub fn numberp(c: *ctx.JitContext, a: Value) arith.Error!Value {
    _ = c;
    return if (a.isNumber()) Value.t else Value.nil;
}

pub fn integerp(c: *ctx.JitContext, a: Value) arith.Error!Value {
    _ = c;
    return if (a.isFixnum() or a.isBignum()) Value.t else Value.nil;
}

pub fn realp(c: *ctx.JitContext, a: Value) arith.Error!Value {
    _ = c;
    return if (a.isFixnum() or a.isBignum() or a.isFloat() or a.isRational()) Value.t else Value.nil;
}

pub fn consp(c: *ctx.JitContext, a: Value) vm_mod.Error!Value {
    _ = c;
    return if (a.isCons()) Value.t else Value.nil;
}

pub fn symbolp(c: *ctx.JitContext, a: Value) vm_mod.Error!Value {
    _ = c;
    return if (a.isSymbolLike()) Value.t else Value.nil;
}

pub fn stringp(c: *ctx.JitContext, a: Value) vm_mod.Error!Value {
    _ = c;
    return if (a.isString()) Value.t else Value.nil;
}

pub fn vectorp(c: *ctx.JitContext, a: Value) vm_mod.Error!Value {
    _ = c;
    return if (a.isVector()) Value.t else Value.nil;
}

pub fn closurep(c: *ctx.JitContext, a: Value) vm_mod.Error!Value {
    _ = c;
    return if (a.isClosure()) Value.t else Value.nil;
}

pub fn keywordp(c: *ctx.JitContext, a: Value) vm_mod.Error!Value {
    _ = c;
    return if (a.isKeyword()) Value.t else Value.nil;
}

pub fn characterp(c: *ctx.JitContext, a: Value) vm_mod.Error!Value {
    _ = c;
    return if (a.isCharacter()) Value.t else Value.nil;
}

pub fn floatp(c: *ctx.JitContext, a: Value) vm_mod.Error!Value {
    _ = c;
    return if (a.isFloat()) Value.t else Value.nil;
}

pub fn charCode(c: *ctx.JitContext, val: Value) vm_mod.Error!Value {
    _ = c;
    if (!val.isCharacter()) return error.TypeMismatch;
    const cp = val.toCharacter();
    return Value.makeFixnum(@intCast(cp));
}

pub fn codeChar(c: *ctx.JitContext, val: Value) vm_mod.Error!Value {
    _ = c;
    if (!val.isFixnum()) return error.TypeMismatch;
    const n = val.toFixnum();
    if (n < 0 or n > 0x10FFFF) return error.InvalidArgument;
    return Value.makeCharacter(@intCast(n));
}

pub fn charEq(c: *ctx.JitContext, a: Value, b: Value) vm_mod.Error!Value {
    _ = c;
    return char_prims.charEq(a, b);
}

pub fn charLt(c: *ctx.JitContext, a: Value, b: Value) vm_mod.Error!Value {
    _ = c;
    return char_prims.charLt(a, b);
}

pub fn charGt(c: *ctx.JitContext, a: Value, b: Value) vm_mod.Error!Value {
    _ = c;
    return char_prims.charGt(a, b);
}

pub fn charUpcase(c: *ctx.JitContext, val: Value) vm_mod.Error!Value {
    _ = c;
    return char_prims.charUpcase(val);
}

pub fn charDowncase(c: *ctx.JitContext, val: Value) vm_mod.Error!Value {
    _ = c;
    return char_prims.charDowncase(val);
}

pub fn digitCharP(c: *ctx.JitContext, val: Value) vm_mod.Error!Value {
    _ = c;
    return char_prims.digitp(val);
}

pub fn alphaCharP(c: *ctx.JitContext, val: Value) vm_mod.Error!Value {
    _ = c;
    return char_prims.alphap(val);
}

pub fn stringUpcase(c: *ctx.JitContext, val: Value) vm_mod.Error!Value {
    if (!val.isString()) return error.TypeMismatch;
    var args = [_]Value{ val };
    return str_prims.stringUpcase(c.heap, args[0]) catch |err| switch (err) {
        error.OutOfMemory => blk: {
            try collectJitGarbage(c, &args);
            break :blk try str_prims.stringUpcase(c.heap, args[0]);
        },
        else => return err,
    };
}

pub fn stringDowncase(c: *ctx.JitContext, val: Value) vm_mod.Error!Value {
    if (!val.isString()) return error.TypeMismatch;
    var args = [_]Value{ val };
    return str_prims.stringDowncase(c.heap, args[0]) catch |err| switch (err) {
        error.OutOfMemory => blk: {
            try collectJitGarbage(c, &args);
            break :blk try str_prims.stringDowncase(c.heap, args[0]);
        },
        else => return err,
    };
}

pub fn writeToString(c: *ctx.JitContext, val: Value) vm_mod.Error!Value {
    var args = [_]Value{ val };
    return io_prims.writeToString(c.heap, args[0]) catch |err| switch (err) {
        error.OutOfMemory => blk: {
            try collectJitGarbage(c, &args);
            break :blk try io_prims.writeToString(c.heap, args[0]);
        },
        else => return err,
    };
}

pub fn random(c: *ctx.JitContext, n: Value) vm_mod.Error!Value {
    return try arith.random(c.heap, &c.vm.prng, &c.vm.prng_seeded, n);
}

pub fn randomSeed(c: *ctx.JitContext, seed: Value) vm_mod.Error!Value {
    return try arith.randomSeed(&c.vm.prng, &c.vm.prng_seeded, seed);
}

pub fn strRef(c: *ctx.JitContext, str_val: Value, idx_val: Value) vm_mod.Error!Value {
    _ = c;
    if (!str_val.isString() or !idx_val.isFixnum()) return error.TypeMismatch;
    const str = str_val.toPtr(runtime.String);
    const idx_signed = idx_val.toFixnum();
    if (idx_signed < 0) return error.TypeMismatch;
    const idx: usize = @intCast(idx_signed);
    if (idx >= str.length) return error.TypeMismatch;
    return Value.makeFixnum(str.bytes()[idx]);
}

pub fn strLen(c: *ctx.JitContext, str_val: Value) vm_mod.Error!Value {
    _ = c;
    if (!str_val.isString()) return error.TypeMismatch;
    const str = str_val.toPtr(runtime.String);
    return Value.makeFixnum(@intCast(str.length));
}

pub fn strSet(c: *ctx.JitContext, _: u8) vm_mod.Error!Value {
    const sp = stackLen(c);
    if (sp < 3) return error.StackUnderflow;

    const char_val = c.frame_base[sp - 1];
    const idx_val = c.frame_base[sp - 2];
    const str_val = c.frame_base[sp - 3];
    if (!str_val.isString() or !idx_val.isFixnum()) return error.TypeMismatch;
    const str = str_val.toPtr(runtime.String);
    const idx_signed = idx_val.toFixnum();
    if (idx_signed < 0) return error.TypeMismatch;
    const idx: usize = @intCast(idx_signed);
    if (idx >= str.length) return error.TypeMismatch;
    const char_int = switch (char_val.typeKind()) {
        .fixnum => char_val.toFixnum(),
        .char => @as(i64, @intCast(char_val.toCharacter())),
        else => return error.TypeMismatch,
    };
    if (char_int < 0 or char_int > 255) return error.TypeMismatch;
    str.mutableBytes()[idx] = @intCast(char_int);

    c.frame_base[sp - 3] = str_val;
    c.sp = c.frame_base + sp - 2;
    return str_val;
}

pub fn strConcat(c: *ctx.JitContext, s1: Value, s2: Value) vm_mod.Error!Value {
    if (!s1.isString() or !s2.isString()) return error.TypeMismatch;
    var args = [_]Value{ s1, s2 };
    var len1: usize = s1.toPtr(runtime.String).length;
    var len2: usize = s2.toPtr(runtime.String).length;
    var new_len = try std.math.add(usize, len1, len2);

    const result = c.heap.allocStringUninitialized(new_len) catch |err| switch (err) {
        error.OutOfMemory => blk: {
            try collectJitGarbage(c, &args);
            len1 = args[0].toPtr(runtime.String).length;
            len2 = args[1].toPtr(runtime.String).length;
            new_len = try std.math.add(usize, len1, len2);
            break :blk try c.heap.allocStringUninitialized(new_len);
        },
        else => return err,
    };

    const dest = result.toPtr(runtime.String).mutableBytes();
    const str1 = args[0].toPtr(runtime.String);
    const str2 = args[1].toPtr(runtime.String);
    @memcpy(dest[0..len1], str1.bytes());
    @memcpy(dest[len1..new_len], str2.bytes());
    return result;
}

pub fn strEq(c: *ctx.JitContext, a: Value, b: Value) vm_mod.Error!Value {
    _ = c;
    return if (str_prims.stringEqual(a, b)) Value.t else Value.nil;
}

pub fn strLt(c: *ctx.JitContext, a: Value, b: Value) vm_mod.Error!Value {
    _ = c;
    return if (str_prims.stringLt(a, b)) Value.t else Value.nil;
}

pub fn strGt(c: *ctx.JitContext, a: Value, b: Value) vm_mod.Error!Value {
    _ = c;
    return if (str_prims.stringGt(a, b)) Value.t else Value.nil;
}

pub fn strLe(c: *ctx.JitContext, a: Value, b: Value) vm_mod.Error!Value {
    _ = c;
    return if (str_prims.stringLe(a, b)) Value.t else Value.nil;
}

pub fn strGe(c: *ctx.JitContext, a: Value, b: Value) vm_mod.Error!Value {
    _ = c;
    return if (str_prims.stringGe(a, b)) Value.t else Value.nil;
}

pub fn write(c: *ctx.JitContext, val: Value) vm_mod.Error!Value {
    try c.vm.syncPrintGlobals();
    return try io_prims.write(val, Value.nil);
}

pub fn print(c: *ctx.JitContext, val: Value) vm_mod.Error!Value {
    try c.vm.syncPrintGlobals();
    return try io_prims.print(val, Value.nil);
}

pub fn princ(c: *ctx.JitContext, val: Value) vm_mod.Error!Value {
    try c.vm.syncPrintGlobals();
    return try io_prims.princ(val, Value.nil);
}

pub fn terpri(c: *ctx.JitContext, _: Value) vm_mod.Error!Value {
    _ = c;
    try io_prims.sysNewline();
    return Value.nil;
}

pub fn writeChar(c: *ctx.JitContext, val: Value) vm_mod.Error!Value {
    _ = c;
    if (!val.isCharacter()) return error.TypeMismatch;
    const cp = val.toCharacter();
    if (cp < 128) {
        try io_prims.sysWriteChar(@intCast(cp));
    } else {
        var buf: [4]u8 = undefined;
        const len = try std.unicode.utf8Encode(@intCast(cp), &buf);
        try io_prims.sysWriteBytes(buf[0..len]);
    }
    return val;
}

pub fn listp(c: *ctx.JitContext, a: Value) vm_mod.Error!Value {
    _ = c;
    return if (a.isNil() or a.isCons()) Value.t else Value.nil;
}

pub fn atom(c: *ctx.JitContext, a: Value) vm_mod.Error!Value {
    _ = c;
    return if (!a.isCons()) Value.t else Value.nil;
}

pub fn listMember(c: *ctx.JitContext, item: Value, list: Value) vm_mod.Error!Value {
    _ = c;
    var curr = list;
    while (curr.isCons()) {
        const cell = curr.toPtr(runtime.Cons);
        if (cell.car.raw == item.raw) return curr;
        curr = cell.cdr;
    }
    return Value.nil;
}

pub fn listMemberEql(c: *ctx.JitContext, item: Value, list: Value) vm_mod.Error!Value {
    _ = c;
    var curr = list;
    while (curr.isCons()) {
        const cell = curr.toPtr(runtime.Cons);
        if (vm_mod.hashKeyEqualWithTest(cell.car, item, .eql)) return curr;
        curr = cell.cdr;
    }
    return Value.nil;
}

pub fn listMemberEqual(c: *ctx.JitContext, item: Value, list: Value) vm_mod.Error!Value {
    _ = c;
    var curr = list;
    while (curr.isCons()) {
        const cell = curr.toPtr(runtime.Cons);
        if (vm_mod.hashKeyEqualWithTest(cell.car, item, .equal)) return curr;
        curr = cell.cdr;
    }
    return Value.nil;
}

pub fn assoc(c: *ctx.JitContext, key: Value, alist: Value) vm_mod.Error!Value {
    _ = c;
    var curr = alist;
    while (curr.isCons()) {
        const cell = curr.toPtr(runtime.Cons);
        if (cell.car.isCons()) {
            const pair = cell.car.toPtr(runtime.Cons);
            if (pair.car.raw == key.raw) return cell.car;
        }
        curr = cell.cdr;
    }
    return Value.nil;
}

pub fn assocEql(c: *ctx.JitContext, key: Value, alist: Value) vm_mod.Error!Value {
    _ = c;
    var curr = alist;
    while (curr.isCons()) {
        const cell = curr.toPtr(runtime.Cons);
        if (cell.car.isCons()) {
            const pair = cell.car.toPtr(runtime.Cons);
            if (vm_mod.hashKeyEqualWithTest(pair.car, key, .eql)) return cell.car;
        }
        curr = cell.cdr;
    }
    return Value.nil;
}

pub fn assocEqual(c: *ctx.JitContext, key: Value, alist: Value) vm_mod.Error!Value {
    _ = c;
    var curr = alist;
    while (curr.isCons()) {
        const cell = curr.toPtr(runtime.Cons);
        if (cell.car.isCons()) {
            const pair = cell.car.toPtr(runtime.Cons);
            if (vm_mod.hashKeyEqualWithTest(pair.car, key, .equal)) return cell.car;
        }
        curr = cell.cdr;
    }
    return Value.nil;
}

pub fn listFind(c: *ctx.JitContext, item: Value, seq: Value) vm_mod.Error!Value {
    return try c.vm.findInSeq(item, seq, .eql);
}

pub fn listFindEq(c: *ctx.JitContext, item: Value, seq: Value) vm_mod.Error!Value {
    return try c.vm.findInSeq(item, seq, .eq);
}

pub fn listFindEqual(c: *ctx.JitContext, item: Value, seq: Value) vm_mod.Error!Value {
    return try c.vm.findInSeq(item, seq, .equal);
}

pub fn listPosition(c: *ctx.JitContext, item: Value, seq: Value) vm_mod.Error!Value {
    return try c.vm.positionInSeq(item, seq, .eql);
}

pub fn listCount(c: *ctx.JitContext, item: Value, seq: Value) vm_mod.Error!Value {
    return try c.vm.countInSeq(item, seq, .eql);
}

pub fn listCountEq(c: *ctx.JitContext, item: Value, seq: Value) vm_mod.Error!Value {
    return try c.vm.countInSeq(item, seq, .eq);
}

pub fn listCountEqual(c: *ctx.JitContext, item: Value, seq: Value) vm_mod.Error!Value {
    return try c.vm.countInSeq(item, seq, .equal);
}

pub fn listRemove(c: *ctx.JitContext, item: Value, seq: Value) vm_mod.Error!Value {
    syncVmSp(c);
    return try c.vm.listRemoveWithTest(item, seq, .eql);
}

pub fn listRemoveEq(c: *ctx.JitContext, item: Value, seq: Value) vm_mod.Error!Value {
    syncVmSp(c);
    return try c.vm.listRemoveWithTest(item, seq, .eq);
}

pub fn listRemoveEqual(c: *ctx.JitContext, item: Value, seq: Value) vm_mod.Error!Value {
    syncVmSp(c);
    return try c.vm.listRemoveWithTest(item, seq, .equal);
}

pub fn listLast(c: *ctx.JitContext, list: Value) vm_mod.Error!Value {
    _ = c;
    switch (list.typeKind()) {
        .nil => return Value.nil,
        .cons => {
            var curr = list;
            while (curr.isCons()) {
                const cell = curr.toPtr(runtime.Cons);
                if (!cell.cdr.isCons()) return curr;
                curr = cell.cdr;
            }
            return Value.nil;
        },
        else => return error.TypeMismatch,
    }
}

pub fn listLength(c: *ctx.JitContext, seq: Value) vm_mod.Error!Value {
    _ = c;
    switch (seq.typeKind()) {
        .nil => return Value.makeFixnum(0),
        .cons => {
            var len: i64 = 0;
            var curr = seq;
            while (curr.isCons()) {
                len += 1;
                curr = curr.toPtr(runtime.Cons).cdr;
            }
            if (!curr.isNil()) return error.TypeMismatch;
            return Value.makeFixnum(len);
        },
        .vector => {
            const vec = seq.toPtr(runtime.Vector);
            return Value.makeFixnum(@intCast(vec.length));
        },
        .string => {
            const str = seq.toPtr(runtime.String);
            return Value.makeFixnum(@intCast(str.length));
        },
        else => return error.TypeMismatch,
    }
}

pub fn listReverse(c: *ctx.JitContext, list: Value) vm_mod.Error!Value {
    var curr = list;
    var res = Value.nil;
    var roots = [_]Value{ curr, res };

    while (curr.isCons()) {
        var cell = curr.toPtr(runtime.Cons);
        var car = cell.car;
        var next = cell.cdr;
        roots[0] = curr;
        roots[1] = res;

        const new_cell = c.heap.allocCons(car, res) catch |err| switch (err) {
            error.OutOfMemory => blk: {
                try collectJitGarbage(c, &roots);
                curr = roots[0];
                res = roots[1];
                cell = curr.toPtr(runtime.Cons);
                car = cell.car;
                next = cell.cdr;
                break :blk try c.heap.allocCons(car, res);
            },
            else => return err,
        };

        res = new_cell;
        curr = next;
    }
    if (!curr.isNil()) return error.TypeMismatch;
    return res;
}

pub fn appendLists(c: *ctx.JitContext, list1: Value, list2_in: Value) vm_mod.Error!Value {
    var list2 = list2_in;
    switch (list1.typeKind()) {
        .nil => return list2,
        .cons => {},
        else => return error.TypeMismatch,
    }

    var curr = list1;
    var head = Value.nil;
    var tail = Value.nil;
    var roots = [_]Value{ curr, list2, head, tail };

    while (curr.isCons()) {
        var cell = curr.toPtr(runtime.Cons);
        var car = cell.car;
        var next = cell.cdr;
        roots[0] = curr;
        roots[1] = list2;
        roots[2] = head;
        roots[3] = tail;

        const new_cell = c.heap.allocCons(car, Value.nil) catch |err| switch (err) {
            error.OutOfMemory => blk: {
                try collectJitGarbage(c, &roots);
                curr = roots[0];
                list2 = roots[1];
                head = roots[2];
                tail = roots[3];
                cell = curr.toPtr(runtime.Cons);
                car = cell.car;
                next = cell.cdr;
                break :blk try c.heap.allocCons(car, Value.nil);
            },
            else => return err,
        };

        if (tail.isCons()) {
            tail.toPtr(runtime.Cons).cdr = new_cell;
        } else {
            head = new_cell;
        }
        tail = new_cell;
        curr = next;
    }
    if (!curr.isNil()) return error.TypeMismatch;

    if (tail.isCons()) {
        tail.toPtr(runtime.Cons).cdr = list2;
    }
    return if (head.isNil()) list2 else head;
}

pub fn listNth(c: *ctx.JitContext, n_val: Value, list: Value) vm_mod.Error!Value {
    _ = c;
    if (!n_val.isFixnum()) return error.TypeMismatch;
    const n = n_val.toFixnum();
    if (n < 0) return error.TypeMismatch;
    var idx: i64 = 0;
    var curr = list;
    while (curr.isCons()) {
        if (idx == n) return curr.toPtr(runtime.Cons).car;
        idx += 1;
        curr = curr.toPtr(runtime.Cons).cdr;
    }
    return Value.nil;
}

pub fn listNthcdr(c: *ctx.JitContext, n_val: Value, list: Value) vm_mod.Error!Value {
    _ = c;
    if (!n_val.isFixnum()) return error.TypeMismatch;
    const n = n_val.toFixnum();
    if (n < 0) return error.TypeMismatch;
    var idx: i64 = 0;
    var curr = list;
    while (idx < n and curr.isCons()) {
        idx += 1;
        curr = curr.toPtr(runtime.Cons).cdr;
    }
    return curr;
}

pub fn rplaca(c: *ctx.JitContext, cons_val: Value, new_car: Value) vm_mod.Error!Value {
    _ = c;
    if (!cons_val.isCons()) return error.TypeMismatch;
    const cell = cons_val.toPtr(runtime.Cons);
    cell.car = new_car;
    return new_car;
}

pub fn rplacd(c: *ctx.JitContext, cons_val: Value, new_cdr: Value) vm_mod.Error!Value {
    _ = c;
    if (!cons_val.isCons()) return error.TypeMismatch;
    const cell = cons_val.toPtr(runtime.Cons);
    cell.cdr = new_cdr;
    return new_cdr;
}

pub fn eql(c: *ctx.JitContext, a: Value, b: Value) vm_mod.Error!Value {
    _ = c;
    return if (vm_mod.hashKeyEqualWithTest(a, b, .eql)) Value.t else Value.nil;
}

pub fn equal(c: *ctx.JitContext, a: Value, b: Value) vm_mod.Error!Value {
    _ = c;
    return if (vm_mod.hashKeyEqualWithTest(a, b, .equal)) Value.t else Value.nil;
}

pub fn equalp(c: *ctx.JitContext, a: Value, b: Value) vm_mod.Error!Value {
    _ = c;
    return if (vm_mod.hashKeyEqualWithTest(a, b, .equalp)) Value.t else Value.nil;
}

pub fn lt(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    _ = c;
    return if (try arith.lt(a, b)) Value.t else Value.nil;
}

pub fn gt(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    _ = c;
    return if (try arith.gt(a, b)) Value.t else Value.nil;
}

pub fn le(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    _ = c;
    return if (try arith.le(a, b)) Value.t else Value.nil;
}

pub fn ge(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    _ = c;
    return if (try arith.ge(a, b)) Value.t else Value.nil;
}

pub fn numEq(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    _ = c;
    return if (arith.numEq(a, b)) Value.t else Value.nil;
}

pub fn loadGlobal(c: *ctx.JitContext, idx: u16) vm_mod.Error!Value {
    return try c.vm.loadGlobal(idx);
}

pub fn storeGlobal(c: *ctx.JitContext, val: Value, idx: u16) vm_mod.Error!Value {
    try c.vm.storeGlobal(idx, val);
    return val;
}

pub fn makeList(c: *ctx.JitContext, count: u8) vm_mod.Error!Value {
    const sp = stackLen(c);
    const count_usize: usize = count;
    if (count_usize > sp) return error.StackUnderflow;
    if (count_usize == 0 and sp >= stackCap(c)) return error.StackOverflow;

    const start = sp - count_usize;
    const items = c.frame_base[start..sp];
    var list = Value.nil;
    var i: usize = count_usize;
    while (i > 0) {
        i -= 1;
        list = try allocConsWithGc(c, items[i], list);
    }

    c.frame_base[start] = list;
    c.sp = c.frame_base + start + 1;
    return list;
}

pub fn makeVecN(c: *ctx.JitContext, count: u8) vm_mod.Error!Value {
    const sp = stackLen(c);
    const count_usize: usize = count;
    if (count_usize > sp) return error.StackUnderflow;
    if (count_usize == 0 and sp >= stackCap(c)) return error.StackOverflow;

    const start = sp - count_usize;
    const items = c.frame_base[start..sp];
    const vec = try allocVectorWithGc(c, count_usize);
    const vec_obj = vec.toPtr(runtime.Vector);
    for (items, 0..) |item, i| {
        vec_obj.data[i] = item;
    }

    c.frame_base[start] = vec;
    c.sp = c.frame_base + start + 1;
    return vec;
}

pub fn makeVec(c: *ctx.JitContext, _: u8) vm_mod.Error!Value {
    const sp = stackLen(c);
    if (sp < 2) return error.StackUnderflow;

    const size_val = c.frame_base[sp - 2];
    if (!size_val.isFixnum()) return error.TypeMismatch;
    const size_signed = size_val.toFixnum();
    if (size_signed < 0) return error.TypeMismatch;
    const size: usize = @intCast(size_signed);

    const vec = try allocVectorWithGc(c, size);
    const init_val_post = c.frame_base[sp - 1];
    const vec_obj = vec.toPtr(runtime.Vector);
    for (0..size) |i| {
        vec_obj.data[i] = init_val_post;
    }

    c.frame_base[sp - 2] = vec;
    c.sp = c.frame_base + sp - 1;
    return vec;
}

pub fn vecRef(c: *ctx.JitContext, vec_val: Value, idx_val: Value) vm_mod.Error!Value {
    _ = c;
    if (!vec_val.isVector() or !idx_val.isFixnum()) return error.TypeMismatch;
    const vec = vec_val.toPtr(runtime.Vector);
    const idx_signed = idx_val.toFixnum();
    if (idx_signed < 0) return error.TypeMismatch;
    const idx: usize = @intCast(idx_signed);
    if (idx >= vec.length) return error.TypeMismatch;
    return vec.get(idx);
}

pub fn vecSet(c: *ctx.JitContext, _: u8) vm_mod.Error!Value {
    const sp = stackLen(c);
    if (sp < 3) return error.StackUnderflow;

    const val = c.frame_base[sp - 1];
    const idx_val = c.frame_base[sp - 2];
    const vec_val = c.frame_base[sp - 3];
    if (!vec_val.isVector() or !idx_val.isFixnum()) return error.TypeMismatch;
    const vec = vec_val.toPtr(runtime.Vector);
    const idx_signed = idx_val.toFixnum();
    if (idx_signed < 0) return error.TypeMismatch;
    const idx: usize = @intCast(idx_signed);
    if (idx >= vec.length) return error.TypeMismatch;
    vec.set(idx, val);

    c.frame_base[sp - 3] = val;
    c.sp = c.frame_base + sp - 2;
    return val;
}

pub fn vecLen(c: *ctx.JitContext, vec_val: Value) vm_mod.Error!Value {
    _ = c;
    if (!vec_val.isVector()) return error.TypeMismatch;
    const vec = vec_val.toPtr(runtime.Vector);
    return Value.makeFixnum(@intCast(vec.length));
}

pub fn copyStructure(c: *ctx.JitContext, obj: Value) vm_mod.Error!Value {
    var args = [_]Value{obj};
    return vec_prims.copyStructure(c.heap, args[0]) catch |err| switch (err) {
        error.OutOfMemory => blk: {
            try collectJitGarbage(c, &args);
            break :blk try vec_prims.copyStructure(c.heap, args[0]);
        },
        else => return err,
    };
}

pub fn functionLambdaExpression(c: *ctx.JitContext, fn_val: Value) vm_mod.Error!Value {
    switch (fn_val.typeKind()) {
        .closure => {
            const cls = fn_val.toPtr(runtime.Closure);
            if (cls.code.typeKind() != .chunk) return error.TypeMismatch;
            const chunk = cls.code.toPtr(runtime.Chunk);
            c.vm.secondary_values[0] = if (cls.num_captures != 0) Value.t else Value.nil;
            c.vm.secondary_values[1] = chunk.name;
            c.vm.secondary_values_count = 2;
            return chunk.lambda_expr;
        },
        .generic_function => {
            const gf = fn_val.toPtr(runtime.objects.GenericFunction);
            if (!gf.dispatcher.isClosure()) return error.TypeMismatch;
            const cls = gf.dispatcher.toPtr(runtime.Closure);
            if (cls.code.typeKind() != .chunk) return error.TypeMismatch;
            const chunk = cls.code.toPtr(runtime.Chunk);
            c.vm.secondary_values[0] = if (cls.num_captures != 0) Value.t else Value.nil;
            c.vm.secondary_values[1] = gf.name;
            c.vm.secondary_values_count = 2;
            return chunk.lambda_expr;
        },
        else => return error.TypeMismatch,
    }
}

pub fn vecFillPtr(c: *ctx.JitContext, vec_val: Value) vm_mod.Error!Value {
    _ = c;
    if (!vec_val.isVector()) return error.TypeMismatch;
    if (vec_prims.fillPointer(vec_val)) |fp| {
        return Value.makeFixnum(fp);
    }
    return Value.nil;
}

pub fn vecPush(c: *ctx.JitContext, vec_val: Value, elem: Value) vm_mod.Error!Value {
    _ = c;
    const result = vec_prims.vectorPush(vec_val, elem);
    return Value.makeFixnum(result);
}

pub fn vecPushExt(c: *ctx.JitContext, _: u8) vm_mod.Error!Value {
    const sp = stackLen(c);
    if (sp < 3) return error.StackUnderflow;

    const ext_val = c.frame_base[sp - 1];
    const elem = c.frame_base[sp - 2];
    const vec_val = c.frame_base[sp - 3];
    if (!ext_val.isFixnum()) return error.TypeMismatch;

    var args = [_]Value{ vec_val, elem, ext_val };
    const ext: u64 = @intCast(ext_val.toFixnum());
    const result = vec_prims.vectorPushExtend(c.heap, args[0], args[1], ext) catch |err| switch (err) {
        error.OutOfMemory => blk: {
            try collectJitGarbage(c, &args);
            const ext_retry: u64 = @intCast(args[2].toFixnum());
            break :blk try vec_prims.vectorPushExtend(c.heap, args[0], args[1], ext_retry);
        },
        else => return err,
    };

    const res_val = Value.makeFixnum(result);
    c.frame_base[sp - 3] = res_val;
    c.sp = c.frame_base + sp - 2;
    return res_val;
}

pub fn vecPop(c: *ctx.JitContext, vec_val: Value) vm_mod.Error!Value {
    _ = c;
    return vec_prims.vectorPop(vec_val);
}

pub fn vecSetFillPtr(c: *ctx.JitContext, vec_val: Value, fp_val: Value) vm_mod.Error!Value {
    _ = c;
    if (!fp_val.isFixnum()) return error.TypeMismatch;
    const ok = vec_prims.setFillPointer(vec_val, fp_val.toFixnum());
    return if (ok) Value.t else Value.nil;
}

pub fn vecSetAdjustable(c: *ctx.JitContext, vec_val: Value, bool_val: Value) vm_mod.Error!Value {
    _ = c;
    const ok = vec_prims.setAdjustable(vec_val, !bool_val.isNil());
    return if (ok) Value.t else Value.nil;
}

pub fn vecAdjust(c: *ctx.JitContext, _: u8) vm_mod.Error!Value {
    const sp = stackLen(c);
    if (sp < 3) return error.StackUnderflow;

    const fill_val = c.frame_base[sp - 1];
    const size_val = c.frame_base[sp - 2];
    const vec_val = c.frame_base[sp - 3];
    if (!size_val.isFixnum()) return error.TypeMismatch;

    var args = [_]Value{ vec_val, size_val, fill_val };
    const new_size: u64 = @intCast(size_val.toFixnum());
    const result = vec_prims.adjustArray(c.heap, args[0], new_size, args[2]) catch |err| switch (err) {
        error.OutOfMemory => blk: {
            try collectJitGarbage(c, &args);
            const new_size_retry: u64 = @intCast(args[1].toFixnum());
            break :blk try vec_prims.adjustArray(c.heap, args[0], new_size_retry, args[2]);
        },
        else => return err,
    };

    c.frame_base[sp - 3] = result;
    c.sp = c.frame_base + sp - 2;
    return result;
}

pub fn makeClosure(c: *ctx.JitContext, chunk_idx: u16, num_captures: u8) vm_mod.Error!Value {
    if (num_captures > 64) return error.StackOverflow;

    const sp = stackLen(c);
    const cap_count: usize = num_captures;
    if (cap_count > sp) return error.StackUnderflow;

    const abs_idx = c.vm.chunk_base + @as(usize, chunk_idx);
    if (abs_idx >= c.vm.chunk_pool.len) return error.InvalidConstant;
    const closure_chunk = c.vm.chunk_pool[abs_idx];

    const start = sp - cap_count;
    const captures = c.frame_base[start..sp];
    const chunk_val = Value.makeChunk(closure_chunk);
    const closure = try allocClosureWithGc(c, chunk_val, closure_chunk.arity, captures);

    c.frame_base[start] = closure;
    c.sp = c.frame_base + start + 1;
    return closure;
}

pub fn loadCapture(c: *ctx.JitContext, idx: u8) vm_mod.Error!Value {
    const closure = if (c.vm.current_closure) |cl| cl else return error.TypeMismatch;
    if (idx >= closure.num_captures) return error.InvalidConstant;
    return closure.getCapture(idx);
}

pub fn loadUpvalue(c: *ctx.JitContext, idx: u8) vm_mod.Error!Value {
    const closure = if (c.vm.current_closure) |cl| cl else return error.TypeMismatch;
    if (idx >= closure.num_captures) return error.InvalidConstant;
    return closure.getCapture(idx);
}

pub fn storeUpvalue(c: *ctx.JitContext, val: Value, idx: u8) vm_mod.Error!Value {
    const closure = if (c.vm.current_closure) |cl| cl else return error.TypeMismatch;
    if (idx >= closure.num_captures) return error.InvalidConstant;
    const captures: [*]Value = @constCast(closure.captures);
    captures[idx] = val;
    return val;
}

pub fn loadArgc(c: *ctx.JitContext, _: u8) vm_mod.Error!Value {
    return Value.makeFixnum(@as(i64, c.vm.current_argc));
}

pub fn call(c: *ctx.JitContext, argc: u8) vm_mod.Error!Value {
    const sp = stackLen(c);
    const argc_usize: usize = argc;
    if (argc_usize + 1 > sp) return error.StackUnderflow;

    const fn_idx = sp - argc_usize - 1;
    const fn_val = c.frame_base[fn_idx];
    const args = c.frame_base[fn_idx + 1 .. fn_idx + 1 + argc_usize];

    syncVmSp(c);
    const res = try c.vm.callFromStackAt(fn_idx, fn_val, args);
    c.frame_base[fn_idx] = res;
    c.sp = c.frame_base + fn_idx + 1;
    syncVmSp(c);
    // const_pool points into a GC-managed Chunk; refresh after potential relocation.
    c.const_pool = c.vm.chunk.const_pool;
    c.const_count = @intCast(c.vm.chunk.const_count);
    return res;
}

/// Like call(), but checks if the callee has JIT code and calls it
/// directly, bypassing the interpreter loop for JIT→JIT transitions.
pub fn callFast(c: *ctx.JitContext, argc: u8) vm_mod.Error!Value {
    const sp = stackLen(c);
    const argc_usize: usize = argc;
    if (argc_usize + 1 > sp) return error.StackUnderflow;

    const fn_idx = sp - argc_usize - 1;
    const fn_val = c.frame_base[fn_idx];
    const args = c.frame_base[fn_idx + 1 .. fn_idx + 1 + argc_usize];

    // callFromStackAtFast takes absolute VM stack indices.
    // Compute the absolute position of fn_idx in vm.stack.
    const abs_fn_idx = absStackIdx(c, fn_idx);

    syncVmSp(c);
    const res = try c.vm.callFromStackAtFast(abs_fn_idx, fn_val, args);
    c.frame_base[fn_idx] = res;
    c.sp = c.frame_base + fn_idx + 1;
    syncVmSp(c);
    // const_pool points into a GC-managed Chunk; refresh after potential relocation.
    c.const_pool = c.vm.chunk.const_pool;
    c.const_count = @intCast(c.vm.chunk.const_count);
    return res;
}

/// Convert a frame-relative index to an absolute VM stack index.
/// JitContext.frame_base always points into vm.stack, so this is
/// a simple pointer arithmetic conversion.
fn absStackIdx(c: *ctx.JitContext, frame_idx: usize) usize {
    const stack_base = @intFromPtr(c.vm.stack[0..].ptr);
    const frame_abs = @intFromPtr(c.frame_base);
    return (frame_abs - stack_base) / @sizeOf(Value) + frame_idx;
}

pub fn apply(c: *ctx.JitContext, _: u8) vm_mod.Error!Value {
    const sp = stackLen(c);
    if (sp < 2) return error.StackUnderflow;

    const fn_idx = sp - 2;
    const fn_val = c.frame_base[fn_idx];
    const args_list = c.frame_base[sp - 1];

    syncVmSp(c);
    const res = try c.vm.applyFromStackAt(fn_idx, fn_val, args_list);
    c.frame_base[fn_idx] = res;
    c.sp = c.frame_base + fn_idx + 1;
    syncVmSp(c);
    // const_pool points into a GC-managed Chunk; refresh after potential relocation.
    c.const_pool = c.vm.chunk.const_pool;
    c.const_count = @intCast(c.vm.chunk.const_count);
    return res;
}

// ── C-ABI entry points for JIT (stable across Debug/Release) ──
// Unary: fn(*JitContext, u64) callconv(.c) u64
pub const j_numberp = cWrap1(numberp);
pub const j_integerp = cWrap1(integerp);
pub const j_realp = cWrap1(realp);
pub const j_consp = cWrap1(consp);
pub const j_symbolp = cWrap1(symbolp);
pub const j_stringp = cWrap1(stringp);
pub const j_vectorp = cWrap1(vectorp);
pub const j_closurep = cWrap1(closurep);
pub const j_keywordp = cWrap1(keywordp);
pub const j_characterp = cWrap1(characterp);
pub const j_floatp = cWrap1(floatp);
pub const j_listp = cWrap1(listp);
pub const j_atom = cWrap1(atom);
pub const j_charCode = cWrap1(charCode);
pub const j_codeChar = cWrap1(codeChar);
pub const j_charUpcase = cWrap1(charUpcase);
pub const j_charDowncase = cWrap1(charDowncase);
pub const j_digitCharP = cWrap1(digitCharP);
pub const j_alphaCharP = cWrap1(alphaCharP);
pub const j_stringUpcase = cWrap1(stringUpcase);
pub const j_stringDowncase = cWrap1(stringDowncase);
pub const j_writeToString = cWrap1(writeToString);
pub const j_strLen = cWrap1(strLen);
pub const j_strSet = cWrap1(strSet);
pub const j_random = cWrap1(random);
pub const j_randomSeed = cWrap1(randomSeed);
pub const j_write = cWrap1(write);
pub const j_print = cWrap1(print);
pub const j_princ = cWrap1(princ);
pub const j_terpri = cWrap1(terpri);
pub const j_writeChar = cWrap1(writeChar);
pub const j_listLength = cWrap1(listLength);
pub const j_listLast = cWrap1(listLast);
pub const j_listReverse = cWrap1(listReverse);
pub const j_loadCapture = cWrap1(loadCapture);
pub const j_loadUpvalue = cWrap1(loadUpvalue);
pub const j_loadGlobal = cWrap1(loadGlobal);
pub const j_loadArgc = cWrap1(loadArgc);
pub const j_makeVec = cWrap1(makeVec);
pub const j_makeVecN = cWrap1(makeVecN);
pub const j_vecSet = cWrap1(vecSet);
pub const j_vecLen = cWrap1(vecLen);
pub const j_vecFillPtr = cWrap1(vecFillPtr);
pub const j_vecPushExt = cWrap1(vecPushExt);
pub const j_vecPop = cWrap1(vecPop);
pub const j_vecAdjust = cWrap1(vecAdjust);
pub const j_copyStructure = cWrap1(copyStructure);
pub const j_functionLambdaExpression = cWrap1(functionLambdaExpression);
pub const j_call = cWrap1(call);
pub const j_callFast = cWrap1(callFast);
pub const j_apply = cWrap1(apply);
pub const j_makeList = cWrap1(makeList);
pub const j_neg = cWrap1(neg);
// Binary: fn(*JitContext, u64, u64) callconv(.c) u64
pub const j_add = cWrap2(add);
pub const j_sub = cWrap2(sub);
pub const j_mul = cWrap2(mul);
pub const j_div = cWrap2(div);
pub const j_mod = cWrap2(mod);
pub const j_equal = cWrap2(equal);
pub const j_eql = cWrap2(eql);
pub const j_equalp = cWrap2(equalp);
pub const j_lt = cWrap2(lt);
pub const j_gt = cWrap2(gt);
pub const j_le = cWrap2(le);
pub const j_ge = cWrap2(ge);
pub const j_numEq = cWrap2(numEq);
pub const j_charEq = cWrap2(charEq);
pub const j_charLt = cWrap2(charLt);
pub const j_charGt = cWrap2(charGt);
pub const j_strRef = cWrap2(strRef);
pub const j_strConcat = cWrap2(strConcat);
pub const j_strEq = cWrap2(strEq);
pub const j_strLt = cWrap2(strLt);
pub const j_strGt = cWrap2(strGt);
pub const j_strLe = cWrap2(strLe);
pub const j_strGe = cWrap2(strGe);
pub const j_listMember = cWrap2(listMember);
pub const j_listMemberEql = cWrap2(listMemberEql);
pub const j_listMemberEqual = cWrap2(listMemberEqual);
pub const j_assoc = cWrap2(assoc);
pub const j_assocEql = cWrap2(assocEql);
pub const j_assocEqual = cWrap2(assocEqual);
pub const j_listFind = cWrap2(listFind);
pub const j_listFindEq = cWrap2(listFindEq);
pub const j_listFindEqual = cWrap2(listFindEqual);
pub const j_listPosition = cWrap2(listPosition);
pub const j_listCount = cWrap2(listCount);
pub const j_listCountEq = cWrap2(listCountEq);
pub const j_listCountEqual = cWrap2(listCountEqual);
pub const j_listRemove = cWrap2(listRemove);
pub const j_listRemoveEq = cWrap2(listRemoveEq);
pub const j_listRemoveEqual = cWrap2(listRemoveEqual);
pub const j_appendLists = cWrap2(appendLists);
pub const j_listNth = cWrap2(listNth);
pub const j_listNthcdr = cWrap2(listNthcdr);
pub const j_rplaca = cWrap2(rplaca);
pub const j_rplacd = cWrap2(rplacd);
pub const j_cons = cWrap2(cons);
pub const j_storeUpvalue = cWrap2(storeUpvalue);
pub const j_storeGlobal = cWrap2(storeGlobal);
pub const j_vecRef = cWrap2(vecRef);
pub const j_vecPush = cWrap2(vecPush);
pub const j_vecSetFillPtr = cWrap2(vecSetFillPtr);
pub const j_vecSetAdjustable = cWrap2(vecSetAdjustable);
pub const j_makeClosure = cWrap2(makeClosure);

test "rt add returns error union" {
    const testing = std.testing;

    var heap = try runtime.Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    var dummy = [_]Value{Value.nil};
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var c = ctx.JitContext{
        .sp = &dummy,
        .const_pool = &dummy,
        .frame_base = &dummy,
        .stack_end = &dummy,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = 0,
        .err_trace = &trace,
        .vm = &vm,
    };

    try testing.expectError(error.TypeMismatch, add(&c, Value.nil, Value.nil));

    const res = try add(&c, Value.makeFixnum(1), Value.makeFixnum(2));
    try testing.expect(res.isFixnum());
    try testing.expectEqual(@as(i64, 3), res.toFixnum());
}

test "rt neg returns error union" {
    const testing = std.testing;

    var heap = try runtime.Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    var dummy = [_]Value{Value.nil};
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var c = ctx.JitContext{
        .sp = &dummy,
        .const_pool = &dummy,
        .frame_base = &dummy,
        .stack_end = &dummy,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = 0,
        .err_trace = &trace,
        .vm = &vm,
    };

    try testing.expectError(error.TypeMismatch, neg(&c, Value.nil));
    const res = try neg(&c, Value.makeFixnum(5));
    try testing.expect(res.isFixnum());
    try testing.expectEqual(@as(i64, -5), res.toFixnum());
}

test "rt gc keeps vm globals" {
    const testing = std.testing;

    var heap = try runtime.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    const cell = try heap.allocCons(Value.makeFixnum(1), Value.nil);
    try vm.storeGlobal(0, cell);

    const base = vm.stack[0..].ptr;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var c = ctx.JitContext{
        .sp = base,
        .const_pool = base,
        .frame_base = base,
        .stack_end = base + vm.stack.len,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = 0,
        .err_trace = &trace,
        .vm = &vm,
    };

    var none: [0]Value = .{};
    try collectJitGarbage(&c, none[0..]);

    const global = vm.globals[0];
    try testing.expect(global.isCons());
    const ptr = @intFromPtr(global.toPtr(runtime.Cons));
    const start = @intFromPtr(heap.from_start);
    const end = start + heap.space_size;
    try testing.expect(ptr >= start and ptr < end);
}

test "rt makeVec reloads init after gc" {
    const testing = std.testing;

    var heap = try runtime.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    const init_cons = try heap.allocCons(Value.makeFixnum(7), Value.nil);
    while (true) {
        _ = heap.allocCons(Value.nil, Value.nil) catch |err| switch (err) {
            error.OutOfMemory => break,
        };
    }

    const base = vm.stack[0..].ptr;
    vm.stack[0] = Value.makeFixnum(1);
    vm.stack[1] = init_cons;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var c = ctx.JitContext{
        .sp = base + 2,
        .const_pool = base,
        .frame_base = base,
        .stack_end = base + vm.stack.len,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = 0,
        .err_trace = &trace,
        .vm = &vm,
    };

    const from_before = heap.from_start;
    const vec = try makeVec(&c, 0);
    try testing.expect(@intFromPtr(heap.from_start) != @intFromPtr(from_before));

    const vec_obj = vec.toPtr(runtime.Vector);
    const elem = vec_obj.data[0];
    try testing.expect(elem.isCons());
    const ptr = @intFromPtr(elem.toPtr(runtime.Cons));
    const start = @intFromPtr(heap.from_start);
    const end = start + heap.space_size;
    try testing.expect(ptr >= start and ptr < end);
}

test "rt cons reloads args after gc" {
    const testing = std.testing;

    var heap = try runtime.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    const base = vm.stack[0..].ptr;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var c = ctx.JitContext{
        .sp = base,
        .const_pool = base,
        .frame_base = base,
        .stack_end = base + vm.stack.len,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = 0,
        .err_trace = &trace,
        .vm = &vm,
    };

    const car = try heap.allocCons(Value.makeFixnum(1), Value.nil);
    while (true) {
        _ = heap.allocCons(Value.nil, Value.nil) catch |err| switch (err) {
            error.OutOfMemory => break,
        };
    }

    const gc_before = heap.stats.gc_count;
    const cell = try cons(&c, car, Value.nil);
    try testing.expect(heap.stats.gc_count > gc_before);

    try testing.expect(cell.isCons());
    const cell_obj = cell.toPtr(runtime.Cons);
    const car_post = cell_obj.car;
    try testing.expect(car_post.isCons());
    const ptr = @intFromPtr(car_post.toPtr(runtime.Cons));
    const start = @intFromPtr(heap.from_start);
    const end = start + heap.space_size;
    try testing.expect(ptr >= start and ptr < end);
}

test "rt call preserves stack below" {
    const testing = std.testing;
    const bytecode = @import("../bytecode/bytecode.zig");
    const Op = bytecode.Op;

    var heap = try runtime.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        7, 0, 0, 0,
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk_val = try heap.allocChunk(&code, &.{}, 2, 0, 0, false, 2);
    const closure = try heap.allocClosure(chunk_val, 2, &.{});
    vm.chunk = chunk_val.toPtr(runtime.Chunk);

    const base = vm.stack[0..].ptr;
    vm.stack[0] = Value.makeFixnum(111);
    vm.stack[1] = closure;
    vm.stack[2] = Value.makeFixnum(1);
    vm.stack[3] = Value.makeFixnum(2);

    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var c = ctx.JitContext{
        .sp = base + 4,
        .const_pool = vm.chunk.const_pool,
        .frame_base = base,
        .stack_end = base + vm.stack.len,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = @intCast(vm.chunk.const_count),
        .err_trace = &trace,
        .vm = &vm,
    };

    const res = try call(&c, 2);
    try testing.expect(res.isFixnum());
    try testing.expectEqual(@as(i64, 7), res.toFixnum());
    try testing.expectEqual(@as(i64, 111), vm.stack[0].toFixnum());
    try testing.expectEqual(@as(i64, 7), vm.stack[1].toFixnum());
    try testing.expectEqual(@as(usize, 2), stackLen(&c));
}

test "rt apply preserves stack below" {
    const testing = std.testing;
    const bytecode = @import("../bytecode/bytecode.zig");
    const Op = bytecode.Op;

    var heap = try runtime.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    const load_local_op: u16 = @intFromEnum(Op.load_local);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(load_local_op & 0xFF), @truncate(load_local_op >> 8),
        0,
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk_val = try heap.allocChunk(&code, &.{}, 2, 0, 0, false, 2);
    const closure = try heap.allocClosure(chunk_val, 2, &.{});
    vm.chunk = chunk_val.toPtr(runtime.Chunk);

    const args = try heap.allocCons(Value.makeFixnum(1), try heap.allocCons(Value.makeFixnum(2), Value.nil));

    const base = vm.stack[0..].ptr;
    vm.stack[0] = Value.makeFixnum(111);
    vm.stack[1] = closure;
    vm.stack[2] = args;

    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var c = ctx.JitContext{
        .sp = base + 3,
        .const_pool = vm.chunk.const_pool,
        .frame_base = base,
        .stack_end = base + vm.stack.len,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = @intCast(vm.chunk.const_count),
        .err_trace = &trace,
        .vm = &vm,
    };

    const res = try apply(&c, 0);
    try testing.expect(res.isFixnum());
    try testing.expectEqual(@as(i64, 1), res.toFixnum());
    try testing.expectEqual(@as(i64, 111), vm.stack[0].toFixnum());
    try testing.expectEqual(@as(i64, 1), vm.stack[1].toFixnum());
    try testing.expectEqual(@as(usize, 2), stackLen(&c));
}

test "rt callFast falls back to interpreter when no JIT code" {
    // callFast should behave identically to call when callee has no JIT code
    const testing = std.testing;
    const bytecode = @import("../bytecode/bytecode.zig");
    const Op = bytecode.Op;

    var heap = try runtime.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        42, 0, 0, 0,
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk_val = try heap.allocChunk(&code, &.{}, 2, 0, 0, false, 2);
    const closure = try heap.allocClosure(chunk_val, 2, &.{});
    vm.chunk = chunk_val.toPtr(runtime.Chunk);

    const base = vm.stack[0..].ptr;
    vm.stack[0] = Value.makeFixnum(111);
    vm.stack[1] = closure;
    vm.stack[2] = Value.makeFixnum(1);
    vm.stack[3] = Value.makeFixnum(2);

    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var c = ctx.JitContext{
        .sp = base + 4,
        .const_pool = vm.chunk.const_pool,
        .frame_base = base,
        .stack_end = base + vm.stack.len,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = @intCast(vm.chunk.const_count),
        .err_trace = &trace,
        .vm = &vm,
    };

    // No JIT enabled, so callFast should fall back to interpreter
    const res = try callFast(&c, 2);
    try testing.expect(res.isFixnum());
    try testing.expectEqual(@as(i64, 42), res.toFixnum());
    try testing.expectEqual(@as(i64, 111), vm.stack[0].toFixnum());
    try testing.expectEqual(@as(i64, 42), vm.stack[1].toFixnum());
    try testing.expectEqual(@as(usize, 2), stackLen(&c));
}
