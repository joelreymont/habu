//! JIT runtime call helpers

const ctx = @import("ctx.zig");
const runtime = @import("../runtime/runtime.zig");
const arith = @import("../runtime/primitives/arith.zig");
const char_prims = @import("../runtime/primitives/char.zig");
const vec_prims = @import("../runtime/primitives/vector.zig");
const vm_mod = @import("../interp/vm.zig");

const Value = runtime.Value;
const std = @import("std");

const BinaryOp = *const fn (*runtime.Heap, Value, Value) arith.Error!Value;

const JitRoots = struct {
    roots: std.ArrayList(Value),
    stack_vals: []Value,
    extra: []Value,
    alloc: std.mem.Allocator,

    fn init(c: *ctx.JitContext, extra: []Value) !JitRoots {
        const alloc = c.heap.backing_allocator;
        var roots = std.ArrayList(Value){};

        const stack_len = stackLen(c);
        const stack_vals = c.frame_base[0..stack_len];
        try roots.appendSlice(alloc, stack_vals);

        try roots.appendSlice(alloc, extra);

        return .{
            .roots = roots,
            .stack_vals = stack_vals,
            .extra = extra,
            .alloc = alloc,
        };
    }

    fn deinit(self: *JitRoots) void {
        self.roots.deinit(self.alloc);
    }

    fn writeBack(self: *JitRoots) void {
        var idx: usize = 0;
        for (self.stack_vals) |*v| {
            v.* = self.roots.items[idx];
            idx += 1;
        }
        for (self.extra) |*v| {
            v.* = self.roots.items[idx];
            idx += 1;
        }
    }
};

fn stackLen(c: *ctx.JitContext) usize {
    const len_bytes = @intFromPtr(c.sp) - @intFromPtr(c.frame_base);
    return @intCast(@divExact(len_bytes, @sizeOf(Value)));
}

fn stackCap(c: *ctx.JitContext) usize {
    const cap_bytes = @intFromPtr(c.stack_end) - @intFromPtr(c.frame_base);
    return @intCast(@divExact(cap_bytes, @sizeOf(Value)));
}

fn collectJitGarbage(c: *ctx.JitContext, extra: []Value) !void {
    var jit_roots = try JitRoots.init(c, extra);
    defer jit_roots.deinit();

    c.vm.setExtRoots(jit_roots.roots.items);
    defer {
        c.vm.clearExtRoots();
        jit_roots.writeBack();
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
            try collectJitGarbage(c, &[_]Value{});
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
    return try c.vm.listRemoveWithTest(item, seq, .eql);
}

pub fn listRemoveEq(c: *ctx.JitContext, item: Value, seq: Value) vm_mod.Error!Value {
    return try c.vm.listRemoveWithTest(item, seq, .eq);
}

pub fn listRemoveEqual(c: *ctx.JitContext, item: Value, seq: Value) vm_mod.Error!Value {
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

    var jit_roots = try JitRoots.init(c, &[_]Value{});
    defer jit_roots.deinit();

    c.vm.setExtRoots(jit_roots.roots.items);
    const call_res = c.vm.callFromStack(fn_val, args);
    c.vm.clearExtRoots();
    jit_roots.writeBack();

    const res = call_res catch |err| return err;
    c.frame_base[fn_idx] = res;
    c.sp = c.frame_base + fn_idx + 1;
    return res;
}

pub fn apply(c: *ctx.JitContext, _: u8) vm_mod.Error!Value {
    const sp = stackLen(c);
    if (sp < 2) return error.StackUnderflow;

    const fn_idx = sp - 2;
    const fn_val = c.frame_base[fn_idx];
    const args_list = c.frame_base[sp - 1];

    var jit_roots = try JitRoots.init(c, &[_]Value{});
    defer jit_roots.deinit();

    c.vm.setExtRoots(jit_roots.roots.items);
    const apply_res = c.vm.applyFromStack(fn_val, args_list);
    c.vm.clearExtRoots();
    jit_roots.writeBack();

    const res = apply_res catch |err| return err;
    c.frame_base[fn_idx] = res;
    c.sp = c.frame_base + fn_idx + 1;
    return res;
}

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

    var stack = [_]Value{Value.nil, Value.nil};
    const base = stack[0..].ptr;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var c = ctx.JitContext{
        .sp = base + 1,
        .const_pool = base,
        .frame_base = base,
        .stack_end = base + stack.len,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = 0,
        .err_trace = &trace,
        .vm = &vm,
    };

    try collectJitGarbage(&c, &[_]Value{});

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

    var stack = [_]Value{
        Value.makeFixnum(1),
        init_cons,
        Value.nil,
    };
    const base = stack[0..].ptr;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var c = ctx.JitContext{
        .sp = base + 2,
        .const_pool = base,
        .frame_base = base,
        .stack_end = base + stack.len,
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
