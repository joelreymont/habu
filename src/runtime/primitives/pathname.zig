const std = @import("std");
const runtime = @import("../runtime.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const objects = @import("../objects.zig");
const Pathname = objects.Pathname;

/// Create a pathname from components
pub fn makePathname(
    allocator: std.mem.Allocator,
    heap: *Heap,
    host: Value,
    device: Value,
    directory: Value,
    name: Value,
    ty: Value,
    version: Value,
) !Value {
    _ = allocator;
    return try heap.allocPathname(host, device, directory, name, ty, version);
}

/// Get pathname host component
pub fn pathnameHost(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.host;
}

/// Get pathname device component
pub fn pathnameDevice(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.device;
}

/// Get pathname directory component
pub fn pathnameDirectory(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.directory;
}

/// Get pathname name component
pub fn pathnameName(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.name;
}

/// Get pathname type component
pub fn pathnameType(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.type;
}

/// Get pathname version component
pub fn pathnameVersion(val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);
    return p.version;
}

/// Check if value is a pathname
pub fn pathnamep(val: Value) Value {
    return Value.fromBool(val.isPathname());
}

/// Convert string or pathname to pathname
pub fn pathname(allocator: std.mem.Allocator, heap: *Heap, val: Value) !Value {
    if (val.isPathname()) return val;
    if (!val.isString()) return error.TypeMismatch;
    return try parseNamestring(allocator, heap, val);
}

/// Convert pathname to namestring
pub fn namestring(allocator: std.mem.Allocator, val: Value) !Value {
    if (!val.isPathname()) return error.TypeMismatch;
    const p = val.toPtr(Pathname);

    var buf = std.ArrayList(u8){};
    defer buf.deinit(allocator);
    const writer = buf.writer(allocator);

    // host://device/directory/name.type;version format
    if (!p.host.isNil()) {
        const host_str = p.host.toString();
        try writer.writeAll(host_str.data);
        try writer.writeAll("://");
    }

    if (!p.device.isNil()) {
        const dev_str = p.device.toString();
        try writer.writeAll(dev_str.data);
        try writer.writeAll("/");
    }

    // directory - list like (:absolute "foo" "bar") or (:relative "baz")
    if (!p.directory.isNil() and p.directory.isCons()) {
        var dir = p.directory;
        const first = dir.car();
        dir = dir.cdr();

        // :absolute starts with /, :relative has no prefix
        if (first.isKeyword()) {
            const kw = first.toKeyword();
            if (std.mem.eql(u8, kw.name.data, "absolute")) {
                try writer.writeByte('/');
            }
        }

        while (!dir.isNil()) {
            const component = dir.car();
            if (component.isString()) {
                const comp_str = component.toString();
                try writer.writeAll(comp_str.data);
                dir = dir.cdr();
                if (!dir.isNil()) try writer.writeByte('/');
            } else break;
        }
    }

    if (!p.name.isNil()) {
        if (buf.items.len > 0 and buf.items[buf.items.len - 1] != '/') {
            try writer.writeByte('/');
        }
        const name_str = p.name.toString();
        try writer.writeAll(name_str.data);
    }

    if (!p.type.isNil()) {
        try writer.writeByte('.');
        const type_str = p.type.toString();
        try writer.writeAll(type_str.data);
    }

    if (!p.version.isNil()) {
        try writer.writeByte(';');
        if (p.version.isKeyword()) {
            const kw = p.version.toKeyword();
            try writer.writeAll(kw.name.data);
        } else if (p.version.isFixnum()) {
            try writer.print("{d}", .{p.version.toFixnum()});
        }
    }

    const str = try objects.String.create(allocator, buf.items);
    return Value.fromString(str);
}

/// Parse namestring into pathname
pub fn parseNamestring(allocator: std.mem.Allocator, heap: *Heap, str: Value) !Value {
    if (!str.isString()) return error.TypeMismatch;
    const s = str.toString();
    const path = s.data;

    var host = Value.nil();
    var device = Value.nil();
    var directory = Value.nil();
    var name = Value.nil();
    var ty = Value.nil();
    var version = Value.nil();

    var i: usize = 0;

    // Parse host://
    if (std.mem.indexOf(u8, path, "://")) |host_end| {
        const host_str = try objects.String.create(allocator, path[0..host_end]);
        host = Value.fromString(host_str);
        i = host_end + 3;
    }

    // Parse device/ (single letter followed by /)
    if (i + 2 < path.len and path[i + 1] == '/') {
        const dev_str = try objects.String.create(allocator, path[i .. i + 1]);
        device = Value.fromString(dev_str);
        i += 2;
    }

    // Find last slash to separate directory from name
    const last_slash = std.mem.lastIndexOf(u8, path[i..], "/");

    // Parse directory
    if (last_slash) |ls_idx| {
        const dir_part = path[i .. i + ls_idx];
        var dir_list = Value.nil();

        // Determine if absolute or relative
        const dir_type = if (dir_part.len > 0 and dir_part[0] == '/')
            heap.intern("absolute")
        else
            heap.intern("relative");

        dir_list = try heap.allocCons(dir_type, Value.nil());
        var tail = dir_list;

        // Split directory components
        var start: usize = if (dir_part.len > 0 and dir_part[0] == '/') @as(usize, 1) else 0;
        var j = start;
        while (j < dir_part.len) : (j += 1) {
            if (dir_part[j] == '/') {
                if (j > start) {
                    const comp_str = try objects.String.create(allocator, dir_part[start..j]);
                    const new_cons = try heap.allocCons(Value.fromString(comp_str), Value.nil());
                    tail.toCons().cdr = new_cons;
                    tail = new_cons;
                }
                start = j + 1;
            }
        }
        if (j > start and start < dir_part.len) {
            const comp_str = try objects.String.create(allocator, dir_part[start..j]);
            const new_cons = try heap.allocCons(Value.fromString(comp_str), Value.nil());
            tail.toCons().cdr = new_cons;
        }

        directory = dir_list;
        i += ls_idx + 1;
    }

    // Parse name.type;version
    const remainder = path[i..];

    // Find version
    if (std.mem.lastIndexOf(u8, remainder, ";")) |ver_idx| {
        const ver_part = remainder[ver_idx + 1 ..];
        if (std.mem.eql(u8, ver_part, "newest")) {
            version = heap.internKeyword("newest");
        } else if (std.mem.eql(u8, ver_part, "unspecific")) {
            version = heap.internKeyword("unspecific");
        } else {
            const ver_num = std.fmt.parseInt(i64, ver_part, 10) catch 0;
            version = Value.fromFixnum(ver_num);
        }
        // Continue parsing name.type before version
        i = remainder.len - (remainder.len - ver_idx);
    } else {
        i = remainder.len;
    }

    const name_type_part = if (std.mem.lastIndexOf(u8, remainder, ";")) |v|
        remainder[0..v]
    else
        remainder;

    // Find type
    if (std.mem.lastIndexOf(u8, name_type_part, ".")) |dot_idx| {
        const type_part = name_type_part[dot_idx + 1 ..];
        if (type_part.len > 0) {
            const type_str = try objects.String.create(allocator, type_part);
            ty = Value.fromString(type_str);
        }

        const name_part = name_type_part[0..dot_idx];
        if (name_part.len > 0) {
            const name_str = try objects.String.create(allocator, name_part);
            name = Value.fromString(name_str);
        }
    } else if (name_type_part.len > 0) {
        const name_str = try objects.String.create(allocator, name_type_part);
        name = Value.fromString(name_str);
    }

    return try heap.allocPathname(host, device, directory, name, ty, version);
}

/// Merge pathnames with defaults
pub fn mergePathnames(
    heap: *Heap,
    pn: Value,
    defaults: Value,
) !Value {
    if (!pn.isPathname()) return error.TypeMismatch;
    if (!defaults.isPathname()) return error.TypeMismatch;

    const p = pn.toPtr(Pathname);
    const d = defaults.toPtr(Pathname);

    const host = if (!p.host.isNil()) p.host else d.host;
    const device = if (!p.device.isNil()) p.device else d.device;
    const directory = if (!p.directory.isNil()) p.directory else d.directory;
    const name = if (!p.name.isNil()) p.name else d.name;
    const ty = if (!p.type.isNil()) p.type else d.type;
    const version = if (!p.version.isNil()) p.version else d.version;

    return try heap.allocPathname(host, device, directory, name, ty, version);
}
