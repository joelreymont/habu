    // Parse command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var eval_mode = false;
    var eval_expr: ?[]const u8 = null;
    var file_args = std.ArrayList([]const u8).init(allocator);
    defer file_args.deinit();

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "-c") or std.mem.eql(u8, arg, "--eval")) {
            if (i + 1 >= args.len) {
                try writer.print("Error: {s} requires an argument\n", .{arg});
                return error.InvalidArgs;
            }
            eval_mode = true;
            eval_expr = args[i + 1];
            i += 1; // Skip next arg
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            try writer.print("Usage: habu [options] [files...]\n", .{});
            try writer.print("Options:\n", .{});
            try writer.print("  -c, --eval EXPR    Evaluate expression and exit\n", .{});
            try writer.print("  -h, --help         Show this help message\n", .{});
            try writer.flush();
            return;
        } else {
            try file_args.append(arg);
        }
    }

    // Load files
    for (file_args.items) |arg| {
        repl.loadFilePublic(arg, writer) catch |err| {
            try writer.print("Error loading {s}: {s}\n", .{ arg, @errorName(err) });
        };
        try writer.flush();
    }

    // If eval mode, evaluate and exit
    if (eval_mode) {
        if (eval_expr) |expr| {
            const result = repl.eval(expr) catch |err| {
                try writer.print("Error: {s}\n", .{@errorName(err)});
                try writer.flush();
                return err;
            };
            try repl.printValue(result, writer);
            try writer.print("\n", .{});
            try writer.flush();
        }
        return;
    }

    try repl.runWithFiles(fs.File.stdin(), stdout);
