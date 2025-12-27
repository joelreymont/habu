//! Line editing with readline-style features
//!
//! - Arrow keys for cursor movement
//! - Ctrl-A: beginning of line
//! - Ctrl-E: end of line
//! - Ctrl-K: kill to end of line
//! - Ctrl-U: kill to beginning of line
//! - Ctrl-W: kill word backward
//! - Ctrl-L: clear screen
//! - Backspace/Delete
//! - Up/Down arrows for history

const std = @import("std");
const posix = std.posix;

const MAX_LINE = 4096;
const MAX_HISTORY = 100;

pub const LineEditor = struct {
    allocator: std.mem.Allocator,
    orig_termios: ?posix.termios = null,
    raw_mode_enabled: bool = false,
    history: std.ArrayList([]const u8),
    history_idx: usize = 0,

    // Line state
    buf: [MAX_LINE]u8 = undefined,
    len: usize = 0,
    pos: usize = 0,

    // For multi-line support
    prompt: []const u8 = "",

    pub fn init(allocator: std.mem.Allocator) LineEditor {
        return .{
            .allocator = allocator,
            .history = std.ArrayList([]const u8){},
        };
    }

    pub fn deinit(self: *LineEditor) void {
        self.disableRawMode();
        for (self.history.items) |entry| {
            self.allocator.free(entry);
        }
        self.history.deinit(self.allocator);
    }

    fn enableRawMode(self: *LineEditor) !void {
        if (self.raw_mode_enabled) return;

        const stdin_fd = std.posix.STDIN_FILENO;
        if (!std.posix.isatty(stdin_fd)) return error.NotATty;

        self.orig_termios = try posix.tcgetattr(stdin_fd);
        var raw = self.orig_termios.?;

        // Input flags: disable break, CR->NL, parity, strip, flow control
        raw.iflag.BRKINT = false;
        raw.iflag.ICRNL = false;
        raw.iflag.INPCK = false;
        raw.iflag.ISTRIP = false;
        raw.iflag.IXON = false;

        // Output flags: disable post-processing
        raw.oflag.OPOST = false;

        // Control flags: 8-bit chars
        raw.cflag.CSIZE = .CS8;

        // Local flags: disable echo, canonical, signals, extended
        raw.lflag.ECHO = false;
        raw.lflag.ICANON = false;
        raw.lflag.ISIG = false;
        raw.lflag.IEXTEN = false;

        // Control chars: read returns after 1 byte, no timeout
        raw.cc[@intFromEnum(posix.V.MIN)] = 1;
        raw.cc[@intFromEnum(posix.V.TIME)] = 0;

        try posix.tcsetattr(stdin_fd, .FLUSH, raw);
        self.raw_mode_enabled = true;
    }

    fn disableRawMode(self: *LineEditor) void {
        if (self.raw_mode_enabled) {
            if (self.orig_termios) |orig| {
                posix.tcsetattr(std.posix.STDIN_FILENO, .FLUSH, orig) catch {};
            }
            self.raw_mode_enabled = false;
        }
    }

    fn refreshLine(self: *LineEditor, writer: anytype) !void {
        // Move to beginning of line, clear, print prompt and buffer
        try writer.writeAll("\r\x1b[K");
        try writer.writeAll(self.prompt);
        try writer.writeAll(self.buf[0..self.len]);

        // Move cursor to position
        if (self.pos < self.len) {
            const cursor_pos = self.prompt.len + self.pos;
            try writer.print("\r\x1b[{d}C", .{cursor_pos});
        }
    }

    fn insertChar(self: *LineEditor, c: u8) void {
        if (self.len >= MAX_LINE - 1) return;

        if (self.pos < self.len) {
            // Shift right
            std.mem.copyBackwards(u8, self.buf[self.pos + 1 .. self.len + 1], self.buf[self.pos..self.len]);
        }
        self.buf[self.pos] = c;
        self.pos += 1;
        self.len += 1;
    }

    fn deleteCharBackward(self: *LineEditor) void {
        if (self.pos > 0 and self.len > 0) {
            std.mem.copyForwards(u8, self.buf[self.pos - 1 .. self.len - 1], self.buf[self.pos..self.len]);
            self.pos -= 1;
            self.len -= 1;
        }
    }

    fn deleteCharForward(self: *LineEditor) void {
        if (self.pos < self.len) {
            std.mem.copyForwards(u8, self.buf[self.pos .. self.len - 1], self.buf[self.pos + 1 .. self.len]);
            self.len -= 1;
        }
    }

    fn killToEnd(self: *LineEditor) void {
        self.len = self.pos;
    }

    fn killToBeginning(self: *LineEditor) void {
        std.mem.copyForwards(u8, self.buf[0 .. self.len - self.pos], self.buf[self.pos..self.len]);
        self.len -= self.pos;
        self.pos = 0;
    }

    fn killWordBackward(self: *LineEditor) void {
        if (self.pos == 0) return;

        // Skip trailing spaces
        var end = self.pos;
        while (end > 0 and self.buf[end - 1] == ' ') {
            end -= 1;
        }
        // Skip word
        while (end > 0 and self.buf[end - 1] != ' ') {
            end -= 1;
        }

        const deleted = self.pos - end;
        std.mem.copyForwards(u8, self.buf[end .. self.len - deleted], self.buf[self.pos..self.len]);
        self.len -= deleted;
        self.pos = end;
    }

    fn historyPrev(self: *LineEditor) void {
        if (self.history.items.len == 0) return;
        if (self.history_idx > 0) {
            self.history_idx -= 1;
            self.loadHistoryEntry();
        }
    }

    fn historyNext(self: *LineEditor) void {
        if (self.history.items.len == 0) return;
        if (self.history_idx < self.history.items.len) {
            self.history_idx += 1;
            if (self.history_idx == self.history.items.len) {
                // Past end - clear line
                self.len = 0;
                self.pos = 0;
            } else {
                self.loadHistoryEntry();
            }
        }
    }

    fn loadHistoryEntry(self: *LineEditor) void {
        if (self.history_idx < self.history.items.len) {
            const entry = self.history.items[self.history_idx];
            const copy_len = @min(entry.len, MAX_LINE - 1);
            @memcpy(self.buf[0..copy_len], entry[0..copy_len]);
            self.len = copy_len;
            self.pos = copy_len;
        }
    }

    fn addToHistory(self: *LineEditor, line: []const u8) !void {
        if (line.len == 0) return;

        // Don't add duplicates of the most recent entry
        if (self.history.items.len > 0) {
            const last = self.history.items[self.history.items.len - 1];
            if (std.mem.eql(u8, last, line)) return;
        }

        const copy = try self.allocator.dupe(u8, line);
        try self.history.append(self.allocator, copy);

        // Trim history if too long
        while (self.history.items.len > MAX_HISTORY) {
            const old = self.history.orderedRemove(0);
            self.allocator.free(old);
        }
    }

    /// Read a line with editing. Returns null on EOF (Ctrl-D on empty line).
    pub fn readline(self: *LineEditor, prompt: []const u8) !?[]const u8 {
        self.prompt = prompt;
        self.len = 0;
        self.pos = 0;
        self.history_idx = self.history.items.len;

        // Try to enable raw mode
        self.enableRawMode() catch {
            // Fallback to simple line reading
            return self.readlineSimple(prompt);
        };

        const stdin = std.fs.File{ .handle = std.posix.STDIN_FILENO };
        const stdout = std.fs.File{ .handle = std.posix.STDOUT_FILENO };
        var out_buf: [256]u8 = undefined;
        var writer = stdout.writer(&out_buf);
        const w = &writer.interface;

        // Print prompt
        try w.writeAll(prompt);
        try w.flush();

        while (true) {
            var c: [1]u8 = undefined;
            const n = stdin.read(&c) catch return null;
            if (n == 0) return null;

            switch (c[0]) {
                13 => { // Enter
                    try w.writeAll("\r\n");
                    try w.flush();
                    const result = self.buf[0..self.len];
                    try self.addToHistory(result);
                    return result;
                },
                127, 8 => { // Backspace
                    self.deleteCharBackward();
                    try self.refreshLine(w);
                    try w.flush();
                },
                4 => { // Ctrl-D
                    if (self.len == 0) {
                        try w.writeAll("\r\n");
                        try w.flush();
                        return null;
                    }
                },
                1 => { // Ctrl-A - beginning of line
                    self.pos = 0;
                    try self.refreshLine(w);
                    try w.flush();
                },
                5 => { // Ctrl-E - end of line
                    self.pos = self.len;
                    try self.refreshLine(w);
                    try w.flush();
                },
                11 => { // Ctrl-K - kill to end
                    self.killToEnd();
                    try self.refreshLine(w);
                    try w.flush();
                },
                21 => { // Ctrl-U - kill to beginning
                    self.killToBeginning();
                    try self.refreshLine(w);
                    try w.flush();
                },
                23 => { // Ctrl-W - kill word backward
                    self.killWordBackward();
                    try self.refreshLine(w);
                    try w.flush();
                },
                12 => { // Ctrl-L - clear screen
                    try w.writeAll("\x1b[2J\x1b[H");
                    try self.refreshLine(w);
                    try w.flush();
                },
                27 => { // Escape sequence
                    var seq: [3]u8 = undefined;
                    const n1 = stdin.read(seq[0..1]) catch return null;
                    if (n1 == 0) return null;
                    const n2 = stdin.read(seq[1..2]) catch return null;
                    if (n2 == 0) return null;

                    if (seq[0] == '[') {
                        if (seq[1] >= '0' and seq[1] <= '9') {
                            // Extended escape like Delete key
                            const n3 = stdin.read(seq[2..3]) catch return null;
                            if (n3 == 0) return null;
                            if (seq[1] == '3' and seq[2] == '~') {
                                self.deleteCharForward();
                            }
                        } else {
                            switch (seq[1]) {
                                'A' => self.historyPrev(), // Up
                                'B' => self.historyNext(), // Down
                                'C' => { // Right
                                    if (self.pos < self.len) self.pos += 1;
                                },
                                'D' => { // Left
                                    if (self.pos > 0) self.pos -= 1;
                                },
                                'H' => self.pos = 0, // Home
                                'F' => self.pos = self.len, // End
                                else => {},
                            }
                        }
                        try self.refreshLine(w);
                        try w.flush();
                    }
                },
                else => {
                    if (c[0] >= 32 and c[0] < 127) {
                        self.insertChar(c[0]);
                        try self.refreshLine(w);
                        try w.flush();
                    }
                },
            }
        }
    }

    /// Simple fallback readline without editing
    fn readlineSimple(self: *LineEditor, prompt: []const u8) !?[]const u8 {
        const stdout = std.fs.File{ .handle = std.posix.STDOUT_FILENO };
        const stdin = std.fs.File{ .handle = std.posix.STDIN_FILENO };

        var out_buf: [256]u8 = undefined;
        var writer = stdout.writer(&out_buf);
        try writer.interface.writeAll(prompt);
        try writer.interface.flush();

        // Use self.buf instead of a local array so the slice remains valid
        self.len = 0;
        while (self.len < self.buf.len) {
            var byte: [1]u8 = undefined;
            const n = stdin.read(&byte) catch return null;
            if (n == 0) return null;
            if (byte[0] == '\n') break;
            self.buf[self.len] = byte[0];
            self.len += 1;
        }
        if (self.len == 0) return "";
        return self.buf[0..self.len];
    }
};
