//! Shared symbol cache for R symbols.
//! Uses an open-addressing Wyhash table with linear probing.
//! R symbols live forever so no deletion logic is needed.
//! Not threadlocal (R is single-threaded). If embedding zigr in
//! a multi-threaded context, wrap calls in a mutex.

const std = @import("std");
const R = @import("R");
const max_name_len = @import("sexp.zig").max_symbol_name;

const cap = 64;
const mask = cap - 1;
threadlocal var hashes: [cap]u64 = undefined;
threadlocal var names: [cap][max_name_len:0]u8 = undefined;
threadlocal var lengths: [cap]usize = undefined;
threadlocal var sexps: [cap]R.SEXP = undefined;
threadlocal var count: usize = 0;

/// Install a symbol. Result is cached in an open-addressing hash table
/// with linear probing. O(1) average lookup. R symbols live forever
/// so no deletion logic is needed.  Thread-local storage allows safe
/// concurrent use from R's parallel package (Windows workers) or
/// embedded multi-threaded contexts.
pub fn install(name: []const u8) R.SEXP {
    const hash = std.hash.Wyhash.hash(0, name);
    var idx = @as(usize, @truncate(hash)) & mask;

    while (hashes[idx] != 0) {
        if (hashes[idx] == hash and lengths[idx] == name.len and
            std.mem.eql(u8, names[idx][0..name.len], name))
        {
            return sexps[idx];
        }
        idx = (idx + 1) & mask;
    }

    var buf: [max_name_len:0]u8 = undefined;
    const n = @min(name.len, buf.len - 1);
    @memcpy(buf[0..n], name[0..n]);
    buf[n] = 0;
    const sxp = R.Rf_install(@ptrCast(&buf));

    if (count < cap) {
        hashes[idx] = hash;
        lengths[idx] = n;
        @memcpy(names[idx][0..n], name[0..n]);
        names[idx][n] = 0;
        sexps[idx] = sxp;
        count += 1;
    }

    return sxp;
}
