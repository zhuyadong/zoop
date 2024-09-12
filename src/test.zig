const std = @import("std");
const zoop = @import("zoop.zig");
pub fn main() !void {}

pub const IHuman = struct {
    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn getName(self: IHuman) []const u8 {
        return zoop.vptr(self).getName(self.ptr);
    }

    pub fn setName(self: IHuman, name: []const u8) void {
        zoop.vptr(self).setName(self.ptr, name);
    }
};

pub const Human = struct {
    name: []const u8 = "default",

    pub fn init(self: *Human, name: []const u8) void {
        self.name = name;
    }

    pub fn deinit(self: *Human) void {
        self.name = "";
    }

    pub fn getName(self: *const Human) []const u8 {
        return self.name;
    }

    pub fn setName(self: *Human, name: []const u8) void {
        self.name = name;
    }
};

pub const Sub = struct {
    pub const extends = .{IHuman};
    super: Human,

    pub fn init(self: *Sub, name: []const u8) void {
        self.super.init(name);
    }
};

pub const SubSub = struct {
    anyname: Sub,

    pub fn init(self: *SubSub, name: []const u8) void {
        self.anyname.init(name);
    }
};

pub const Custom = struct {
    super: Sub,
    age: u16 = 99,
    pub fn init(self: *Custom, name: []const u8) void {
        self.super.init(name);
    }

    // override
    pub fn getName(_: *Custom) []const u8 {
        return "custom";
    }
};
test "zoop" {
    const t = std.testing;

    var human = zoop.make(Human, null);
    try t.expect(zoop.as(&human, IHuman) == null);

    // test inherit
    var sub = zoop.make(Sub, .{ .super = .{ .name = "sub" } });
    var ihuman = zoop.cast(sub.ptr(), IHuman);
    try t.expectEqualStrings(ihuman.getName(), "sub");
    ihuman = zoop.cast(&sub, IHuman);
    try t.expectEqualStrings(ihuman.getName(), "sub");
    ihuman.setName("sub2");
    try t.expectEqualStrings(ihuman.getName(), "sub2");
    try t.expectEqualStrings(sub.ptr().super.getName(), "sub2");

    // test deep inherit
    var subsub = zoop.make(SubSub, null);
    subsub.ptr().init("subsub");
    ihuman = zoop.cast(subsub.ptr(), IHuman);
    try t.expectEqualStrings(ihuman.getName(), "subsub");
    ihuman = zoop.cast(&subsub, IHuman);
    try t.expectEqualStrings(ihuman.getName(), "subsub");

    // test override and as()
    var custom = zoop.make(Custom, null);
    custom.ptr().init("sub");
    ihuman = zoop.as(zoop.as(&custom, zoop.IObject).?, IHuman).?;
    try t.expect(zoop.isRootPtr(&custom));
    try t.expect(zoop.isRootPtr(custom.ptr()));
    try t.expect(!zoop.isRootPtr(&custom.ptr().super));
    try t.expect(!zoop.isRootPtr(zoop.Klass(Sub).from(&custom.ptr().super)));
    try t.expectEqualStrings(custom.ptr().super.super.getName(), "sub");
    try t.expectEqualStrings(zoop.cast(&custom, Human).name, "sub");
    try t.expectEqualStrings(zoop.cast(custom.ptr(), Human).name, "sub");
    try t.expectEqualStrings(zoop.as(&custom, Human).?.name, "sub");
    try t.expectEqualStrings(zoop.as(custom.ptr(), Human).?.name, "sub");
    try t.expectEqualStrings(zoop.getField(&custom, "name", []const u8).*, "sub");
    try t.expectEqualStrings(ihuman.getName(), "custom");

    // test deinit()
    zoop.destroy(&human);
    try t.expect(human.ptr().getName().len == 0);
    zoop.destroy(&sub);
    try t.expect(sub.ptr().super.getName().len == 0);
    zoop.destroy(custom.ptr());
    try t.expect(custom.ptr().super.super.name.len == 0);

    // test default field value
    custom = zoop.make(Custom, null);
    try t.expect(custom.ptr().age == 99);
    try t.expectEqualStrings(custom.ptr().super.super.name, "default");

    // test mem
    var psubsub = try zoop.new(t.allocator, SubSub);
    zoop.destroy(zoop.cast(psubsub, zoop.IObject));
    psubsub = try zoop.new(t.allocator, SubSub);
    zoop.destroy(psubsub);
    psubsub = try zoop.new(t.allocator, SubSub);
    zoop.destroy(zoop.cast(psubsub, Human));
}
