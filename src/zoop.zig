const std = @import("std");
const builtin = @import("builtin");
const FieldAttributes = std.builtin.Type.Struct.FieldAttributes;
const FieldInfo = struct {
    name: [:0]const u8,
    type: type,
    attrs: FieldAttributes,
};
const FieldEnum = std.meta.FieldEnum;
const Tuple = type;
const nameCast = struct {
    pub fn cast(comptime E: type, name: []const u8) E {
        return @field(E, name);
    }
}.cast;
const compfmt = std.fmt.comptimePrint;
const assert = std.debug.assert;
const zoop = @This();

//===== public content ======
pub const alignment = @alignOf(KlassHeader);
pub const type_id = [*]const u8;
pub const ClassCheckFunc = fn (class_id: type_id) bool;

pub const HookFunc = *const fn (obj: IObject) void;
var destroy_hook_func: ?HookFunc = null;
var new_hook_func: ?HookFunc = null;

pub const ClassInfo = struct {
    pub const VtableInfo = struct {
        typeid: type_id,
        vtable: *anyopaque,
    };
    vtables: []const VtableInfo,
    /// class's typeinfo
    typeinfo: *const TypeInfo,
    /// offset to class in klass
    offset: usize,
    /// check if can cast to other class
    isClass: *const ClassCheckFunc,
    /// call class and all super classes's deinit() and free mem if need
    deinit: *const fn (pklass: *anyopaque) void,

    pub fn getVtable(self: *const ClassInfo, iface_typeid: type_id) ?*anyopaque {
        for (self.vtables) |*item| {
            if (item.typeid == iface_typeid) return item.vtable;
        }
        return null;
    }

    pub fn getVtableOf(self: *const ClassInfo, comptime T: type, comptime I: type) *Vtable(I) {
        comptime {
            if (!tupleHas(interfaces(T), I)) @compileError(compfmt("{s} don't support interface:{s}", .{ @typeName(T), @typeName(I) }));
        }
        assert((comptime interfaceIndex(T, I)) < self.vtables.len);
        return @ptrCast(@alignCast(self.vtables[comptime interfaceIndex(T, I)].vtable));
    }
};

pub const TypeInfo = struct {
    /// @typeNmae()
    typename: []const u8,
    /// typeid of the type
    typeid: type_id,
};

pub const Nil = struct {
    const payload: usize = 0;
    pub fn ptr() *anyopaque {
        return @ptrCast(@constCast(&payload));
    }

    pub fn of(comptime I: type) I {
        return .{ .ptr = @ptrCast(ptr()), .vptr = @ptrCast(@alignCast(ptr())) };
    }
};

pub const IObject = struct {
    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn formatAny(self: IObject, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try icall(self, .formatAny, .{writer});
    }

    pub fn format(self: *const IObject, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try (self.*).formatAny(writer);
    }

    pub fn Default(comptime Class: type) type {
        return struct {
            pub fn formatAny(self: *Class, writer: *std.Io.Writer) std.Io.Writer.Error!void {
                try writer.print("{}", .{self});
            }
        };
    }
};

pub const IRaw = struct {
    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn cast(self: IRaw, comptime I: type) I {
        return I{ .ptr = self.ptr, .vptr = self.vptr };
    }
};

/// Set up hooks to monitor the creation and destruction of objects on the heap
pub fn setHook(new_hook: ?HookFunc, destroy_hook: ?HookFunc) void {
    new_hook_func = new_hook;
    destroy_hook_func = destroy_hook;
}

/// get field of special type from any's inherit tree
pub fn getField(any: anytype, comptime name: []const u8, comptime T: type) t: {
    switch (pointerType(@TypeOf(any))) {
        else => {},
        .read => break :t *const T,
        .write => break :t *T,
    }
    @compileError(compfmt("zoop.getField(any): any must be *Class/*Klass", .{}));
} {
    const V = std.meta.Child(@TypeOf(any));
    if (comptime isKlassType(V)) return getField(&any.class, name, T);
    if (comptime isClassType(V)) {
        const offset = comptime fieldOffset(V, name, T);
        return @ptrFromInt(@intFromPtr(any) + offset);
    }
    @compileError(compfmt("{s} is not Class/Klass.", .{@typeName(V)}));
}

/// get method of Klass/Class
pub fn getMethod(comptime T: type, comptime name: []const u8) MethodType(T, name) {
    comptime {
        if (!isClassType(T) and !isKlassType(T)) @compileError(compfmt("{s} is not an Klass/Class type.", .{@typeName(T)}));

        return (struct {
            pub const method = blk: {
                var Cur = if (isKlassType(T)) T.Class else T;
                while (Cur != void and isClassType(Cur)) {
                    if (@hasDecl(Cur, name)) {
                        const FT = @TypeOf(@field(Cur, name));
                        if (@typeInfo(FT) == .@"fn") {
                            break :blk @field(Cur, name);
                        }
                    }
                    const field_types = @typeInfo(Cur).@"struct".field_types;
                    if (field_types.len > 0 and @typeInfo(field_types[0]) == .@"struct") {
                        Cur = field_types[0];
                    } else {
                        Cur = void;
                    }
                }
                break :blk void;
            };
        }).method;
    }
}

/// get method of Klass/Class' super class
pub fn getUpMethod(comptime T: type, comptime name: []const u8) UpMethodType(T, name) {
    comptime {
        if (!isClassType(T) and !isKlassType(T)) @compileError(compfmt("{s} is not an Klass/Class type.", .{@typeName(T)}));

        const Class = if (isKlassType(T)) T.Class else T;
        const Super = blk: {
            const field_types = @typeInfo(Class).@"struct".field_types;
            if (field_types.len > 0 and @typeInfo(field_types[0]) == .@"struct" and isClassType(field_types[0])) {
                break :blk field_types[0];
            } else {
                break :blk void;
            }
        };
        return getMethod(Super, name);
    }
}

pub const KlassHeader = if (builtin.mode == .debug) packed struct {
    const kmagic: u32 = 0xaabbccdd;
    magic: u32 = kmagic,
    info_addr: usize,
    allocator_addr: usize,
    fn init(_info: *const ClassInfo, _allocator: *const fn (*anyopaque) ?std.mem.Allocator) @This() {
        return .{ .info_addr = @intFromPtr(_info), .allocator_addr = @intFromPtr(_allocator), .magic = kmagic };
    }
    fn getInfo(self: *const @This()) *const ClassInfo {
        return @ptrFromInt(self.info_addr);
    }
    fn getAllocator(self: *const @This()) *const fn (*anyopaque) ?std.mem.Allocator {
        return @ptrFromInt(self.allocator_addr);
    }
} else packed struct {
    info_addr: usize,
    allocator_addr: usize,
    fn init(_info: *const ClassInfo, _allocator: *const fn (*anyopaque) ?std.mem.Allocator) @This() {
        return .{ .info_addr = @intFromPtr(_info), .allocator_addr = @intFromPtr(_allocator) };
    }
    fn getInfo(self: *const @This()) *const ClassInfo {
        return @ptrFromInt(self.info_addr);
    }
    fn getAllocator(self: *const @This()) *const fn (*anyopaque) ?std.mem.Allocator {
        return @ptrFromInt(self.allocator_addr);
    }
};

pub fn Klass(comptime T: type) type {
    comptime {
        if (!isClassType(T)) @compileError(compfmt("{s} is not a class type.(check if @alignOf({s}) == zoop.alignment)", .{ @typeName(T), @typeName(T) }));

        return struct {
            pub const @"#klass" = true;
            pub const class_offset: usize = blk: {
                const pklass: *allowzero @This() = @ptrFromInt(0);
                break :blk @intFromPtr(&pklass.class);
            };
            pub const Class = T;
            header: KlassHeader,
            allocator: ?std.mem.Allocator = null,
            class: T,

            pub fn new(allocator: std.mem.Allocator, init: ?T) !*@This() {
                var self = try allocator.create(@This());
                self.header = .init(comptime makeClassInfo(T), @ptrCast(&getAlly));
                self.allocator = allocator;
                if (init) |v| {
                    self.class = v;
                } else {
                    initClass(&self.class);
                }
                if (new_hook_func) |func| {
                    func(cast(&self.class, IObject));
                }
                return self;
            }

            pub fn make(init: ?T) @This() {
                var self: @This() = undefined;
                self.header = .init(comptime makeClassInfo(T), @ptrCast(&getAlly));
                self.allocator = null;
                if (init) |v| {
                    self.class = v;
                } else {
                    initClass(&self.class);
                }
                if (new_hook_func) |func| {
                    func(cast(&self.class, IObject));
                }
                return self;
            }

            pub fn from(pclass: *const T) *@This() {
                const self: *@This() = @ptrFromInt(@intFromPtr(pclass) - class_offset);
                if (comptime builtin.mode == .debug) {
                    assert(self.header.magic == KlassHeader.kmagic);
                }
                return self;
            }

            pub fn ptr(self: *@This()) *Class {
                return &self.class;
            }

            pub fn deinit(self: *@This()) void {
                if (comptime builtin.mode == .debug) {
                    assert(self.header.magic == KlassHeader.kmagic);
                    assert(self.header.getInfo() == comptime makeClassInfo(T));
                }
                if (destroy_hook_func) |func| {
                    func(cast(&self.class, IObject));
                }
                inline for (comptime classes(T).items) |V| {
                    if (comptime @hasDecl(V, "deinit")) {
                        var p: *V = @ptrCast(&self.class);
                        p.deinit();
                    }
                }
                if (self.allocator) |dtor| {
                    dtor.destroy(self);
                }
            }

            fn getAlly(self: *@This()) ?std.mem.Allocator {
                return self.allocator;
            }
        };
    }
}

pub fn ApiEnum(comptime I: type) type {
    comptime {
        if (!isInterfaceType(I)) @compileError(compfmt("{s} is not an interface type.", .{@typeName(I)}));

        const decls = std.meta.declarations(I);
        var apis: [decls.len][:0]const u8 = undefined;
        var idx = 0;
        for (decls) |decl| {
            const info = @typeInfo(@TypeOf(@field(I, decl)));
            if (info == .@"fn" and info.@"fn".param_types.len > 0 and !info.@"fn".is_generic) {
                const first = info.@"fn".param_types[0] orelse unreachable;
                const Self = switch (@typeInfo(first)) {
                    else => void,
                    .@"struct" => first,
                    .pointer => std.meta.Child(first),
                };
                if (Self == I) {
                    if (!hasDecl(apis[0..idx], decl)) {
                        apis[idx] = decl;
                        idx += 1;
                    }
                }
            }
        }

        if (idx == 0) {
            return @Enum(u0, .exhaustive, &.{}, &.{});
        }

        var field_names: [idx][]const u8 = undefined;
        var field_values: [idx]std.math.IntFittingRange(0, idx - 1) = undefined;
        for (0..idx) |i| {
            field_names[i] = apis[i];
            field_values[i] = @intCast(i);
        }
        return @Enum(std.math.IntFittingRange(0, idx - 1), .exhaustive, &field_names, &field_values);
    }
}

pub fn MethodEnum(comptime T: type) type {
    comptime {
        const Class = if (isKlassType(T)) T.Class else if (isClassType(T) or isInterfaceType(T)) T else @compileError(compfmt("{s} is not Klass/Class/Interface.", .{@typeName(T)}));

        const supers = if (isInterfaceType(T)) interfaces(T) else classes(Class);
        const count = blk: {
            var n = 0;
            for (supers.items) |super| {
                n += std.meta.declarations(super).len;
            }
            break :blk n;
        };
        var methods: [count][:0]const u8 = undefined;
        var idx = 0;
        for (supers.items) |super| {
            for (std.meta.declarations(super)) |decl| {
                const info = @typeInfo(@TypeOf(@field(super, decl)));
                if (info == .@"fn" and info.@"fn".param_types.len > 0 and !info.@"fn".is_generic) {
                    const first = info.@"fn".param_types[0].?;
                    const Self = switch (@typeInfo(first)) {
                        else => void,
                        .@"struct" => first,
                        .pointer => std.meta.Child(first),
                    };
                    if (Self == super and !hasDecl(methods[0..idx], decl)) {
                        methods[idx] = decl;
                        idx += 1;
                    }
                }
            }
        }

        if (idx == 0) {
            return @Enum(u0, .exhaustive, &.{}, &.{});
        }

        var field_names: [idx][]const u8 = undefined;
        var field_values: [idx]std.math.IntFittingRange(0, idx - 1) = undefined;
        for (0..idx) |i| {
            field_names[i] = methods[i];
            field_values[i] = @intCast(i);
        }
        return @Enum(std.math.IntFittingRange(0, idx - 1), .exhaustive, &field_names, &field_values);
    }
}

pub fn Vtable(comptime I: type) type {
    comptime {
        if (!isInterfaceType(I)) @compileError(compfmt("{s} is not an interface type.", .{@typeName(I)}));
        const ifaces = interfaces(I);
        var vtables: [ifaces.items.len]type = undefined;
        // ifaces.items.len - 1 for saving pointer to super interfaces vtable, -1 for I itself,
        // for making interface to interface casting faster.
        var nfield: comptime_int = ifaces.items.len - 1;

        @setEvalBranchQuota(20000);
        for (ifaces.items, 0..) |iface, i| {
            vtables[i] = VtableDirect(iface);
            nfield += @typeInfo(vtables[i]).@"struct".field_names.len;
        }

        var names: [nfield][]const u8 = undefined;
        var types: [nfield]type = undefined;
        var attrs: [nfield]FieldAttributes = undefined;
        var idx: comptime_int = 0;
        for (ifaces.items) |iface| {
            if (iface != I) {
                names[idx] = @typeName(iface);
                types[idx] = *Vtable(iface);
                attrs[idx] = .{
                    .@"align" = @alignOf(*Vtable(iface)),
                };
                idx += 1;
            }
        }
        for (vtables) |vt| {
            const info = @typeInfo(vt).@"struct";
            @setEvalBranchQuota(10000);
            for (info.field_names, info.field_types, info.field_attrs) |name, FT, attr| {
                names[idx] = name;
                types[idx] = FT;
                attrs[idx] = attr;
                idx += 1;
            }
        }
        const field_count = idx;
        return @Struct(.auto, null, names[0..field_count], types[0..field_count], attrs[0..field_count]);
    }
}

pub fn tupleInit(comptime any: anytype) type {
    comptime {
        if (isTuple(any)) return any;

        if (@TypeOf(any) == @TypeOf(.{})) {
            return struct {
                pub const items = .{};
            };
        }

        return struct {
            pub const items = .{any};
        };
    }
}

pub fn tupleAppend(comptime tuple: Tuple, comptime any: anytype) Tuple {
    comptime {
        if (isTuple(any)) {
            return struct {
                pub const items = tuple.items ++ any.items;
            };
        } else {
            return struct {
                pub const items = tuple.items ++ .{any};
            };
        }
    }
}

pub fn tupleAppendUnique(comptime tuple: Tuple, comptime any: anytype) Tuple {
    comptime {
        var ret = tuple;
        @setEvalBranchQuota(30000);
        for (tupleInit(any).items) |item| {
            if (!tupleHas(ret, item)) {
                ret = tupleAppend(ret, item);
            }
        }
        return ret;
    }
}

pub inline fn tupleHas(comptime tuple: Tuple, comptime any: anytype) bool {
    comptime {
        @setEvalBranchQuota(3000);
        for (tuple.items) |item| {
            if (@TypeOf(item) == @TypeOf(any)) {
                if (@TypeOf(any) == [:0]const u8 or @TypeOf(any) == []const u8) {
                    if (std.mem.eql(u8, item, any)) return true;
                } else if (item == any) return true;
            }
        }
        return false;
    }
}

pub fn typeInfo(any: anytype) *const TypeInfo {
    if (comptime @TypeOf(any) == type) {
        return comptime makeTypeInfo(any);
    } else {
        return comptime makeTypeInfo(@TypeOf(any));
    }
}

pub fn classInfo(any: anytype) *const ClassInfo {
    return switch (@typeInfo(@TypeOf(any))) {
        else => ClassInfoGetter(@TypeOf(any)).get(any),
        .type => comptime makeClassInfo(any),
    };
}

pub fn typeId(any: anytype) type_id {
    if (comptime @TypeOf(any) == type) {
        return comptime makeTypeId(any);
    } else {
        return comptime makeTypeId(@TypeOf(any));
    }
}

pub fn new(allocator: std.mem.Allocator, comptime T: type, init: ?T) !*align(alignment) T {
    comptime {
        if (!isClassType(T)) @compileError(compfmt("{s} is not a class type.", .{@typeName(T)}));
    }
    var self = try Klass(T).new(allocator, init);
    return &self.class;
}

pub fn destroy(any: anytype) void {
    const T = comptime @TypeOf(any);
    if (comptime isInterfaceType(T)) {
        var header: *KlassHeader = @ptrCast(@alignCast(any.ptr));
        return header.getInfo().deinit(@ptrCast(header));
    } else {
        switch (comptime @typeInfo(T)) {
            else => {},
            .pointer => |p| {
                switch (p.size) {
                    else => {},
                    .one => {
                        if (comptime isKlassType(p.child)) {
                            return any.header.getInfo().deinit(@ptrCast(any));
                        } else if (comptime isClassType(p.child)) {
                            var klass = Klass(p.child).from(any);
                            return klass.header.getInfo().deinit(@ptrCast(klass));
                        }
                    },
                }
            },
        }
    }
    @compileError(compfmt("'{s}' is not a *Klass/*Class.", .{@typeName(@TypeOf(any))}));
}

pub fn make(comptime T: type, init: ?T) Klass(T) {
    return Klass(T).make(init);
}

/// call interface method
/// example: zoop.icall(ihuman, .getAge, .{})
pub fn icall(iface: anytype, comptime api_enum: ApiEnum(@TypeOf(iface)), args: anytype) ReturnType(@TypeOf(iface), api_enum) {
    comptime {
        if (!isInterfaceType(@TypeOf(iface))) @compileError(compfmt("'{s}' is not an interface type.", .{@typeName(@TypeOf(iface))}));
    }
    const pklass: *Klass(struct { x: u8 align(alignment) }) = @ptrFromInt(@intFromPtr(iface.ptr));
    const vptr: *const Vtable(@TypeOf(iface)) = @ptrFromInt(@intFromPtr(iface.vptr));
    const ptr: *anyopaque = @ptrFromInt(@intFromPtr(pklass) + pklass.header.getInfo().offset);
    return @call(.auto, @field(vptr, @tagName(api_enum)), .{ptr} ++ args);
}

/// call interface method of Klass/Class. (virtual call)
/// example: zoop.vcall(pclass, IHuman.getName, .{})
pub fn vcall(any: anytype, comptime method: anytype, args: anytype) ApiInfo(method).Return {
    comptime {
        var pass = false;
        switch (@typeInfo(@TypeOf(any))) {
            else => {},
            .@"struct" => pass = isInterfaceType(@TypeOf(any)),
            .pointer => |p| {
                pass = isKlassType(p.child) or isClassType(p.child);
            },
        }
        if (!pass) @compileError(compfmt("'{s}' is not *Klass/*Class/interface.", .{@typeName(@TypeOf(any))}));
        if (!isInterfaceType(ApiInfo(method).Iface)) @compileError(compfmt("'{s}'' is not Interface.", .{@typeName(ApiInfo(method).Iface)}));
    }
    const info = comptime ApiInfo(method);
    if (comptime canCast(@TypeOf(any), info.Iface)) {
        return icall(zoop.cast(any, info.Iface), info.name, args);
    }
    if (zoop.as(any, info.Iface)) |iface| {
        return icall(iface, info.name, args);
    }
    @panic(StackBuf(2048).init().print("{s} don't support method: {s}.{s}", .{ @typeName(@TypeOf(any)), @typeName(info.Iface), @tagName(info.name) }));
}

/// call super class's method.
/// example: zoop.upcall(pclass, .methodOfSuperClass, .{});
pub fn upcall(any: anytype, comptime method_enum: MethodEnum(std.meta.Child(@TypeOf(any))), args: anytype) t: {
    const T = @TypeOf(any);
    switch (@typeInfo(T)) {
        else => {},
        .pointer => |p| {
            if (isKlassType(p.child) or isClassType(p.child)) {
                const Class = if (isKlassType(p.child)) p.child.Class else p.child;
                const MT = UpMethodType(Class, @tagName(method_enum));
                if (MT != void) {
                    break :t @typeInfo(MT).@"fn".return_type orelse void;
                } else {
                    @compileError(compfmt("no method named '{s}' founded in inherit tree of {s}.", .{ @tagName(method_enum), @typeName(Class) }));
                }
            }
        },
    }
    @compileError(compfmt("'{s}' is not *Klass/*Class.", .{@typeName(@TypeOf(any))}));
} {
    const T = comptime std.meta.Child(@TypeOf(any));
    const Class = comptime if (isKlassType(T)) T.Class else T;
    const method = comptime getUpMethod(Class, @tagName(method_enum));
    const Ptr = comptime @typeInfo(@TypeOf(method)).@"fn".param_types[0].?;
    const ptr: Ptr = @ptrCast(if (comptime isKlassType(T)) &any.class else any);
    return @call(.auto, method, .{ptr} ++ args);
}

pub inline fn isRootPtr(ptr: anytype) bool {
    const T = comptime std.meta.Child(@TypeOf(ptr));
    if (comptime isClassType(T)) {
        return classInfo(ptr) == comptime makeClassInfo(T);
    } else if (comptime isKlassType(T)) {
        return classInfo(ptr) == comptime makeClassInfo(T.Class);
    }
    @compileError(compfmt("{s} is not *Klass/*Class.", .{@typeName(T)}));
}

pub fn getAllocator(any: anytype) ?std.mem.Allocator {
    const T = comptime @TypeOf(any);
    switch (comptime @typeInfo(T)) {
        else => {},
        .@"struct" => {
            if (comptime isInterfaceType(T)) {
                const pklass: *Klass(struct { x: u8 align(alignment) }) = @ptrFromInt(@intFromPtr(any.ptr));
                return pklass.header.getAllocator()(any.ptr);
            }
        },
        .pointer => |p| {
            if (comptime p.size == .one) {
                if (comptime isKlassType(p.child)) {
                    return any.allocator;
                } else if (comptime isClassType(p.child)) {
                    const pklass = Klass(p.child).from(any);
                    return pklass.header.getAllocator()(@ptrFromInt(@intFromPtr(pklass)));
                }
            }
        },
    }
    @compileError(compfmt("'{s}' is not *Klass/*Class/interface.", .{@typeName(@TypeOf(any))}));
}

pub fn cast(any: anytype, comptime T: type) t: {
    break :t if (isInterfaceType(T)) T else switch (pointerType(@TypeOf(any))) {
        .read => *const T,
        .write => *T,
        else => @compileError(compfmt("'{s}' is not *Class/interface.", .{@typeName(@TypeOf(any))})),
    };
} {
    const caster = comptime Caster(@TypeOf(any), T);
    if (comptime caster != void) {
        return caster.cast(any, T);
    }
    @compileError(compfmt("'{s}' can not cast to '{s}'", .{ @typeName(@TypeOf(any)), @typeName(T) }));
}

pub fn as(any: anytype, comptime T: type) t: {
    break :t if (isInterfaceType(T)) ?T else switch (pointerType(@TypeOf(any))) {
        .read => ?*const T,
        .write => ?*T,
        else => if (isInterfaceType(@TypeOf(any))) ?*T else @compileError(compfmt("'{s}' is not *Klass/*Class/interface.", .{@typeName(@TypeOf(any))})),
    };
} {
    const V = comptime @TypeOf(any);
    const ptr: *anyopaque = blk: {
        if (comptime isInterfaceType(V)) break :blk @alignCast(any.ptr);
        if (comptime isKlassType(std.meta.Child(V))) break :blk @ptrCast(@alignCast(any));
        if (comptime isClassType(std.meta.Child(V))) break :blk @ptrCast(@alignCast(Klass(std.meta.Child(V)).from(any)));
        unreachable;
    };
    const info = classInfo(any);

    if (comptime isInterfaceType(T)) {
        if (info.getVtable(comptime makeTypeId(T))) |pvtable| {
            return T{ .ptr = ptr, .vptr = pvtable };
        }
    }
    if (comptime isClassType(T)) {
        if (info.isClass(comptime makeTypeId(T))) {
            const klass: *Klass(T) = @ptrCast(@alignCast(ptr));
            return &klass.class;
        }
    }

    return null;
}

pub fn asptr(any: anytype) *anyopaque {
    switch (comptime @typeInfo(@TypeOf(any))) {
        else => {},
        .@"struct" => {
            if (comptime isInterfaceType(@TypeOf(any))) {
                return any.ptr;
            }
        },
        .pointer => |p| {
            if (comptime isKlassType(p.child)) return @ptrCast(any);
            if (comptime isClassType(p.child)) return @ptrCast(Klass(p.child).from(any));
        },
    }
    @compileError(compfmt("'{s}' is not *Klass/*Class/interface.", .{@typeName(@TypeOf(any))}));
}

pub fn nil(comptime I: type) I {
    comptime {
        if (!isInterfaceType(I)) @compileError(@typeName(I) ++ " is not Interface.");
    }
    return Nil.of(I);
}

pub inline fn isNil(any: anytype) bool {
    comptime {
        if (!isInterfaceType(@TypeOf(any))) @compileError(compfmt("'{s}' is not interface", .{@typeName(@TypeOf(any))}));
    }
    return any.ptr == Nil.ptr();
}

//===== private content ======
inline fn hasDecl(comptime decls: []const [:0]const u8, comptime decl: [:0]const u8) bool {
    comptime {
        @setEvalBranchQuota(5000);
        for (decls) |d| {
            if (std.mem.eql(u8, d, decl)) return true;
        }
        return false;
    }
}

inline fn isMethod(comptime T: type, comptime name: []const u8) bool {
    comptime {
        if (@hasDecl(T, name)) {
            const FT = @TypeOf(@field(T, name));
            switch (@typeInfo(FT)) {
                else => {},
                .@"fn" => |f| {
                    if (f.param_types.len > 0) {
                        return RealType(f.param_types[0].?) == T;
                    }
                },
            }
        }
        return false;
    }
}

fn RealType(comptime T: type) type {
    comptime {
        return switch (@typeInfo(T)) {
            else => T,
            .pointer => std.meta.Child(T),
        };
    }
}

fn DefaultMethodType(comptime T: type, comptime I: type, comptime name: []const u8) type {
    comptime {
        const ifaces = interfaces(I);
        var all = tupleInit(.{});
        for (ifaces.items) |iface| {
            if (@hasDecl(iface, "Default")) {
                const def = iface.Default(T);
                if (@hasDecl(def, name)) {
                    all = tupleAppend(all, iface);
                }
            }
        }
        switch (all.items.len) {
            0 => {},
            1 => return @TypeOf(@field(all.items[0].Default(T), name)),
            else => @compileError(compfmt("multi default method '{s}' found in: {}", .{ name, all.items })),
        }

        return void;
    }
}

fn defaultMethod(comptime T: type, comptime I: type, comptime name: []const u8) DefaultMethodType(T, I, name) {
    comptime {
        const ifaces = interfaces(I);
        for (ifaces.items) |iface| {
            if (@hasDecl(iface, "Default")) {
                const def = iface.Default(T);
                if (@hasDecl(def, name)) {
                    return @field(def, name);
                }
            }
        }
        unreachable;
    }
}

fn MethodType(comptime T: type, comptime name: []const u8) type {
    comptime {
        var Cur = if (isKlassType(T)) T.Class else T;
        while (Cur != void) {
            if (isMethod(Cur, name)) return @TypeOf(@field(Cur, name));

            const field_types = @typeInfo(Cur).@"struct".field_types;
            if (field_types.len > 0 and isClassType(RealType(field_types[0]))) {
                Cur = field_types[0];
            } else {
                Cur = void;
            }
        }
        return void;
    }
}

fn UpMethodType(comptime T: type, comptime name: []const u8) type {
    comptime {
        const Class = if (isKlassType(T)) T.Class else T;
        const Super = blk: {
            const field_types = @typeInfo(Class).@"struct".field_types;
            if (field_types.len > 0 and @typeInfo(field_types[0]) == .@"struct" and isClassType(field_types[0])) {
                break :blk field_types[0];
            } else {
                break :blk void;
            }
        };
        return if (Super == void) void else MethodType(Super, name);
    }
}

fn ReturnType(comptime I: type, comptime method: ApiEnum(I)) type {
    comptime {
        if (!isInterfaceType(I)) @compileError(compfmt("{s} is not an interface type.", .{@typeName(I)}));
        return @typeInfo(@TypeOf(@field(I, @tagName(method)))).@"fn".return_type orelse void;
    }
}

fn ApiInfo(comptime method: anytype) type {
    comptime {
        const info = @typeInfo(@TypeOf(method));
        if (info != .@"fn") @compileError("method is not a .fn");
        if (info.@"fn".param_types.len == 0 or !isInterfaceType(info.@"fn".param_types[0].?))
            @compileError(compfmt("{s} is not an interface type.", .{@typeName(info.@"fn".param_types[0].?)}));

        const I = info.@"fn".param_types[0] orelse unreachable;
        const method_name = blk: {
            for (std.meta.declarations(I)) |decl| {
                if (@TypeOf(@field(I, decl)) == @TypeOf(method)) {
                    if (@field(I, decl) == method)
                        break :blk decl;
                }
            }
            unreachable;
        };
        return struct {
            pub const Iface = I;
            pub const Return = @typeInfo(@TypeOf(method)).@"fn".return_type orelse void;
            pub const name = nameCast(ApiEnum(I), method_name);
        };
    }
}

fn Caster(comptime V: type, comptime T: type) type {
    comptime {
        if (isInterfaceType(V)) {
            if (isInterfaceType(T)) {
                if (tupleHas(interfaces(V), T)) {
                    // interface -> interface
                    return struct {
                        pub fn cast(any: anytype, comptime I: type) I {
                            if (V == I) return any;
                            const vtable: *Vtable(V) = @ptrFromInt(@intFromPtr(any.vptr));
                            return T{ .ptr = any.ptr, .vptr = @field(vtable, @typeName(I)) };
                        }
                    };
                }
            }
        } else switch (@typeInfo(V)) {
            else => {},
            .pointer => |p| {
                if (isClassType(p.child)) {
                    if (isInterfaceType(T)) {
                        if (tupleHas(interfaces(p.child), T)) {
                            // class -> interface
                            return struct {
                                pub fn cast(any: anytype, comptime I: type) I {
                                    const pklass = Klass(p.child).from(any);
                                    const header = pklass.header;
                                    const info = header.getInfo();
                                    return I{ .ptr = @ptrCast(pklass), .vptr = info.getVtableOf(p.child, I) };
                                }
                            };
                        }
                    } else if (isClassType(T)) {
                        if (tupleHas(classes(p.child), T)) {
                            // class -> class
                            return struct {
                                pub fn cast(any: anytype, comptime C: type) t: {
                                    break :t switch (pointerType(V)) {
                                        else => unreachable,
                                        .read => *const C,
                                        .write => *C,
                                    };
                                } {
                                    if (p.child == C) {
                                        return any;
                                    } else {
                                        return @ptrFromInt(@intFromPtr(any));
                                    }
                                }
                            };
                        }
                    }
                } else if (isKlassType(p.child)) {
                    // klass -> T
                    const caster = switch (pointerType(V)) {
                        else => unreachable,
                        .read => Caster(*const p.child.Class, T),
                        .write => Caster(*p.child.Class, T),
                    };
                    return struct {
                        pub fn cast(any: anytype, comptime ANY: type) @TypeOf(caster.cast(&any.class, ANY)) {
                            return caster.cast(&any.class, ANY);
                        }
                    };
                }
            },
        }
        return void;
    }
}

fn ClassInfoGetter(comptime T: type) type {
    comptime {
        switch (@typeInfo(T)) {
            else => {},
            .type => {
                if (isKlassType(T)) {
                    return struct {
                        pub fn get(_: anytype) *const ClassInfo {
                            return comptime makeClassInfo(T.Class);
                        }
                    };
                }
                if (isClassType(T)) {
                    return struct {
                        pub fn get(_: anytype) *const ClassInfo {
                            return comptime makeClassInfo(T);
                        }
                    };
                }
            },
            .pointer => |p| {
                if (isKlassType(p.child)) {
                    return struct {
                        pub fn get(klass: anytype) *const ClassInfo {
                            return klass.header.getInfo();
                        }
                    };
                }
                if (isClassType(p.child)) {
                    return struct {
                        pub fn get(class: anytype) *const ClassInfo {
                            return Klass(p.child).from(class).header.getInfo();
                        }
                    };
                }
            },
            .@"struct" => {
                if (isInterfaceType(T)) {
                    return struct {
                        pub fn get(iface: anytype) *const ClassInfo {
                            const pklass: *Klass(struct { x: u8 align(alignment) }) = @ptrFromInt(@intFromPtr(iface.ptr));
                            return pklass.header.getInfo();
                        }
                    };
                }
            },
        }
        @compileError(compfmt("'{s}' is not Class/*Class/Klass/*Klass/Interface.", .{@typeName(T)}));
    }
}

fn defaultFields(comptime T: type) []const FieldInfo {
    comptime {
        const info = @typeInfo(T).@"struct";
        var fields: [info.field_names.len]FieldInfo = undefined;
        var idx = 0;
        for (info.field_names, info.field_types, info.field_attrs) |name, FT, attrs| {
            if (attrs.default_value_ptr != null) {
                fields[idx] = .{ .name = name, .type = FT, .attrs = attrs };
                idx += 1;
            }
        }
        const ret = fields[0..idx];
        return ret;
    }
}

fn initDefaultFields(pclass: anytype) void {
    const T = comptime std.meta.Child(@TypeOf(pclass));
    inline for (comptime defaultFields(T)) |field| {
        const ptr: *const field.type = @ptrCast(@alignCast(field.attrs.default_value_ptr.?));
        @field(pclass, field.name) = ptr.*;
    }
}

fn initClass(pclass: anytype) void {
    const T = comptime std.meta.Child(@TypeOf(pclass));
    const supers = comptime classes(T);
    inline for (supers.items) |V| {
        const pv: *V = @ptrCast(@alignCast(pclass));
        initDefaultFields(pv);
    }
}

fn fieldOffset(comptime T: type, comptime name: []const u8, comptime FT: type) usize {
    comptime {
        const supers = classes(T);
        var owners = tupleInit(.{});
        for (supers.items) |V| {
            if (@hasField(V, name)) {
                if (@FieldType(V, name) == FT) {
                    owners = tupleAppend(owners, V);
                }
            }
        }
        switch (owners.items.len) {
            0 => @compileError(compfmt("no field of type '{s}' named '{s}' in '{s}'", .{ @typeName(FT), name, @typeName(T) })),
            1 => {
                const V = owners.items[0];
                const pv: *allowzero V = @ptrFromInt(0);
                const pf = &@field(pv, name);
                return @intFromPtr(pf);
            },
            else => @compileError(compfmt("more than one field of type '{s}' named '{s}' found: {}", .{ @typeName(FT), name, owners.items })),
        }
    }
}

fn classChecker(comptime T: type) *const ClassCheckFunc {
    comptime {
        if (!isClassType(T)) @compileError(compfmt("'{s}' is not Class.", .{@typeName(T)}));

        return (struct {
            pub fn func(class_id: type_id) bool {
                inline for (comptime classes(T).items) |Class| {
                    if (class_id == comptime makeTypeId(Class)) return true;
                }
                return false;
            }
        }).func;
    }
}

fn makeClassVtables(comptime T: type) []const ClassInfo.VtableInfo {
    comptime {
        const ifaces = interfaces(T);
        return @ptrCast((struct {
            pub const val: [ifaces.items.len]ClassInfo.VtableInfo = blk: {
                var vtables: [ifaces.items.len]ClassInfo.VtableInfo = undefined;
                for (ifaces.items, 0..) |iface, i| {
                    vtables[i] = .{
                        .typeid = makeTypeId(iface),
                        .vtable = @ptrCast(makeVtable(T, iface)),
                    };
                }
                break :blk vtables;
            };
        }).val[0..]);
    }
}

fn makeClassInfo(comptime T: type) *const ClassInfo {
    comptime {
        return &(struct {
            pub const info: ClassInfo = .{
                .vtables = makeClassVtables(T),
                .offset = Klass(T).class_offset,
                .typeinfo = makeTypeInfo(T),
                .isClass = classChecker(T),
                .deinit = @ptrCast(&Klass(T).deinit),
            };
        }).info;
    }
}

fn makeTypeInfo(comptime T: type) *const TypeInfo {
    comptime {
        return &(struct {
            pub const info: TypeInfo = .{
                .typename = @typeName(T),
                .typeid = makeTypeId(T),
            };
        }).info;
    }
}

fn makeTypeId(comptime T: type) type_id {
    comptime {
        // return @intCast(@intFromError(@field(anyerror, "#" ++ @typeName(T))));
        return (struct {
            pub const name = @typeName(T);
        }).name.ptr;
    }
}

fn makeVtable(comptime T: type, comptime I: type) *anyopaque {
    comptime {
        if (!isKlassType(T) and !isClassType(T)) @compileError(compfmt("{s} is not Klass/Class.", .{@typeName(T)}));
        if (!isInterfaceType(I)) @compileError(compfmt("{s} is not interface.", .{@typeName(I)}));

        const VT = Vtable(I);
        const ifaces = interfaces(I);
        const nsuper = ifaces.items.len - 1;
        return @constCast(&(struct {
            pub const vt: VT = blk: {
                var val: VT = undefined;
                for (ifaces.items) |iface| {
                    if (iface != I) {
                        @field(val, @typeName(iface)) = @ptrCast(@alignCast(makeVtable(T, iface)));
                    }
                }
                for (std.meta.fieldNames(VT)[nsuper..], std.meta.fieldTypes(VT)[nsuper..]) |field_name, field_type| {
                    const MT = MethodType(T, field_name);
                    if (MT != void) {
                        checkApi(T, I, field_name);
                        @field(val, field_name) = @ptrCast(&getMethod(T, field_name));
                    } else if (DefaultMethodType(T, I, field_name) != void) {
                        checkInterface(I);
                        @field(val, field_name) = @ptrCast(&defaultMethod(T, I, field_name));
                    } else {
                        @compileError(compfmt("{s} must implement method '{s}: {}'", .{ @typeName(T), field_name, field_type }));
                    }
                }
                break :blk val;
            };
        }).vt);
    }
}

fn checkInterface(comptime I: type) void {
    comptime {
        if (!isInterfaceType(I)) @compileError(compfmt("{s} is not Interface.", .{@typeName(I)}));
        if (@hasDecl(I, "Default")) {
            const field = @field(I, "Default");
            switch (@typeInfo(@TypeOf(field))) {
                else => @compileError("{s}.Default must be: fn (comptime T:type) type"),
                .@"fn" => |info| {
                    if (!info.is_generic or info.return_type != type)
                        @compileError("{s}.Default must be: fn (comptime T:type) type");
                },
            }
            const methods = I.Default(struct {});
            if (@typeInfo(methods) != .@"struct") {
                @compileError(compfmt("{s}.Default() must return a struct but:{}", .{ @typeName(I), methods }));
            }
            for (std.meta.declarations(methods)) |decl| {
                if (!@hasDecl(I, decl))
                    @compileError(compfmt("'{s}' is not a '{s}' api but in '{s}.Default'.", .{ decl, @typeName(I), @typeName(I) }));
                const definfo = @typeInfo(@TypeOf(@field(methods, decl)));
                const info = @typeInfo(@TypeOf(@field(I, decl)));
                var fail = false;
                if (definfo == .@"fn" and info == .@"fn") {
                    if (definfo.@"fn".param_types.len == info.@"fn".param_types.len and definfo.@"fn".param_types.len > 0) {
                        for (1..definfo.@"fn".param_types.len) |i| {
                            if (definfo.@"fn".param_types[i] != info.@"fn".param_types[i]) {
                                fail = true;
                                break;
                            }
                        }
                    } else fail = true;
                } else fail = true;
                if (fail) {
                    @compileError(compfmt("type missmatch: {s}.{s}, {s}.Default.{s}", .{ @typeName(I), decl, @typeName(I), decl }));
                }
            }
        }
    }
}

/// Check whether the type of the function named field in T and the pointer of the function with the same name in VT match
fn checkApi(comptime T: type, comptime I: type, comptime field: []const u8) void {
    comptime {
        @setEvalBranchQuota(5000);
        const VT = Vtable(I);
        const vtinfo = @typeInfo(@typeInfo(@FieldType(VT, field)).pointer.child);
        const tinfo = @typeInfo(MethodType(T, field));
        if (vtinfo.@"fn".return_type.? != tinfo.@"fn".return_type.?) @compileError(compfmt("'{s}.{s}' must return '{}' as same as '{s}.{s}'.", .{ @typeName(T), field, vtinfo.@"fn".return_type.?, @typeName(I), field }));
        if (vtinfo.@"fn".param_types.len != tinfo.@"fn".param_types.len) @compileError(compfmt("parameters number of '{s}.{s}' must as same as '{s}.{s}'.", .{ @typeName(T), field, @typeName(I), field }));
        if (vtinfo.@"fn".param_types.len < 2) return;
        for (vtinfo.@"fn".param_types[1..], tinfo.@"fn".param_types[1..], 1..) |vt, t, i| {
            const idx = i;
            if (vt.? != t.?) {
                @compileError(compfmt("The {d}th parameter of {s}.{s} must be '{}'.", .{ idx + 1, @typeName(T), field, vt.? }));
            }
        }
    }
}

pub inline fn isInterfaceType(comptime T: type) bool {
    const field_names = comptime @typeInfo(IObject).@"struct".field_names;
    const field_types = comptime @typeInfo(IObject).@"struct".field_types;
    return comptime blk: {
        @setEvalBranchQuota(8000);
        switch (@typeInfo(T)) {
            else => break :blk false,
            .@"struct" => |s| break :blk s.field_names.len == field_names.len and
                s.field_types[0] == field_types[0] and
                s.field_types[1] == field_types[1] and
                std.mem.eql(u8, s.field_names[0], field_names[0]) and
                std.mem.eql(u8, s.field_names[1], field_names[1]),
        }
    };
}

pub inline fn isClassType(comptime T: type) bool {
    return comptime blk: {
        if (isKlassType(T)) break :blk false;
        if (isInterfaceType(T)) break :blk false;
        break :blk switch (@typeInfo(T)) {
            else => false,
            .@"struct" => |s| !s.is_tuple and s.field_types.len > 0 and s.field_attrs[0].@"align" == alignment,
        };
    };
}

pub inline fn isKlassType(comptime T: type) bool {
    return comptime blk: {
        if (@typeInfo(T) == .@"struct") {
            break :blk @hasDecl(T, "#klass");
        } else break :blk false;
    };
}

pub inline fn isTuple(any: anytype) bool {
    return comptime blk: {
        if (@TypeOf(any) == type and @typeInfo(any) == .@"struct") {
            if (!@hasDecl(any, "items")) break :blk false;
            const T = @TypeOf(@field(any, "items"));
            break :blk switch (@typeInfo(T)) {
                .@"struct" => |s| s.is_tuple,
                else => false,
            };
        } else {
            break :blk false;
        }
    };
}

pub inline fn interfaceIndex(comptime T: type, comptime I: type) usize {
    return comptime blk: {
        const ifaces = interfaces(T);
        for (ifaces.items, 0..) |iface, i| {
            if (iface == I) break :blk i;
        }
        @compileError(compfmt("{s} don't support interface:{s}", .{ @typeName(T), @typeName(I) }));
    };
}

fn interfaces(comptime T: type) Tuple {
    comptime {
        var ret = tupleInit(IObject);

        if (isInterfaceType(T)) {
            if (@hasDecl(T, "extends")) {
                const extends = @field(T, "extends");
                switch (@typeInfo(@TypeOf(extends))) {
                    else => {},
                    .@"struct" => |s| {
                        if (s.is_tuple) {
                            for (extends) |iface| {
                                if (isInterfaceType(iface)) {
                                    ret = tupleAppendUnique(ret, interfaces(iface));
                                } else @compileError(compfmt("{s} in {s}.extends but is not an interface type.", .{ @typeName(iface), @typeName(T) }));
                            }
                        }
                    },
                }
            }
            ret = tupleAppendUnique(ret, T);
        } else if (isClassType(T)) {
            const field_types = @typeInfo(T).@"struct".field_types;
            if (field_types.len > 0 and isClassType(field_types[0])) {
                ret = tupleAppendUnique(ret, interfaces(field_types[0]));
            }
            if (@hasDecl(T, "extends")) {
                const extends = @field(T, "extends");
                switch (@typeInfo(@TypeOf(extends))) {
                    else => {},
                    .@"struct" => |s| {
                        if (s.is_tuple) {
                            for (extends) |iface| {
                                if (isInterfaceType(iface)) {
                                    ret = tupleAppendUnique(ret, interfaces(iface));
                                } else @compileError(compfmt("{s} in {s}.extends but is not Interface.", .{ @typeName(iface), @typeName(T) }));
                            }
                        }
                    },
                }
            }
        }

        return ret;
    }
}

fn classes(comptime T: type) Tuple {
    comptime {
        var ret = tupleInit(.{});
        if (!isClassType(T)) return ret;

        var Cur = T;
        while (Cur != void) {
            ret = tupleAppend(ret, Cur);
            const field_types = @typeInfo(Cur).@"struct".field_types;
            if (field_types.len > 0 and isClassType(field_types[0])) {
                Cur = field_types[0];
            } else {
                Cur = void;
            }
        }

        return ret;
    }
}

inline fn isExclude(comptime I: type, comptime method: []const u8) bool {
    return comptime blk: {
        if (@hasDecl(I, "excludes")) {
            @setEvalBranchQuota(20000);
            for (@field(I, "excludes")) |name| {
                if (std.mem.eql(u8, method, name)) break :blk true;
            }
        }
        break :blk false;
    };
}

fn VtableDirect(comptime I: type) type {
    comptime {
        const decls = std.meta.declarations(I);
        var names: [decls.len][]const u8 = undefined;
        var types: [decls.len]type = undefined;
        var attrs: [decls.len]FieldAttributes = undefined;
        var idx = 0;
        @setEvalBranchQuota(7000);
        for (decls) |decl| {
            if (!isExclude(I, decl)) {
                const info = @typeInfo(@TypeOf(@field(I, decl)));
                switch (info) {
                    .@"fn" => |f| {
                        if (!f.is_generic and f.param_types.len > 0 and f.param_types[0] == I) {
                            const FT = VtableFieldType(f);
                            names[idx] = decl;
                            types[idx] = FT;
                            attrs[idx] = .{
                                .@"align" = @alignOf(FT),
                            };
                            idx += 1;
                        }
                    },
                    else => {},
                }
            }
        }
        const field_count = idx;
        return @Struct(.auto, null, names[0..field_count], types[0..field_count], attrs[0..field_count]);
    }
}

fn VtableFieldType(comptime F: std.builtin.Type.Fn) type {
    comptime {
        var param_types: [F.param_types.len]type = undefined;
        var param_attrs: [F.param_types.len]std.builtin.Type.Fn.ParamAttributes = undefined;
        param_types[0] = *anyopaque;
        param_attrs[0] = .{
            .@"noalias" = F.param_attrs[0].@"noalias",
        };
        for (1..F.param_types.len) |i| {
            param_types[i] = F.param_types[i] orelse anyopaque;
            param_attrs[i] = .{
                .@"noalias" = F.param_attrs[i].@"noalias",
            };
        }
        return *const @Fn(
            &param_types,
            &param_attrs,
            F.return_type orelse void,
            .{
                .@"callconv" = F.attrs.@"callconv",
                .varargs = F.attrs.varargs,
            },
        );
    }
}

fn StackBuf(comptime N: usize) type {
    return comptime struct {
        buf: [N]u8 = undefined,

        pub fn init() @This() {
            return @This(){};
        }

        pub fn print(self: *const @This(), comptime fmt: []const u8, args: anytype) []const u8 {
            var this = @constCast(self);
            return std.fmt.bufPrint(&this.buf, fmt, args) catch @panic("FMT ERROR");
        }
    };
}

fn pointerType(any: anytype) enum {
    none,
    read,
    write,
} {
    return comptime blk: {
        const T = switch (@typeInfo(@TypeOf(any))) {
            else => @TypeOf(any),
            .type => any,
        };
        break :blk switch (@typeInfo(T)) {
            else => .none,
            .pointer => |p| if (p.attrs.@"const") .read else .write,
        };
    };
}

inline fn canCast(comptime V: type, comptime T: type) bool {
    return comptime Caster(V, T) != void;
}

test {
    _ = @import("test.zig");
}
