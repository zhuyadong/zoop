const std = @import("std");
const StructField = std.builtin.Type.StructField;
const FieldEnum = std.meta.FieldEnum;
const DeclEnum = std.meta.DeclEnum;
const FieldType = std.meta.FieldType;
const compfmt = std.fmt.comptimePrint;
const assert = std.debug.assert;
const nameCast = std.enums.nameCast;
const zoop = @This();

pub const tuple = @import("tuple.zig");
pub const Method = tuple.Init;

pub const VtableFunc = *const fn (ifacename: []const u8) ?*IObject.Vtable;
pub const SuperPtrFunc = *const fn (rootptr: *anyopaque, typename: []const u8) ?*anyopaque;

/// The data used for type conversions
pub const TypeInfo = struct {
    typename: []const u8,

    /// return vtable by interface name
    getVtable: VtableFunc,
    /// return pointer to the parent class data by parent class name
    getSuperPtr: SuperPtrFunc,

    /// return pointer to T's TypeInfo
    pub fn of(comptime T: type) *const TypeInfo {
        return &(struct {
            pub const info = TypeInfo{ .typename = @typeName(T), .getVtable = getVtableFunc(T), .getSuperPtr = getSuperPtrFunc(T) };
        }).info;
    }
};

/// The data for type conversions of object that created by new() / make()
pub const MetaInfo = packed struct {
    /// pointer to object created by new() / make()
    rootptr: ?*anyopaque = null,
    /// TypeInfo of object that rootptr pointer to
    typeinfo: ?*const TypeInfo = null,

    /// static cast rootptr to T, T can be Class or Interface
    pub fn cast(self: *MetaInfo, comptime T: type) t: {
        break :t if (isInterface(T)) ?T else ?*T;
    } {
        assert(self.rootptr != null and self.typeinfo != null);
        if (isInterface(T)) {
            assert(self.typeinfo != null);
            if (self.typeinfo.?.getVtable(@typeName(T))) |vptr| {
                return makeFatPtr(T, self.rootptr.?, vptr);
            }
        } else {
            if (self.typeinfo.?.getSuperPtr(self.rootptr.?, mixinName(T))) |ptr| {
                return @ptrCast(@alignCast(ptr));
            }
        }

        return null;
    }

    pub fn typename(self: *MetaInfo) []const u8 {
        assert(self.typeinfo != null);
        return self.typeinfo.?.typename;
    }

    pub fn init(rootptr: anytype) MetaInfo {
        const T = @TypeOf(rootptr.*);
        return .{ .rootptr = @ptrCast(rootptr), .typeinfo = TypeInfo.of(T) };
    }
};

/// The base interface from which all interfaces are automatically inherited
pub const IObject = struct {
    pub const Vtable = struct {
        __meta__: *const fn (*anyopaque) *MetaInfo,
        destroy: *const fn (*anyopaque) void,
    };
    pub usingnamespace CoreApi(@This());

    ptr: *anyopaque,
    vptr: *Vtable,
};

/// A fatptr that is used to simply store the pointer value of an interface,
pub const IRaw = struct {
    ptr: *anyopaque,
    vptr: *anyopaque,

    pub fn cast(self: IRaw, comptime I: type) I {
        if (!isInterface(I)) @compileError("IRaw.cast(I) where I must be interface type.");
        return I{ .ptr = @ptrCast(self.ptr), .vptr = @ptrCast(@alignCast(self.vptr)) };
    }
};

/// Return a struct contained all function pointer deinitions from APIs and from all I's super interfaces's Vtable.
/// APIs is a struct that contains only function pointer definitions.
pub fn DefVtable(comptime I: type, comptime APIs: type) type {
    const supers = Interfaces(I).value;
    const iobjfields = directFields(IObject);
    const newfields = directFields(APIs);

    comptime var nfield: comptime_int = iobjfields.len + newfields.len;
    for (supers) |iface| {
        nfield += directFields(iface).len;
    }

    comptime var allfields: [nfield]StructField = undefined;
    comptime var count: usize = 0;
    for (iobjfields, count..) |f, i| {
        allfields[i] = f;
        count += 1;
    }
    for (supers) |super| {
        const fields = filterFields(directFields(super), allfields[0..count]);
        for (fields, count..) |f, i| {
            allfields[i] = f;
            count += 1;
        }
    }
    for (newfields, count..) |f, i| {
        allfields[i] = f;
        count += 1;
    }
    return @Type(.{ .Struct = .{
        .layout = .auto,
        .fields = allfields[0..count],
        .decls = &.{},
        .is_tuple = false,
    } });
}

/// Return a struct whose api field is a struct that contains all ApiMethods from I and I's super interfaces
pub fn Api(comptime I: type) type {
    checkInterfaceExtends(I);

    comptime var ret = struct {
        pub const api = CoreApi(I);
    };

    ret = ConcatApi(I, I, ret);
    const supers = Interfaces(I).value;
    for (supers) |super| {
        if (@hasDecl(super, "Api")) {
            ret = ConcatApi(I, super, ret);
        }
    }

    return ret.api;
}

/// Returns a struct containing all the methods returned by calling the Fn() function of T and T's parent class
pub fn Fn(comptime T: type) type {
    comptime var ret = tuple.Init(if (@hasDecl(T, "Fn")) T.Fn(T) else struct {});
    const mixins = SuperClasses(T);
    for (mixins.value) |m| {
        if (@hasDecl(m, "Fn")) {
            const SuperMethods = m.Fn(T);
            ret = MergeMethods(SuperMethods, ret);
        }
    }

    ret = MergeMethods(CoreFn(T), ret);
    return JoinDecls(ret);
}

/// Returns a tuple contained all interfaces exclude IObject implemented by T
pub fn Interfaces(comptime T: type) type {
    comptime var ifaces = tuple.Init(.{});

    if (@hasDecl(T, "extends")) {
        inline for (T.extends) |super| {
            if (isInterface(super)) {
                ifaces = tuple.AppendUnique(ifaces, super);
            }
            ifaces = tuple.AppendUnique(ifaces, Interfaces(super));
        }
    }

    return ifaces;
}

/// Returns Mixin type for T
pub fn Mixin(comptime T: type) type {
    return struct {
        const Self = @This();

        deallocator: ?std.mem.Allocator = null,
        meta: ?MetaInfo = null,
        data: MixinData(T) = .{},

        fn init(self: *const Self, castinfo: ?MetaInfo) void {
            var pself = @constCast(self);
            const tptr: *T = @alignCast(@fieldParentPtr("mixin", pself));
            const isroot = castinfo == null;
            pself.meta = if (isroot) MetaInfo.init(tptr) else castinfo.?;
            inline for (comptime DirectSuperClasses(T).value) |m| {
                var inner = &@field(pself.data, mixinName(m));
                inner.mixin.init(pself.meta);
            }
        }

        pub fn cast(self: *const Self, comptime TCast: type) t: {
            break :t if (isInterface(TCast)) ?TCast else ?*TCast;
        } {
            var pself = @constCast(self);
            if (Self == TCast) return pself;
            return pself.meta.?.cast(TCast);
        }

        pub fn format(
            self: *const Self,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            var buf: [512]u8 = undefined;
            const msg = try std.fmt.bufPrint(&buf, "{s}@{x}{{{any}}} ", .{ @typeName(Self), @intFromPtr(self), self.meta });
            try writer.writeAll(msg);
        }
    };
}

/// Returns a tuple contained all direct and indirect super classes of T
pub fn SuperClasses(comptime Self: type) type {
    comptime var supers = DirectSuperClasses(Self);

    for (supers.value) |m| {
        supers = tuple.AppendUnique(supers, SuperClasses(m));
    }

    return supers;
}

/// Returns a tuple contained all direct super classes of T
pub fn DirectSuperClasses(comptime Self: type) type {
    comptime var ret = tuple.Init(.{});
    if (@hasDecl(Self, "extends")) {
        inline for (Self.extends) |super| {
            if (!isInterface(super)) {
                ret = tuple.Append(ret, super);
            }
        }
    }

    return ret;
}

/// Returns a tuple containing all nodes in the inheritance tree from T to Super, excluding T itself
pub fn SuperRoute(comptime T: type, comptime Super: type) type {
    return (struct {
        pub const ret = blk: {
            const route = FindRoute(tuple.Init(.{}), T, Super);
            if (route == void) @compileError(compfmt("no route: {} -> {}", .{ T, Super }));
            break :blk route;
        };
    }).ret;
}

pub const HookFunc = *const fn (obj: IObject) void;
var destroy_hook_func: ?HookFunc = null;
var new_hook_func: ?HookFunc = null;

/// Set up hooks to monitor the creation and destruction of objects on the heap
pub fn setHook(new_hook: ?HookFunc, destroy_hook: ?HookFunc) void {
    new_hook_func = new_hook;
    destroy_hook_func = destroy_hook;
}

/// Returns TypeInfo of interface or object
pub fn typeInfo(any: anytype) *const TypeInfo {
    const T = @TypeOf(any);
    if (T == type) {
        return TypeInfo.of(any);
    } else {
        return metaInfo(any).typeinfo.?;
    }
}

/// Returns MetaInfo of interface or object
pub fn metaInfo(any: anytype) *MetaInfo {
    if (isInterface(@TypeOf(any))) {
        return any.vptr.__meta__(any.ptr);
    }
    if (@typeInfo(@TypeOf(any)) == .Pointer) {
        if (@hasField(std.meta.Child(@TypeOf(any)), "mixin")) {
            return &@constCast(any).mixin.meta.?;
        }
        @compileError("zoop.meta(self): self.* is not a zoop class.");
    }
    @compileError("zoop.meta(self): self must be interface or pointer");
}

/// Check whether the object to which ptr points to is the object that rootptr points to
pub inline fn isRootPtr(ptr: anytype) bool {
    const T = @TypeOf(ptr);
    if (@typeInfo(T) == .Pointer) {
        if (isClass(std.meta.Child(T))) {
            return @intFromPtr(metaInfo(ptr).rootptr) == @intFromPtr(ptr);
        }
    }
    @compileError("zoop.isRoot(ptr): ptr must be zoop object pointer");
}

/// Check whether T is interface type
pub inline fn isInterface(comptime T: type) bool {
    return comptime @typeInfo(T) == .Struct and
        std.meta.fieldNames(T).len == 2 and
        @hasField(T, "ptr") and
        @hasField(T, "vptr") and
        @hasDecl(T, "Vtable") and
        std.meta.FieldType(T, .ptr) == *anyopaque and
        (std.meta.FieldType(T, .vptr) == *const T.Vtable or
        std.meta.FieldType(T, .vptr) == *T.Vtable);
}

/// Returns a [:0]const u8 containing all the '.' in the @typeName(T) Replace with '_'
pub fn mixinName0(comptime T: type) [:0]const u8 {
    const typename = @typeName(T);
    return &(struct {
        pub const ret: [typename.len + 1:0]u8 = blk: {
            var name: [typename.len + 1:0]u8 = undefined;
            var idx: u32 = 0;
            for (0..typename.len) |i| {
                const char = typename[i];
                switch (char) {
                    '.', '?', '*', '(', ')', ' ' => name[idx] = '_',
                    else => name[idx] = char,
                }
                idx += 1;
            }
            name[typename.len] = 0;
            break :blk name;
        };
    }).ret;
}

/// Returns a []const u8 containing all the '.' in the @typeName(T) Replace with '_'
pub fn mixinName(comptime T: type) []const u8 {
    const name = mixinName0(T);
    return name[0 .. name.len - 1];
}

/// Check whether T is class type
pub inline fn isClass(comptime T: type) bool {
    return @typeInfo(T) == .Struct and
        @hasField(T, "mixin") and
        std.meta.FieldType(T, .mixin) == Mixin(T);
}

/// The deinit() function is called in order from the child class to the parent class,
/// freeing up memory if the object is on the heap
fn destroyObj(ptr: anytype) void {
    assert(isRootPtr(ptr));
    const T = std.meta.Child(@TypeOf(ptr));
    const supers = SuperClasses(T);
    const deallocator = ptr.mixin.deallocator;

    if (destroy_hook_func) |func| {
        if (deallocator != null) {
            func(ptr.as(IObject).?);
        }
    }

    if (std.meta.hasMethod(T, "deinit")) {
        ptr.deinit();
    }

    if (tuple.len(supers) > 0) {
        inline for (supers.value) |Super| {
            var pmixin = superPtr(ptr, Super);
            const TMixin = std.meta.Child(@TypeOf(pmixin));
            if (std.meta.hasMethod(TMixin, "deinit")) {
                pmixin.deinit();
            }
        }
    }

    if (deallocator) |ally| {
        ally.destroy(ptr);
    }
}

/// Returns a tuple that contains the basic methods of all classes
fn CoreFn(comptime Class: type) type {
    return Method(.{
        struct {
            pub fn as(self: *const Class, comptime T: type) t: {
                break :t if (isInterface(T)) ?T else ?*T;
            } {
                if (self.mixin.meta == null) @panic("obj create by make() must call obj.initMixin() before use it.");
                if (T == Class) return @constCast(self);
                if (isInterface(T) or FindRoute(tuple.Init(.{}), Class, T) != void) {
                    if (isRootPtr(self)) {
                        return self.cast(T);
                    }
                }
                return self.mixin.cast(T);
            }
        },
        struct {
            pub fn asptr(self: *const Class) *anyopaque {
                if (self.mixin.meta == null) @panic("obj create by make() must call obj.initMixin() before use it.");
                return metaInfo(self).rootptr.?;
            }
        },
        struct {
            pub fn cast(self: *const Class, comptime T: type) t: {
                break :t if (isInterface(T)) T else *T;
            } {
                if (self.mixin.meta == null) @panic("obj create by make() must call obj.initMixin() before use it.");
                const pself = @constCast(self);
                if (T == Class) return pself;
                if (isInterface(T)) {
                    if (!isRootPtr(pself)) {
                        @panic("only root object can cast() to interface, use as() instead cast().");
                    }
                    return makeFatPtr(T, pself, makeVtable(Class, RealVtable(T)));
                } else if (isClass(T)) {
                    return @ptrFromInt(@intFromPtr(pself) + ptrOffset(Class, SuperRoute(Class, T)));
                } else {
                    @compileError(@typeName(Class) ++ " can not cast to " ++ @typeName(T));
                }
            }
        },
        struct {
            pub fn new(ally: std.mem.Allocator) !*Class {
                comptime {
                    const abs_methods = getAbstractMethods(Class);
                    if (abs_methods.idx > 0) {
                        @compileError(compfmt("{s} must implement these methods:\n{}", .{ @typeName(Class), abs_methods }));
                    }
                }
                var self: *Class = try ally.create(Class);
                self.* = makeStruct(Class);
                self.initMixin();
                self.mixin.deallocator = ally;
                if (new_hook_func) |func| {
                    func(self.cast(IObject));
                }
                return self;
            }
        },
        struct {
            pub fn make() Class {
                comptime {
                    const abs_methods = getAbstractMethods(Class);
                    if (abs_methods.idx > 0) {
                        @compileError(compfmt("{s} must implement these methods:\n{}", .{ @typeName(Class), abs_methods }));
                    }
                }
                const self = makeStruct(Class);
                return self;
            }
        },
        struct {
            pub fn initMixin(self: *Class) void {
                if (self.mixin.meta == null) {
                    self.mixin.init(null);
                }
            }
        },
        struct {
            pub fn __meta__(self: *const Class) *MetaInfo {
                if (self.mixin.meta == null) @panic("obj create by make() must call obj.initMixin() before use it.");
                var pself = @constCast(self);
                return &pself.mixin.meta.?;
            }
        },
        struct {
            pub fn destroy(self: *Class) void {
                if (self.mixin.meta == null) @panic("obj create by make() must call obj.initMixin() before use it.");
                destroyObj(self);
            }
        },
        struct {
            pub fn format(
                self: *const Class,
                comptime _: []const u8,
                _: std.fmt.FormatOptions,
                writer: anytype,
            ) !void {
                if (self.mixin.meta == null) @panic("obj create by make() must call obj.initMixin() before use it.");
                var buf: [512]u8 = undefined;
                const msg = try std.fmt.bufPrint(&buf, "{X}:{s}", .{ @intFromPtr(self), @typeName(Class) });
                try writer.writeAll(msg);
            }
        },
    });
}

/// Returns a struct that contains the basic ApiMethods of all interfaces
fn CoreApi(comptime I: type) type {
    return struct {
        pub fn __meta__(self: I) *MetaInfo {
            return self.vptr.__meta__(self.ptr);
        }

        pub fn as(self: I, comptime T: type) t: {
            break :t if (isInterface(T)) ?T else ?*T;
        } {
            return metaInfo(self).cast(T);
        }

        pub fn asraw(self: I) IRaw {
            return IRaw{ .ptr = @ptrCast(self.ptr), .vptr = @ptrCast(self.vptr) };
        }

        pub fn asptr(self: I) *anyopaque {
            return self.ptr;
        }

        pub fn eql(self: I, other: I) bool {
            return self.ptr == other.ptr and self.vptr == other.vptr;
        }

        pub fn destroy(self: I) void {
            self.vptr.destroy(self.ptr);
        }

        pub fn format(
            self: I,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            var buf: [512]u8 = undefined;
            const msg = try std.fmt.bufPrint(&buf, "{X}:{s}{{{s} vptr@{X}}}", .{ @intFromPtr(self.ptr), @typeName(I), metaInfo(self).typename(), @intFromPtr(self.vptr) });
            try writer.writeAll(msg);
        }
    };
}

/// Returns a tuple containing all the methods in ChildMethods and any methods in SuperMethods that are not in ChildMethods
fn MergeMethods(comptime SuperMethods: type, comptime ChildMethods: type) type {
    const child_decls = JoinDecls(ChildMethods);
    comptime var ret = ChildMethods;
    for (SuperMethods.value) |m| {
        const decls = std.meta.declarations(m);
        if (decls.len > 0 and !@hasDecl(child_decls, decls[0].name)) {
            ret = tuple.Append(ret, m);
        }
    }
    return ret;
}

/// Returns a struct whose api field is a struct that contains all the methods in Tuple.api and all methods in the struct returned by Super.Api(I).
fn ConcatApi(comptime I: type, comptime Super: type, comptime Tuple: type) type {
    if (!@hasDecl(Super, "Api")) @compileError(compfmt("'{s}.Api not defined.'", .{@typeName(Super)}));

    return struct {
        pub const api = struct {
            pub usingnamespace Tuple.api;
            pub usingnamespace Super.Api(I);
        };
    };
}

fn RealVtable(comptime T: type) type {
    return switch (@hasDecl(T, "Vtable")) {
        true => T.Vtable,
        false => T,
    };
}

fn directFields(comptime T: type) []const StructField {
    if (T == void) return &.{};
    return (@typeInfo(RealVtable(T))).Struct.fields;
}

fn fieldExisted(comptime field: StructField, comptime fields: []const StructField) bool {
    for (fields) |f| {
        if (std.mem.eql(u8, field.name, f.name)) return true;
    }
    return false;
}

fn filterFields(comptime in: []const StructField, comptime exclude: []const StructField) []const StructField {
    comptime var ret: [in.len]StructField = undefined;
    var cur: comptime_int = 0;
    for (in) |f| {
        if (!fieldExisted(f, exclude)) {
            ret[cur] = f;
            cur += 1;
        }
    }

    return ret[0..cur];
}

/// Returns a VT where the field is assigned with the address of the function with the same name in T,
/// and if there is no function with the same name in T, the value is undefined
fn makeVtable(comptime T: type, comptime VT: type) *VT {
    return @constCast(&(struct {
        pub const vt: VT = blk: {
            var val: VT = undefined;

            for (std.meta.fields(VT)) |field| {
                if (@hasDecl(T, field.name)) {
                    checkApi(T, VT, field.name);
                    @field(val, field.name) = @ptrCast(&@field(T, field.name));
                } else if (field.default_value) |def_ptr| {
                    const defval = @as(*align(1) const field.type, @ptrCast(def_ptr)).*;
                    @field(val, field.name) = defval;
                } else {
                    @field(val, field.name) = undefined;
                }
            }

            break :blk val;
        };
    }).vt);
}

fn ApiInfoList(comptime len: usize) type {
    return struct {
        const Self = @This();

        items: [len]struct {
            iface: type,
            name: []const u8,
        } = undefined,

        idx: usize = 0,

        pub fn append(self: *Self, name: []const u8, iface: type) void {
            self.items[self.idx] = .{ .name = name, .iface = iface };
            self.idx += 1;
        }

        pub fn format(
            self: *const Self,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            for (self.items[0..self.idx]) |item| {
                try writer.writeAll(compfmt("> {s}.{s} : {}\n", .{ @typeName(item.iface), item.name, @TypeOf(@field(item.iface, item.name)) }));
            }
        }
    };
}

/// Returns all interface methods that are not implemented in T
fn getAbstractMethods(comptime T: type) t: {
    var nmethods: usize = 0;
    for (Interfaces(T).value) |iface| {
        nmethods += std.meta.fieldNames(iface.Vtable).len;
    }
    break :t ApiInfoList(nmethods);
} {
    return blk: {
        const ifaces = Interfaces(T).value;
        comptime var nmethods: usize = 0;
        inline for (ifaces) |iface| {
            nmethods += std.meta.fieldNames(iface.Vtable).len;
        }

        comptime var apilist: ApiInfoList(nmethods) = .{};

        inline for (ifaces) |iface| {
            inline for (std.meta.fields(iface.Vtable)) |field| {
                if (!@hasDecl(T, field.name) and field.default_value == null) {
                    apilist.append(field.name, iface);
                }
            }
        }
        const ret = apilist;
        break :blk ret;
    };
}

/// Check whether the type of the function named field in T and the pointer of the function with the same name in VT match
fn checkApi(comptime T: type, comptime VT: type, comptime field: []const u8) void {
    if (@hasField(IObject.Vtable, field)) return;
    const vtinfo = @typeInfo(@typeInfo(FieldType(VT, std.enums.nameCast(FieldEnum(VT), field))).Pointer.child);
    const tinfo = @typeInfo(@TypeOf(@field(T, field)));
    if (vtinfo.Fn.return_type.? != tinfo.Fn.return_type.?) @compileError(compfmt("{s}.{s} and {s}.{s} must both return '{}'.", .{ @typeName(T), field, @typeName(VT), field, vtinfo.Fn.return_type.? }));
    if (vtinfo.Fn.params.len != tinfo.Fn.params.len) @compileError(compfmt("{s}.{s} and {s}.{s} number of parameters must be the same.", .{ @typeName(T), field, @typeName(VT), field }));
    if (vtinfo.Fn.params.len < 2) return;
    for (vtinfo.Fn.params[1..], tinfo.Fn.params[1..], 1..) |vt, t, i| {
        const idx = i;
        if (vt.type.? != t.type.?) {
            @compileError(compfmt("The {d}th parameter of {s}.{s} must be '{}'.", .{ idx + 1, @typeName(T), field, vt.type.? }));
        }
    }
}

/// Check that all types in I.extends are interface types
fn checkInterfaceExtends(comptime I: type) void {
    if (@hasDecl(I, "extends")) {
        inline for (I.extends) |iface| {
            if (!isInterface(iface)) @compileError(compfmt("{s}.extends error: {s} is not an interface.", .{ @typeName(I), @typeName(iface) }));
        }
    }
}

/// Returns a T in which all fields with default values are assigned default values
fn defaultStruct(comptime T: type) T {
    comptime {
        var data: T = undefined;
        for (std.meta.fields(T)) |field| {
            const ptr: ?*const field.type = @ptrCast(@alignCast(field.default_value));
            if (ptr) |p| {
                @field(data, field.name) = p.*;
            }
        }
        return data;
    }
}

/// Returns a T that initializes all .mixin along the inheritance chain
fn makeStruct(comptime T: type) T {
    var data: T = undefined;
    inline for (std.meta.fields(T)) |field| {
        const ptr: ?*const field.type = @ptrCast(@alignCast(field.default_value));
        if (field.type == Mixin(T)) {
            @field(data, field.name) = makeStruct(Mixin(T));
        } else if (ptr) |p| {
            @field(data, field.name) = p.*;
        }
    }
    return data;
}

/// Generate defaults for StructField.default_value
fn default(comptime T: type) *const anyopaque {
    const Dummy = switch (@typeInfo(T)) {
        .Optional => struct {
            val: T = null,
        },
        .Pointer => struct {
            val: T = undefined,
        },
        .Bool => struct {
            val: T = false,
        },
        else => struct {
            val: T = defaultStruct(T),
        },
    };
    return @ptrCast(&(struct {
        pub const data: Dummy = .{};
    }).data.val);
}

/// Returns MixinData type for T
fn MixinData(comptime T: type) type {
    const direct_supers = DirectSuperClasses(T).value;
    comptime var allfields: [direct_supers.len]StructField = undefined;
    comptime var idx = 0;
    for (direct_supers) |super| {
        allfields[idx] = .{
            .name = mixinName0(super),
            .type = super,
            .default_value = default(super),
            .is_comptime = false,
            .alignment = @alignOf(super),
        };
        idx += 1;
    }

    return @Type(.{ .Struct = .{
        .layout = .auto,
        .fields = allfields[0..],
        .decls = &.{},
        .is_tuple = false,
    } });
}

/// Look for the path from T to Super on the class inheritance tree.
/// Successfully returns a tuple containing the path, otherwise returns void.
fn FindRoute(comptime inroute: type, comptime T: type, comptime Super: type) type {
    const supers = DirectSuperClasses(T).value;
    for (supers) |m| {
        if (m == Super) return tuple.Append(inroute, m);
        const ret = FindRoute(tuple.Append(inroute, m), m, Super);
        if (ret != void) return ret;
    }

    return void;
}

/// Merge all struct methods in struct_tuple into a single struct
fn JoinDecls(comptime struct_tuple: anytype) type {
    const input = tuple.Init(struct_tuple);
    comptime var ret = struct {};
    inline for (input.value) |t| {
        ret = MergeDecls(ret, t);
    }
    return ret;
}

/// Merge methods of T and V into a single struct
fn MergeDecls(comptime T: type, comptime V: type) type {
    return struct {
        pub usingnamespace T;
        pub usingnamespace V;
    };
}

/// Returns a pointer to the parent class Super in the obj
fn superPtr(obj: anytype, comptime Super: type) t: {
    break :t if (Super == @TypeOf(obj.*)) @TypeOf(obj) else *Super;
} {
    const T = @TypeOf(obj.*);
    if (Super == T) {
        return obj;
    } else {
        return @ptrFromInt(@intFromPtr(obj) + ptrOffset(T, SuperRoute(T, Super)));
    }
}

/// Returns the offset of the last class in the route_tuple in the T object
inline fn ptrOffset(comptime T: type, comptime route_tuple: anytype) usize {
    comptime {
        var CurType: type = T;
        var curptr: *allowzero anyopaque = @ptrFromInt(0);
        for (route_tuple.value) |f| {
            var p: *allowzero CurType = @ptrCast(@alignCast(curptr));
            const nextPtr = &@field(&p.mixin.data, mixinName(f));
            CurType = @TypeOf(nextPtr.*);
            curptr = nextPtr;
        }
        const ret = @intFromPtr(curptr);
        return ret;
    }
}

/// Returns the function for T that is used to get Vtable by interface type name.
fn getVtableFunc(comptime T: type) VtableFunc {
    const ifaces = tuple.Append(.{IObject}, Interfaces(T)).value;
    const kvs = comptime build_kvs: {
        const KV = struct { []const u8, *IObject.Vtable };
        var kvs_array: [ifaces.len]KV = undefined;
        for (ifaces, 0..) |iface, i| {
            kvs_array[i] = .{ @typeName(iface), @ptrCast(makeVtable(T, RealVtable(iface))) };
        }
        break :build_kvs kvs_array[0..];
    };
    const map = std.StaticStringMap(*IObject.Vtable).initComptime(kvs);
    return &(struct {
        pub fn func(ifacename: []const u8) ?*IObject.Vtable {
            return map.get(ifacename);
        }
    }).func;
}

/// Returns the function for T that is used to get super pointer by super type name.
fn getSuperPtrFunc(comptime T: type) SuperPtrFunc {
    const all_supers = SuperClasses(T);
    const kvs = comptime build_kvs: {
        const KV = struct { []const u8, usize };
        var kvs_array: [tuple.len(all_supers) + 1]KV = undefined;
        kvs_array[0] = .{ mixinName(T), @as(usize, 0) };
        for (all_supers.value, 1..) |super, i| {
            kvs_array[i] = .{ mixinName(super), ptrOffset(T, SuperRoute(T, super)) };
        }
        break :build_kvs kvs_array[0..];
    };
    const map = std.StaticStringMap(usize).initComptime(kvs);
    return (struct {
        pub fn func(self: *anyopaque, mixinname: []const u8) ?*anyopaque {
            if (map.get(mixinname)) |offset| {
                return @ptrFromInt(@intFromPtr(self) + offset);
            }

            return null;
        }
    }).func;
}

fn makeFatPtr(comptime I: type, ptr: *anyopaque, vptr: *const anyopaque) I {
    if (isInterface(I)) {
        return I{ .ptr = ptr, .vptr = @ptrCast(@alignCast(@constCast(vptr))) };
    }
    @compileError(@typeName(I) ++ " is not fatptr type.");
}

test "zoop" {
    _ = @import("tuple.zig");
    _ = @import("test.zig");
}
