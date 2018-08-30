const std  = @import("std");
const warn = std.debug.warn;

// TODO(cgag): note we left out the optimization path

const Float_Info = struct {
    mantbits: u64,
    expbits:  u64,
    bias:     i64,
};

// const float32_info = Float_Info{.mantbits = 23, .expbits = 8,  .bias = -127};
const float64_info = Float_Info{.mantbits = 52, .expbits = 11, .bias = -1023};

const ParseFloat = error {
    NoDigits,
    EmptyString,
    MultipleDots,
    ContainedInvalidCharacters,
    Overflow,
};

pub const Decimal = struct {
    digits: [800]u8,
    num_digits: i64,
    decimal_point_index: i64,
    negative: bool,
    truncated: bool,

    pub fn init() Decimal {
        return Decimal {
            .digits = []u8{0} ** 800,
            .num_digits = 0,
            .decimal_point_index = 0,
            .negative = false,
            .truncated = false,
        };
    }

    pub fn set(self: *Decimal, s: []const u8) !void {
        var i: u32 = 0;
        self.negative = false;
        self.truncated = false;

        if (i >= s.len) {
            return ParseFloat.EmptyString;
        }

        switch (s[i]) {
            '+' => { i += 1; },
            '-' => { self.negative = true; i += 1; },
            else => {}
        }

        var saw_dot = false;
        var saw_digits = false;
        while (i < s.len) : (i += 1) {
            switch (s[i]) {
                '.' => {
                    if (saw_dot) {
                        return ParseFloat.MultipleDots;
                    }
                    saw_dot = true;
                    self.decimal_point_index = self.num_digits;
                    continue;
                },
                '0'...'9' => {
                    saw_digits = true;
                    if ((s[i] == '0') and (self.num_digits == 0)) {
                        // ignore leading zeros
                        self.decimal_point_index -= 1;
                        continue;
                    }
                    if (self.num_digits < @intCast(i64,self.digits.len)) {
                        self.digits[@intCast(usize, self.num_digits)] = s[i];
                        self.num_digits += 1;
                    } else if (s[i] != '0') {
                        self.truncated = true;
                    }
                    continue;
                },
                else => {
                    // TODO(cgag): should this just be in the 'else' case for the switch?
                    // I think it's this way in the go code because of the processing of scientific notation further manipulates
                    // the index 'i', and then checks to be sure i == s.len.  Because we're not supporting that we could just return
                    // an error here, but I want to stay as true to the go as possible to flesh this out later.
                    break;
                }
            }
        }

        if (!saw_digits) {
            return ParseFloat.NoDigits;
        }

        if (!saw_dot) {
            self.decimal_point_index = self.num_digits;
        }

        // TODO(cgag): here would be the exponential notation handling but i'm leaving it out for now
        // TODO(cgag): don't forget we left out special cases like NaN and inf.

        if (i != s.len) {
            return ParseFloat.ContainedInvalidCharacters;
        }
    }

    // decimal power of ten to binary power of two.
    const pow_tab = []i64{1, 3, 6, 9, 13, 16, 19, 23, 26};

    // TODO(cgag): shit, i think i've been putting u64 as a replacment for int when
    // it needs to be i64.

    // TODO(cgag): how do we communicate overflow?  as an error? return a struct?
    // TODO(cgag): just going to panic for now or something
    pub fn float_bits(self: *Decimal, info: *const Float_Info) ParseFloat!u64 {
        var exp: i64 = undefined;
        var mant: u64 = undefined;

        if (self.num_digits == 0) {
            mant = 0;
            exp = info.bias;
            // TODO(cgag): goto fuck me.  how do we model that?
            // just call it return assemble_bits everywhere and @inline it?
            return self.assemble_bits(info, mant, exp);
        }

        // obvious overflow/underflow
        // these bounds are for 64 bit floats, wkill have to change for 80 bit in the future
        if (self.decimal_point_index > 310) {
            return ParseFloat.Overflow;
        }
        if (self.decimal_point_index < -330) {
            mant = 0;
            exp = info.bias;
            return self.assemble_bits(info, mant, exp);
        }

        // scale by powers of two until in range [0.5, 1.0)
        exp = 0;
        while (self.decimal_point_index > 0) {
            var n: i64 = undefined;
            if (self.decimal_point_index >= @intCast(i64, pow_tab.len)) {
                n = 27;
            } else {
                n = pow_tab[@intCast(usize,self.decimal_point_index)];
            }
            self.shift(-n);
            exp += n;
        }

        while ((self.decimal_point_index < 0) or (self.digits[0] < '5')) {
            var n: i64 = 0;
            if (-self.decimal_point_index >= @intCast(i64, pow_tab.len)) {
                n = 27;
            } else {
                n = pow_tab[@intCast(usize,-self.decimal_point_index)];
            }
            self.shift(n);
            exp -= n;
        }

        // Our range is [0.5,1) but floating point range is [1,2).
        exp -= 1;

        // minimum representable exponent is info.bias + 1.
        // if the exponent is smaller, move it up and adjust self accordingly.
        if (exp < info.bias+1) {
            var n = info.bias + 1 - exp;
            self.shift(-n);
            exp += n;
        }

        // TODO(cgag): wrong precedence?
        if (exp-info.bias >= (i64(1)<<@intCast(u6, info.expbits)) - 1) {
            // TODO(cgag): handle for real
            warn("overflow!!!, exp - info.bias >= ...\n");
            return ParseFloat.Overflow;
        }

        self.shift(@intCast(i64, 1 + info.mantbits));
        mant = self.rounded_integer();

        if (mant == (@intCast(u64,2) << @intCast(u6,info.mantbits))) {
            mant >>= 1;
            exp += 1;
            if (exp - info.bias >= (i64(1) << @intCast(u6, info.expbits)) - 1) {
                return ParseFloat.Overflow;
            }
        }

        if ((mant & (u64(1)<<@intCast(u6, info.mantbits))) == 0) {
            exp = info.bias;
        }
        return self.assemble_bits(info, mant, exp);
    }

    fn rounded_integer(self: *Decimal) u64 {
        if (self.decimal_point_index > 20) {
            // TODO(cgag): is this how it works in zig?
            return 0xFFFFFFFFFFFFFFFF;
        }
        var i: i64 = 0;
        var n: u64 = 0;
        while ((i < self.decimal_point_index) and (i < self.num_digits)) : ( i += 1) {
            n = n*10 + self.digits[@intCast(usize,i)] - '0';
        }
        while (i < self.decimal_point_index) : ( i += 1 ) {
            n *= 10;
        }
        if (self.should_round_up(self.decimal_point_index)) {
            n += 1;
        }
        return n;
    }

    // if we chop at nd digits, should we round up?
    fn should_round_up(self: *Decimal, target_num_digits: i64) bool {
        if ((target_num_digits < 0) or (target_num_digits >= self.num_digits)) {
            return false;
        }
        if ((self.digits[@intCast(usize,target_num_digits)] == '5') and ((target_num_digits+1) == self.num_digits)) {
            if (self.truncated) {
                return true;
            }
            return (target_num_digits > 0) and ((self.digits[@intCast(usize,target_num_digits - 1)] - '0')%2 != 0);
        }
        // not halfway - digit tells all
        return self.digits[@intCast(usize,target_num_digits)] >= '5';
    }

    // TODO(cgag): just change Float_Info to be u6's instead of u64s or whatever
    fn assemble_bits(self: *Decimal, info: *const Float_Info, mant: u64, exp: i64) u64 {
        // TODO(cgag): is the bit fuckery precedence the same between go and zig
        // TODO(cgag): doesn't appear to be so at at all.
        var bits: u64 = mant & ((u64(1) << @intCast(u6, info.mantbits)) - @intCast(u64,1));
        bits |= @intCast(u64, (exp-info.bias) & (
            (i64(1) << @intCast(u6,info.expbits)) - 1
        )) << @intCast(u6, info.mantbits);
        if (self.negative) {
            // TODO(cgag): is this cast correct, go doesn't specify the type of 1
            bits |= u64(1) << @intCast(u6, info.mantbits) <<  @intCast(u6, info.expbits);
        }
        return bits;
    }



    // Maximum shift that we can do in one pass without overflow.
    // A uint has 32 or 64 bits, and we have to be able to accommodate 9<<k.
    // const uintSize = 32 << (^uint(0) >> 63)
    // const maxShift = uintSize - 4
    // TODO(cgag): i don't want to deal with this shit, just saying it's always 64
    const maxShift = 60;

    // binary shift left (k > 0) or right (k < 0)
    fn shift(self: *Decimal, n: i64) void {
        if (self.num_digits == 0) { return; }

        var k = n;
        if (k > 0) {
            while (k > maxShift) {
                self.left_shift(maxShift);
                k -= maxShift;
            }
            self.left_shift(@intCast(u64, k));
        } else if (k < 0) {
            while (k < -maxShift) {
                self.right_shift(maxShift);
                k += maxShift;
            }
            self.right_shift(@intCast(u64, -k));
        }
    }

    // binary shift right (/2) by k bits. k <= maxShift to avoid overflow
    fn right_shift(self: *Decimal, k: u64) void {

        var r: u64 = 0; // read pointer
        var w: u64 = 0; // write pointer

        // pick up enough leading digits to cover first shift
        var n: u64 = 0;
        while ((n >> @intCast(u6, k)) == 0) : (r += 1) {
            if (@intCast(i64,r) >= self.num_digits) {
                if (n == 0) {
                    // self == 0, shouldn't get here but we handle it anyway
                    self.num_digits = 0;
                    return;
                }
                while ((n >> @intCast(u6, k)) == 0) {
                    n = n * 10;
                    r += 1;
                }
                break;
            }
            var c = @intCast(u64, self.digits[r]);
            n = n*10 + c - '0';
        }
        self.decimal_point_index -= @intCast(i64,r) - 1;

        var mask: u64 = (@intCast(u64,1) << @intCast(u6, k)) - 1;

        while (@intCast(i64,r) < self.num_digits) : (r += 1) {
            var c = @intCast(u64, self.digits[r]);
            var dig = n >> @intCast(u6,k);
            n &= mask;
            self.digits[w] = @truncate(u8, dig + '0');
            w += 1;
            n = n*10 + c - '0';
        }

        // put down extra digits
        while (n > 0) {
            var dig = n >> @intCast(u6,k);
            n &= mask;
            if (w < self.digits.len) {
                self.digits[w] = @truncate(u8, dig + '0');
                w += 1;
            } else if (dig > 0) {
                self.truncated = true;
            }
            n = n * 10;
        }

        self.num_digits = @intCast(i64, w);
        self.trim();
    }

    fn trim(self: *Decimal) void {
        while ((self.num_digits > 0) and (self.digits[@intCast(usize, self.num_digits - 1)] == '0')) {
            self.num_digits -= 1;
        }
        if (self.num_digits == 0) {
            self.decimal_point_index = 0;
        }
    }

    fn left_shift(self: *Decimal, k: u64) void {
        var delta = leftcheats[k].delta;
        if (prefix_is_less_than(self.digits[0..@intCast(usize, self.num_digits)], leftcheats[k].cutoff)) {
            delta -= 1;
        }

        var r = self.num_digits;         // read index
        var w = self.num_digits + delta; // write index

        var n: u64 = 0;

        r = r - 1;
        while (r >= 0) : (r -= 1) {
            n += @intCast(u64, self.digits[@intCast(usize,r)] - '0') << @intCast(u6,k);
            var quo = n / 10;
            var rem = n - 10 * quo;
            w -= 1;
            if (w < @intCast(i64, self.digits.len)) {
                self.digits[@intCast(usize,w)] = @intCast(u8,rem + '0');
            } else if (rem != 0) {
                self.truncated = true;
            }
            n = quo;
        }

        while (n > 0) {
            var quo = n / 10;
            var rem = n - 10*quo;
            w -= 1;
            if (w < @intCast(i64, self.digits.len)) {
                self.digits[@intCast(usize,w)] = @intCast(u8,rem + '0');
            } else if (rem != 0) {
                self.truncated = true;
            }
            n = quo;
        }

        self.num_digits += delta;
        if (self.num_digits >= @intCast(i64,self.digits.len)) {
            // TODO(cgag): things like this seem like they might be
            // go-isms that are fucking me.  What is len() returning?
            self.num_digits = @intCast(i64,self.digits.len);
        }

        self.decimal_point_index += delta;
        self.trim();
        return;
    }

    pub fn print(self: *Decimal) void {
        warn("digits: {}\n", self.digits[0..12]);
        warn("num_digits: {}\n", self.num_digits);
        warn("decimal_point_index: {}\n",self.decimal_point_index );
        warn("negative: {}\n", self.negative);
        warn("truncated: {}\n", self.truncated);
        warn("---\n\n");
    }
};

fn prefix_is_less_than(b: []u8, s: []const u8) bool {
    var i: u64 = 0;
    while (i < s.len) : (i += 1) {
        if (i >= b.len) {
            return true;
        }
        if (b[i] != s[i]) {
            return b[i] < s[i];
        }
    }
    return false;
}

pub fn atof(s: []const u8) ParseFloat!f64 {
    var d = Decimal.init();
    try d.set(s);

    var bits = try d.float_bits(&float64_info);
    return @bitCast(f64, bits);
}


const leftCheat = struct {
    delta: i64,
    cutoff: []const u8,
};

const leftcheats = []leftCheat{
    // Leading digits of 1/2^i = 5^i.

    // 5^23 is not an exact 64-bit floating point number,

    // so have to use bc for the math.

    // Go up to 60 to be large enough for 32bit and 64bit platforms.

    // seq 60 | sed 's/^/5^/' | bc |
    // awk 'BEGIN{ print "\t{ 0, \"\" }," }
    // {
    // 	log2 = log(2)/log(10)
    // 	printf("\t{ %d, \"%s\" },\t// * %d\n",
    // 		int(log2*NR+1), $0, 2**NR)

    // }'
    leftCheat{.delta=0, .cutoff=""},
    leftCheat{.delta=1, .cutoff="5"},                                           // * 2
    leftCheat{.delta=1, .cutoff="25"},                                          // * 4
    leftCheat{.delta=1, .cutoff="125"},                                         // * 8
    leftCheat{.delta=2, .cutoff="625"},                                         // * 16
    leftCheat{.delta=2, .cutoff="3125"},                                        // * 32
    leftCheat{.delta=2, .cutoff="15625"},                                       // * 64
    leftCheat{.delta=3, .cutoff="78125"},                                       // * 128
    leftCheat{.delta=3, .cutoff="390625"},                                      // * 256
    leftCheat{.delta=3, .cutoff="1953125"},                                     // * 512
    leftCheat{.delta=4, .cutoff="9765625"},                                     // * 1024
    leftCheat{.delta=4, .cutoff="48828125"},                                    // * 2048
    leftCheat{.delta=4, .cutoff="244140625"},                                   // * 4096
    leftCheat{.delta=4, .cutoff="1220703125"},                                  // * 8192
    leftCheat{.delta=5, .cutoff="6103515625"},                                  // * 16384
    leftCheat{.delta=5, .cutoff="30517578125"},                                 // * 32768
    leftCheat{.delta=5, .cutoff="152587890625"},                                // * 65536
    leftCheat{.delta=6, .cutoff="762939453125"},                                // * 131072
    leftCheat{.delta=6, .cutoff="3814697265625"},                               // * 262144
    leftCheat{.delta=6, .cutoff="19073486328125"},                              // * 524288
    leftCheat{.delta=7, .cutoff="95367431640625"},                              // * 1048576
    leftCheat{.delta=7, .cutoff="476837158203125"},                             // * 2097152
    leftCheat{.delta=7, .cutoff="2384185791015625"},                            // * 4194304
    leftCheat{.delta=7, .cutoff="11920928955078125"},                           // * 8388608
    leftCheat{.delta=8, .cutoff="59604644775390625"},                           // * 16777216
    leftCheat{.delta=8, .cutoff="298023223876953125"},                          // * 33554432
    leftCheat{.delta=8, .cutoff="1490116119384765625"},                         // * 67108864
    leftCheat{.delta=9, .cutoff="7450580596923828125"},                         // * 134217728
    leftCheat{.delta=9, .cutoff="37252902984619140625"},                        // * 268435456
    leftCheat{.delta=9, .cutoff="186264514923095703125"},                       // * 536870912
    leftCheat{.delta=10, .cutoff="931322574615478515625"},                      // * 1073741824
    leftCheat{.delta=10, .cutoff="4656612873077392578125"},                     // * 2147483648
    leftCheat{.delta=10, .cutoff="23283064365386962890625"},                    // * 4294967296
    leftCheat{.delta=10, .cutoff="116415321826934814453125"},                   // * 8589934592
    leftCheat{.delta=11, .cutoff="582076609134674072265625"},                   // * 17179869184
    leftCheat{.delta=11, .cutoff="2910383045673370361328125"},                  // * 34359738368
    leftCheat{.delta=11, .cutoff="14551915228366851806640625"},                 // * 68719476736
    leftCheat{.delta=12, .cutoff="72759576141834259033203125"},                 // * 137438953472
    leftCheat{.delta=12, .cutoff="363797880709171295166015625"},                // * 274877906944
    leftCheat{.delta=12, .cutoff="1818989403545856475830078125"},               // * 549755813888
    leftCheat{.delta=13, .cutoff="9094947017729282379150390625"},               // * 1099511627776
    leftCheat{.delta=13, .cutoff="45474735088646411895751953125"},              // * 2199023255552
    leftCheat{.delta=13, .cutoff="227373675443232059478759765625"},             // * 4398046511104
    leftCheat{.delta=13, .cutoff="1136868377216160297393798828125"},            // * 8796093022208
    leftCheat{.delta=14, .cutoff="5684341886080801486968994140625"},            // * 17592186044416
    leftCheat{.delta=14, .cutoff="28421709430404007434844970703125"},           // * 35184372088832
    leftCheat{.delta=14, .cutoff="142108547152020037174224853515625"},          // * 70368744177664
    leftCheat{.delta=15, .cutoff="710542735760100185871124267578125"},          // * 140737488355328
    leftCheat{.delta=15, .cutoff="3552713678800500929355621337890625"},         // * 281474976710656
    leftCheat{.delta=15, .cutoff="17763568394002504646778106689453125"},        // * 562949953421312
    leftCheat{.delta=16, .cutoff="88817841970012523233890533447265625"},        // * 1125899906842624
    leftCheat{.delta=16, .cutoff="444089209850062616169452667236328125"},       // * 2251799813685248
    leftCheat{.delta=16, .cutoff="2220446049250313080847263336181640625"},      // * 4503599627370496
    leftCheat{.delta=16, .cutoff="11102230246251565404236316680908203125"},     // * 9007199254740992
    leftCheat{.delta=17, .cutoff="55511151231257827021181583404541015625"},     // * 18014398509481984
    leftCheat{.delta=17, .cutoff="277555756156289135105907917022705078125"},    // * 36028797018963968
    leftCheat{.delta=17, .cutoff="1387778780781445675529539585113525390625"},   // * 72057594037927936
    leftCheat{.delta=18, .cutoff="6938893903907228377647697925567626953125"},   // * 144115188075855872
    leftCheat{.delta=18, .cutoff="34694469519536141888238489627838134765625"},  // * 288230376151711744
    leftCheat{.delta=18, .cutoff="173472347597680709441192448139190673828125"}, // * 576460752303423488
    leftCheat{.delta=19, .cutoff="867361737988403547205962240695953369140625"}, // * 1152921504606846976
};
