
const DecimalError = error {
    NoDigits,
    EmptyString,
    MultipleDots,
    ContainedInvalidCharacters,
};

const Decimal = struct {
    digits: [800]u8,
    num_digits: u32,
    decimal_point_index: u32,
    negative: bool,
    truncted: bool,

    pub fn set(self: *Decimal) !void {
        var i: u32 = 0;
        self.negative = false;
        self.truncated = false;

        if (i >= s.len) {
            return DecimalError.EmptyString;
        }

        switch (s[i]) {
            '+' => { i += 1; },
            '-' => { self.negative = true; i += 1; },
        }

        var saw_dot = false;
        var saw_digits = false;
        while (i < s.len) : (i += 1) {
            switch (s[i]) {
                '.' => {
                    if (saw_dot) {
                        return DecimalError.MultipleDots;
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
                    if (self.num_digits < self.digits.len) {
                        self.digits[self.num_digits] = s[i];
                        self.num_digits += 1;
                    } else if (s[i] != '0') {
                        self.truncated = true;
                    }
                    continue;
                },
            }
            // TODO(cgag): should this just be in the 'else' case for the switch?
            break;
        }

        if (!saw_digits) {
            return DecimalError.NoDigits;
        }

        if (!saw_dot) {
            self.decimal_point_index = self.num_digits;
        }

        // here would be the exponential notation handling but i'm leaving it out for now

        if (i != s.len) {
            return DecimalError.ContainedInvalidCharacters;
        }
    }
};

pub fn atof(s: []const u8) DecimalError!f64 {
    if (false) {
        return DecimalError.NoDigits;
    }
    return 1.0;
}
