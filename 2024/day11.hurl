let log10 = func(value) {
    try {
        hurl value < 10;
    } catch(true) {
        hurl 1;
    } catch(false) {
        try {
            log10(value / 10);
        } catch as x {
            hurl 1 + x;
        };
    };
};

let split_stone = func(value) {
    try {
        log10(value);
    } catch as digits {
        let divisor = 10 ^ (digits / 2);
        toss floor(value / 10 ^ (digits / 2));
        hurl value % divisor;
    };
};

let even_digits = func(value) {
    try {
        log10(value);
    } catch as digits {
        hurl digits % 2 == 0;
    };
};

let count_stones = func(value, steps) {
    try {
        hurl steps;
    } catch (0) {
        hurl 1;
    } catch as x {
        try {
            hurl value;
        } catch (0) {
            count_stones(1, steps - 1);
        } catch as y {
            try {
                even_digits(value);
            } catch (true) {
                let count = 0;
                try {
                    split_stone(value);
                } catch as part {
                    try {
                        count_stones(part, steps - 1);
                    } catch as subcount {
                        count = count + subcount;
                    };
                    return;
                };
                hurl count;
            } catch (false) {
                count_stones(value * 2024, steps - 1);
            };
        };
    };
};

let count = 0;
let steps = 25;

# let input = [125, 17];
# let input = [773, 79858, 0, 71, 213357, 2937, 1, 3998391];

try {
    count_stones(773, steps);
} catch as x {
    count = count + x;
};

try {
    count_stones(79858, steps);
} catch as x {
    count = count + x;
};

try {
    count_stones(0, steps);
} catch as x {
    count = count + x;
};

try {
    count_stones(71, steps);
} catch as x {
    count = count + x;
};

try {
    count_stones(213357, steps);
} catch as x {
    count = count + x;
};

try {
    count_stones(2937, steps);
} catch as x {
    count = count + x;
};

try {
    count_stones(1, steps);
} catch as x {
    count = count + x;
};

try {
    count_stones(3998391, steps);
} catch as x {
    count = count + x;
};

println("Part 1: ", count);
