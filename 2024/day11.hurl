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

let map = func(f, elements) {
    let inner = func(n) {
        try {
            hurl n > len(elements);
        } catch(true) {
            true;
        } catch(false) {
            try {
                f(at(elements, n));
            } catch as x {
                try {
                    hurl n == len(elements);
                } catch(true) {
                    hurl x;
                } catch(false) {
                    toss x;
                };
                return;
            };
            inner(n + 1);
        };
    };

    inner(1);
};

let sum = func(f) {
    let total = 0;
    try {
        f();
    } catch as x {
        total = total + x;
        return;
    };
    hurl total;
};

# let input = [125, 17];
let input = [773, 79858, 0, 71, 213357, 2937, 1, 3998391];
let steps = 25;

try {
    sum(func() {
        let f = func(x) {
            count_stones(x, steps);
        };
        map(f, input);
    });
} catch as part1 {
    println("Part 1: ", part1);
};
