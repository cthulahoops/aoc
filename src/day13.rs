fn read_input(filename: &str) -> (i64, Vec<Repetition>) {
    let contents = std::fs::read_to_string(filename).unwrap();
    let mut lines = contents.lines();
    let departure_time = lines.next().unwrap().parse::<i64>().unwrap();

    let buses = lines
        .next()
        .unwrap()
        .split(",")
        .map(|x| x.parse::<i64>().ok())
        .enumerate()
        .filter_map(|(i, x)| match x {
            Some(x) => Some(Repetition {
                offset: i as i64,
                period: x,
            }),
            None => None,
        })
        .collect();

    (departure_time, buses)
}

fn earliest_bus(departure_time: i64, buses: &Vec<i64>) -> i64 {
    let mut wait_time = 0;

    loop {
        for bus in buses {
            if (departure_time + wait_time) % bus == 0 {
                println!("{} {} {}", departure_time, wait_time, bus);
                return bus * wait_time;
            }
        }
        wait_time += 1
    }
}

fn lcm(first: i64, second: i64) -> i64 {
    first * second / gcd(first, second)
}

fn min_max<T>(first: T, second: T) -> (T, T)
where
    T: PartialOrd,
{
    if first > second {
        (second, first)
    } else {
        (first, second)
    }
}

fn gcd(first: i64, second: i64) -> i64 {
    let (mut min, mut max) = min_max(first, second);

    loop {
        let res = max % min;
        if res == 0 {
            return min;
        }

        max = min;
        min = res;
    }
}

#[derive(Copy, Clone, Debug)]
struct Repetition {
    period: i64,
    offset: i64,
}

fn combine(a: Repetition, b: &Repetition) -> Repetition {
    println!("{:?}, {:?}", a, b);
    let period = lcm(a.period, b.period);

    let mut trial = a.offset;

    loop {
        trial += a.period;
        if (trial + b.offset) % b.period == 0 {
            println!("Collision at: {}", trial);
            let offset = trial % period;
            return Repetition { period, offset };
        }
    }
}

fn main() {
    let (departure_time, buses) = read_input("input/day13");

    let part1 = earliest_bus(departure_time, &buses.iter().map(|r| r.period).collect());
    println!("Part 1 = {}", part1);

    let result = buses.iter().fold(
        Repetition {
            offset: 0,
            period: 1,
        },
        combine,
    );

    println!("Part 2 = {}", result.offset);
}
