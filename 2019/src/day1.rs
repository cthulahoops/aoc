fn fuel_reqs1(mass: i64) -> i64 {
    mass / 3 - 2
}

fn fuel_reqs2(mass: i64) -> i64 {
    let fuel = fuel_reqs1(mass);
    if fuel < 0 {
        return 0;
    }
    return fuel + fuel_reqs2(fuel);
}

fn main() {
    let masses: Vec<i64> = std::fs::read_to_string("input/day1")
        .unwrap()
        .lines()
        .map(|x| x.parse::<i64>().unwrap())
        .collect();
    let total_fuel: i64 = masses.iter().cloned().map(fuel_reqs1).sum();
    println!("Part 1 = {}", total_fuel);
    let total_fuel: i64 = masses.iter().cloned().map(fuel_reqs2).sum();
    println!("Part 2 = {}", total_fuel);
}
