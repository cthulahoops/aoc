use std::collections::HashMap;
use std::str::FromStr;

#[derive(Debug)]
struct Map {
    orbits: HashMap<String, String>
}

impl FromStr for Map {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        let orbits = s.lines().map(parse_orbit).collect::<HashMap<String, String>>();
        Ok(Map{orbits})
    }
}

fn parse_orbit(s: &str) -> (String, String) {
    let parts = s.split(")").collect::<Vec<&str>>();

    (parts[1].to_string(), parts[0].to_string())
}



fn depth(body: &str, map: &HashMap<String, String>) -> usize {
    if body == "COM" {
        return 0;
    }
    return 1 + depth(map.get(body).unwrap(), map)
}

fn path(body: &str, map: &HashMap<String, String>) -> Vec<String> {
    let mut path = if body == "COM" {
        vec![]
    } else {
        path(map.get(body).unwrap(), map)
    };
    path.push(body.to_string());
    path
}

fn common_prefix_length<T>(v1: &Vec<T>, v2: &Vec<T>) -> usize where T: PartialEq {
    let mut i = 0;
    loop {
        if i >= v1.len() || i >= v2.len() || v1[i] != v2[i] {
            break;
        }
        i += 1;
    }
    i
}

impl Map {
    fn orbital_distance(&self, a: &str, b: &str) -> usize {
        let a_path = path(a, &self.orbits);
        let b_path = path(b, &self.orbits);

        let prefix = common_prefix_length(&a_path, &b_path);

        a_path.len() + b_path.len() - 2 * prefix - 2
    }
}

fn main() {
    let map = std::fs::read_to_string("input/day6").unwrap().parse::<Map>().unwrap();

    let mut count = 0;
    for (k, _v) in &map.orbits {
        count += depth(k, &map.orbits)
    }
    println!("Part 1 = {}", count);

    println!("Part 2 = {}", map.orbital_distance("YOU", "SAN"));
}

#[cfg(test)]
mod tests {
    use super::Map;

    #[test]
    fn test_transfers() {
        let map = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN\n".parse::<Map>().unwrap();
        assert_eq!(map.orbital_distance("YOU", "SAN"), 4);
    }
}
