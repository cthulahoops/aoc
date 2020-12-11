#![feature(unboxed_closures, fn_traits)]

use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;

fn read_sorted_numbers(filename: &str) -> Vec<i64> {
    let mut numbers: Vec<i64> = aoclib::read_numbers(filename).unwrap();
    numbers.sort();
    numbers
}

struct HashCache<A, B, R> {
    cache: HashMap<A, R>,
    func: fn(&mut HashCache<A, B, R>, B, A) -> R,
}

impl<A, B, R> HashCache<A, B, R>
where
    A: Eq + Hash,
{
    fn from_func(func: fn(&mut Self, B, A) -> R) -> Self {
        HashCache {
            cache: HashMap::new(),
            func: func,
        }
    }
}

impl<A, B, R> FnMut<(B, A)> for HashCache<A, B, R>
where
    A: Eq + Hash + Clone,
    R: Clone,
{
    extern "rust-call" fn call_mut(&mut self, args: (B, A)) -> Self::Output {
        let arg = args.1;

        if let Some(result) = self.cache.get(&arg).map(|x| x.clone()) {
            return result;
        }

        let result = (self.func.clone())(self, args.0, arg.clone());
        self.cache.insert(arg, result.clone());
        result
    }
}

// We need to implement `FnOnce` to implement `FnMut`.
impl<A, B, R> FnOnce<(B, A)> for HashCache<A, B, R>
where
    A: Eq + Hash + Clone,
    R: Clone,
{
    type Output = R;
    extern "rust-call" fn call_once(mut self, args: (B, A)) -> Self::Output {
        self.call_mut(args)
    }
}

fn arrangements<'a, F>(recurse: &mut F, adaptors: &'a HashSet<i64>, n: i64) -> i64
where
    F: FnMut(&'a HashSet<i64>, i64) -> i64,
{
    let mut count = 0;
    for x in 1..4 {
        if adaptors.contains(&(n - x)) {
            count += recurse(adaptors, n - x);
        }
    }

    if n <= 3 {
        count += 1;
    }

    count
}

fn main() {
    let numbers = read_sorted_numbers("input/day10");

    let mut last = 0;
    let mut ones = 0;
    let mut threes = 0;

    for number in &numbers {
        let gap = number - last;
        if gap == 3 {
            threes += 1;
        } else if gap == 1 {
            ones += 1;
        }
        last = number.clone();
    }

    threes += 1; // Yuck!

    println!(
        "Ones = {}\nThrees = {}\nPart 1 = {}",
        ones,
        threes,
        ones * threes
    );

    let target = last + 3;
    let adaptors: HashSet<i64> = numbers.iter().cloned().collect();

    let mut memoized = HashCache::from_func(arrangements);
    println!("Part 2 = {}\n", memoized(&adaptors, target));
}
