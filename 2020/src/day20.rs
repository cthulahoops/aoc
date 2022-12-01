use lazy_static::lazy_static;
use regex::Regex;
use std::collections::{HashMap, HashSet};

lazy_static! {
    static ref TILE_HEADER: Regex = Regex::new(r"^Tile (\d+):").unwrap();
    static ref SEAHEAD: Regex = Regex::new(r"..................#.").unwrap();
    static ref SEABODY: Regex = Regex::new(r"#....##....##....###").unwrap();
    static ref SEABASE: Regex = Regex::new(r".#..#..#..#..#..#...").unwrap();
}

fn parse_line(s: &str) -> usize {
    let mut result = 0;
    for c in s.chars() {
        result <<= 1;
        if c == '#' {
            result += 1;
        }
    }
    result
}

fn reverse_bits(x: usize) -> usize {
    let mut result = 0;
    for b in 0..10 {
        if x & (1 << b) != 0 {
            result |= 1 << (9 - b);
        }
    }
    result
}

#[derive(Debug, Copy, Clone)]
struct Block {
    tile_id: usize,
    top: usize,
    bottom: usize,
    left: usize,
    right: usize,
    rotations: usize,
    flipped: bool,
}

impl Block {
    fn rotate(self) -> Self {
        Block {
            tile_id: self.tile_id,
            top: reverse_bits(self.left),
            right: self.top,
            bottom: reverse_bits(self.right),
            left: self.bottom,
            rotations: self.rotations + 1,
            flipped: self.flipped,
        }
    }

    fn flip(self) -> Self {
        Block {
            tile_id: self.tile_id,
            top: reverse_bits(self.top),
            right: self.left,
            bottom: reverse_bits(self.bottom),
            left: self.right,
            rotations: self.rotations,
            flipped: !self.flipped,
        }
    }

    fn orientations(self) -> Vec<Self> {
        let a = self.rotate();
        let b = a.rotate();
        let c = b.rotate();
        vec![self, a, b, c, self.flip(), a.flip(), b.flip(), c.flip()]
    }
}

type Img = Vec<Vec<char>>;

fn parse_block(s: &str) -> (Block, Img) {
    let mut lines = s.lines();

    let header = lines.next().unwrap();
    let tile_id = TILE_HEADER
        .captures(header)
        .unwrap()
        .get(1)
        .unwrap()
        .as_str()
        .parse::<usize>()
        .unwrap();

    let lines: Vec<&str> = lines.collect();

    let mut img: Img = lines
        .iter()
        .map(|x| {
            let mut x = x.to_string();
            x.pop();
            x.remove(0);
            x.chars().collect()
        })
        .collect();
    img.pop();
    img.remove(0);

    let lines: Vec<usize> = lines.iter().map(|x| parse_line(x)).collect();

    let top = *lines.first().unwrap();
    let bottom = *lines.last().unwrap();

    let mut right = 0;
    let mut left = 0;
    for line in lines {
        right <<= 1;
        left <<= 1;
        if line & 1 != 0 {
            right += 1;
        }

        if line & (1 << 9) != 0 {
            left += 1;
        }
    }

    (
        Block {
            tile_id,
            top,
            bottom,
            left,
            right,
            rotations: 0,
            flipped: false,
        },
        img,
    )
}

fn rotate_img_n(img: Img, n: usize) -> Img {
    let mut img = img.clone();
    for _ in 0..n {
        img = rotate_img(img)
    }
    img
}

fn rotate_img(img: Img) -> Img {
    let size = img.len();
    let mut result = vec![];
    for i in 0..size {
        let mut line = vec![];
        for j in 0..size {
            line.push(img[size - j - 1][i])
        }
        result.push(line);
    }
    result
}

fn flip_img(img: Img) -> Img {
    img.iter()
        .map(|x| x.iter().rev().cloned().collect())
        .collect()
}

fn compose_img(imgs: Vec<Img>) -> Img {
    let grid_size = (imgs.len() as f64).sqrt() as usize;
    let tile_size = imgs[0].len();

    let mut result = vec![];
    for i in 0..(grid_size * tile_size) {
        let mut line = vec![];
        for j in 0..(grid_size * tile_size) {
            let img_j = j % tile_size;
            let img_i = i % tile_size;

            let grid_i = i / tile_size;
            let grid_j = j / tile_size;
            let grid_box = grid_i * grid_size + grid_j;

            line.push(imgs[grid_box][img_i][img_j])
        }
        result.push(line);
    }
    result
}

fn do_search(
    grid_size: usize,
    result: &mut Vec<Block>,
    used: &mut HashSet<usize>,
    blocks: &Vec<Block>,
) -> bool {
    let position = result.len();

    if position == grid_size * grid_size {
        return true;
    }

    // println!("{}: Position: {:?}", position, result);

    for block in blocks {
        if used.contains(&block.tile_id) {
            continue;
        }
        for block in block.orientations() {
            if position % grid_size != 0 && result[position - 1].right != block.left {
                continue;
            }
            if position > grid_size && result[position - grid_size].bottom != block.top {
                // No match up.
                continue;
            }
            result.push(block);
            used.insert(block.tile_id);
            if do_search(grid_size, result, used, blocks) {
                return true;
            }
            used.remove(&block.tile_id);
            result.pop();
        }
    }
    false
}

fn find_seamonsters(img: Vec<String>) -> usize {
    let mut monsters = 0;
    for i in 1..(img.len() - 1) {
        for m in SEABODY.find_iter(&img[i]) {
            if SEABASE.is_match_at(&img[i + 1], m.start())
                && SEAHEAD.is_match_at(&img[i - 1], m.start())
            {
                println!("Sea monster: {}, {}", i, m.start());
                monsters += 1;
            }
        }
    }
    monsters
}

fn main() -> anyhow::Result<()> {
    let content = std::fs::read_to_string("input/day20")?;

    let blocks_imgs: Vec<(Block, Img)> = content
        .split("\n\n")
        .filter(|x| *x != "")
        .map(parse_block)
        .collect();

    let blocks: Vec<Block> = blocks_imgs.iter().map(|(b, _)| *b).collect();
    let imgs: HashMap<usize, Img> = blocks_imgs
        .iter()
        .map(|(b, i)| (b.tile_id, i.clone()))
        .collect();

    let grid_size = (blocks.len() as f64).sqrt() as usize;

    // println!("Grid size: {}", grid_size);

    let mut arrangement = vec![];
    let mut used = HashSet::new();

    if !do_search(grid_size, &mut arrangement, &mut used, &blocks) {
        println!("SEARCH FAILED");
        return Ok(());
    }
    let corners = vec![
        arrangement[0],
        arrangement[grid_size - 1],
        arrangement[grid_size * (grid_size - 1)],
        arrangement[grid_size * grid_size - 1],
    ];
    for corner in &corners {
        println!("{:?}", corner);
    }
    let part1: usize = corners.iter().map(|x| x.tile_id).product();
    println!("{}", part1);

    let img_tiles: Vec<Img> = arrangement
        .iter()
        .map(|b| {
            let img = rotate_img_n(imgs[&b.tile_id].clone(), b.rotations);
            if b.flipped {
                flip_img(img)
            } else {
                img
            }
        })
        .collect();

    let img = compose_img(img_tiles);
    for r in 0..4 {
        let img = rotate_img_n(img.clone(), r);
        let string_img: Vec<String> = img.iter().map(|x| x.iter().collect()).collect();
        let waves: usize = string_img
            .iter()
            .map(|x| x.chars().filter(|x| *x == '#').count())
            .sum();
        let monsters = find_seamonsters(string_img);

        println!("Part 2 = {}", waves - monsters * 15);

        let img = flip_img(rotate_img_n(img.clone(), r));
        let string_img: Vec<String> = img.iter().map(|x| x.iter().collect()).collect();
        let waves: usize = string_img
            .iter()
            .map(|x| x.chars().filter(|x| *x == '#').count())
            .sum();
        let monsters = find_seamonsters(string_img);

        println!("Part 2 = {}", waves - monsters * 15);
    }

    // let string_img : Vec<String> = img.iter().map(|x| x.iter().collect()).collect();
    // // for line in &string_img {
    //     // println!("{}", line);
    // // }
    // let waves : usize = string_img.iter().map(|x| x.chars().filter(|x| *x == '#').count()).sum();

    // let monsters = find_seamonsters(string_img);

    // println!("Part 2 = {}", waves - monsters * 15);

    Ok(())
}
