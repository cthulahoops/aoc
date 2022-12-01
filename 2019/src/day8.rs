const WIDTH: usize = 25;
const HEIGHT: usize = 6;
fn main() {
    let image = std::fs::read_to_string("input/day8").unwrap();

    let layers: Vec<Vec<char>> = image
        .as_bytes()
        .chunks(WIDTH * HEIGHT)
        .map(|x| x.into_iter().map(|x| *x as char).collect::<Vec<char>>())
        .filter(|x| x.len() > 1)
        .collect();

    for layer in layers.clone() {
        let mut zeros = 0;
        let mut ones = 0;
        let mut twos = 0;
        for x in layer {
            match x {
                '0' => zeros += 1,
                '1' => ones += 1,
                '2' => twos += 1,
                _ => (),
            }
        }
        println!("{}, {}, {} => {}", zeros, ones, twos, ones * twos);
    }

    let mut output = vec![];
    output.resize(WIDTH * HEIGHT, ' ');
    for layer in layers.iter().rev() {
        for (i, pixel) in layer.iter().enumerate() {
            match pixel {
                '0' => output[i] = ' ',
                '1' => output[i] = '#',
                '2' => (),
                _ => panic!("Invalid pixel: {:?}", pixel)
            }
        }
    }

    for line in output.chunks(WIDTH) {
        println!("{}", line.iter().collect::<String>());
    }
}
