use aoclib::intcode::Program;

fn main() {
    let program: Program = aoclib::intcode::read_program("input/day17");

    let img : String = program.clone().map(|x| char::from(x as u8)).collect();

    let layout : Vec<Vec<char>> = img.lines().map(|x| x.chars().collect::<Vec<char>>()).collect();

    let mut align = 0;
    for (i, line) in layout.iter().enumerate() {
        for (j, x) in line.iter().enumerate() {
            if *x == '#' && i > 0 && j > 0 && i < (layout.len() - 2) && layout[i-1][j] == '#' && layout[i+1][j] == '#' && layout[i][j-1] == '#' && layout[i][j+1] == '#' {
                align += i * j;
                print!("O");
            } else {
                print!("{}", x);
            }
        }
        println!("");
    }

    println!("Align: {}", align);

    program.disassemble();

}
