fn loop_size(subject_number: usize, target: usize) -> usize {
    let mut value = 1;
    let mut loop_size = 0;
    loop {
        value = (value * subject_number) % 20201227;
        loop_size += 1;

        if value == target {
            break
        }
    }
    loop_size
}

fn transform(subject_number: usize, loop_size: usize) -> usize {
    let mut value = 1;
    for _ in 0..loop_size {
        value = (value * subject_number) % 20201227;
    }
    value
}

fn encryption_key(key_pk: usize, door_pk: usize) -> usize {
    let key_ls = loop_size(7, key_pk);
    // let door_ls = loop_size(7, door_pk);

    transform(door_pk, key_ls)
}
fn main() {

//     println!("{}", loop_size(7, 5764801));
//     println!("{}", loop_size(7, 17807724));

//     println!("{}", transform(17807724, 8));
    println!("Example = {}", encryption_key(5764801, 17807724));

    println!("Part 1 = {}", encryption_key(1327981, 2822615));

}
