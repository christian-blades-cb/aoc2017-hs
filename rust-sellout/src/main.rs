extern crate itertools;

use std::fs::File;
use std::io::Read;
use std::string::String;

fn main() {
    let mut contents = File::open("../day5-input").expect("unable to open input file");
    let mut buf = String::new();
    contents.read_to_string(&mut buf).expect(
        "unable to read file",
    );
    let mut input = buf.split_whitespace()
        .map(|l| l.parse::<isize>().expect("line wasn't a valid int!"))
        .collect();
    let steps = jump(&mut input);

    println!("day5pt2 {}", steps);
}

fn jump(stack: &mut Vec<isize>) -> usize {
    let mut steps = 0;

    let mut i = 0;
    while i < stack.len() {
        let offset = stack[i];
        if offset >= 3 {
            stack[i] -= 1;
        } else {
            stack[i] += 1;
        }
        i = (i as isize + offset) as usize;
        steps += 1;
    }

    return steps;
}
