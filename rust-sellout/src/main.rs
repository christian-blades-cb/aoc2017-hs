extern crate itertools;

use std::fs::File;
use std::io::Read;
use std::string::String;

fn main() {
    day5pt2("../day5-input");

    day6pt1("../day6-input");
    day6pt2("../day6-input");
}

fn day5pt2(filename: &str) {
    let mut contents = File::open(filename).expect("unable to open input file");
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

fn day6pt1(filename: &str) {
    let mut contents = File::open(filename).expect("unable to open input file");
    let mut buf = String::new();
    contents.read_to_string(&mut buf).expect(
        "unable to read input file",
    );
    let mut input = buf.split_whitespace()
        .map(|word| word.parse::<usize>().expect("work not a valid uint"))
        .collect();
    let cycles = reallocations(&mut input);

    println!("day6pt1 {}", cycles);
}

fn reallocations(banks: &mut Vec<usize>) -> usize {
    println!("banks {:?}", banks);

    let mut seen: Vec<Vec<usize>> = vec![banks.clone()];
    let mut count = 0;
    let banks_len = banks.len();

    loop {
        // let (biggest_bank, _) = banks
        //     .iter()
        //     .enumerate()
        //     .max_by_key(|&(_, val)| val)
        //     .unwrap();
        //
        // sigh:
        // > If several elements are equally maximum, the last element is returned.
        let biggest_bank = max_index(banks);

        let bank_size = banks[biggest_bank];
        banks[biggest_bank] = 0;
        for i in (0..bank_size).map(|x| (x + biggest_bank + 1) % banks_len) {
            // println!("i: {}", i);
            banks[i] += 1;
        }
        count += 1;

        // println!("biggest: {} sizeat: {}", biggest_bank, bank_size);
        // println!("banks {:?}", banks);
        if seen.iter().any(|s| s == banks) {
            break;
        }
        seen.push(banks.clone());
    }

    count
}

fn day6pt2(filename: &str) {
    let mut contents = File::open(filename).expect("unable to open input file");
    let mut buf = String::new();
    contents.read_to_string(&mut buf).expect(
        "unable to read input file",
    );
    let mut input = buf.split_whitespace()
        .map(|word| word.parse::<usize>().expect("work not a valid uint"))
        .collect();
    let distance = cycle_size(&mut input);

    println!("day6pt2 {}", distance);
}

fn cycle_size(banks: &mut Vec<usize>) -> usize {
    println!("banks {:?}", banks);

    let mut seen: Vec<Vec<usize>> = vec![banks.clone()];
    let mut count = 0;
    let banks_len = banks.len();

    loop {
        let biggest_bank = max_index(banks);

        let bank_size = banks[biggest_bank];
        banks[biggest_bank] = 0;
        for i in (0..bank_size).map(|x| (x + biggest_bank + 1) % banks_len) {
            // println!("i: {}", i);
            banks[i] += 1;
        }
        count += 1;

        // println!("biggest: {} sizeat: {}", biggest_bank, bank_size);
        // println!("banks {:?}", banks);
        if let Some((i, _)) = seen.iter().enumerate().find(|&(_, prev)| prev == banks) {
            return count - i;
        }
        seen.push(banks.clone());
    }
}

fn max_index(banks: &Vec<usize>) -> usize {
    let mut idx = 0;
    let mut max_val = 0;

    for (i, &val) in banks.iter().enumerate() {
        if val > max_val {
            idx = i;
            max_val = val;
        } else if val == max_val && i <= idx {
            idx = i;
            max_val = val;
        }
    }

    idx
}
