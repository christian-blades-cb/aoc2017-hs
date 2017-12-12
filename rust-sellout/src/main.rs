extern crate itertools;
#[macro_use]
extern crate nom;

use std::fs::File;
use std::io::Read;
use std::string::String;
use nom::{digit, alpha, IResult};
use std::str::{FromStr, from_utf8};
use std::error::Error;
use std::fmt;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
struct SomeKindOfError;
impl Error for SomeKindOfError {
    fn description(&self) -> &str {
        "something went wrong"
    }
}
impl fmt::Display for SomeKindOfError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "something's gone wrong")
    }
}

fn main() {
    day5pt2("../day5-input");

    day6pt1("../day6-input");
    day6pt2("../day6-input");
    day7pt1("../day7-input");

    day12pt1("../day12-input");
}

fn day12pt1(filename: &str) {
    let mut fd = File::open(filename).expect("unable to open input file");
    let mut body = Vec::new();
    fd.read_to_end(&mut body).expect("unable to read input");
    let connections = d12parsefile(&body);
    let conn_map: HashMap<usize, Vec<usize>> =
        connections.iter().cloned().map(|(a, b)| (a, b)).collect();

    let mut zero_grp: HashSet<usize> = conn_map
        .get(&0)
        .unwrap()
        .to_owned()
        .iter()
        .map(|&x| x)
        .collect();
    let mut working = conn_map.get(&0).unwrap().to_owned();

    while working.len() != 0 {
        let w = working.pop().unwrap();
        let c = conn_map.get(&w).unwrap();
        for &x in c.iter() {
            if zero_grp.insert(x) {
                working.push(x);
            }
        }
    }

    println!("day12pt1: {}", zero_grp.len());
}

fn d12parsefile(body: &[u8]) -> Vec<(usize, Vec<usize>)> {
    let mut out = Vec::new();
    let mut rest = body;
    loop {
        let result = d12parser(rest);
        if let IResult::Done(r, ssink) = result {
            rest = r;
            out.push(ssink);
        } else {
            break;
        }
    }

    out
}

named!(d12parser<&[u8], (usize, Vec<usize>)>,
       do_parse!(
           src: d12src >>
               tag!("<->") >>
               sinks: separated_list!(
                   tag!(","),
                   d12src) >>
               (src, sinks)
    ));

named!(d12src<&[u8], usize>,
       map_res!(
           map_res!(
               ws!(digit),
               from_utf8
           ),
           usize::from_str
       ));

fn day7pt1(filename: &str) {
    let mut fd = File::open(filename).expect("unable to open input file");
    let mut body = Vec::new();
    fd.read_to_end(&mut body).expect("unable to read input");
    let towers = parseday7(&body).unwrap();
    // std::io::stderr().write(&body).unwrap();
    // println!("{:?}", body);

    let mut owned = Vec::new();
    let mut names = Vec::new();


    for x in towers.iter() {
        // if x.name == "idfyy" {
        //     println!("idfyy -> {:?}", x.holding);
        // }
        names.push(x.to_owned().name);
    }

    for x in towers.iter() {
        if let Some(owns) = x.to_owned().holding {
            owned.append(&mut owns.to_owned());
        };
    }

    let fin: Vec<String> = names
        .iter()
        .filter_map(|x| if owned.iter().all(|y| y != x) {
            Some(x.to_owned())
        } else {
            None
        })
        .collect();

    if fin.len() != 1 {
        println!("size {}, fin: {:?}", fin.len(), fin);
        panic!("fin size is wrong");
    }

    println!("day7pt1: {}", fin[0]);
}

fn find_base(stack_list: Vec<StackPgm>) -> Result<String, SomeKindOfError> {
    let mut owned = Vec::new();
    let mut names = Vec::new();


    for x in stack_list.iter() {
        // if x.name == "idfyy" {
        //     println!("idfyy -> {:?}", x.holding);
        // }
        names.push(x.to_owned().name);
    }

    for x in stack_list.iter() {
        if let Some(owns) = x.to_owned().holding {
            owned.append(&mut owns.to_owned());
        };
    }

    let fin: Vec<String> = names
        .iter()
        .filter_map(|x| if owned.iter().all(|y| y != x) {
            Some(x.to_owned())
        } else {
            None
        })
        .collect();

    if fin.len() != 1 {
        println!("size {}, fin: {:?}", fin.len(), fin);
        panic!("fin size is wrong");
    }

    Ok(fin[0].to_owned())
}

fn parseday7(body: &[u8]) -> Option<Vec<StackPgm>> {
    let mut towers = Vec::new();
    let mut rest = body;
    loop {
        if let IResult::Done(r, tow) = pgm_stack(rest) {
            rest = r;
            towers.push(tow);
        } else {
            break;
        }
    }

    if towers.len() == 0 {
        None
    } else {
        Some(towers)
    }
}
// named!(parseday7<&[u8], Vec<StackPgm>>,
//        many1!(pgm_stack));

named!(weight<&[u8], usize>,
       ws!(
           delimited!(
               tag!("(") ,
               map_res!(map_res!(digit, from_utf8), FromStr::from_str) ,
               tag!(")")
           )));

named!(pgmname<&[u8], &str>,
       map_res!(
           ws!(alpha),
           from_utf8
       ));

named!(holding<&[u8], Option<Vec<&str>>>,
       opt!(
           do_parse!(
               ws!(tag!("->")) >>
               pgms: separated_list!(
                   tag!(",") ,
                   pgmname) >>
               (pgms)                   
           )));

named!(pgm_stack<&[u8], StackPgm>,
       map_res!(
           do_parse!(
               name: pgmname >>
               wgt: weight >>
               held: holding >>                  
               (name, wgt, held)) ,
           |x: (&str, usize, Option<Vec<&str>>)| Ok::<StackPgm, SomeKindOfError>(StackPgm::new(x.0, x.1, x.2))
       ));

#[derive(Clone, Debug)]
struct StackPgm {
    name: String,
    weight: usize,
    holding: Option<Vec<String>>,
}

impl StackPgm {
    fn new(name: &str, weight: usize, holding: Option<Vec<&str>>) -> StackPgm {
        let n = String::from(name);
        let h = holding.map(|hv| hv.iter().map(|&x| String::from(x)).collect());

        StackPgm {
            name: n,
            weight: weight,
            holding: h,
        }
    }
}

// day5
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

// day6

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
    // println!("banks {:?}", banks);

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
    // println!("banks {:?}", banks);

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
