use std::fs;

fn find_first(s: &str) -> Option<u32> {
    for c in s.chars() {
        if c.is_digit(10) {
            return c.to_digit(10);
        }
    }
    None
}

fn find_last(s: &str) -> Option<u32> {
    for c in s.chars().rev() {
        if c.is_digit(10) {
            return c.to_digit(10);
        }
    }
    None
}

fn replace_parallel(s: &str) -> String {
    const DICT: [(&str, char); 9] = [
        ("one", '1'),
        ("two", '2'),
        ("three", '3'),
        ("four", '4'),
        ("five", '5'),
        ("six", '6'),
        ("seven", '7'),
        ("eight", '8'),
        ("nine", '9'),
    ];

    let mut result: String = Default::default();
    'outer: for i in 0..s.len() {
        let slice = s.get(i..).unwrap();
        for (search, replace) in DICT {
            if slice.starts_with(search) {
                result.push(replace);
                continue 'outer;
            }
        }
        result.push_str(s.get(i..i + 1).unwrap());
    }

    result
}

fn main() {
    let contents = fs::read_to_string("input").expect("Could not open input file.");

    let result: u32 = contents
        .lines()
        .map(|line| {
            let parsed = replace_parallel(line);
            find_first(&parsed).expect("Failed to find first digit") * 10
                + find_last(&parsed).expect("Failed to find last digit")
        })
        .sum();

    println!("{}", result);
}
