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

fn main() {
    let contents = fs::read_to_string("input").expect("Could not open input file.");

    let result: u32 = contents
        .lines()
        .map(|line| {
            find_first(line).expect("Failed to find first digit") * 10
                + find_last(line).expect("Failed to find last digit")
        })
        .sum();

    println!("{}", result);
}
