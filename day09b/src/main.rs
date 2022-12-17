use std::collections::HashSet;
use std::fmt;
use std::ops::{Add, Sub};

fn main() {
    let input = include_str!("../input.txt")
        .trim()
        .split("\n")
        .collect::<Vec<_>>();

    let rope_knots = [Point { x: 0, y: 0 }; 10];

    let tail_history = input
        .iter()
        .scan(rope_knots, |t, inp| Some(get_tail_movement(*inp, t))) // getting tail history
        .flatten()
        .collect::<HashSet<_>>();
    println!("history: {:?}", tail_history.len());
}

fn get_tail_movement(input: &str, knots: &mut [Point<i32>]) -> Vec<Point<i32>> {
    let directions = parse_directions(input);

    let tail_history: Vec<Point<i32>> = Vec::new();

    let movement = directions.iter().fold(tail_history, |mut hist, step| {
        let head = knots.first_mut().unwrap();
        *head = *head + *step;
        rec_move_knots(knots);
        hist.push(knots.iter().last().unwrap().to_owned()); // push history of last knot
        hist
    });
    movement
}

fn rec_move_knots(knots: &mut [Point<i32>]) {
    // recursively move each knot
    match knots {
        [head, tail @ ..] => {
            if tail.is_empty() {
                ()
            } else {
                move_next_knot(*head, tail);
                rec_move_knots(tail)
            }
        }
        _ => (),
    }
}

fn move_next_knot(head: Point<i32>, tail_array: &mut [Point<i32>]) {
    let tail = tail_array.first_mut().unwrap();
    let diff = head - *tail;

    if diff.x.abs() > 1 && diff.y.abs() < 2 {
        tail.x += diff.x / 2;
        tail.y += diff.y;
    } else if diff.y.abs() > 1 && diff.x.abs() < 2 {
        tail.x += diff.x;
        tail.y += diff.y / 2;
    } else if diff.y.abs() > 1 && diff.x.abs() > 1 {
        tail.x += diff.x / 2;
        tail.y += diff.y / 2;
    }
}

fn parse_directions(dir: &str) -> Vec<Point<i32>> {
    // convert directions into a Vector of steps the Head will take
    let mut components = dir.split(' ');
    let (dir, amt): (&str, i32) = (
        components.next().unwrap(),
        components.next().unwrap().parse().unwrap(),
    );
    let directions = match (dir, amt) {
        ("R", x) => (1..=x).map(|_| Point { x: 1, y: 0 }).collect::<Vec<_>>(),
        ("L", x) => (1..=x).map(|_| Point { x: -1, y: 0 }).collect::<Vec<_>>(),
        ("U", x) => (1..=x).map(|_| Point { x: 0, y: 1 }).collect::<Vec<_>>(),
        ("D", x) => (1..=x).map(|_| Point { x: 0, y: -1 }).collect::<Vec<_>>(),
        _ => panic!("unexpected directions"),
    };
    directions
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Point<T> {
    x: T,
    y: T,
}

impl<T: Add<Output = T>> Add for Point<T> {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl<T: Sub<Output = T>> Sub for Point<T> {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl<T: std::fmt::Display> fmt::Display for Point<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}
