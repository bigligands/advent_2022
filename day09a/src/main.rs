#![allow(unused_variables, dead_code, unused_mut)]
use std::collections::HashSet;
use std::ops::{Add, Sub};

fn main() {
    let input = include_str!("../input.txt")
        .trim()
        .split("\n")
        .collect::<Vec<_>>();
    println!("{:?}", input);

    let tail_history = input
        .iter()
        .scan(
            (Point { x: 0, y: 0 }, Point { x: 0, y: 0 }),
            |(head, tail), inp| Some(move_head(*inp, head, tail)),
        )
        .flatten()
        .collect::<HashSet<_>>()
        .len();
    println!("{} visited at least once.", tail_history);
}

fn move_head(input: &str, head: &mut Point<i32>, tail: &mut Point<i32>) -> HashSet<Point<i32>> {
    // Move head and determine tail movement for each step
    let movement = parse_directions(input);
    let mut tail_history: HashSet<Point<i32>> = HashSet::new();
    tail_history.insert(Point { x: 0, y: 0 });
    let history = movement.iter().fold(tail_history, |mut h, step| {
        *head = *head + *step;
        move_tail(head, tail);
        h.insert(tail.clone());
        h
    });
    history
}

fn move_tail(head: &Point<i32>, tail: &mut Point<i32>) {
    let diff = *head - *tail;
    if diff.x.abs() > 1 {
        match (diff.x, diff.y) {
            (x, y) => {
                tail.x += x / 2;
                tail.y += y;
            }
        }
    }
    if diff.y.abs() > 1 {
        match (diff.x, diff.y) {
            (x, y) => {
                tail.x += x;
                tail.y += y / 2;
            }
        }
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
