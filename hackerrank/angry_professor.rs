// https://www.hackerrank.com/challenges/angry-professor/problem

/*
 * Implementation questions:
 *
 * 1. Why do I need to use `&t` in: `filter(|&t| t <= 0)`?
 */

use std::io;
use std::fmt;
use std::convert::TryFrom;

struct Case {
    min_students_on_time: i32,
    arrival_times: Vec<i32>,
}

enum Answer {
    YES,
    NO,
}

impl fmt::Display for Answer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Answer::YES => write!(f, "YES"),
            Answer::NO => write!(f, "NO"),
        }
    }
}

fn main() {
    let cases = to_int(&read_line());
    for _ in 0..cases {
        let case = read_case();
        println!("{}", is_class_cancelled(case));
    }
}

fn read_line() -> String {
    let mut buffer = String::new();
    io::stdin()
        .read_line(&mut buffer)
        .expect("Failed to read line");
    return buffer.trim().to_owned();
}

fn to_int(str: &str) -> i32 {
    return str.parse::<i32>().unwrap();
}

fn read_case_first_line() -> (i32, i32) {
    let line = &read_line();
    let list: Vec<&str> = line.split(" ").collect();
    let students = to_int(list[0]);
    let min_students_on_time = to_int(list[1]);
    return (students, min_students_on_time);
}

fn read_case_second_line() -> Vec<i32> {
    let line = &read_line();
    let list: Vec<&str> = line.split(" ").collect();
    return list.into_iter().map(|x| to_int(x)).collect();
}

fn read_case() -> Case {
    let (students, min_students_on_time) = read_case_first_line();
    let arrival_times = read_case_second_line();
    let arrivals_size = i32::try_from(arrival_times.len()).unwrap();
    assert_eq!(students, arrivals_size, "arrival_times size is not correct");
    return Case {
        min_students_on_time: min_students_on_time,
        arrival_times: arrival_times,
    };
}

fn is_class_cancelled(case: Case) -> Answer {
    let students_on_time = case.arrival_times.into_iter().filter(|&t| t <= 0).count();
    if i32::try_from(students_on_time).unwrap() >= case.min_students_on_time {
        return Answer::NO;
    }
    return Answer::YES;
}
