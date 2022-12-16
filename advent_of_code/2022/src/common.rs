use std::{error::Error, time::SystemTime};

pub type AocResult<T> = Result<T, Box<dyn Error>>;

pub fn time_closure<F: Fn() -> T, T>(f: F) -> T {
  let start = SystemTime::now();
  let result = f();
  let end = SystemTime::now();
  let duration = end.duration_since(start).unwrap();
  println!("it took {} ms", duration.as_millis());
  result
}
