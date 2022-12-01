use std::error::Error;

pub type AocResult<T> = Result<T, Box<dyn Error>>;
