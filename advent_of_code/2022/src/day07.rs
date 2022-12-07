use crate::common::*;
use sscanf::sscanf;
use std::{collections::HashMap, rc::Rc, cell::RefCell};

struct Dir {
    parent: Option<Rc<RefCell<Dir>>>,
    // name -> dir
    subdirs: HashMap<String, Rc<RefCell<Dir>>>,
    // name -> size
    files: HashMap<String, u64>,
}

impl Dir {
    fn new() -> Self {
        Dir {
            parent: None,
            subdirs: HashMap::new(),
            files: HashMap::new(),
        }
    }

    fn add_file(&mut self, name: String, size: u64) {
        self.files.insert(name, size);
    }

    fn size(&self) -> u64 {
        self.files.values().sum::<u64>() +
            self.subdirs.values().map(|sub| sub.borrow().size()).sum::<u64>()
    }
}

fn cd(cwd: Rc<RefCell<Dir>>, name: String) -> Rc<RefCell<Dir>> {
    if name == ".." {
        cwd.borrow().parent.as_ref().expect("No parent dir").clone()
    } else if cwd.borrow().subdirs.contains_key(&name) {
        cwd.borrow().subdirs.get(&name).unwrap().clone()
    } else {
        mkdir(cwd, name)
    }
}

fn mkdir(cwd: Rc<RefCell<Dir>>, name: String) -> Rc<RefCell<Dir>> {
    let new = Rc::new(RefCell::new(Dir::new()));
    new.borrow_mut().parent = Some(cwd.clone());
    cwd.borrow_mut().subdirs.insert(name, new.clone());
    new
}

fn goto_root(cwd: Rc<RefCell<Dir>>) -> Rc<RefCell<Dir>> {
    if let Some(parent) = &cwd.borrow().parent {
        return parent.clone();
    }
    cwd
}

fn parse() -> AocResult<Rc<RefCell<Dir>>> {
    let raw = std::fs::read_to_string("data/day07.txt")?;
    let mut tree = Rc::new(RefCell::new(Dir::new()));
    for line in raw.lines() {
        if line.starts_with('$') {
            // We can actually ignore ls
            if let Ok(dir) = sscanf!(line, "$ cd {String}") {
                if dir != "/" {
                    tree = cd(tree.clone(), dir);
                }
            }
        } else if let Ok(name) = sscanf!(line, "dir {String}") {
            mkdir(tree.clone(), name);
        } else {
            let (size, name) = sscanf!(line, "{u64} {String}")?;
            tree.borrow_mut().add_file(name, size);
        }
    }
    Ok(goto_root(tree))
}

fn part1(cwd: Rc<RefCell<Dir>>) -> u64 {
    let sub_size = cwd.borrow().subdirs.values().cloned().map(part1).sum::<u64>();
    let cwd_size = cwd.borrow().size();
    sub_size + if cwd_size <= 100000 {
        cwd_size
    } else {
        0
    }
}

fn all_dir_sizes(cwd: Rc<RefCell<Dir>>) -> Vec<u64> {
    let mut sizes = cwd.borrow().subdirs.values().cloned()
        .flat_map(all_dir_sizes)
        .collect::<Vec<_>>();
    sizes.push(cwd.borrow().size());
    sizes
}

fn part2(cwd: Rc<RefCell<Dir>>) -> u64 {
    let free_space = 70000000 - cwd.borrow().size();
    let required_space = 30000000;
    all_dir_sizes(cwd).iter().copied()
        .filter(|size| free_space + size >= required_space)
        .min().unwrap()
}

pub fn day07() -> AocResult<()> {
    let root = parse()?;
    println!("Part 1: {}", part1(root.clone()));
    println!("Part 2: {}", part2(root));
    Ok(())
}
