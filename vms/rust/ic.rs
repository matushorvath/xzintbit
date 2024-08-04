use std::env;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::process::exit;

struct Vm {
    ip: usize,
    rb: i32,
    mem: Vec<i32>
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            ip: 0,
            rb: 0,
            mem: vec![0; 64]
        }
    }

    fn get_mem(self: &Vm, addr: usize) -> i32 {
        if addr >= self.mem.len() {
            0
        } else {
            self.mem[addr]
        }
    }

    fn set_mem(self: &mut Vm, addr: usize, val: i32) {
        if addr >= self.mem.len() {
            let mut new_size = self.mem.len();
            while addr >= new_size {
                new_size <<= 1;
            }
            self.mem.resize(new_size, 0);
        }
        self.mem[addr] = val
    }

    const MODE_MUL: [i32; 3] = [100, 1000, 10000];

    fn get_param(self: &Vm, idx: usize) -> Result<i32, String> {
        match self.get_mem(self.ip) / Vm::MODE_MUL[idx] % 10 {
            0 => Ok(self.get_mem(self.get_mem(self.ip + idx + 1).try_into().unwrap())), // position mode
            1 => Ok(self.get_mem(self.ip + idx + 1)), // immediate mode
            2 => Ok(self.get_mem((self.rb + self.get_mem(self.ip + idx + 1)).try_into().unwrap())), // relative mode
            _ => Err(format!("mode error: ip {} idx {}", self.ip, idx))
        }
    }

    fn set_param(self: &mut Vm, idx: usize, val: i32) -> Result<(), String> {
        match self.get_mem(self.ip) / Vm::MODE_MUL[idx] % 10 {
            0 => Ok(self.set_mem(self.get_mem(self.ip + idx + 1).try_into().unwrap(), val)), // position mode
            2 => Ok(self.set_mem((self.rb + self.get_mem(self.ip + idx + 1)).try_into().unwrap(), val)), // relative mode
            _ => Err(format!("mode error: ip {} idx {}", self.ip, idx))
        }
    }

    pub fn run(self: &mut Vm, get_input: fn() -> i32, set_output: fn(i32)) -> Result<(), String> {
        loop {
            let oc = self.get_mem(self.ip) % 100;

            match oc {
                1 => { // add
                    self.set_param(2, self.get_param(0)? + self.get_param(1)?)?;
                    self.ip += 4;
                },
                2 => { // mul
                    self.set_param(2, self.get_param(0)? * self.get_param(1)?)?;
                    self.ip += 4;
                },
                3 => { // in
                    self.set_param(0, get_input())?;
                    self.ip += 2;
                },
                4 => { // out
                    set_output(self.get_param(0)?);
                    self.ip += 2;
                },
                5 => { // jnz
                    match self.get_param(0)? {
                        0 => self.ip += 3,
                        _ => self.ip = self.get_param(1)?.try_into().unwrap()
                    };
                },
                6 => { // jz
                    match self.get_param(0)? {
                        0 => self.ip = self.get_param(1)?.try_into().unwrap(),
                        _ => self.ip += 3
                    };
                },
                7 => { // lt
                    self.set_param(2, if self.get_param(0)? < self.get_param(1)? { 1 } else { 0 })?;
                    self.ip += 4;
                },
                8 => { // eq
                    self.set_param(2, if self.get_param(0)? == self.get_param(1)? { 1 } else { 0 })?;
                    self.ip += 4;
                },
                9 => { // arb
                    self.rb += self.get_param(0)?;
                    self.ip += 2;
                },
                99 => { // hlt
                    return Ok(());
                }
                _ => {
                    return Err(format!("opcode error: ip {} oc {}", self.ip, oc));
                }
            }
        }
    }
}

#[cfg(windows)]
const ENDL: &'static str = "\r\n";
#[cfg(not(windows))]
const ENDL: &'static str = "\n";

fn get_input() -> i32 {
    let mut buf: [u8; 1] = [0];
    match io::stdin().read_exact(&mut buf) {
        Ok(()) => (),
        Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => {
            eprint!("no more inputs{}", ENDL);
            exit(1);
        },
        Err(e) => {
            panic!("{}", e);
        }
    }
    buf[0] as i32
}

fn set_output(val: i32) {
    let val = u8::try_from(val).expect("output value out of range");
    print!("{}", val as char);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = args.get(1)
        .expect("no program specified");

    let data = fs::read_to_string(path)
        .expect("error reading program file");
    let data = data.trim_end().split(',');

    let mut vm = Vm::new();

    for (idx, item) in data.enumerate() {
        let item: i32 = item.parse().expect("invalid program");
        vm.set_mem(idx, item);
    }

    vm.run(get_input, set_output).unwrap();
}
