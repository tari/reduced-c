//! Machine representation of instructions.
use std::fmt;

/// A single memory location
pub type Addr = u8;

#[derive(PartialEq, Eq, Debug, Clone)]
/// A register, such as `Register(0)` for `r0`.
pub struct Register(pub u8);

impl Copy for Register {}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            x @ 0 | x @ 1 => write!(f, "r{}", x),
            x => panic!("Invalid register number: {}", x),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
/// A memory reference.
pub enum Label {
    /// No name and address not yet resolved.
    None,
    /// Known location in memory.
    Address(Addr),
    /// Abstract location, with address not yet resolved.
    Name(String),
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Label::None => Ok(()),
            Label::Address(_) => panic!("Bare addresses are not printable"),
            Label::Name(ref s) => write!(f, "{}", s),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
/// Machine instructions.
pub enum Instruction {
    /// Store register to memory.
    Store(Register, Label),
    /// Load from memory to register.
    Load(Label, Register),
    /// Move from first register to second register.
    Move(Register, Register),

    /// Add the first two registers, storing to the third.
    Add(Register, Register, Register),
    /// Subtract the first register from the second, storing to the third.
    Subtract(Register, Register, Register),
    /// Bitwise AND of first two, store to third.
    And(Register, Register, Register),
    /// Bitwise OR of first two, store to third.
    Or(Register, Register, Register),
    /// Bitwise NAND of first two, store to third.
    Nand(Register, Register, Register),
    /// Bitwise NOR of first two, store to third.
    Nor(Register, Register, Register),
    /// One's complement of first value, stored to second.
    Complement(Register, Register),
    /// Set the specified register to zero.
    Clear(Register),

    /// Set condition flags per comparison of two registers.
    Compare(Register, Register),
    /// Jump to specified label if first operand of previous `Compare` was
    /// greater than the second.
    BranchGreater(Label),
    /// Jump to label if operands of the previous `Compare` were not equal.
    BranchNotEqual(Label),
    /// Unconditionally jump to label.
    Jump(Label),

    /// Stop execution.
    Halt,
    /// Do nothing.
    Nop,

    /// A single byte of data (not a real instruction)
    Value(i8),
}

use self::Instruction::*;

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Store(ref r, ref l) => write!(f, "store {}, {}", r, l),
            Load(ref l, ref r) => write!(f, "load {}, {}", l, r),
            Move(ref s, ref d) => write!(f, "move {}, {}", s, d),
            Add(ref s1, ref s2, ref d) => write!(f, "add {}, {}, {}", s1, s2, d),
            Subtract(ref s1, ref s2, ref d) => write!(f, "sub {}, {}, {}", s1, s2, d),
            And(ref s1, ref s2, ref d) => write!(f, "and {}, {}, {}", s1, s2, d),
            Or(ref s1, ref s2, ref d) => write!(f, "or {}, {}, {}", s1, s2, d),
            Nand(ref s1, ref s2, ref d) => write!(f, "nand {}, {}, {}", s1, s2, d),
            Nor(ref s1, ref s2, ref d) => write!(f, "nor {}, {}, {}", s1, s2, d),
            Complement(ref s, ref d) => write!(f, "not {}, {}", s, d),
            Clear(ref r) => write!(f, "clear {}", r),
            Compare(ref r1, ref r2) => write!(f, "cmp {}, {}", r1, r2),
            BranchGreater(ref l) => write!(f, "bgt {}", l),
            BranchNotEqual(ref l) => write!(f, "bne {}", l),
            Jump(ref l) => write!(f, "jump {}", l),
            Halt => write!(f, "halt"),
            Nop => write!(f, "nop"),
            Value(ref x) => write!(f, "{}", x),
        }
    }
}
