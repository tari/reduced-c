//! Machine representation of instructions.

/// A single memory location
pub type Addr = u8;

#[derive(PartialEq, Eq, Debug)]
/// A register, such as `Register(0)` for `r0`.
pub struct Register(pub u8);

#[derive(PartialEq, Eq, Debug, Clone)]
/// A memory reference.
pub enum Label {
    /// No name and address not yet resolved.
    None,
    /// Known location in memory.
    Address(Addr),
    /// Abstract location, with address not yet resolved.
    Name(String)
}

#[derive(PartialEq, Eq, Debug)]
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

