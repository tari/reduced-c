pub use super::instr::{self, Label, Instruction, Register};

pub fn optimize(program: &mut Vec<(Label, Instruction)>) {
    // Loads of a constant zero can be transofrmed into a 'clear' instruction
    // and the constant can be eliminated.
    // TODO lifetime analysis can make this even smarter, determine when
    // variables can be assumed to be zero as well.
    for &mut (_, ref mut instruction) in program.iter_mut() {
        let mut target_reg: Option<Register> = None;
        if let &mut Instruction::Load(ref label, ref reg) = instruction {
            if let &Label::Name(ref name) = label {
                if name == "#0" {
                    target_reg = Some(*reg);
                }
            }
        }

        if let Some(reg) = target_reg {
            *instruction = Instruction::Clear(reg);
        }
    }
    // TODO remove the now-unused constant

}
