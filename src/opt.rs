pub use super::instr::{self, Instruction, Label, Register};

pub fn optimize(program: &mut Vec<(Label, Instruction)>) {
    eliminate_zero_loads(program);
    eliminate_noplike_jumps(program);
}

fn eliminate_zero_loads(program: &mut [(Label, Instruction)]) {
    // Loads of a constant zero can be transformed into a 'clear' instruction
    // and the constant can be eliminated.
    for &mut (ref mut label, ref mut instruction) in program.iter_mut() {
        // Load from '#0'?
        let mut target_reg: Option<Register> = None;
        if let &mut Instruction::Load(ref label, ref reg) = instruction {
            if let &Label::Name(ref name) = label {
                if name == "#0" {
                    target_reg = Some(*reg);
                }
            }
        }
        // If yes, replace with clear
        if let Some(reg) = target_reg {
            *instruction = Instruction::Clear(reg);
        }

        // Definition of #0?
        let remove_def = match label {
            &mut Label::Name(ref n) if n == "#0" => true,
            _ => false,
        };
        if remove_def {
            // If yes, eliminate it
            assert_eq!(*instruction, Instruction::Value(0));
            *instruction = Instruction::Nop;
            *label = Label::None;
        }
    }
}

fn eliminate_noplike_jumps(program: &mut [(Label, Instruction)]) {
    // Nop-like jumps (which go to the following instruction) are useless,
    // and not permitted by the assembler.
    //
    // This implementation is kind of inefficient.
    loop {
        let mut modified = false;

        for i in 0..program.len() {
            // Find a branch and record its target
            let target_label = {
                let (_, ref mut instruction) = program[i];
                match instruction {
                    &mut Instruction::BranchNotEqual(ref l)
                    | &mut Instruction::BranchGreater(ref l)
                    | &mut Instruction::Jump(ref l) => {
                        // Must move so we no longer borrow from `program`
                        l.clone()
                    }
                    // Any other instruction is ignored
                    _ => continue,
                }
            };

            // Find the target label
            let mut label_idx = None;
            for (idx, &(ref label, _)) in program.iter().enumerate() {
                if label == &target_label {
                    label_idx = Some(idx);
                    break;
                }
            }
            let label_idx = label_idx.expect("Jump targeted label which was not found");
            debug!(
                "Jump with target {}, span={}..{} ",
                target_label, i, label_idx
            );

            if label_idx <= i {
                // Does not jump forward, cannot be elided
                continue;
            }

            // Can elide jump if there are only nops between it and the destination
            let all_nops = program[i + 1..label_idx]
                .iter()
                .all(|&(_, ref instr)| match instr {
                    &Instruction::Nop => true,
                    _ => false,
                });
            if all_nops {
                debug!("Range is NOP; stripping jump at index {}", i);
                program[i].1 = Instruction::Nop;
                modified = true;
            }
        }

        // Terminate when we make a pass without making any replacements.
        if !modified {
            break;
        }
    }
}
