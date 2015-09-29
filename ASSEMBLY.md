# RISC G Assembly Language Specification

`Ra`, `Rb`, and `Rc` stand for either `r0` or `r1`, and `L` stands for a label
that must appear elsewhere in the program. Everything between the two characters
`//` and the end of the line is treated as a comment and is ignored.

`store Ra, L`
:   Store the value stored in Ra into L

`load L, Ra`
:   Load the value stored at L into Ra

`move Ra, Rb`
:   Move a value from Ra to Rb

`nop`
:   Do nothing

`cmp Ra, Rb`
:   Compare the values of Ra and Rb, setting condition codes

`add Ra, Rb, Rc`
:   Add the values of Ra and Rb, put result in Rc

`sub Ra, Rb, Rc`
:   Subtract the values of Ra and Rb, put result in Rc

`and Ra, Rb, Rc`
:   And the values of Ra and Rb, put result in Rc

`or Ra, Rb, Rc`
:   Or the values of Ra and Rb, put result in Rc

`nand Ra, Rb, Rc`
:   Nand the values of Ra and Rb, put result in Rc

`nor Ra, Rb, Rc`
:   Nor the values of Ra and Rb, put result in Rc

`not Ra, Rb`
:   Complement the value of Ra, put result in Rb

`clear Ra`
:   Store zero into Ra.

`halt`
:   Halt the computer.

`jump L`
:   Execute the instruction labeled L next.

`bgt L`
:   If in the last arithmetic operation or comparison
    the first operand was greater than the second
    operand, execute the instruction labeled `L` next.

`bne L`
:   If in the last arithmetic operation or comparison
    the operands were unequal, execute the instruction
    labeled `L` next.
