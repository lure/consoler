This document serves as a remainder and backlog. Better ignore it.

**Commands as general:**
- One command per line.
- Command tokens are separated by one or more spaces or tabs. 
- On parse error display appropriate message with explanation what went wrong.
- Every command parsed to Case Class (CC)
- CC comprise execution stack, allowing for undo, redo will require a position in a stack. New command after redo truncates the stack.
- CC contains action function which encodes operation to perform with arguments as a seed

**State:**
- Initial, allowed commands are: C, Q
- Draw state, allowed: L, R, B, Q 
- If command cannot be  parsed, current state is kept, error message is displayed, promt for new command.

---
**Commands, thoughts**

**Color:** 
- coded by a letter. => color is always a single letter.
- next color application overwrites previous if any.

**Grid:**
- limited size of x  y  axis
- stored as a Char[x * y]. Array per line seems to be memory inneficient. Of char - to store colors
- checks must be performed against L, T, R, B boundaries.
- separate canvas from drawing procedure, so we could use any canvas inctead of console


**Square:**
1. what if one or more square lines are out of grid dimensions?

**Line:**
1. what if start or end is out of grid?
2. how to draw diagonal lines.
	- how to fill color when lines are diagonal...
	
___
**Algos**
Fill = WeightedQuickUnionUF / flood fill, and the pwoblem is to detect that some isles in the middle of the "river" must not 
prevent the whole river from being colored, and the isles must preserve their color.
So, solution: 4-way queue floodFill (stack implementatin WILL crash as java has quite small stack)

advanced technic Scan Line Algorithm

---
nextCommand call has caveat: no reason to display parsing errors if parsed command is not applicable to
current scope. That could be achieved by augmenting Left[String] with command type. No time for it now.
