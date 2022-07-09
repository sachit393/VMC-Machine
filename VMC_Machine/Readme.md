
# VMC Machine

## Design Descriptions - 

- The VMC Machine consists of a structure Vmc with signature VMC
- The functions accessible to the user are rules, execute, toString whose descriptions are as per the specifications of the assignment
- The rules are implemented as a series of if else statements with case statements to account for various semantics rules
- A rule is added for appending one stack over the other in the signature of FunStack, rest all things are same
- Internally the Stack is implemented as a recursively defined datatype  = EmptySt, Cons of 'a*'a Stack
- These are contained in VMC.sml
- There is a file called postfix.sml, which contains a structure PostFixConverter containing function postfix which converts the AST to postfix form
- The main file is names as main.sml
- I have continuously kept a track of how many operands are required and how many operators are there which helps us to make boundaries between different commands
- Contents are poured from C stack to V stack and V stack to C stack in case of Wh and Ite commands to handle cases of nested whiles and if then elses

- I have taken care of the following runtime errors - 
    - Division by zero
    - A boolean variable whose value is read as int(value other than 0 or 1)
    - When the given input is not an int(cannot be interpreted as int or bool)
## Acknowledgements
- I also had discussions regarding possible alternatives of using 2 stacks for rules and how 2 stacks with memory are turing complete
- Array structure documentation, hashtable documentation, list documentation


