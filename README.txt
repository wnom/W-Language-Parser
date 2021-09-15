William Norman
428002797
314-500

For my commentary I'm going to explain what my syntax design choices for the W language were and how I 
implemented them.  
	So, I'll start at the top and say I designed that every statement must end with a semicolon 
exluding brackets for the blocks and if/while statements.  The variable declaration and assign statements are similar syntax to C.
For If statements I implemented so that they can be written with brackets and a block of statements of just a single statement and 
no brackets, but every if statement must come with an else as well.  For the while loop I implemented in a similar way with
the option for a single statement or a block.  And finally, I designed a block so that it starts and ends with brackets and can
either contain a single or many statements.
	Now, for expressions I had to put in a bit more legwork.  I started by implementing all the literals.  intLiteral takes in
a choice for pos or negative.  Bool literal reads the identifier and checks if it is "true" or "false" (I also gave an option
for true and false to be capitolized as well).  Var literal reads a var and makes that type using the string it read.  And finally
I implemented a not literal that takes an expression and "nots" said expression.  I then implemented all of the expressions in order
in the manner given by the expression parsing example starting from least precedence (boolean operators) to highest precedence(Not).
And at the end included all of my literal statements.
	In conclusion, this was my design choice for my W language and I believe I made it prety close to a higher level language.
Anyways, thanks for reading this if you got all the way through.  This project was really cool and I thoroughly enjoyed it.
