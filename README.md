# Athena
Athena is a CPU build for control systems written in Chisel.



### General architecture

### Floating point arithmetic
Athena supports both addition, multiplication and division of floating point numbers.
##### Division
Division is implemented using Goldschmidt's algorithm, which works by using reciprocal estimation. The design is fully pipelined and has a 17 clock cycle delay.
##### Multiplication
Multiplication is implemented using the built in multiplication operator along with partial product generation. The design is fully pipelined and has a 7 clock cycle delay.


##### Build



##### Configuration


