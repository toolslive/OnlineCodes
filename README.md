This is an implementation of Maymounkov's online codes,
which are described in the paper
[rateless codes and big downloads](http://pdos.csail.mit.edu/~petar/papers/maymounkov-bigdown-lncs.ps)

This implementation does not use the suggested lazy decoding, 
but uses the classical strategy of reducing the matrix to a right triangle 
combined with back substitution.

