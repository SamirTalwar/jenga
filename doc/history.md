
## History of Jenga

This project is a redo of the Jenga build system, which I first wrote whilst working for Jane Street in around 2010.
The original version of Jenga was used at Jane Street after I left in 2016 and right up until 2024.
[As discussed in this Jane Street Blog](https://blog.janestreet.com/how-we-accidentally-built-a-better-build-system-for-ocaml-index/).

Jane Street has now switched to using the Dune build system.
Dune is one of the better build systems out there, although it is focussed on building just Ocaml,
whereas Jenga (original and this redo) are language agnostic build systems.

Recently I was nerd-sniped into re-implementing Jenga after discussion with friends.
I was quite proud of the original Jenga; I really like the concept of a general purpose build system.
This time I hoped to make something even better.
In particular, original Jenga lacked some features -- proper sandboxing and full shared caching -- which I now regard as critical.
This redo is designed with support for these features from the outset.
Other features that the original Jenga got right -- dynamic dependencies and dynamic rule construction -- remain part of this new Jenga.

This redo of Jenga shares no code with the original.
The original was written in Ocaml; this redo is written in Haskell.
I have not looked at the code of original since I left Jane Street in 2016.
However, my experience and opinions of what features are important are strongly influenced by the original version of Jenga.

Since original Jenga no longer exists, even within Jane Street, I refer to the new version simply as Jenga without confusion.
