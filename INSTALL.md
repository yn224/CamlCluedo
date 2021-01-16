## How to play our game

Similar to the projects that we have done in this class (CS 3110), we have a `Makefile` that will automatically build our project.

* `make build` will build our system and verify any errors in our scripts.
* `make test` will run our OUnit2 test cases to check that our implementation works properly.
* `make play` launches our board game. Users have access to a help message that can be toggled throughout the game. Follow the help manual if lost at anytime
  * We support both multiplayer and single-player modes of our game. For any issues, please contact us.

To obtain any documentation information, you can run `make docs` to get an automatically generated HTML files containing all of our specifications. Please use the public version of it.

For our fellow developers: To meaningfully contribute, please make use of `make check` command to see whether any changes that you have made on top of our design has caused in changes in type signatures of functions or variables.

We have not used any third-party libraries except for those provided by the OCaml standard library, so there is no special command that you have to run to install any OPAM packages, given that the OCaml is installed properly.

Last revised: 12/20/2020
