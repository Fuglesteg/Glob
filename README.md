# Glob

My submission to the [Spring Lisp Game Jam 2025](https://itch.io/jam/spring-lisp-game-jam-2025/rate/3569976).
The first game made using [Slither](https://github.com/fuglesteg/slither)

It's a simple game similar to, and inspired by [Agar.io](https://agar.io). You eat other blobs to
gain size and become the biggest. You win by growing big enough to eat the whole
arena.

Glob started out as a test project for shader development using Slither. The
intention was to make a screensaver. When the game jam started I though it
would be fun to try and turn it into a game.

An executable can be built for linux, presuming you have guix installed. By
simply running the `build-linux.lisp` script. It should download and load
dependencies using guix and build the program using `deploy`.

On windows the build presumes that you have quicklisp installed in you
`.sbclrc`. If it is you should be able to build by calling `sbcl --script
build-windows.lisp`
