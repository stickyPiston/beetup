To run the frontend, run the following commands. You need to have nix installed. You can also install elm and elm-live manually via npm if you do not have nix installed.

```bash
nix-shell
elm-live src/Main.elm --pushstate --dir=public --start-page=index.html -- --output=public/main.js --debug
```
