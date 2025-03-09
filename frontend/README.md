To run the frontend, run the following commands:

```bash
nix-shell
elm-live src/Main.elm --pushstate --dir=public --start-page=index.html -- --output=public/main.js --debug
```