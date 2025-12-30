## Generate Docs

To generate docs, run this command in a REPL:

```lisp
(asdf:load-system "coalton-io/docs")
```

Once the system has been loaded, regen the docs by running:

```lisp
(coalton-io/docs:write-docs)
```
