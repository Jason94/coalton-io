# Instructions

To run the Redis program, open up two repls. Run the server in one, and run the client in another.

#### Run the Server

```lisp

CL-USER> (asdf:load-system "coalton-io/examples")
CL-USER> (in-package :io/examples/redis)
IO/EXAMPLES/REDIS> (run-server)
```

#### Run the Client

```lisp
CL-USER> (asdf:load-system "coalton-io/examples")
CL-USER> (in-package :io/examples/redis)
IO/EXAMPLES/REDIS> (run-client)
```

## Example Session
