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

This session was run in two separate Common Lisp REPL's on the same machine.

#### Server Session

``` lisp
* (asdf:load-system "coalton-io/examples")
T
* (io/examples/redis:run-server)
Press enter to close the server...
listening on 127.0.0.1:5556
client connected
Received command: #.(IO/EXAMPLES/REDIS/PROTOCOL:PING "Hello redis server")
Sending response: #.(IO/EXAMPLES/REDIS/PROTOCOL:RESPBULKSTRING "Hello redis server")
Received command: #.(IO/EXAMPLES/REDIS/PROTOCOL:SETKEY "a" "100")
Sending response: #.(IO/EXAMPLES/REDIS/PROTOCOL:RESPSIMPLESTRING "OK")
Received command: #.(IO/EXAMPLES/REDIS/PROTOCOL:GETKEY "a")
Sending response: #.(IO/EXAMPLES/REDIS/PROTOCOL:RESPBULKSTRING "100")
Received command: #.IO/EXAMPLES/REDIS/PROTOCOL:SAVE
Sending response: #.(IO/EXAMPLES/REDIS/PROTOCOL:RESPSIMPLESTRING "OK")
Received command: #.(IO/EXAMPLES/REDIS/PROTOCOL:SETKEY "a" "200")
Sending response: #.(IO/EXAMPLES/REDIS/PROTOCOL:RESPSIMPLESTRING "OK")
Received command: #.(IO/EXAMPLES/REDIS/PROTOCOL:GETKEY "a")
Sending response: #.(IO/EXAMPLES/REDIS/PROTOCOL:RESPBULKSTRING "200")
Received command: #.IO/EXAMPLES/REDIS/PROTOCOL:LOAD
Sending response: #.(IO/EXAMPLES/REDIS/PROTOCOL:RESPSIMPLESTRING "OK")
Received command: #.(IO/EXAMPLES/REDIS/PROTOCOL:GETKEY "a")
Sending response: #.(IO/EXAMPLES/REDIS/PROTOCOL:RESPBULKSTRING "100")
Received command: #.IO/EXAMPLES/REDIS/PROTOCOL:QUIT
Sending response: #.(IO/EXAMPLES/REDIS/PROTOCOL:RESPSIMPLESTRING "OK")
client disconnected
```

#### Client Session

``` lisp
* (asdf:load-system "coalton-io/examples")
T
* (io/examples/redis:run-client)
connected to 127.0.0.1:5556
Type --help for commands.
redis> PING "Hello redis server"    
Hello redis server
redis> SET a 100
OK
redis> GET a
100
redis> SAVE
OK
redis> SET a 200
OK
redis> GET a
200
redis> LOAD
OK
redis> GET a
100
redis> QUIT
OK
COALTON::UNIT/UNIT
```

## Example Session - Actual Production Client

The example server and client are compatible with a subset of commands from the production Redis client and server. Below is an example session using the Docker image of the production Redis client to connect with the coalton-io Redis example server.

#### Coalton-IO Server

``` lisp
* (asdf:load-system "coalton-io/examples")
T
* (io/examples/redis:run-server)
Press enter to close the server...
listening on 127.0.0.1:5556
client connected
Received command: #.(IO/EXAMPLES/REDIS/PROTOCOL:PING #.COALTON:NONE)
Sending response: #.(IO/EXAMPLES/REDIS/PROTOCOL:RESPSIMPLESTRING "PONG")
client disconnected
client connected
Received command: #.(IO/EXAMPLES/REDIS/PROTOCOL:SETKEY "mykey" "hello")
Sending response: #.(IO/EXAMPLES/REDIS/PROTOCOL:RESPSIMPLESTRING "OK")
client disconnected
client connected
Received command: #.(IO/EXAMPLES/REDIS/PROTOCOL:GETKEY "mykey")
Sending response: #.(IO/EXAMPLES/REDIS/PROTOCOL:RESPBULKSTRING "hello")
client disconnected
```

#### Redis Production Client

``` sh
[]$ docker run --rm -it --network host redis:7-alpine redis-cli -h 127.0.0.1 -p 5556 PING
PONG
[]$ docker run --rm -it --network host redis:7-alpine redis-cli -h 127.0.0.1 -p 5556 SET mykey hello
OK
[]$ docker run --rm -it --network host redis:7-alpine redis-cli -h 127.0.0.1 -p 5556 GET mykey
"hello"
```
