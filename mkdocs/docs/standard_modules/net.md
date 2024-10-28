# Net Module

#### `net.bind(addr: string, port: int): tcp-listener?`
Creates a new TCP-listener if it can which will be bound to the specified `addr` and `port`. If the port is invalid, it will throw an error

#### `net.connect(addr: string, port: int): tcp-stream?`
Opens a TCP connection to a remote host as a TCP-stream with the address `addr` and port `port` if it can.

## TCP Listener
All the functions in this module can only be self-called on the tcp-listener user-object, if that function expects a tcp-listener user-object as the first argument.

#### `<tcp-stream>.addr(self: tcp-stream): string?`
Returns the local address of the listener.

#### `<tcp-listener>.accept(self: tcp-listener): tcp-stream?`
Waits for the next incoming connection and returns the tcp-stream, if it doesn't time out.

## TCP Stream
All the functions in this module can only be self-called on the tcp-stream user-object, if that function expects a tcp-stream user-object as the first argument.

#### `<tcp-stream>.local_addr(self: tcp-stream): string?`
Returns the local address of the stream.

#### `<tcp-stream>.peer_addr(self: tcp-stream): string?`
Returns the peer address of the stream.

#### `<tcp-stream>.read(self: tcp-stream): string?`
Reads from the stream if there is anything to read.

#### `<tcp-stream>.write(self: tcp-stream, msg: string): int`
Writes to the stream and returns the amount of bytes sent (0 if message failed to send).

#### `<tcp-stream>.flush(self: tcp-stream)`
Flushes the stream.
