# TCP Stream
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
