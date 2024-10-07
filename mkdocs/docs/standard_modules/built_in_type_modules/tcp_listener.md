# TCP Listener
All the functions in this module can only be self-called on the tcp-listener user-object, if that function expects a tcp-listener user-object as the first argument.

#### `<tcp-stream>.addr(self: tcp-stream): string?`
Returns the local address of the listener.

#### `<tcp-listener>.accept(self: tcp-listener): tcp-stream?`
Waits for the next incoming connection and returns the tcp-stream, if it doesn't time out.
