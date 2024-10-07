# Net Module

#### `net.bind(addr: string, port: int): tcp-listener?`
Creates a new TCP-listener if it can which will be bound to the specified `addr` and `port`. If the port is invalid, it will throw an error

#### `net.connect(addr: string, port: int): tcp-stream?`
Opens a TCP connection to a remote host as a TCP-stream with the address `addr` and port `port` if it can.