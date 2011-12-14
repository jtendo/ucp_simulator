EMI/UCP simulator - A hunky Erlang emulating ERMES/UCP-based EMI protocol server
================================================================================

The simulator is an application for near-to-live testing of your EMI/UCP
applications without need of access to real SMSC. The application behaves as
a real SMSC with UCP interface. Your application can bind to it, send messages,
etc., however nothing will get delivered anywhere as all the responses are
only made-up by the simulator.

The simulator is based on LogicaCMG's EMI/UCP interface 4.6 specification.

Usage
-----

1. Install:

    ```
    $ make initenv
    ```
    
2. Run

    ```
    $ make initenv
    ```

Configuration
-------------

- `listen_port`: the listening port (default: 7777)


TODO
----

- TRN checking
- Syntax checking
- UCP windowing (flow control)
- Concatenated messages checking
- Sending messages from server to clients (CMD 52)
- Delivery notifications
- ...


License
-------

BSD License -- http://www.opensource.org/licenses/bsd-license.php

