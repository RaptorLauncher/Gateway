# Gateway cable protocol

This protocol is based on
[Shinmera's wire protocol](https://shirakumo.github.io/lichat-protocol/) and
differes from it in the following ways:

  * Symbols are simplified. All symbols are written to the cable without any
    package information and read as gensyms (symbols without any home package).
  * Cable data is comparable using the `CABLE-EQUAL` function.
  * The protocol allows for asynchronous reading of messages. If the `FROM-WIRE`
    function is unable to complete a message while reading, it returns NIL as
    its primary value and the string read so far as the secondary value. The
    function `FROM-WIRE-BUFFERED` uses a simple internal buffer to capture this
    string for use in later read attempts.

## License

This module is licensed under Artistic License 2.0.
