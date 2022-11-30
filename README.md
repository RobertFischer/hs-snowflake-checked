# SnowChecked

## A Checksummed UID Generator based on Twitter's Snowflake

Unique ids are useful, but traditional GUID/UUID formats are lengthy, inefficient, and hard for humans to use. Twitter
created a novel format for UIDs called ["Snowflake"](https://developer.twitter.com/en/docs/twitter-ids) which addressed
these issues, with the added benefit that the UIDs monotonically increase over time.
This library extends the Snowflake format by adding checksum bits at the end. If you use this library with
the number of checksum bits set to 0, then you have a Snowflake implementation.

This extension is valuable because the checksum detects error on input. If you're using ids in a human setting
(eg: having users type them in or scan them from QR codes), then the checksum is valuable to catch typos,
miscommunications, and other input issues.

Like Snowflake, this algorithm uses some bits from the timestamp, some bits from a counter, and some bits of the node id.
This algorithm extends Snowflake by also using some bits to store the checksum, which derives from the sum of the other
parts.

This implementation allows the number of bits in the id to range from 0 bits to 255^4 bits. The default configuration uses
64 bits, with 40 bits used for time, 10 bits used for the counter, 8 bits used for the node id, and 6 bits for the checksum.
The odds of a false positive on the checksum is `1/(2^checkbits)`, so the odds of a false positive in the default configuration
is ~1.5%. 

## Encoding

If you want to marshall a flake, then you can use the encodings in the `Encoding` sub-packages of this library. Implementations
include a `ByteString` (strict or lazy), any `Text`-like value (using base 16), or any `Integral` value.

# Credit

This project derives distantly from the [`snowfake` package on Hackage](https://hackage.haskell.org/package/snowflake).
