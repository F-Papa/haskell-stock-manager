# Haskell Stock Management Server

Simplistic stock management server made in Haskell to try out some of its features for concurrency.

The server's stock is read upon startup from: `./data/stock.txt`

When the server receives a message from the client, it tries to reserve the quantity requested by the client, which is "locked" until the transaction succeeds or fails.

The transaction is simulated asynchronously and has a 50% probability of success.
If it succeeds, the reserved stock is "sold". Otherwise, it is released and available once again.

NOTE: The client doesn't know if the transaction was successful or not.

## How to Run
1. Run the server (port argument is optional and 3000 by default)
```bash
cabal run -- <port>
```

2. Connect to the server via TCP

3. Send messages to the server (see [Protocol](#protocol))


## Protocol
Packets consist of one line with two ints representing `ProductId` and `Quantity`.

e.g.: A client reserves `30` items of product `1`

```json
1,30 
```
