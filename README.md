# Battle

Battle is a small text-based game written in Idris and JavaScript.

It accompanies [this blog post](https://medium.com/the-web-tub/idris-state-machines-in-javascript-apps-b969e2cb6ed2).

The game is a normal Idris state machine, with some extra jiggery-pokery to get persistent state working with JavaScript event listeners.

Works with Idris 1.2.0.

## Running

Compile with:

```
idris --codegen javascript -o battle.js Battle.idr
```

To run, open battle.html in a browser window.
