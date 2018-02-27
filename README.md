# Battle

Battle is a small text-based game written in Idris and JavaScript.

It accompanies this blog post (add link once published).

The game is a normal Idris state machine, with some extra jiggery-pokery to get persistent state working with JavaScript event listeners.

## Running

Compile with:

```
idris --codegen javascript -o battle.js Battle.idr
```

To run, open battle.html in a browser window.
