# A* Pathfinding with Elm

![Example of a path found between two points](./snap.png)

Quick experiment porting [`Data.Graph.AStar`](https://hackage.haskell.org/package/astar-0.3.0.0/docs/Data-Graph-AStar.html) to Elm.

Using a local 0.18-upgraded version of [elm-pairing-heap](http://package.elm-lang.org/packages/rhofour/elm-pairing-heap/latest) for the priority queue.

Comes with a demo: `npm install` then `npm start` to launch an editable grid with a path that will route around the walls you build.