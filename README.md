# [DEMO](https://d-bl.github.io/TesseLaceD/)
TesseLaceD : [TesseLace] Diargrams - variable stitches and thread styles in matching pair an thread diagrams.

The code under `src/main/scala` generates  alternative data for `sample.js` in the [gh-pages branch].
The patterns are generated from predefined [matrices] found by [TesseLace.com].

[matrices]: https://github.com/d-bl/TesseLaceD/blob/3158d5de673af09c9569a17737f07f6c5e8afa15/src/main/scala/dibl/Matrix.scala#L91-L120
[TesseLace]: http://TesseLace.com
[TesseLace.com]: http://TesseLace.com


## Compile and preview

Requirements

- [sbt] 0.13.7 or higher
- a browser with proper SVG support, for example FireFox or Safari but not Internet Explorer.
  Chrome has proper SVG support but with the default settings it has intranet problems with the development view.


Steps

- Make a local clone of the project, it can be convenient to have two local clones,
  one for the master branch, one for the gh-pages branch.
- Go to the directory of your local clone with the master branche
- Start the command `sbt ~fastOptJS`
- Monitor the result on `http://localhost:12345/target/scala-2.11/classes/index-dev.html`
  It is a dressed down version of the demo page, with experimental features added.
  Nodes and links invisible in the demo page are shown faint for debugging purposes.
- Saving code changes updates the page automatically but not completely properly.


## Debug a specific pattern

Configure the diagram with an [encoded matrix] in the parameter-less [apply method] of the Settings class.
Follow the steps above to view the result.
Run the [ad-hoc] test of ThreadDiagramSpec to step through the code or analyse the println-s.
Note that a `println` translates to console.log and can be analysed by a browser too.

[encoded matrix]: https://github.com/d-bl/TesseLaceD/blob/3158d5de673af09c9569a17737f07f6c5e8afa15/src/main/scala/dibl/Matrix.scala#L122-L138
[apply method]: https://github.com/d-bl/TesseLaceD/blob/3158d5de673af09c9569a17737f07f6c5e8afa15/src/main/scala/dibl/Settings.scala#L44-L46
[ad-hoc]: https://github.com/d-bl/TesseLaceD/blob/fa4698f3282c6eb9974447caa395b273ec4db697/src/test/scala/dibl/ThreadDiagramSpec.scala#L56


## Important code conventions

- Don't catch exceptions in a `Try` but prevent them to create a `Failure` for safe execution with JavaScript.
- Restrict the use of raw js objects to the `D3Data` class to allow execution of test classes with a recent JVM.


## Unit tests

The command `sbt test` only compiles the test classes.
Haven't found the proper incantation to execute the test with sbt.

With the project imported in Intellij it is possible to execute the test classes one by one.
Launching tests may cause problems when sbt is still processing a change, just try again.


## Publish

- Compile with `sbt ~fullOptJS`
- Copy the content of `target\scala-2.11\tessedlace-opt.js` in the master branch
  into `matrix-graphs.js` in the [gh-pages branch]
- Check the results with `index.html`
- If ok: commit, push and check the online demo in your own github fork: `http://YOURID.github.io/TesseLaceD/`

[sbt]: http://www.scala-sbt.org/download.html
[gh-pages branch]: https://github.com/d-bl/TesseLaceD/tree/gh-pages
