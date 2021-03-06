# Topological exploration of patterns

The bridges page explores the interleaved patterns of https://tesselace.com/research/bridges2012/

The 4x4 and misc pages explore patterns of the [inkscape plugin]. Some of the checker board matrices happen to be valid when stacked as a brick wall.

## Collect PNG thumbnails in a web page

Requirements: InkScape, Bash

* The web pages add the matrix strings as id's on the SVG elements. These id's are used to create links from the PNG thumbnails to the advanced page.
* Let the animation of the web page complete.
* Use the inspector of a browser like FireFox to copy all the SVG objects into a file, say: thumbs.txt. Copy the innerHTML of the div with id "diagrams" to get all the SVG objects at once.
* Make sure each `<svg>` element starts at a new line and the file(s) end with a new line.
* Execute the following commands, they:
  1 split the files
  2 convert SVG files to PNG files
  3 create a thumbnail page using the id's of the SVG root element to create links to the advanced page.

            cat thumbs.txt | awk '/<svg id/ {n++}{print > sprintf("%03d.svg", n)}'
            for i in *.svg; do '/C/Program Files/Inkscape/inkscape.exe' $i --export-png=`echo $i | sed -e 's/svg$/png/'`; done
            grep svg *.svg | sed -e 's!" .*!!' -e 's!\([0-9][0-9][0-9]\)[^"]*"\(checker\|brick\) \(.*\)!<a href="index.html?matrix=\3\&\2s=on"><img src="\1.png"></a>!' > tmp.html

* **Note**: Reducing the initial [scale] of the diagrams is not supported since the upgrade to v4 of D3js.

[inkscape plugin]: https://github.com/d-bl/inkscape-bobbinlace/tree/master/input/lace_ground/checker
[scale]: https://github.com/d-bl/GroundForge/commit/be75dde4e74f6255677154b67696209236a43802#diff-d954fe9b7e3c4b571ef0e2859242bb80L124