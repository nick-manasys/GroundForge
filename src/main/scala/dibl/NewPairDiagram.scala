package dibl

import dibl.Force.Point
import dibl.Matrix.relativeSourceMap

import scala.scalajs.js.annotation.JSExport

@JSExport
object NewPairDiagram {

  @JSExport
  def create(config: Config): Diagram = {

    val itemMatrix = config.itemMatrix
    val rows: Int = itemMatrix.length
    val cols: Int = itemMatrix.head.length
    val northEastNode = toPoint(0, 0)
    val southWestNode = toPoint(rows - 1, cols - 1)
    var seqNr = 0

    def inc = {
      seqNr += 1
      seqNr
    }

    def isFringe(startPoint: Point): Boolean = {
      startPoint.x < northEastNode.x || startPoint.x > southWestNode.x || startPoint.y < northEastNode.y
    }

    def toNodeSeq(row: Int, col: Int): Seq[Node] = {
      val item = itemMatrix(row)(col)
      relativeSourceMap
        .get(item.vectorCode.toUpper)
        .map { case Array((leftRow, leftCol), (rightRow, rightCol)) =>
          val sourceLeft = toPoint(row + leftRow, col + leftCol)
          val sourceRight = toPoint(row + rightRow, col + rightCol)
          val target = toPoint(row, col)
          val colorName = Stitches.defaultColorName(item.stitch)
          val node = TargetNode(inc, target, sourceLeft, sourceRight, item.stitch, item.id, colorName)
          (isFringe(sourceLeft), isFringe(sourceRight)) match {
            case (false, false) => Seq(node)
            case (true, false) => Seq(node, SourceNode(inc, sourceLeft, target))
            case (false, true) => Seq(node, SourceNode(inc, sourceRight, target))
            case (true, true) => Seq(node, SourceNode(inc, sourceLeft, target), SourceNode(inc, sourceRight, target))
          }
        }.getOrElse(Seq.empty)
    }

    val nodes: Seq[Node] = Seq(SourceNode(0, Point(0, 0), Point(0, 0))) ++
      (0 until rows).flatMap(row =>
        (0 until cols).map(col =>
          toNodeSeq(row, col)
        )
      ).flatten

    // lookup table
    val nodeMap: Map[(Point, Point), Int] = nodes.map {
      case n: TargetNode => (n.target, n.target) -> n.seqNr
      case n: SourceNode => (n.source, n.target) -> n.seqNr
    }.toMap

    /** @param source the position of the node to look for
      * @param target position of a V's target, in case the source is the tip o a V
      *               the position of the target uniquely identifies the node
      * @return sequence number in the list of the nodes passed on to D3js
      */
    def findNodeSeqNr(source: Point, target: Point): Node = {
      // try to find a SourceNode, if not found, try a TargetNode, fall back to first dummy node
      nodes(nodeMap.getOrElse((source, target), nodeMap.getOrElse((source, source), 0)))
    }

    /** @param target the bottom of a V
      * @return the sides of the V as objects for D3js
      */
    def toPairLinks(target: TargetNode) = {
      val leftNode = findNodeSeqNr(target.srcLeft, target.target)
      val rightNode = findNodeSeqNr(target.srcRight, target.target)
      Seq(
        LinkProps.pairLink(
          source = leftNode.seqNr,
          target = target.seqNr,
          start = leftNode.color,
          mid = leftNode.twistsToLeftOf(target) - 1,
          end = target.color),
        LinkProps.pairLink(
          source = rightNode.seqNr,
          target = target.seqNr,
          start = rightNode.color,
          mid = rightNode.twistsToRightOf(target) - 1,
          end = target.color)
      )
    }

    var pairNr = 0
    Diagram(
      nodes.map {
        case SourceNode(_, Point(x, y), _) =>
          pairNr += 1 // TODO why does it start with 3?
          NodeProps.node(s"Pair $pairNr", x, y)
        case TargetNode(_, Point(x, y), _, _, stitch, id, color) => NodeProps.node(s"$stitch - $id", color, x, y)
      },
      nodes.withFilter(_.isInstanceOf[TargetNode])
        .flatMap { case target: TargetNode => toPairLinks(target) }
    )
  }

  private def toPoint(row: Double, col: Double) = {
    Point(x = 30 + 15 * col, y = 30 + 15 * row)
  }

  private trait Node {
    val seqNr: Int
    val target: Point
    val color: String

    def twistsToLeftOf(target: TargetNode) = 0

    def twistsToRightOf(target: TargetNode) = 0
  }

  /** @param seqNr  index in the list of nodes passed on to D3js
    * @param source one of the tips of a V, the legs may lie flat but never collapse
    * @param target the bottom of the V, makes the point unique when we have to search for it
    * @return
    */
  private case class SourceNode(override val seqNr: Int,
                                source: Point,
                                override val target: Point
                               ) extends Node {
    override val color = "pair" // the "color" is a named marker alias customized "arrow head"
  }

  /** @param seqNr    index in the list of nodes passed on to D3js
    * @param target   bottom of the v
    * @param srcLeft  left tip of the V
    * @param srcRight right  tip of the V
    * @param stitch   instructions for thread movements, sequence of ctrl (cross twist left-twist right-twist)
    * @param id       spreadsheet like index in the item matrix
    * @param color    belgian color code for the stitch
    */
  private case class TargetNode(override val seqNr: Int,
                                override val target: Point,
                                srcLeft: Point,
                                srcRight: Point,
                                stitch: String,
                                id: String,
                                override val color: String
                               ) extends Node {
    private val openingTwists: String = stitch.replaceAll("c.*", "").replaceAll("t", "lr")
    private val closingTwists = stitch.replaceAll(".*c", "").replaceAll("t", "lr")
    private val openingTwistsLeft: Int = openingTwists.count(_ == 'l')
    private val openingTwistsRight: Int = openingTwists.count(_ == 'r')
    private val closingTwistsLeft: Int = closingTwists.count(_ == 'l')
    private val closingTwistsRight: Int = closingTwists.count(_ == 'r')

    // stitches (X's) arranged in ascii art:
    // X-X source
    // -X- target
    // the caller knows if this (the source) is the left or right stitch

    override def twistsToLeftOf(target: TargetNode): Int = closingTwistsRight + target.openingTwistsLeft

    override def twistsToRightOf(target: TargetNode): Int = closingTwistsLeft + target.openingTwistsRight
  }

}