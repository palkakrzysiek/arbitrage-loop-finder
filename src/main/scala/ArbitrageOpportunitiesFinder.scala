package com.kpalka

import scala.collection.mutable

case class Conversion(from: String, to: String)

class Vertex[A](val id: A, var distance: Double = Double.MaxValue, var predecessor: Option[Vertex[A]] = None) {
  override def toString: String = id.toString
}
class Edge[A](val from: Vertex[A], val to: Vertex[A], val weight: Double) {

  override def toString: String = s"(${from.toString} -> ${to.toString}: ${weight})"
}
object Edge {
  def unapply[A](edge: Edge[A]): Option[(Vertex[A], Vertex[A], Double)] = Some(edge.from, edge.to, edge.weight)
}

object TarjanAlgorithm {

  def splitByStronglyConnectedComponents[A](edges: Seq[Edge[A]]): Set[Map[Vertex[A], Seq[Edge[A]]]] = {

    var index = 0
    val stack = mutable.Stack.empty[Vertex[A]]
    val onStack = mutable.Set.empty[Vertex[A]]
    val indexes = mutable.Map.empty[Vertex[A], Int]
    val lowLinks = mutable.Map.empty[Vertex[A], Int]
    val stronglyConnectedComponents = mutable.Set.empty[Map[Vertex[A], Seq[Edge[A]]]]

    // complexity O(E), E - the number of edges
    val edgesByVertex = edges
      .foldLeft(Map.empty[Vertex[A], Seq[Edge[A]]])((acc, edge) =>
        acc + (edge.from -> (acc.getOrElse(edge.from, Seq.empty[Edge[A]]) :+ edge))
      )

    // complexity O(V + E), V - the number of vertices, E - the number of edges
    for (vertex <- edgesByVertex.keys) {
      if (!indexes.isDefinedAt(vertex))
        stronglyConnect(vertex)
    }

    def stronglyConnect(vertex: Vertex[A]): Unit = {
      indexes(vertex) = index
      lowLinks(vertex) = index
      index += 1
      stack.push(vertex)
      onStack.add(vertex)

      for (Edge(from, to, _) <- edgesByVertex.getOrElse(vertex, Seq.empty)) {
        if (!indexes.isDefinedAt(to)) {
          stronglyConnect(to)
          lowLinks(from) = Math.min(lowLinks(from), lowLinks(to))
        } else if (onStack.contains(to)) {
          lowLinks(from) = Math.min(lowLinks(from), indexes(to))
        }
      }

      if (lowLinks(vertex) == indexes(vertex)) {
        val currentSCC = mutable.Map.empty[Vertex[A], Seq[Edge[A]]]
        var next = stack.pop()
        while (next != vertex) {
          onStack.remove(next)
          currentSCC += next -> edgesByVertex(next)
          next = stack.pop()
        }
        currentSCC += next -> edgesByVertex.getOrElse(next, Seq.empty)
        stronglyConnectedComponents += currentSCC.toMap
      }

    }
    stronglyConnectedComponents.toSet
  }
}

object BellmanFordAlgorithm {

  def findNegativeCycle[A](edges: Seq[Edge[A]]): Option[Seq[Vertex[A]]] = {

    val startVertex = edges.head.from
    startVertex.distance = 0

    def pathIsShorterWith(edge: Edge[A]): Boolean = edge.from.distance + edge.weight < edge.to.distance

    val verticesCount = edges.foldLeft(Set.empty[Vertex[A]])((acc, edge) => acc + edge.from + edge.to).size

    // complexity O(E * V)
    // there are (V - 1) * E loops here to make sure all vertices had their distance from root vertex calculated
    // the final V * E iterations are made to detect negative cycle in the subsequent loop
    for (_ <- 1 until verticesCount) {
      for (edge <- edges) {
        if (pathIsShorterWith(edge)) {
          edge.to.distance = edge.from.distance + edge.weight
          edge.to.predecessor = Some(edge.from)
        }
      }
    }

    var detectedCycle: Option[Seq[Vertex[A]]] = None

    // final V * E iterations
    for (edge <- edges if detectedCycle.isEmpty) {
      if (pathIsShorterWith(edge)) {
        // cycle detected
        val cycle = mutable.ArrayBuffer.empty[Vertex[A]]
        val alreadyInCycle = mutable.Set.empty[Vertex[A]]
        var vertex = edge.from
        cycle += vertex
        alreadyInCycle += vertex
        var shorterCycleExists = false
        // trace back the cycle
        while (vertex != edge.to && !shorterCycleExists) {
          vertex = vertex.predecessor.get
          if (alreadyInCycle.contains(vertex)) {
            shorterCycleExists = true
          } else {
            cycle += vertex
            alreadyInCycle += vertex
          }
        }
        // previous loop stops on returning to the start edge, add it here
        cycle += edge.from
        detectedCycle = if (shorterCycleExists) None else Some(cycle.toSeq)
      }
    }
    detectedCycle
  }
}

object JsonParser {
  private def parsePair(pair: String): Conversion = {
    val split = pair.split("_")
    Conversion(split(0), split(1))
  }
  def parse(str: String): Map[Conversion, Double] = {
    ujson
      .read(str)
      .obj
      .map { case (pair, rate) =>
        (parsePair(pair), rate.str.toDouble)
      }
      .toMap
  }
}

object ArbitrageLoop {
  def find(conversions: Map[Conversion, Double]): Set[Seq[Vertex[String]]] = {
    val verticesCache = mutable.Map.empty[String, Vertex[String]]

    // do not create separate vertex instance on every parsed asset ticker name
    def mkVertex(asset: String): Vertex[String] = verticesCache.getOrElseUpdate(asset, new Vertex(asset))

    // if there's arbitrage loop, after a chain of trades, the product of rates will be greater than 1
    // eg. A->B at 5 and B->A at 0.21, then trading A->B->A would yield 5*0.21=1.05 multiplication
    // of the Initial stake.
    //
    // To replace the problem of finding a product greater than 1 to finding a distance in graph
    // we could you a property of logarithms, i.e. log(a*b) = log(a) + log(b), now
    // a*b > 1 iff log(a*b) > 0 iff -log(a*b) < 0 iff -log(a) - log(b) < 0
    val conversionRatesAsDistances: Seq[Edge[String]] = conversions.map { case (Conversion(from, to), rate) =>
      new Edge(mkVertex(from), mkVertex(to), -Math.log(rate))
    }.toSeq

    // To find a distance smaller than 0 and return to initial asset we must find a cycle in graph
    // that it's total distance is a negative number. For this purpose we could use Bellman-Ford algorithm.
    // It requires a start vertex and it traverses all the reachable vertices.
    // To find negative cycles in subgraphs disjoint to the one with vertex used as a root in BF algorithm run,
    // the initial graph could be spited to subgraphs of strongly connected components (where there's a path to each
    // vertex, a necessary condition to find a loop) and run BF algorithm in each strongly connected component.
    // Strongly connected components can be found with Tarjan Algorithm.
    val stronglyConnectedComponents: Set[Map[Vertex[String], Seq[Edge[String]]]] =
      TarjanAlgorithm.splitByStronglyConnectedComponents(conversionRatesAsDistances)

    stronglyConnectedComponents.flatMap(scc => BellmanFordAlgorithm.findNegativeCycle(scc.values.toSeq.flatten))
  }

  def reportArbitrageLoops(conversions: Map[Conversion, Double], loops: Set[Seq[Vertex[String]]]): Set[String] = {
    loops.map { cycle =>
      val returnOnArbitrage = cycle
        .zip(cycle.tail)
        .map { case (next, prev) =>
          conversions(Conversion(prev.id, next.id))
        }
        .product
      cycle.mkString("[", "->", "]") + s" return on arbitrage: $returnOnArbitrage×"
    }
  }
}

object ArbitrageOpportunitiesFinder extends App {

  val res = requests.get("http://fx.priceonomics.com/v1/rates/")

  val conversions = JsonParser.parse(res.text())

  val loops = ArbitrageLoop.find(conversions)

  ArbitrageLoop.reportArbitrageLoops(conversions, loops)

}
