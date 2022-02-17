package com.kpalka

import scala.collection.mutable

/** Instantiation only via helper's apply method to avoid having
  * multiple instances of the same asset. Instantiation is not thread-safe.
  */
class Asset private (val ticker: String) {
  override def toString: String = ticker
}

object Asset {
  private val instances: mutable.Map[String, Asset] = mutable.Map[String, Asset]()
  def apply(ticker: String): Asset = instances.getOrElseUpdate(ticker, new Asset(ticker))
}

case class Conversion(from: Asset, to: Asset)

object JsonParser {
  private def parsePair(pair: String): Conversion = {
    val split = pair.split("_")
    Conversion(Asset(split(0)), Asset(split(1)))
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

object Tarjan {
  def split[A](edgesByVertex: Map[Vertex[A], Seq[Edge[A]]]): Set[Map[Vertex[A], Seq[Edge[A]]]] = {

    var index = 0
    val stack = mutable.Stack.empty[Vertex[A]]
    val onStack = mutable.Set.empty[Vertex[A]]
    val indexes = mutable.Map.empty[Vertex[A], Int]
    val lowLinks = mutable.Map.empty[Vertex[A], Int]
    val stronglyConnectedComponents = mutable.Set.empty[Map[Vertex[A], Seq[Edge[A]]]]

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

class Vertex[A](val id: A, var distance: Double = Double.MaxValue, var predecessor: Option[Vertex[A]] = None) {
  override def toString: String = id.toString
}
class Edge[A](val from: Vertex[A], val to: Vertex[A], val weight: Double) {

  override def toString: String = s"(${from.toString} -> ${to.toString}: ${weight})"
}
object Edge {
  def unapply[A](edge: Edge[A]): Option[(Vertex[A], Vertex[A], Double)] = Some(edge.from, edge.to, edge.weight)
}

object BF {
  def groupEdgesByVertex[A](edges: Seq[Edge[A]]): Map[Vertex[A], Seq[Edge[A]]] = edges
    .foldLeft(Map.empty[Vertex[A], Seq[Edge[A]]])((acc, edge) =>
      acc + (edge.from -> {
        val x: Seq[Edge[A]] = acc.getOrElse(edge.from, Seq.empty[Edge[A]])
        x :+ edge
      })
    )
  def find[A](edges: Seq[Edge[A]]) = {
    val edgesByVertex: Map[Vertex[A], Seq[Edge[A]]] = groupEdgesByVertex(edges)
    val startVertex = edges.head.from
    startVertex.distance = 0
    val verticesCount = edgesByVertex.size
    for {
      _ <- 1 until verticesCount
      edge <- edges
      _ = if (edge.from.distance + edge.weight < edge.to.distance) {
        edge.to.distance = edge.from.distance + edge.weight
        edge.to.predecessor = Some(edge.from)
      }
    } yield ()

    var detectedCycle: Option[Seq[Vertex[A]]] = None

    for (edge <- edges if detectedCycle.isEmpty) {
      if (edge.from.distance + edge.weight < edge.to.distance) {
        // cycle detected
        val cycle = mutable.ArrayBuffer.empty[Vertex[A]]
        var vertex = edge.from
        cycle += vertex
        while (vertex != edge.to) {
          vertex = vertex.predecessor.get
          cycle += vertex
          println(vertex, edge.to)
        }
        cycle += edge.from
        detectedCycle = Some(cycle.toSeq)
      }
    }
    detectedCycle
  }
}

object Converter {
  def calculate(c: Map[Conversion, Double]): Unit = {
    val verticesCache = mutable.Map.empty[Asset, Vertex[Asset]]
    def mkVertex(asset: Asset): Vertex[Asset] = verticesCache.getOrElseUpdate(asset, new Vertex(asset))
    val domain: Seq[Edge[Asset]] = c.map { case (Conversion(from, to), rate) =>
      new Edge(mkVertex(from), mkVertex(to), -Math.log(rate))
    }.toSeq
    val groupedByVertex = BF.groupEdgesByVertex(domain)
    val stronglyConnectedComponents = Tarjan.split(groupedByVertex)
    val cycles = stronglyConnectedComponents.flatMap(scc => BF.find(scc.values.toSeq.flatten))

//    val cycles = BF.find(domain)
    cycles.foreach { cycle =>
      val returnOnArbitrage = cycle
        .zip(cycle.tail)
        .map { case (next, prev) =>
          c(Conversion(prev.id, next.id))
        }
        .product
      println(cycle.mkString("[", "->", "]") + ": " + returnOnArbitrage + ", ")
    }
  }

}

object ArbitrageOpportunitiesFinder extends App {

  val res = requests.get("http://fx.priceonomics.com/v1/rates/")

  println(res.text())
  val json = JsonParser.parse(res.text())
  Converter.calculate(json)
//  println(StronglyConnectedComponents.toVerticesWithAdjacencyList(json.keys.toSeq))

  println(json)
}
