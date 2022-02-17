package com.kpalka

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

import scala.io.Source

class ArbitrageOpportunitiesFinderTest extends AnyFunSuite with should.Matchers {

  test("single edge") {
    val usd = new Vertex("USD")
    val res = BellmanFordAlgorithm.findNegativeCycle(Seq(new Edge(usd, usd, 1.0)))
    res.map(_.map(_.id)) should be(None)
  }

  test("single cycle equilibrium") {
    val usd = new Vertex("USD")
    val pln = new Vertex("PLN")
    val res = BellmanFordAlgorithm.findNegativeCycle(
      Seq(
        new Edge(usd, pln, -Math.log(0.25)),
        new Edge(pln, usd, -Math.log(4))
      )
    )
    res.map(_.map(_.id)) should be(None)
  }

  test("single cycle arbitrage opportunity") {
    val usd = new Vertex("USD")
    val pln = new Vertex("PLN")
    val res = BellmanFordAlgorithm.findNegativeCycle(
      Seq(
        new Edge(usd, pln, -Math.log(0.25)),
        new Edge(pln, usd, -Math.log(5))
      )
    )
    res.map(_.map(_.id)) should be(Some(List("USD", "PLN", "USD")))
  }

  test("two cycles three assets equilibrium") {
    val usd = new Vertex("USD")
    val pln = new Vertex("PLN")
    val eur = new Vertex("EUR")
    val res = BellmanFordAlgorithm.findNegativeCycle(
      Seq(
        new Edge(usd, pln, -Math.log(0.25)),
        new Edge(pln, usd, -Math.log(4)),
        new Edge(pln, eur, -Math.log(5)),
        new Edge(eur, pln, -Math.log(0.2))
      )
    )
    res.map(_.map(_.id)) should be(None)
  }

  test("two cycles three pairs arbitrage opportunity") {
    val usd = new Vertex("USD")
    val pln = new Vertex("PLN")
    val eur = new Vertex("EUR")
    val res = BellmanFordAlgorithm.findNegativeCycle(
      Seq(
        new Edge(usd, pln, -Math.log(0.25)),
        new Edge(pln, usd, -Math.log(5)),
        new Edge(pln, eur, -Math.log(5)),
        new Edge(eur, pln, -Math.log(0.25))
      )
    )
    res.map(_.map(_.id)) should be(Some(List("PLN", "EUR", "PLN")))
  }

  test("two cycles detected") {
    val a = new Vertex("A")
    val b = new Vertex("B")
    val c = new Vertex("C")
    val res = BellmanFordAlgorithm.findNegativeCycle(
      Seq(
        new Edge(a, b, -1),
        new Edge(b, a, -1),
        new Edge(b, c, -1),
        new Edge(c, b, -1)
      )
    )
    res.map(_.map(_.id)) should be(Some(List("B", "C", "B")))
  }

  test("scc") {
    val a = new Vertex("A")
    val b = new Vertex("B")
    val c = new Vertex("C")
    val d = new Vertex("D")
    val e = new Vertex("E")
    val ab = new Edge(a, b, -1)
    val ba = new Edge(b, a, -1)
    val bc = new Edge(b, c, -1)
    val cb = new Edge(c, b, -1)
    val ed = new Edge(e, d, -1)
    val res = TarjanAlgorithm.splitByStronglyConnectedComponents(
      Seq(
        ab,
        ba,
        bc,
        cb,
        ed
      )
    )
    res should be(
      Set(
        Map(
          a -> List(ab),
          b -> List(ba, bc),
          c -> List(cb)
        ),
        Map(
          e -> List(ed)
        ),
        Map(
          d -> List()
        )
      )
    )
  }

  test("from.json") {
    val text = Source.fromResource("rates.json").mkString
    val parsed = JsonParser.parse(text)
    val loops = ArbitrageLoop.find(parsed)
    val report = ArbitrageLoop.reportArbitrageLoops(parsed, loops)
    report should be(
      Set(
        "[PLN->CZK->PLN] return on arbitrage: 1.0185590003465×",
        "[JPY->EUR->JPY] return on arbitrage: 1.15902147810912×"
      )
    )
  }
}
