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
    println(res)
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
    println(res)
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
    println(res)
  }

  test("two cycles single cycle three pairs equilibrium") {
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
    println(res)
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
    println(res)
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
    println(res)
  }

  test("scc") {
    val a = new Vertex("A")
    val b = new Vertex("B")
    val c = new Vertex("C")
    val d = new Vertex("D")
    val e = new Vertex("E")
    val res = Tarjan.split(
      Seq(
        new Edge(a, b, -1),
        new Edge(b, a, -1),
        new Edge(b, c, -1),
        new Edge(c, b, -1),
        new Edge(e, d, -1)
      )
    )
    println(res)
  }

  test("from.json") {
    val res = Source.fromResource("rates.json").mkString
    val parsed = JsonParser.parse(res)
    Converter.calculate(parsed)
    println(res)
  }
}
