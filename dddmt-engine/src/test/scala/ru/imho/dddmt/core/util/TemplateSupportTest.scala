package ru.imho.dddmt.core.util

import org.scalatest.FlatSpec

class TemplateSupportTest extends FlatSpec {
  import TemplateSupport._
  
  "parseSimpleTemplate" should "pass all tests" in {
    def c(s: String)(l: Op*) =
      assertResult(l.toList)(parseSimpleTemplate(s))
      
    c("")()
    c("a")(Str("a"))
    c("$a")(Var("a", None))
    c("${a}")(Var("a", None))
    c("a$a")(Str("a"), Var("a", None))
    c("abc$a")(Str("abc"), Var("a", None))
    c("abc${a}be")(Str("abc"), Var("a", None), Str("be"))
    c("abc$a.be")(Str("abc"), Var("a", None), Str(".be"))   
    c("abc$$be")(Str("abc$be"))
    c("$a${b}$c$d$e")(Var("a", None), Var("b", None), Var("c", None), Var("d", None), Var("e", None))    
    c("/c/drive/${t%02d}-${v%03d}.$e/r")(
        Str("/c/drive/"), Var("t", Some("%02d")), Str("-"), Var("v", Some("%03d")), Str("."), Var("e", None), Str("/r"))
    
    intercept[IllegalStateException]{ parseSimpleTemplate("ab${d") }
    intercept[IllegalStateException]{ parseSimpleTemplate("ab$*aa") }
  }

  "simpleTemplateExpander" should "pass all tests" in {
    val x = simpleTemplateExpander[Vector[Int]]("/a/${y%04d}-${m%02d}-${d%02d}.avro", Map(
        "y" -> ((_: Vector[Int])(0)),
        "m" -> ((_: Vector[Int])(1)),
        "d" -> ((_: Vector[Int])(2))
        ))
    assertResult("/a/2015-01-23.avro")(x(Vector(2015,1,23)))
  }
  
}