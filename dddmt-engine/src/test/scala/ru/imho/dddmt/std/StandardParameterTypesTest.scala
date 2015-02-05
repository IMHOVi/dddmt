package ru.imho.dddmt.std

import org.scalatest.FlatSpec

class StandardParameterTypesTest extends FlatSpec {
  import StandardParameterTypes._

  "YMDHParameterType" should "pass range tests" in {
    assertResult {     
        List(
        "2015-01-03-20", "2015-01-03-21", "2015-01-03-22", "2015-01-03-23",
        "2015-01-04-00", "2015-01-04-01", "2015-01-04-02", "2015-01-04-03", 
        "2015-01-04-04", "2015-01-04-05", "2015-01-04-06"
        ).map(YMDHParameterType.fromString)
        }{
          YMDHParameterType.getValues(
            YMDHRangePeriod(YMDH(YMD(YM(Y(2015), 1), 3), 20), YMDH(YMD(YM(Y(2015), 1), 4), 6))
          ).toList
        }
  }
}


// import ru.imho.dddmt.core.Base._
//
//object StandardParameterTypesTest {
//  def getStandardParameterTypeByName(name: String): ParameterType = {
//    val ru = scala.reflect.runtime.universe
//    val m = ru.runtimeMirror(super.getClass.getClassLoader)
//    val t = ru.typeOf[StandardParameterTypes.type]
//    val n = ru.newTermName("YMParameterType")
//    val d = t.member(n)
//    val mm = m.reflectModule(d.asModule)
//    mm.instance.asInstanceOf[ParameterType]
//  }
//   //getStandardParameterTypeByName("YMParameterType")
//}