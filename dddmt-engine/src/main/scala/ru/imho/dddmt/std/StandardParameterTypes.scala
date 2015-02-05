package ru.imho.dddmt.std

import ru.imho.dddmt.core.BaseConfig.ParameterExpander

/**
 * @author VVybornov
 *
 */
object StandardParameterTypes {
  import ru.imho.dddmt.core.Base.{Period, ParameterType, ParameterValue}
  
  class IntListParameterValue(
      parentV: ParameterValue, 
      thisV: Int, 
      val parameterType: ParameterType, 
      formatSpecifier: String) 
    extends ParameterValue {
      def serial = parentV.serial * 100 + thisV
      
      /**
       * <p>a) `this conformsTo parentV = true`
       * <p>b) `this conformsTo parent and parent conformsTo b` => `this conformsTo b` 
       */
      def conformsTo(other: ParameterValue) = (other == this) || parentV.conformsTo(other)
      override def toString = 
        (if(parentV == ConstParameterValue) "" else parentV.toString() + "-") + 
        (thisV formatted formatSpecifier)
  }
 
  case class Y(val y: Int) extends IntListParameterValue(ConstParameterValue, y, YParameterType, "%04d")
  case class YM(val y: Y, val m: Int) extends IntListParameterValue(y, m, YMParameterType, "%02d")
  case class YMD(val ym: YM, val d: Int) extends IntListParameterValue(ym, d, YMDParameterType, "%02d")
  case class YMDH(val ymd: YMD, val h: Int) extends IntListParameterValue(ymd, h, YMDHParameterType, "%02d")
  
  object YParameterType extends ParameterType with ParameterExpander {
    val id = "year"
    def getValues(p: Period): Stream[ParameterValue] = p match {
      case YMDHRangePeriod(YMDH(YMD(YM(y1, _), _), _), YMDH(YMD(YM(y2, _), _), _)) =>
        ntu.iter(y1, (_: Y) <= y2).toStream
    }
    def fromString(s: String): Y = s match {
      case fourDigitRE(d) => Y(d.toInt)
    } 
    val templateParameterExtractors: TPMap = Map(
        "y" -> (_.asInstanceOf[Y].y)
        )    
  }
  
  object YMParameterType extends ParameterType with ParameterExpander {
    val id = "yearMonth"
    def getValues(p: Period): Stream[ParameterValue] = p match {
      case YMDHRangePeriod(YMDH(YMD(ym1, _), _), YMDH(YMD(ym2, _), _)) =>
        ntu.iter(ym1, (_: YM) <= ym2).toStream
    }
    def fromString(s: String): YM = s match {
      case twoDigitSuffixRE(r, d) => YM(YParameterType.fromString(r), d.toInt)
    } 
    val templateParameterExtractors: TPMap = Map(
        "y" -> (_.asInstanceOf[YM].y.y),
        "m" -> (_.asInstanceOf[YM].m)
        )    
 }
  
  object YMDParameterType extends ParameterType with ParameterExpander {
    val id = "yearMonthDay"
    def getValues(p: Period): Stream[ParameterValue] = p match {
      case YMDHRangePeriod(YMDH(ymd1, _), YMDH(ymd2, _)) =>
        ntu.iter(ymd1, (_: YMD) <= ymd2).toStream
    }
    def fromString(s: String): YMD = s match {
      case twoDigitSuffixRE(r, d) => YMD(YMParameterType.fromString(r), d.toInt)
    } 
    val templateParameterExtractors: TPMap = Map(
        "y" -> (_.asInstanceOf[YMD].ym.y.y),
        "m" -> (_.asInstanceOf[YMD].ym.m),
        "d" -> (_.asInstanceOf[YMD].d)
        )    
  }
  
  object YMDHParameterType extends ParameterType with ParameterExpander {
    val id = "yearMonthDayHour"
    def getValues(p: Period): Stream[ParameterValue] = p match {
      case YMDHRangePeriod(ymdh1, ymdh2) =>
        ntu.iter(ymdh1, (_: YMDH) <= ymdh2).toStream
    }   
    def fromString(s: String): YMDH = s match {
      case twoDigitSuffixRE(r, d) => YMDH(YMDParameterType.fromString(r), d.toInt)
    } 
    val templateParameterExtractors: TPMap = Map(
        "y" -> (_.asInstanceOf[YMDH].ymd.ym.y.y),
        "m" -> (_.asInstanceOf[YMDH].ymd.ym.m),
        "d" -> (_.asInstanceOf[YMDH].ymd.d),
        "h" -> (_.asInstanceOf[YMDH].h)
        )    
  }
  
  val twoDigitSuffixRE = """(.+)-([0-9][0-9])""".r  
  val fourDigitRE = """([0-9][0-9][0-9][0-9])""".r  
  
  import java.util.{Calendar, GregorianCalendar}
  
  private class NextTimeUnit(c: Calendar) {
    @inline
    def s0(field: Int, value: Int) = c.set(field, value)
    @inline
    def sy(y: Y): Unit = s0(Calendar.YEAR, y.y)
    @inline
    def sym(ym: YM): Unit = { sy(ym.y); s0(Calendar.MONTH, ym.m - 1) }
    @inline
    def symd(ymd: YMD): Unit = { sym(ymd.ym); s0(Calendar.DAY_OF_MONTH, ymd.d) }
    @inline
    def symdh(ymdh: YMDH): Unit = { symd(ymdh.ymd); s0(Calendar.HOUR_OF_DAY, ymdh.h); }
    
    @inline
    def g0[R](field: Int, i: Boolean, f: Int => R): R = { if(i) c.add(field, 1); f(c.get(field)) }
    
    @inline
    def gy(i: Boolean = false): Y = g0(Calendar.YEAR, i, Y)
    @inline
    def gym(i: Boolean = false): YM = g0(Calendar.MONTH, i, n => YM(gy(), n + 1))
    @inline
    def gymd(i: Boolean = false): YMD = g0(Calendar.DAY_OF_MONTH, i, n => YMD(gym(), n))
    @inline
    def gymdh(i: Boolean = false): YMDH = g0(Calendar.HOUR_OF_DAY, i, n => YMDH(gymd(), n))    
    
    def iter(start: Y, whileCond: Y => Boolean) = new TIterator[Y]({sy(start); gy()}, gy(true), whileCond)
    def iter(start: YM, whileCond: YM => Boolean) = new TIterator[YM]({sym(start); gym()}, gym(true), whileCond)
    def iter(start: YMD, whileCond: YMD => Boolean) = new TIterator[YMD]({symd(start); gymd()}, gymd(true), whileCond)
    def iter(start: YMDH, whileCond: YMDH => Boolean) = new TIterator[YMDH]({symdh(start); gymdh()}, gymdh(true), whileCond)
  }
  
  private def ntu = new NextTimeUnit(new GregorianCalendar())
  
  class TIterator[T](init: T, gen: => T, whileCond: T => Boolean) extends Iterator[T] {
    var nextV = init
    def hasNext = whileCond(nextV)
    def next = { val rv = nextV; nextV = gen; rv}   
  }

  
  /**
   * A closed contiguous period with hour accuracy
   */
  case class YMDHRangePeriod(val start: YMDH, val end: YMDH) extends Period
  
  /**
   * The only value of type ConstParameterType
   */
  case object ConstParameterValue extends ParameterValue {
    val parameterType = ConstParameterType
    val serial = 0L
    def conformsTo(other: ParameterValue) = other == this
    override def toString = "constant"
  }
  
  /**
   * A Parameter Type that constitutes no variable parameter
   */
  object ConstParameterType extends ParameterType with ParameterExpander {
    val id= "const"
    def getValues(p: Period): Stream[ParameterValue] = Stream(ConstParameterValue)
    val templateParameterExtractors: TPMap = Map()    
  }


  def asList = 
    YMDHParameterType ::
    YMDParameterType ::
    YMParameterType ::
    YParameterType ::
    ConstParameterType ::
    Nil
}