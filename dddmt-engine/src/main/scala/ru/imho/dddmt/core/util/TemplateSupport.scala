package ru.imho.dddmt.core.util

object TemplateSupport {
  
  abstract sealed class Op
  case class Str(s: String) extends Op
  case class Var(id: String, format: Option[String]) extends Op
  
  val idStart = Character.isLetter _
  val idPart = Character.isLetterOrDigit _
  
  def parseSimpleTemplate(template: String): List[Op] = 
    template.foldLeft[(Symbol, List[Op])]('S, Nil) {

      //String part
      case (('S, l), '$') => ('WBR, l)
      case (('S, Str(s) :: l), c) => ('S, Str(s + c) :: l)
      case (('S, l), c) => ('S, Str(c.toString) :: l)

      //$_{
      case (('WBR, l), '{') => ('IDBR, Var("", None) :: l)
      case (('WBR, Str(s) :: l), '$') => ('S, Str(s + '$') :: l)
      case (('WBR, l), '$') => ('S, Str("$") :: l)
      case (('WBR, l), c) if idStart(c) => ('ID, Var(c.toString, None) :: l)

      //${id_}
      case (('IDBR, l), '}') => ('S, l)
      case (('IDBR, Var(id, _) :: l), '%') => ('IDBRFMT, Var(id, Some("%")) :: l)
      case (('IDBR, Var(id, fmt) :: l), c) => ('IDBR, Var(id + c, fmt) :: l)

      //${id%fmt_}
      case (('IDBRFMT, l), '}') => ('S, l)
      case (('IDBRFMT, Var(id, Some(fmt)) :: r), c) => ('IDBRFMT, Var(id, Some(fmt + c)) :: r)

      //$id_
      case (('ID, Var(id, fmt) :: r), c) if idPart(c) => ('ID, Var(id + c, fmt) :: r)
      case (('ID, l), '$') => ('WBR, l)
      case (('ID, l), c) => ('S, Str(c.toString) :: l)
      
      case ((s, l), c) => 
        throw new IllegalStateException(s"Invalid template: `$template`, at char `$c`, s=`$s`")

    } match {
      case ('S, l) => l.reverse
      case ('ID, l) => l.reverse
      case _ => throw new IllegalStateException("Incorrectly terminated template: " + template)
    }

  
  def simpleTemplateExpander[T](
      template: String, 
      varAccessors: Map[String, T => Any]): T => String = {
      
        def w(in: T => StringBuilder => Unit) = in 

        val ops: List[T => StringBuilder => Unit] = parseSimpleTemplate(template).map {
          case Str(s) => 
            w(_ => _.append(s))
          case Var(v, f) => 
            val op = try { 
              var r = varAccessors(v)
              f.map(fmt => r andThen (_ formatted fmt)).getOrElse(r)
              } catch { 
                case t: NoSuchElementException => 
                  throw new IllegalArgumentException(s"variable not found: `$v`")
                  } 
            w(t => _.append(op(t)))
        }
      
        def fx(t: T, sb: StringBuilder): Unit = ops foreach (_(t)(sb))
        
        t => {
          var sb = new StringBuilder
          fx(t, sb)
          sb.toString
        }
      }
   
}