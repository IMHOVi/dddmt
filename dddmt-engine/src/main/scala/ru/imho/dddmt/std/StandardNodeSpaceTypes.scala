package ru.imho.dddmt.std

import ru.imho.dddmt.core.Base._
import ru.imho.dddmt.core.BaseConfig._
import java.net.URI

object StandardNodeSpaceTypes {
  
  object GenericFileSet extends NodeSpaceType {
    val id = "fileSet"
    def newInstance(config: Configuration, 
        technology: NodeTechnology,
        parameter: ParameterType): NodeSpace = new NodeSpace {
      val id = config.getString("id")
      val parameterType = parameter
      val nodeTechnology = technology
      
      private val expander = 
        parameterType.asInstanceOf[ParameterExpander].compileTemplate(config.getString("pathTemplate"))

      def uri(parameterValue: ParameterValue): URI =
        new URI(id, expander(parameterValue), null)
    }
      
  }
  
  def asList: List[NodeSpaceType] = GenericFileSet :: Nil

}