package ru.imho.dddmt.core

import Base._
import ru.imho.dddmt.core.util.TemplateSupport

object BaseConfig {
  /**
   * Instantiation/configuration support
   */
  trait Configuration {
    def getString(path: String): String
    def getInt(path: String): Int
    def getStringList(path: String): List[String]
    def getConfig(path: String): Configuration
  }
  
  
  trait NodeTechnologyType extends Identifiable {
    def newInstance(config: Configuration): NodeTechnology
  }
  
  trait NodeSpaceType extends Identifiable {
    def newInstance(config: Configuration, 
        technology: NodeTechnology, 
        parameter: ParameterType): NodeSpace
  }
  
  
  trait ParameterExpander {
    type TPMap = Map[String, ParameterValue => Any]
    
    val templateParameterExtractors: TPMap
    
    def compileTemplate(template: String) = 
      TemplateSupport.simpleTemplateExpander(template, templateParameterExtractors)
  }
  


}