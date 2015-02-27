package ru.imho.dddmt.config.typesafe

import ru.imho.dddmt.core._
import ru.imho.dddmt.std._
import BaseConfig._
import RichConfig._
import Base._
import BaseExec._
import com.typesafe.config.ConfigObject

object UniverseBuilder {
  val rootKey = "dddmt"
  val dependsOn = "dependsOn"

  def getInstance[T <: Identifiable](id: String, name: String) = {
    val rv = Class.forName(name).newInstance().asInstanceOf[T]
    if (rv.id != id)
      throw new IllegalArgumentException(s"IDentifiable `${rv.id}`: embedded id differs from provided id=`$id`")
    rv
  }

  private def ws[T <: Identifiable](sl: List[T]) = sl.map(s => (s.id, s)).toMap
  
  private def wall[T <: Identifiable](conf: RichConfig, std: List[T], ckey: String) = ws(std) ++ {
      for ((k, clazz) <- conf.getStringMap(ckey))
        yield (k, getInstance[T](k, clazz))    
  }
  

  def buildUniverse(additionalConfigFile: Option[String]): ConfigUniverse = new ConfigUniverse {

    val conf = com.typesafe.config.ConfigFactory.load().getConfig(rootKey)

    val parameterTypes = wall(conf, StandardParameterTypes.asList, "parameterTypes")
    val nodeTechnologyTypes = wall(conf, StandardTechnologies.asList, "nodeTechnologyTypes")
    val nodeStateTypes = wall(conf, StandardNodeStateTypes.asList, "nodeStateTypes")
    val nodeSpaceTypes = wall(conf, StandardNodeSpaceTypes.asList, "nodeSpaceTypes")
    
    val nodespaces = conf.getConfigMap("nodespaces").map { case (id, nsConfig) =>
      val tech = nsConfig.getConfig("technology")
      val ns = nodeSpaceTypes(nsConfig.getString("type")).newInstance(
          id,
          nsConfig,
          nodeTechnologyTypes(tech.getString("type")).newInstance(tech),
          parameterTypes(nsConfig.getString("parameterType")))
          
      val deps: Map[String, String] = 
        if(nsConfig.hasPath(dependsOn)) nsConfig.getStringMap("dependsOn") else Map()
      (id, (ns, deps))
    }
    
    val nsDeps = for((_, (ns, deps)) <- nodespaces; (dep, deptype) <- deps) 
      yield (ns, nodespaces(dep)._1) ->  DepAttr(nodeStateTypes(deptype))
      
    val jobFactories: Map[String, JobFactory] = Map()
  }

}