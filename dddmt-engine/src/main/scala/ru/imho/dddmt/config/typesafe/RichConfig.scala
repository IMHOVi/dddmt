package ru.imho.dddmt.config.typesafe

import ru.imho.dddmt.core.BaseConfig.Configuration
import com.typesafe.config.{Config, ConfigFactory, ConfigValue, ConfigObject}
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.language.implicitConversions

class RichConfig(val conf: Config) extends Configuration {
  def getString(path: String) = conf.getString(path)
  def getInt(path: String) = conf.getInt(path)
  def optString(path: String) = if(conf.hasPath(path)) Some(conf.getString(path)) else None
  def optInt(path: String) = if(conf.hasPath(path)) Some(conf.getInt(path)) else None
  def optBoolean(path: String) = if(conf.hasPath(path)) Some(conf.getBoolean(path)) else None
  def getStringList(path: String): List[String] = asScalaBuffer(conf.getStringList(path)).toList
  def getConfig(path: String) = new RichConfig(conf.getConfig(path))
  /**
   * Walks deep the whole tree
   */
  def getMap[R](f: Any => R)(path: String) =
    conf.getConfig(path).entrySet().asScala.map(e => (e.getKey(), f(e.getValue().unwrapped()))).toMap
  val getStringMap = getMap(_.toString) _
  val getStringListMap = getMap(_.asInstanceOf[java.util.List[Object]].asScala.toList.map(_.toString)) _
  val getIntMap = getMap(_.asInstanceOf[Int]) _
  /**
   * Only walks first-level keys
   */
  def getShallowMap[R](f: ConfigValue => R)(path: String) =
    conf.getConfig(path).root().entrySet().asScala.map(e => (e.getKey(), f(e.getValue()))).toMap
  val getConfigMap = getShallowMap(v => RichConfig.richConfig(v.asInstanceOf[ConfigObject].toConfig())) _
  def this(path: String) = this(ConfigFactory.load().getConfig(path))
}

object RichConfig {
  import scala.language.implicitConversions
  implicit def richConfig(conf: Config) = new RichConfig(conf)
  implicit def config(conf: RichConfig): Config = conf.conf
}