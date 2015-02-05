package ru.imho.dddmt.std

import ru.imho.dddmt.core.Base._
import ru.imho.dddmt.core.BaseConfig._
import java.net.URI
import java.io.File
import StandardNodeStateTypes.MTime

object StandardTechnologies {

  object LocalFileSystem extends NodeTechnologyType {
    val id = "localFileSystem"
    def newInstance(config: Configuration): NodeTechnology = new NodeTechnology {
      val root = config.getString("root")
      val idURI = new URI("local", root, null)
      val id = idURI.toString()
      val rootF = new File(root)

      def mtime(f: File) = if (f.exists())
        MTime.fromLongTime(f.lastModified())
      else
        MTime.nonexistent
        
      def file(uri: URI) = new File(rootF, uri.getSchemeSpecificPart())
        
      def newNodeStateResolver(nst: NodeStateType): URINodeStateResolver = nst match {
        case `MTime` =>
          _.map(uri => mtime(file(uri)))
        case _ =>
          throw new IllegalArgumentException(s"LocalFileSystem does not support `${nst.id}`")
      }

      def newNodeStateAggregateResolver(nst: NodeStateType): URINodeStateAResolver = nst match {
        case `MTime` =>           
          uri => {
            val f = file(uri)
            if(f.exists()) 
              Some(MTime.fromLongTimeA(f.lastModified())) 
            else None
          }
        case _ =>
          throw new IllegalArgumentException(s"LocalFileSystem does not support `${nst.id}`")
      }
    }
  }
  
  def asList: List[NodeTechnologyType] = LocalFileSystem :: Nil
}