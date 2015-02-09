package ru.imho.dddmt.sftp.jsch

import ru.imho.dddmt.core._
import Base._
import BaseConfig._
import com.jcraft.jsch._
import scala.collection.JavaConverters._
import java.net.URI
import ru.imho.dddmt.std.StandardNodeStateTypes._

class SFTPTechnologyType extends NodeTechnologyType {
  val id = "sftp"
  def newInstance(config: Configuration): NodeTechnology = new NodeTechnology {

    val knownHostsFile = config.optString("knownHostsFile")
    val host = config.getString("host")
    val port = config.getInt("port")
    val user = config.getString("user")
    val password = config.optString("password")
    val passphrase = config.optString("passphrase")
    val timeShift = config.optInt("timeShift").getOrElse(0)
    val dir = config.getString("dir")

    val idURI = new URI("sftp", user, host, port, dir, null, null)
    val id = idURI.toString()

    private val jsch = new JSch()
    knownHostsFile.foreach(jsch setKnownHosts _)

    /**
     * @return a list of pair (filename, mtime) where mtime is in seconds since Epoch
     */
    def getRemoteListing: Seq[(String, Long)] = {
      val session = jsch.getSession(user, host, port)
      password.foreach(session setPassword _)
      session.connect()
      val channel = session.openChannel("sftp").asInstanceOf[ChannelSftp]
      channel.connect()
      channel.cd(dir)
      val ls = channel.ls(".").asScala.map(_.asInstanceOf[channel.LsEntry])
      channel.exit()
      session.disconnect()
      if (!ls.forall(v => (v.getAttrs().getFlags() & SftpATTRS.SSH_FILEXFER_ATTR_ACMODTIME) != 0))
        throw new IllegalStateException(s"unable to retireve mtime from some of files at $id")
      ls.map(le => (le.getFilename(), (le.getAttrs().getMTime() + timeShift) * 1000L))
    }

    def newNodeStateResolver(nst: NodeStateType): URINodeStateResolver = nst match {
      case `MTime` =>
        val fm = getRemoteListing.toMap
        _.map(uri => fm.get(uri.getSchemeSpecificPart()).map(MTime.fromLongTime).getOrElse(MTime.nonexistent))
      case _ =>
        throw new IllegalArgumentException(s"SFTP does not support `${nst.id}`")
    }
    
    def newNodeStateAggregateResolver(nst: NodeStateType): URINodeStateAResolver = nst match {
      case `MTime` =>
        val fm = getRemoteListing.toMap
        uri =>
          fm.get(uri.getSchemeSpecificPart()).map(MTime.fromLongTimeA)
      case _ =>
        throw new IllegalArgumentException(s"SFTP does not support `${nst.id}`")
    }

  }
}
//    	  def promptPassword(message: String) = false
//    	  def promptPassphrase(message: String) = false
//    	  def promptYesNo(message: String) = false
//    	  def showMessage(message: String) = {}
//      }
//     session.setUserInfo(userInfo)
