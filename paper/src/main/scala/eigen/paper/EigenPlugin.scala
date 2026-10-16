package eigen.paper

import com.github.retrooper.packetevents.PacketEvents
import org.bukkit.plugin.java.JavaPlugin

final class EigenPlugin extends JavaPlugin:

  override def onEnable(): Unit =
    getLogger.info(s"Eigen ${getPluginMeta.getVersion} on PacketEvents ${PacketEvents.getAPI.getVersion}")

  override def onDisable(): Unit = ()
