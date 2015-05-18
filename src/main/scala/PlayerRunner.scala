import org.ggp.base.player.GamePlayer

object PlayerRunner {
    val defaultPort = 9147

    def main(args: Array[String]): Unit = {
        val port = args.headOption.getOrElse(defaultPort.toString).toInt
        val player = new GamePlayer(port, new MCTSMultiPlayer())
        player.start()
    }
}
