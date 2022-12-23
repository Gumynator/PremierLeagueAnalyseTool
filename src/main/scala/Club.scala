package club
import PremierLeague.Players
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.FutureTask
import scala.util.Success
import scala.util.Failure



case class Club(players: List[Players] ){
        /**
         * Sort the players by goals, assists or name
         * @param attribute Column name
         * @param sortedList List of players
         * @return Top 5 of list
         */
        def sortPlayersBy(attribute:String, sortedList:List[Players]):Unit = {
            val sortedPlayers = sortedList
            attribute match{
                case "goals" => 
                val sortedPlayers = sortedList.sortBy(_.goals)(Ordering[Int].reverse)
                            sortedPlayers.take(5).foreach(println(_))
                case "assists" => 
                    val sortedPlayers = sortedList.sortBy(_.assists)(Ordering[Int].reverse)
                                sortedPlayers.take(5).foreach(println(_))
                case "name" => 
                    val sortedPlayers = sortedList.sortBy(_.name)(Ordering[String])
                                sortedPlayers.take(5).foreach(println(_))
                case _: String => 
                    throw new IllegalArgumentException("Invalid parameter")
            }   
        }
        
        /**
         * Predict the best player based on a random number
         * @return 
         */
        def prediction(): Future[Unit]  = Future{
            println("And the best player is ....")

            def pickPlayer: String = {

                var r = scala.util.Random;
                var a = r.nextInt(players.length-1)
                Thread.sleep(1000)
                players(a).name
            }

            val first = Future {pickPlayer}

            first.onComplete {
                case Success(value) => println("1°: " + value)
                case Failure(exception) => println("No winners")
            }
        }

        /**
         * Sum the main statistics of the league
         * @return Matches, minutes, goals, assists
         */
        def sumStats() = {
            val matchStat = players.map(player => (player.matches, player.mins, player.goals, player.assists))
            val res = matchStat.reduce((total, player) => (total._1 + player._1,  total._2 + player._2, total._3 + player._3, total._4 + player._4))
            (s"Total matches: ${res._1}, Total minutes: ${res._2}, Total goals: ${res._3}, Total assists: ${res._4}")
        }

        /**
         * Get the average age of a club
         * @param team Club name
         * @return
         */
        def avgAgeClub(team: String): Option[Double] = {
            try{
                val list = players.filter(player => player.club.equals(team))
                val totalAge = (list.map(_.age).sum )
                Some(totalAge.toDouble / list.length)
            }
            catch{
                case _: Exception => None
            }
        }

        /**
         * Get the best players per club
         * @return Best xG player, xG, best xA player, xA
         */
        def maxXG() = {
            players.groupMapReduce(_.club)(player => (player.name, player.xG,player.name, player.xA)) {
            case ((name1, xG1,name3, xA1), (name2, xG2,name4, xA2)) =>
            val maxXG = xG1 max xG2
            val maxXA = xA1 max xA2
            val scorerName = if (maxXG == xG1) name1 else name2
            val assitantName = if (maxXA== xA1) name3 else name4
            (scorerName, maxXG, assitantName, maxXA)
            }     
        }

        /**
         * Get a squad with an option of nationality
         * @param team Club name
         * @param nation Nationality
         * @return
         */
        def getClub(team:String, nation:Option[String]):List[Players] = {
            nation match{
                case Some(value) => players.filter(play=>play.club.equals(team) && play.nationality.equals(value));
                case None => players.filter(play=>play.club.equals(team));
            }
        }

        /**
         * Transfer player to another team
         * @param team Club name
         * @param name Player name
         * @return
         */
        def changeClub(team:String,name:String):Club = {
            this.copy(players = players.map(play => if (play.name.equals(name))
            {play.updateClub(team)}
            else play))
        }

        /**
         * Add player to the league
         * @param player New player
         * @return
         */
        def addPlayer(player:Players):Club={
            this.copy(players = players:::player::Nil)
        }

         /**
         * Remove player or complete squad
         * @param team Club name
         * @param name Player name
         * @return
         */
        def deletePlayer(name:String, team:Option[String]) : Club = {
            team match {
                case Some(value) => this.copy(players = players.filterNot(play=>play.club.equals(value)));
                case None => this.copy(players.filterNot(play=>play.name.equals(name)));
            }
        }

        /**
         * Increase scorer goal statistic
         * @param name Player name
         * @return
         */
        def playerScored(name: String) = {
            this.copy(players = players.map(play => if (play.name.equals(name))
            {play.goalScored()}
            else play))
        }

        /**
         * Update player passes statistics
         * @param name Player name
         * @param newPassesAttempted Passes attempted
         * @param newPassesAccurate Passes completed
         * @return
         */
        def passesStat(name: String, newPassesAttempted:Int, newPassesAccurate:Int) = {
        this.copy(players = players.map(play => if (play.name.equals(name)){
            val attemp = play.passesA + newPassesAttempted
            val tot = play.passesA*100/play.percPassesC + newPassesAccurate
            val newPerc = attemp*100/tot - (attemp*100/tot % 0.1)
            play.updatePasses(attemp, newPerc)}
            else play))
        }
}
  

