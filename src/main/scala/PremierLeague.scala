package PremierLeague

import scala.compiletime.ops.double
import scala.util.Try

trait Player{
    val name: String
    val club: String
    val nationality: String
    val position: String
    val age: Int
}

trait Stats{
    val matches: Int
    val starts: Int
    val mins: Int
    val goals: Int
    val assists: Int
    val passesA: Int
    val percPassesC: Double
    val penaltyG: Double
    val penaltyA: Double
    val xG: Double
    val xA: Double
    val yellowC: Int
    val redC: Int
}

case class Players(name:String, club:String, nationality:String, position:String, age:Int, matches:Int, starts:Int, mins:Int, goals:Int, assists:Int, passesA:Int, percPassesC:Double,penaltyG:Double, penaltyA:Double, xG:Double, xA:Double, yellowC:Int, redC:Int) 
    extends Player with Stats{

        override def toString(): String = {
            s"$name($age){Nationality, $nationality, Club: $club, Position: $position, Matches: $matches, Goals: $goals, Passes Attempted: $passesA}, Percentage Passes Completed: $percPassesC"

        }

        def attribute(attribute:String) = {
            Try(
                attribute match {
                    case "name" => this.name
                    case "club" => this.club
                    case "nationality" => this.nationality
                    case "position" => this.position
                    case "age" => this.age
                    case "matches"  => this.matches
                    case "starts" => this.starts
                    case "mins" => this.mins
                    case "goals" => this.goals
                    case "assists" => this.assists
                    case "passes_Attempted" => this.passesA
                    case "perc_Passes_Completed" => this.percPassesC
                    case "penalty_G" => this.penaltyG
                    case "penalty_Attempted" => this.penaltyA
                    case "xG" => this.xG
                    case "xA" => this.xA
                    case "yellow_Cards" => this.yellowC
                    case "red_Cards" => this.redC
                }
            )
        }
        /**
         * Change player club
         * @param newClub New club name
         * @return
         */
        def updateClub(newClub:String)={
            this.copy(club = newClub)
        }

        /**
         * Add goal to player
         * @return
         */
        def goalScored()={
            this.copy(goals = goals+1)
        }

        /**
         * Change player club
         * @param newPassesA Passes attempted
         * @param newPercPassesC Passes completed
         * @return
         */
        def updatePasses(newPassesA:Int, newPercPassesC:Double)={
            this.copy(passesA = newPassesA, percPassesC=newPercPassesC)
        }
}

