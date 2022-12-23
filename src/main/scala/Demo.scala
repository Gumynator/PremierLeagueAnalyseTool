package demo

import com.github.tototoshi.csv._
import java.io.File
import club.Club
import PremierLeague.Players
import math.BigDecimal.double2bigDecimal




object main extends App{
    val filePath = "ressources/data/09-PremierLeague.csv"
    /**
   * Extract values from csv file
   */
    def extractPlayersFromCSV(filePath: String): List[Players] = {
        val reader = CSVReader.open(new File(filePath))
        val data = reader.toStream.toList
        val columnNames = data.head

        // Indexes for each column
        var nameIndex, clubIndex, nationalityIndex, positionIndex, ageIndex, matchesIndex, startsIndex, minsIndex, goalsIndex, assistsIndex, passesAIndex, percPassesCIndex, penaltyGIndex, penaltyAIndex, xGIndex, xAIndex, yellowCIndex, redCIndex = -1
        columnNames.zipWithIndex.foreach { case (columnName, index) =>
        columnName match {
            case "Name" => nameIndex = index
            case "Club" => clubIndex = index
            case "Nationality" => nationalityIndex = index
            case "Position" => positionIndex = index
            case "Age" => ageIndex = index
            case "Matches"  => matchesIndex = index
            case "Starts" => startsIndex = index
            case "Mins" => minsIndex = index
            case "Goals" => goalsIndex = index
            case "Assists" => assistsIndex = index
            case "Passes_Attempted" => passesAIndex = index
            case "Perc_Passes_Completed" => percPassesCIndex = index
            case "Penalty_Goals" => penaltyGIndex = index
            case "Penalty_Attempted" => penaltyAIndex = index
            case "xG" => xGIndex = index
            case "xA" => xAIndex = index
            case "Yellow_Cards" => yellowCIndex = index
            case "Red_Cards" => redCIndex = index
        }
        }

        data.drop(1).map { player =>
        Players(
            player(nameIndex),
            player(clubIndex),
            player(nationalityIndex),
            player(positionIndex),
            player(ageIndex).toInt,
            player(matchesIndex).toInt,
            player(startsIndex).toInt,
            player(minsIndex).toInt,
            player(goalsIndex).toInt,
            player(assistsIndex).toInt,
            player(passesAIndex).toInt,
            player(percPassesCIndex).toDouble,
            player(penaltyGIndex).toDouble,
            player(penaltyAIndex).toDouble,
            player(xGIndex).toDouble,
            player(xAIndex).toDouble,
            player(yellowCIndex).toInt,
            player(redCIndex).toInt
        )
        }
    }


    val players = extractPlayersFromCSV(filePath)
    val club = Club(players)
    println("\nWelcome to the Premier League")

    println("\nSum main statistics: ")
    val resultSum = club.sumStats();
    println((resultSum));

    println("\nAverage age for a team: ")
    var avg = 0.0
    var team = "Arsenal";
    club.avgAgeClub(team) match {
        case Some(value) => 
            avg = value.setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
            println(s"Average age for ${team}: $avg")
        case None => println("Invalid team");
    }

    println("\nHighest xg/xA player per team: ")
    club.maxXG().foreach(maxXG => 
        println(s"Club: ${maxXG._1} - Player: ${maxXG._2._1} xG: ${maxXG._2._2}, Player:${maxXG._2._3} xA ${maxXG._2._4}")
    )

    println("\nTransfer player: ")
    val playerTransfer = "Mason Mount"
    val newClub = "Arsenal"
    val nation = "ENG"
    val transList = club.changeClub(newClub, playerTransfer)
    val transClub = transList.getClub("Arsenal", Some(nation))
    println(transClub);

    println("\nAdd player: ")
    val newPlayer = Players("Loan Rey", "Bobo FC", "SUI", "Bench", 22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val playerAdd = club.addPlayer(newPlayer)
    println(playerAdd.getClub("Tonto FC", None))

    println("\nDelete player: ")
    println("\nCurrent Chelsea squad")
    println(club.getClub("Chelsea", Some("ENG")))
    val deltePlay = club.deletePlayer("Mason Mount", None)
    println("\nNew Chelsea squad")
    println(deltePlay.getClub("Chelsea", Some("ENG")))
    val deleteClub = club.deletePlayer(null, Some("Chelsea"))
    println("\nFinal Chelsea squad")
    println(deleteClub.getClub("Chelsea", None))

    def sort(sortedList:List[Players]):Unit = {
        sortedList.take(5).foreach(println(_))
    }

    println("\nSort goals: ")
    val attribute = "goals"
    try {
        val sortGoals = club.sortPlayersBy(attribute,players)
    } catch {
        case e: IllegalArgumentException =>
        println("Error: " + e.getMessage)
    }   

    println("\nSort name: ")
    val attribute2 = "name"
    try {
    val sortName = club.sortPlayersBy(attribute2,players)
    } catch {
        case e: IllegalArgumentException =>
        println("Error: " + e.getMessage)
    }   

    println("\nBallon d'argent: ")
    club.prediction()
    Thread.sleep(2000)

    println("\nNew goal: ")
    val scorer = "Mason Mount"
    val infoPlayer = club.getClub("Chelsea", None).take(1)
    println(infoPlayer)
    val preGoal = club.playerScored(scorer)
    val postGoal = preGoal.getClub("Chelsea", None
    ).take(1)
    println(postGoal)

    println("\nNew passes stat: ")
    val passer = "Mason Mount"
    val infoPlayer1 = club.getClub("Chelsea", None).take(1)
    println(infoPlayer1)
    val prePasses = club.passesStat(passer, 300, 450)
    val postPasses = prePasses.getClub("Chelsea", None
    ).take(1)
    println(postPasses)



}
