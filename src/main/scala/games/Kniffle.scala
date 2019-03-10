package games

import java.lang.NumberFormatException
import java.io.IOException

import scalaz._
import Scalaz._
import scalaz.zio._
import scalaz.zio.console._
import shapeless._
//import scalaz.zio.interop.scalaz72._

import scala.language.higherKinds
import scala.util.Random

/**
  * Kniffle game
  * 1. choose number of players
  * 2. roll dice
  * 3. choose dice to keep
  * 4. either assign to available hand or re-roll
  * 5. if chose to re-roll repeat step 2-4
  * 6. if chose to re-roll repoeat step 2 and then assign hand
  * 7. next player turn until both players have filled all hands
 **/
object Kniffle extends App {

  sealed trait Die
  case object One   extends Die
  case object Two   extends Die
  case object Three extends Die
  case object Four  extends Die
  case object Five  extends Die
  case object Six   extends Die

  case class FiveDice(d1: Die, d2: Die, d3: Die, d4: Die, d5: Die)

    def countDieMatches(d: Die, roll: FiveDice) =
          Generic[FiveDice]
            .to(roll)
            .toList
            .count(_ == d)



  sealed trait Outcome
  implicit case object Ones extends Outcome
  implicit case object Twos extends Outcome
  implicit case object Threes extends Outcome
  implicit case object Fours extends Outcome
  implicit case object Fives extends Outcome
  implicit case object Sixes extends Outcome
  implicit case object ThreeOfKind extends Outcome
  implicit case object FourOfKind extends Outcome
  implicit case object FiveOfKind extends Outcome
  implicit case object ThreeStraight extends Outcome
  implicit case object FourStraight extends Outcome
  implicit case object Chance extends Outcome


  case class HandLine[T <: Outcome](outcome: T, rollResult: Option[FiveDice])
  object HandLine {
    def empty[T <: Outcome]()(implicit t: T) = HandLine(t, None)
  }

  case class Hand(
      ones: HandLine[Ones.type],
      twos: HandLine[Twos.type],
      threes: HandLine[Threes.type],
      fours: HandLine[Fours.type],
      fives: HandLine[Fives.type],
      sixes: HandLine[Sixes.type],
      threeOfKind: HandLine[ThreeOfKind.type],
      fourOfKind: HandLine[FourOfKind.type],
      fiveOfKind: HandLine[FiveOfKind.type],
      threeStraight: HandLine[ThreeStraight.type],
      fourStraight: HandLine[FourStraight.type],
      chance: HandLine[Chance.type]
  )

  object Hand {
    def empty() = new Hand(
      HandLine.empty[Ones.type],
      HandLine.empty[Twos.type],
      HandLine.empty[Threes.type],
      HandLine.empty[Fours.type],
      HandLine.empty[Fives.type],
      HandLine.empty[Sixes.type],
      HandLine.empty[ThreeOfKind.type],
      HandLine.empty[FourOfKind.type],
      HandLine.empty[FiveOfKind.type],
      HandLine.empty[ThreeStraight.type],
      HandLine.empty[FourStraight.type],
      HandLine.empty[Chance.type],
      )

  }

  //TODO make constructor for emptyHand

  case class PlayerState(name: String, hand: Hand)

  case class State(players: List[PlayerState])

  def advancePlayer(state: State): State =
    State(state.players.tail :+ state.players.head)

  val kniffleGame: IO[IOException, Unit] =
    for {
      _               <- putStrLn("Functional Kniffle")
      numberOfPlayers <- getNumberPlayers
      playerNames     <- getPlayerNames(numberOfPlayers)
      state = State(playerNames.map(name =>  PlayerState(name, Hand.empty)))
      _ <- renderState(state)
      _ <- gameLoop(state)
    } yield ()

  override def run(args: List[String]): IO[Nothing, Kniffle.ExitStatus] =
    kniffleGame.redeemPure(
      _ => ExitStatus.ExitNow(1),
      _ => ExitStatus.ExitNow(0)
    )


  def toDie(s: String): Option[Die] = s match {
    case "1" => Some(One)
    case "2" => Some(Two)
    case "3" => Some(Three)
    case "4" => Some(Four)
    case "5" => Some(Five)
    case "6" => Some(Six)
    case _   => None
  }

  def getDie(r: String, roll: List[Die]): Option[List[Die]] =
    for {
      d <- toDie(r)
      c <- rollContains(d, roll)
    } yield(c)

  def rollContains(die: Die, roll: List[Die]): Option[List[Die]] =
    roll
      .contains(die) match {
        case false => None
        case true => Some(roll.tail)
      }



  def parseRetainString(retain: String): Reader[FiveDice, Option[List[Die]]] = Reader {
    roll => {
      val rollList: List[Die] =
        Generic[FiveDice]
          .to(roll)
          .toList

    val retainedDice: String => Option[List[Die]] =
      _.split(",")
        .toList
        .map(toDie)
        .sequence

    val isValidRetainment: List[Die] => Option[List[Die]] =
      retain => {
        val dieCounts: List[Die] => Map[Die,Int] = _.groupBy(identity).mapValues(_.length)
        val retVals = dieCounts(retain)
        val rollVals = dieCounts(rollList)
        retVals.keys.forall(x => retVals(x) <= rollVals(x)).option(retain)
      }

    val getRetained = Kleisli(retainedDice) >=> Kleisli(isValidRetainment)
    getRetained(retain)

    }
  }
  //TODO take intersection with old dice

  case class Assignment(roll: FiveDice, outcome: Outcome)

  private def getRetained(roll: FiveDice, turnsTaken: Int, currentPlayer: String): IO[IOException, FiveDice] =
    for {
    retainStr <- putStrLn(s"your roll is $roll") *>
     putStrLn(s"which dice would you like to keep? ") *>
     getStrLn
    retained <- parseRetainString(retainStr) match {
      case None => putStrLn("you need to retain 5 dice") *>
        getRetained(roll, turnsTaken, currentPlayer)
      case Some(fd) => IO.now(fd)
    }
  } yield(retained)

  private def rollLoop(currentPlayer: String): IO[IOException, Unit] =
    for {
      _ <- putStrLn(s"current player is $currentPlayer")
      roll <- rollDice
      retained <- getRetained(roll, 0, currentPlayer)
    } yield()


  private def gameLoop(state: State): IO[IOException, State] =
    for {
      _ <- putStrLn("your turn")
      currentPlayer = state.players.head
      _ <- rollLoop(currentPlayer.name)
      _ <- gameLoop(advancePlayer(state))
    } yield (state)


  //TODO move to common code
  def nextInt(max: Int): IO[Nothing, Int] =
    IO.sync(Random.nextInt(max))

  //TODO accept an n: Int that is the number of dice you want rolled
  private val rollDice: IO[Nothing, FiveDice] =
    IO.traverse(List(0,1,2,3,4))(_ => rollDie).map(l =>
        FiveDice(l(0), l(1), l(2), l(3), l(4)))


  private def rollDie(): IO[Nothing, Die] = nextInt(5).map(_ + 1).map(_ match {
    case 1 => One
    case 2 => Two
    case 3 => Three
    case 4 => Four
    case 5 => Five
    case 6 => Six
  })




  private def renderState(state: State): IO[IOException, Unit] = {
    for {
      _ <- putStrLn(s" the players are ${state.players.map(_.name)}")
    } yield ()
  }

  private val getNumberPlayers: IO[IOException, Int] =
    for {
      ans <- putStrLn("How many players?") *> getStrLn
      val answer = ans.toInt // TODO this is unsafe
      loop <- if (answer > 0 && answer <= 10) IO.now(false) else IO.now(true)
      ans <- if (loop) getNumberPlayers else IO.now(answer)
    } yield (ans.toInt)

  private def getName(n: Int): IO[IOException, String] =
    putStrLn(s"player $n: what is your name?") *> getStrLn

  private def getPlayerNames(numberOfPlayers: Int): IO[IOException, List[String]] =
    IO.traverse((1 to numberOfPlayers).toList)(getName(_))

}
