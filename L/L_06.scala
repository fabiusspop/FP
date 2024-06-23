package L

object L_06 extends App{
  /*
  In this lab, we will use lists to implement a lot of string processing.
  The type "String" is not decomposable as a list,
    BUT strings can be converted to lists of Char as follows:
   */

  //  println("Some string".toList)

  // For this reason, we will use the following aliases:
  type Str = List[Char]
  type Email = Str
  // Use higher-order function whenever possible
  // map, foldRight, foldLeft, filter, zip, partition, etc

  type Gradebook = List[(String,Int)] // list of pairs
  val gradebook = List(("G",3), ("F", 10), ("M",6), ("P",4))

  // 6.1.1. increment --> adds one point to all students which have a passing grade (>= 5), and leaves all other grades unchanged
  def increment(g: Gradebook): Gradebook = {
    g.map {
      case (name, grade) =>
        if (grade >= 5) (name, grade + 1)
        else (name, grade)
    }
  }

  // 6.1.2. average --> finds the average grade from a gradebook. You must use foldRight
  def average(g: Gradebook): Double = {
    val sum = g.foldRight(0.0){
      case ((_, grade), acc) => grade + acc
    }
    sum / g.length
  }

  // 6.1.3. percentage --> return the percentage of failed vs passes students, as a pair (x, y)
  def percentage(g: Gradebook): (Double, Double) = {
    val (passed, failed) = g.partition {
      case (_, grade) => grade >= 5
    }
    val total = g.length.toDouble
    val failedPercentage = failed.length.toDouble / total * 100
    val passedPercentrage = passed.length.toDouble / total * 100

    (failedPercentage, passedPercentrage)
  }

  // 6.1.4. pass --> return the list of names which have passed.
  // use filter and map from Scala
  def pass(g: Gradebook): List[String] = {
    g.filter {
      case(_, grade) => grade >= 5
    }.map {
      case (name, _) => name
    }
  }

  // 6.1.5. honorsList --> reports all passing students in descending order of their grade
  def honorsList(g: Gradebook): List[String] = {
    g.filter {
      case (_, grade) => grade >= 5
    }.sortWith {
      case ((_, grade1), (_, grade2)) => grade1 > grade2
    }.map {
      case (name, _) => name
    }
  }

  // we extend the type "Gradebook" to:
  type Name = String
  type Lecture = String
  type ExtGradebook = List[(Name, Lecture, Int)]
  val egradebook = List(("John", "FP", 4))

  // 6.1.6 atLeastOneFail --> reports all students that have failed at least one grade (each student will be reported once)
  def atLeastOneFail(g: ExtGradebook): List[Name] = {
    g.filter{
      case (_, _, grade) => grade < 5
    }.map {
      case (name, _, _) => name
    }.distinct
  }

  // 6.1.7 groupBy --> generalisation of the previous exercise
  def groupBy[A, B](l: List[A])(criterion: A => B): List[(B, List[A])] = {
    l.foldLeft(Map.empty[B, List[A]]) {
      (acc, elem) =>
        val key = criterion(elem)
        acc.get(key) match {
          case Some(list) => acc + (key -> (elem :: list))
          case None => acc + (key -> List(elem))
        }
    }.toList
  }

  // 6.2. List of Emails
  // 6.2.1. getNames -> takes a list of emails and extracts the prefix
  def getName(l: List[Email]): List[Email] = {
    l.map(email => email.takeWhile(_ != '@'))
  }

  // 6.2.2. removeTLD -> filters out emails belonging to a specific Top-Level-Domain (e.g.: com)
  def removeTLD(l: List[Email], tld: Str): List[Email] = {
    l.filterNot(email => email.reverse.takeWhile(_ != '.').reverse == tld)
  }

  // 6.3.3. containsDuplicates -> check if there exists identical names under different domains
  def containsDuplicates(l: List[Email]): Boolean = {
    val names = l.map(email => email.takeWhile(_ != '@'))
    names.distinct.length != names.length
  }

  // 6.4.4. countDuplicates -> reports the number of duplicates of names under different domain in a list of emails
  def countDuplicates(l: List[Email]): Int = {
    val names = l.map(email => email.takeWhile(_ != '@'))
    val duplicates = names.diff(names.distinct)
    duplicates.distinct.length
  }

  // 6.2.5. extractDuplicates -> reports all duplicates of names under different domains in a list of emails
  def extractDuplicates(l: List[Email]): List[List[Email]] = {
    val namesToEmails = l.groupBy(email => email.takeWhile(_ != '@'))
    val duplicates = namesToEmails.filter(_._2.length > 1)
    duplicates.values.map(emails => emails.map(email => email.takeWhile(_ != '@'))).toList
  }
}
