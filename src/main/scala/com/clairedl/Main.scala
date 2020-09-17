package com.clairedl.scala

import scala.collection.mutable.ListBuffer

object Main extends App {
  class Person(val name: String, val father: Option[Person], val mother: Option[Person]) {
    def paternalGrandFather: Option[Person] = {
      father.flatMap(x => x.father)

      // for {
      //   x <- father
      // } yield x.father

      // father match {
      //   case Some(x) => x.father
      //   case None => None
      // }
    }

    def fatherOf(parent: Option[Person]): Option[Person] = {
      parent.flatMap(x => x.father)
    }

    def motherOf(parent: Option[Person]): Option[Person] = {
      parent.flatMap(x => x.mother)
    }

    def grandParents: List[Option[Person]] = {
      fatherOf(father) :: motherOf(father) :: fatherOf(mother) :: motherOf(mother) :: Nil
    }

    def getGrandParent(parent: Option[Person], grandParentIsMale: Boolean): Option[Person] = {
      if (grandParentIsMale) (parent.flatMap(x => x.father))
      else (parent.flatMap(x => x.mother))
    }
  }

  val gerard = new Person("Gerard", None , None)
  val genevieve = new Person("Genevieve", None, None)
  val celine = new Person("Celine", Some(gerard), Some(genevieve))
  val claire = new Person("Claire", Some(gerard), Some(genevieve))
  val jeando = new Person("Jean-Dominique", Some(gerard), Some(genevieve))
  val franck = new Person("Franck", None, Some(new Person("Francoise", None, None)))
  val mael = new Person("Mael", Some(franck), Some(celine))
  val loick = new Person("Loick", Some(franck), Some(celine))

  val grandParents = loick.grandParents
  for (grandParent <- grandParents) grandParent.map(x => println(x.name))
}
