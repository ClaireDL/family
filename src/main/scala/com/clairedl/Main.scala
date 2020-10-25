package com.clairedl.scala

import scala.collection.mutable.ListBuffer

object Main extends App {
  class Person(val name: String, val father: Option[Person], val mother: Option[Person]) {

    def parents: List[Person] = List(father, mother).flatten

    def paternalGrandFather: Option[Person] = {
      father.flatMap(_.father)

      // for {
      //   x <- father
      // } yield x.father

      // father match {
      //   case Some(x) => x.father
      //   case None => None
      // }
    }

    private def fatherOf(parent: Option[Person]): Option[Person] = {
      parent.flatMap(_.father)
    }

    private def motherOf(parent: Option[Person]): Option[Person] = {
      parent.flatMap(_.mother)
    }

    // def grandParents: List[Option[Person]] = {
    //   fatherOf(father) :: motherOf(father) :: fatherOf(mother) :: motherOf(mother) :: Nil
    // }

    val grandParents: List[Person] =
      List(fatherOf(father), motherOf(father), fatherOf(mother), motherOf(mother)).flatten

    def grandChildren: List[Person] = children.flatMap(_.children)

    private var _children: List[Person] = List.empty

    // Getter method
    def children: List[Person] = this._children

    // Setter method
    def setChildren(c: List[Person]): Unit = {
      this._children = c
      identifySiblings(c)
    }

    private var _siblings: List[Person] = List.empty

    def siblings: List[Person] = this._siblings

    private def identifySiblings(children: List[Person]): Unit =
      for (child <- children) {
        child._siblings = children.filter(_ != child)
      }

    def unclesAndAunts: List[Person] = parents.flatMap(_.siblings)

    def cousins: List[Person] = unclesAndAunts.flatMap(_.children)
  }

  // Family members
  val gerard = new Person("Gerard", None , None)
  val genevieve = new Person("Genevieve", None, None)
  val celine = new Person("Celine", Some(gerard), Some(genevieve))
  val claire = new Person("Claire", Some(gerard), Some(genevieve))
  val jeando = new Person("Jean-Dominique", Some(gerard), Some(genevieve))
  val franck = new Person("Franck", None, Some(new Person("Francoise", None, None)))
  val mael = new Person("Mael", Some(franck), Some(celine))
  val loick = new Person("Loick", Some(franck), Some(celine))
  val olivia = new Person("Olivia", Some(jeando), Some(new Person("Pauline", None, None)))
  val andre = new Person("Andre", Some(jeando), Some(new Person("Pauline", None, None)))

  // Establishing parent-child relationships
  gerard.setChildren(List(celine, claire, jeando))
  genevieve.setChildren(gerard.children)
  celine.setChildren(List(mael, loick))
  franck.setChildren(celine.children)
  jeando.setChildren(List(olivia, andre))

  // Other relationships
  // Grandparents
  println(s"These are the grandparents of ${loick.name}:")
  println(loick.grandParents.map(_.name))

  // Children
  println(s"These are the children of ${genevieve.name}:")
  println(genevieve.children.map(_.name))

  // Grandchildren
  println(s"These are the grandchildren of ${gerard.name}:")
  println(gerard.grandChildren.map(_.name))

  // Siblings
  println(s"These are the siblings of ${claire.name}:")
  println(claire.siblings.map(_.name))

  // Parents
  println(s"These are the parents of ${mael.name}:")
  println(mael.parents.map(_.name))

  // Uncles and aunts
  println(s"These are the uncles and aunts of ${mael.name}:")
  println(mael.unclesAndAunts.map(_.name))

  // Cousins
  println(s"These are the cousins of ${mael.name}:")
  println(mael.cousins.map(_.name))
}
