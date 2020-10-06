package com.clairedl.scala

import scala.collection.mutable.ListBuffer

object Main extends App {
  class Person(val name: String, val father: Option[Person], val mother: Option[Person]) {
    def parents: List[Person] = List(father.get, mother.get)

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

    // Solution #1
    private def fatherOf(parent: Option[Person]): Option[Person] = {
      parent.flatMap(x => x.father)
    }

    private def motherOf(parent: Option[Person]): Option[Person] = {
      parent.flatMap(x => x.mother)
    }

    def grandParents: List[Option[Person]] = {
      fatherOf(father) :: motherOf(father) :: fatherOf(mother) :: motherOf(mother) :: Nil
    }

    def grandParent(parent: Option[Person], grandParentIsMale: Boolean): Option[Person] = {
      if (grandParentIsMale) (parent.flatMap(x => x.father))
      else (parent.flatMap(x => x.mother))
    }

    // Solution #2
    def grandParents2: List[Option[Person]] = {
      grandParent(father, true) ::
      grandParent(father, false) ::
      grandParent(mother, true) ::
      grandParent(mother, false) ::
      Nil
    }

    def grandChildren: List[Person] = children.flatMap(x => x.children)

    var children = List[Person]()

    def children(c: List[Person]): Unit = {
      children = c
      identifySiblings(c)
    }

    var siblings = List[Person]()

    private def identifySiblings(children: List[Person]): Unit = {
      if (children.length > 1) {
        for (person <- children) (person.siblings = children.filter(x => x != person))
      }
    }

    def unclesAndAunts: List[Person] = parents.flatMap(x => x.siblings)

    def cousins: List[Person] = unclesAndAunts.flatMap((x => x.children))
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

  // Establishing parent-child relationship
  gerard.children(List(celine, claire, jeando))
  genevieve.children = gerard.children
  celine.children(List(mael, loick))
  franck.children = celine.children
  jeando.children(List(olivia, andre))

  // Other relationships
  // Grandparents, using 2 methods
  println(s"These are the grandparents of ${loick.name}:")
  val grandParents = loick.grandParents
  for (grandParent <- grandParents) grandParent.map(x => println(x.name))
  val grandParents2 = loick.grandParents2
  for (grandParent <- grandParents2) grandParent.map(x => println(x.name))

  // Children
  println(s"These are the children of ${genevieve.name}:")
  println(genevieve.children.map(x => x.name))

  // Grandchildren
  println(s"These are the grandchildren of ${gerard.name}:")
  println(gerard.grandChildren.map(x => x.name))

  // Siblings
  println(s"These are the siblings of ${claire.name}:")
  println(claire.siblings.map(x => x.name))

  // Parents
  println(s"These are the parents of ${mael.name}:")
  println(mael.parents.map(x => x.name))

  // Uncles and aunts
  println(s"These are the uncles and aunts of ${mael.name}:")
  println(mael.unclesAndAunts.map(x => x.name))

  // Cousins
  println(s"These are the cousins of ${mael.name}:")
  println(mael.cousins.map(x => x.name))
}
