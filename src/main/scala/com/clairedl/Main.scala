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

    // Solution #1
    protected def fatherOf(parent: Option[Person]): Option[Person] = {
      parent.flatMap(x => x.father)
    }

    protected def motherOf(parent: Option[Person]): Option[Person] = {
      parent.flatMap(x => x.mother)
    }

    def grandParents: List[Option[Person]] = {
      fatherOf(father) :: motherOf(father) :: fatherOf(mother) :: motherOf(mother) :: Nil
    }

    // Solution #2
    def grandParent(parent: Option[Person], grandParentIsMale: Boolean): Option[Person] = {
      if (grandParentIsMale) (parent.flatMap(x => x.father))
      else (parent.flatMap(x => x.mother))
    }

    def grandParents2: List[Option[Person]] = {
      grandParent(father, true) ::
      grandParent(father, false) ::
      grandParent(mother, true) ::
      grandParent(mother, false) ::
      Nil
    }

    var children = List[Person]()

    def children(c: List[Person]): Unit = {
      children = c
      identifySiblings(c)
    }

    var siblings = List[Person]()

    def identifySiblings(children: List[Person]): Unit = {
      if (children.length > 1) {
        for (person <- children) (person.siblings = children.filter(x => x != person))
      }

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

  println(s"These are the grandparents of ${loick.name}:")
  val grandParents = loick.grandParents
  for (grandParent <- grandParents) grandParent.map(x => println(x.name))
  val grandParents2 = loick.grandParents2
  for (grandParent <- grandParents2) grandParent.map(x => println(x.name))

  gerard.children(List(celine, claire, jeando))
  genevieve.children = gerard.children
  println(s"These are the children of ${genevieve.name}:")
  println(genevieve.children.map(x => x.name))
  celine.children(List(mael, loick))
  franck.children = celine.children

  println(claire.siblings.map(x => x.name))

}
