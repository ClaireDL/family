package com.clairedl.scala

import scala.collection.mutable.ListBuffer

object Main extends App {
  class Person(val name: String, val father: Option[Person], val mother: Option[Person]) {
    lazy val children = childrenTemp
    private var childrenTemp = List[Person]()

    lazy val siblings = siblingsTemp
    private var siblingsTemp = List[Person]()

    lazy val grandParents = grandParentsTemp
    private var grandParentsTemp = List[Person]()

    def setChildren(c: List[Person]): Unit = {
      childrenTemp = c
    }

    def setSiblings: Unit = {
      val allSiblings = father.get.children
      for (s <- allSiblings) {
        val result = allSiblings.filter(x => x != s)
        s.siblingsTemp = result
      }
    }

    def setParents: Unit = {

    }
  }

  val gerard = new Person("Gerard", None , None)
  val genevieve = new Person("Genevieve", None, None)
  val celine = new Person("Celine", Some(gerard), Some(genevieve))
  val claire = new Person("Claire", Some(gerard), Some(genevieve))
  val jeando = new Person("Jean-Dominique", Some(gerard), Some(genevieve))
  gerard.setChildren((List(celine, claire, jeando)))
  val franck = new Person("Franck", None, Some(new Person("Francoise", None, None)))
  val mael = new Person("Mael", Some(franck), Some(celine))
  val loick = new Person("Loick", Some(franck), Some(celine))

  celine.setChildren(List(mael, loick))
  celine.setSiblings
  println(s"${celine.name}'s father is called ${celine.father.get.name}")
  println(s"${celine.name}'s mother is called ${celine.mother.get.name}")
  println(s"${celine.name}'s children are:")
  for (child <- celine.children) println(child.name)
  println(s"${claire.name}'s siblings are:")
  for (sibling <- claire.siblings) println(sibling.name)
  println(s"${celine.name}'s siblings are:")
  for (sibling <- celine.siblings) println(sibling.name)

  mael.setGrandParents
}
