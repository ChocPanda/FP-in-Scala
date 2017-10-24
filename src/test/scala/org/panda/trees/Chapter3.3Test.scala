package org.panda.trees

import org.scalatest._
// import org.scalacheck.{Gen, Arbitrary}
// import org.scalacheck.Gen._
// import org.scalatest.prop.PropertyChecks

class TreeTests extends FlatSpec with Matchers {
  
    private val tree1 = Leaf(1)
    private val tree2 = Branch(Leaf(1), Leaf(2))
    private val tree3 = Branch(tree1, tree2)
    private val tree4 = Branch(Leaf(5), tree3)

    // This can definitely be expanded as a test but will do for now
  "Tree.size" should "be the number of nodes in the tree" in {
    tree1.size shouldBe 1
    tree2.size shouldBe 3
    tree3.size shouldBe (tree1.size + tree2.size + 1)
    tree4.size shouldBe (tree3.size + 2)
  }

  "Tree[Int].maximum" should "be the largest value in a leaf in the tree" in {
    Tree.maximum(tree1) shouldBe 1
    Tree.maximum(tree2) shouldBe 2
    Tree.maximum(tree3) shouldBe 2
    Tree.maximum(tree4) shouldBe 5
  }

  "Tree.depth" should "be the maximum path length in the tree" in {
    tree1.depth shouldBe 1
    tree2.depth shouldBe 2
    tree3.depth shouldBe 3
    tree4.depth shouldBe 4
  }

  // Test description wanted
  "Tree.map(_ + 1)" should "be the same as ..." in {
    val mappedTree1 = tree1.map(_ + 1)
    mappedTree1 shouldBe Leaf(2)

    val mappedTree2 = tree2.map(_ + 1)
    mappedTree2 shouldBe Branch(Leaf(2), Leaf(3))

    val mappedTree3 = tree3.map(_ + 1)
    mappedTree3 shouldBe Branch(mappedTree1, mappedTree2)

    val mappedTree4 = tree4.map(_ + 1)
    mappedTree4 shouldBe Branch(Leaf(6), mappedTree3)
  }

  "Tree.size2 implementations" should "match the behaviour of native implemtation" in {
    tree1.size2 shouldBe tree1.size
    tree2.size2 shouldBe tree2.size
    tree3.size2 shouldBe tree3.size
    tree4.size2 shouldBe tree4.size
  }

  "Tree.maximum2 implementations" should "match the behaviour of native implemtation" in {
    Tree.maximum2(tree1) shouldBe Tree.maximum(tree1)
    Tree.maximum2(tree2) shouldBe Tree.maximum(tree2)
    Tree.maximum2(tree3) shouldBe Tree.maximum(tree3)
    Tree.maximum2(tree4) shouldBe Tree.maximum(tree4)
  }

  "Tree.map2(_  + 5) implementations" should "match the behaviour of native implemtation" in {
    tree1.map2(_  + 5) shouldBe tree1.map(_  + 5)
    tree2.map2(_  + 5) shouldBe tree2.map(_  + 5)
    tree3.map2(_  + 5) shouldBe tree3.map(_  + 5)
    tree4.map2(_  + 5) shouldBe tree4.map(_  + 5)
  }
}

