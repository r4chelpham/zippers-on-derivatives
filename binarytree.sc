class Node(val v: Int) {
    def get(): Int = v
}

class Tree(val node: Node, val left: Option[Tree] = None, val right: Option[Tree] = None) {
    def get(): Node = node
    def left(): Tree = tree.left
    def right(): Tree = tree.right
}


val leftSubtree = new Tree(new Node(2), 
    left = Some(new Tree(new Node(1))), 
    right = Some(new Tree(new Node(4)))
)
val rightSubtree = new Tree(new Node(5), left = Some(new Tree(new Node(4))))
val tree = new Tree(new Node(3), Some(leftSubtree), Some(rightSubtree))

def dfs(n: Int, tree: Tree, trace: List[Int]): List[List[Int]] = {
    val currentTrace = trace :+ tree.node.get()

    val currentMatches = if (tree.node.get() == n) List(currentTrace) else List()

    val leftMatches = tree.left match {
        case Some(left) => dfs(n, left, currentTrace)
        case None => List()
    }

    val rightMatches = tree.right match {
        case Some(right) => dfs(n, right, currentTrace)
        case None => List()
    }

    currentMatches ++ leftMatches ++ rightMatches
}


@main
def main() = {
  val ans1 = dfs(1, tree, Nil)
  println(ans1)

  val ans2 = dfs(6, tree, Nil)
  println(ans2)

  val ans3 = dfs(4, tree, Nil)
  println(ans3)
  
}
