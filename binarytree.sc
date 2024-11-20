class Node(val v: Int)

// Zipper stores the current paths that the current node can traverse 
class ZipperLocation(val tree: Tree, val context: Context)

// Context provides the ability to go back up the tree from its parent (and down other paths)
enum ContextType { case Top, Left, Right }

class Context(val v: Int = -1, val contextType: ContextType = ContextType.Top, val tree: Option[Tree] = None, val context: Option[Context] = None)

class Tree(val node: Node, val left: Option[Tree] = None, val right: Option[Tree] = None)

def up(loc: ZipperLocation): ZipperLocation = loc.context.contextType match {
    case ContextType.Top => loc
    case _ => new ZipperLocation(loc.context.tree.get, new Context(
        loc.context.context.get.tree.get.node.v,
        loc.context.context.get.contextType,
        Some(loc.tree),
        loc.context.context.get.context
    ))
}

def left(loc: ZipperLocation): ZipperLocation = loc.tree.left match {
    case None => loc
    case Some(left: Tree) => new ZipperLocation(left, new Context(
        left.node.v,
        ContextType.Left,
        Some(left),
        Some(loc.context)
    ))
}

def right(loc: ZipperLocation): ZipperLocation = loc.tree.right match {
    case None => loc
    case Some(right: Tree) => new ZipperLocation(right, new Context(
        right.node.v,
        ContextType.Right,
        Some(right),
        Some(loc.context)
    ))
}
 
val leftSubtree = new Tree(
    new Node(2), 
    left = Some(new Tree(new Node(1))), 
    right = Some(new Tree(new Node(4)))
    )

val rightSubtree = new Tree(new Node(5), left = Some(new Tree(new Node(4))))
val tree = new Tree(new Node(3), Some(leftSubtree), Some(rightSubtree))


def dfs(n: Int, tree: Tree, trace: List[Int]): List[List[Int]] = {
    val currentTrace = trace :+ tree.node.v

    val currentMatches = if (tree.node.v == n) List(currentTrace) else List()

    val leftMatches = tree.left match {
        case Some(left: Tree) => dfs(n, left, currentTrace)
        case None => List()
    }

    val rightMatches = tree.right match {
        case Some(right: Tree) => dfs(n, right, currentTrace)
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
