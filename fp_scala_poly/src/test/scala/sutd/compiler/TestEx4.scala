package sutd.compiler
import scala.language.adhocExtensions
import org.scalatest.funsuite
import org.scalatest.matchers
import sutd.compiler.Ex4.{given, *}

class TestEx4 extends funsuite.AnyFunSuite {
    test("test ex4: listFoldable.foldLeft(List(1,2,3,4))(0)((x:Int,y:Int) => x - y) == -10 and listFoldable.foldRight(List(1,2,3,4))(0)((x:Int,y:Int) => x - y) == -2") {
        val l = List(1,2,3,4)
        assert(listFoldable.foldLeft(l)(0)((x:Int,y:Int) => x - y) == -10)
        // (((0-1)-2)-3)-4
        assert(listFoldable.foldRight(l)(0)((x:Int,y:Int) => x - y) == -2)
        // 1-(2-(3-(4-0)))

    }
    test("test ex4: bstFoldable.foldLeft(Node(5, Node(3, Node(1, Empty, Empty), Node(4,Empty, Empty)), Node(7, Empty, Empty)))('')((x:String, y:Int) => x + ' ' + y.toString)  == ' 5 3 1 4 7'" +
    " and bstFoldable.foldRight(Node(5, Node(3, Node(1, Empty, Empty), Node(4,Empty, Empty)), Node(7, Empty, Empty)))('')((x:String, y:Int) => x + ' ' + y.toString)  == '5 7 3 4 1 '") {
        import Ex3.BST.*
        val b = Node(5, Node(3, Node(1, Empty, Empty), Node(4,Empty, Empty)), Node(7, Empty, Empty))

        assert(bstFoldable.foldLeft(b)("")((x:String, y:Int) => x + " " + y.toString)  == " 5 3 1 4 7")
        // C-L-R in order
        assert(bstFoldable.foldRight(b)("")((x:Int, y:String) => x.toString + " " + y)  == "5 7 3 4 1 ")
        // C-R-L order
    }
}