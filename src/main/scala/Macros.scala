import scala.quoted.*
import scala.reflect.*

object Macros {
    
    inline def show[A,B](inline expr: A => B): Unit =
        ${printExpr('expr)}
    
    def printExpr[A,B](expr: Expr[A=>B])(using Quotes) = '{
        println(${showExpr3(expr)})
    }

    def showExpr3[A,B](expr: Expr[A=>B])(using Quotes): Expr[String] =
        import quotes.reflect.*
        // Makes us only print the body of thr function
        def printDefFun(tree: Tree): Unit ={
            val acc = new TreeAccumulator[Unit]{
                def foldTree(s: Unit, tree: Tree)(owner: Symbol): Unit = 
                    tree match
                        case deff : DefDef =>
                            treePrint(deff.rhs.get, 0)
                            println("++++++++++++++++")
                            println(deff.rhs.get.show(using Printer.TreeStructure))
                        case _ =>
                            foldOverTree(s, tree)(owner)
                }
            acc.foldTree(List(), tree)(tree.symbol)
        }

        def treePrint(tree: Tree, level: Int): Unit = {
            val pre = "   " * level
            tree match {
                case body : Term => { 
                    body match {
                        // Normal typed
                        case typed: Typed =>
                            println(pre + typed.getClass())
                            println(pre + s"Typed with ${typed.tpt}:")
                            treePrint(typed.expr , level + 1)
                        case Block(statements, expr) =>
                            println(pre + "Block:{")
                            statements.map(stat => stat match{
                                case term: Term => treePrint(term, level + 1)
                                case deff: Definition =>
                                    println(pre + "Definition statement")
                                    treePrint(deff, level + 1)
                                case _ =>
                                    println(pre + "Non-term statement")
                                    println(stat.show(using Printer.TreeStructure))
                                })
                            treePrint(expr, level + 1)
                            println(pre + "}")

                        case Match(scrutinee, cases) =>
                            println(pre + "Match:")
                            treePrint(scrutinee, level + 1)
                            println(pre + "with")
                            cases.map(treePrint(_, level +1))

                        case Ident(name) =>
                            println(pre + s"Identifier(${name})")
                        
                        case Apply(fun, args) =>
                            println(pre + "Apply")
                            treePrint(fun, level + 1)
                            if !args.isEmpty then
                                println(pre + "with arguments")
                                args.zipWithIndex.map(
                                    (arg, index) =>  
                                        treePrint(arg, level +1)
                                        if args.size > 1 && index < args.size -1  then 
                                            // Used to seperate list of parameters
                                            println(pre + ",")
                                    )
                        case _ => 
                            println("Term")
                            println(tree.getClass())
                            println(tree.show(using Printer.TreeStructure))
                    }
                }
                
                case CaseDef(pattern, guard, rhs) =>
                    println(pre + "caseDef:" )
                    treePrint(pattern, level + 1)
                    treePrint(rhs, level + 1) 
                
                //Adding this unappy makes the typed get swallowed
                /*      
                case Unapply(fun, implicits, pattern) => 
                    println(pre + "Unapply with function") 
                    treePrint(fun , level + 1)
                    println(pre + "with patterns") 
                    pattern.map(treePrint(_ , level + 1))
                */
                case b: Bind => println(pre + "Bind with stuff")
                
                case typed : Typed =>
                        //println(pre + typed.getClass())
                        println(pre + tree.getClass())
                        println(pre + s"Typed2 with ${typed.tpt}:")
                        treePrint(typed.expr , level + 1)
                
                case Unapply(_,_,_) => println(pre + "Unapply with stuff")
                case _ => 
                    tree match
                        case t: Term => println("Term")
                        case _ => ()
                    println(tree.getClass())
                    println(tree.show(using Printer.TreeStructure))
            }
        }

        val tree: Term = expr.asTerm
        printDefFun(tree)
        Expr("Finished")
}

