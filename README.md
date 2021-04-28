## sbt project compiled with Scala 3

Used to find/check bug with typed. 
We have an object that is a Tree, not a Term, but pattern matches to both case Unapply and case Typed

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).
