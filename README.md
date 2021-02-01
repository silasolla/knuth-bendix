# Knuth-Bendix Completion in Standard ML

Knuth-Bendix完備化を行い，等式を向き付けする．

```
$ sml
- CM.make "sources.cm"
- val es = Trs.rdEqs "[+(0,?x) = ?x, +(-(?x),?x) = 0, +(+(?x,?y),?z) = +(?x,+(?y,?z))]";
- val grter = PathOrder.lpoGt (PathOrder.rdPrec [("-",3),("+",2),("0",1)]);
- Comp.kb grter es;
Step 1
   [ +(0,?x) = ?x,
     +(-(?x),?x) = 0,
     +(+(?x,?y),?z) = +(?x,+(?y,?z)) ]
   [  ]
Step 2
   [ +(-(?x),?x) = 0,
     +(+(?x,?y),?z) = +(?x,+(?y,?z)) ]
   [ +(0,?x) -> ?x ]

...

Step 19
Success
   [ -(+(?x,?z_1)) -> +(-(?z_1),-(?x)),
     +(?x,+(-(?x),?z_1)) -> ?z_1,
     +(?x,-(?x)) -> 0,
     +(?x,0) -> ?x,
     -(-(?x)) -> ?x,
     -(0) -> 0,
     +(-(?x),+(?x,?z_1)) -> ?z_1,
     +(+(?x,?y),?z) -> +(?x,+(?y,?z)),
     +(-(?x),?x) -> 0,
     +(0,?x) -> ?x ]
val it = true : bool
-
```
