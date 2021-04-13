# Knuth-Bendix Completion in Standard ML

Knuth-Bendix 完備化をはじめとした，等式論理や一階項書き換え系を操作する機能を提供しています．

## はじめに

### 等式論理について

与えられた等式集合のもとで，どのような等式を導くことができるかを考える論理を等式論理と言います．
例えば，以下の等式集合を考えます．

<img src="https://latex.codecogs.com/svg.latex?E&space;=&space;\{\mathsf{0}&space;&plus;&space;x&space;=&space;x,(-x)&plus;x=\mathsf{0},&space;(x&plus;y)&plus;z=x&plus;(y&plus;z)\}"/>

この等式集合を用いることで，以下のように別の等式を導くことができます．

<img src="https://latex.codecogs.com/svg.latex?\begin{align*}&space;x&plus;(-x)&=\mathsf{0}&plus;(x&plus;(-x))\\&space;&=((-(-x))&plus;(-x))&plus;(x&plus;(-x))\\&space;&=(-(-x))&plus;((-x)&plus;(x&plus;(-x)))\\&space;&=(-(-x))&plus;(((-x)&plus;x)&plus;(-x))\\&space;&=(-(-x))&plus;(\mathsf{0}&plus;(-x))\\&space;&=(-(-x))&plus;(-x)\\&space;&=\mathsf{0}&space;\end{align*}"/>

### 項書き換え系と Knuth-Bendix 完備化について

前節で紹介した等式論理では，等式の左辺から右辺への変形と右辺から左辺への変形の両方が可能なので，コンピュータで扱うのが難しいです．そこで，等式に向きづけして書き換え規則の集合を作ります．これを項書き換え系といいます．

<img src="https://latex.codecogs.com/svg.latex?\mathcal{R}=\{\mathsf{0}&plus;x\to&space;x,(-x)&plus;x\to\mathsf{0},(x&plus;y)&plus;z\to&space;x&plus;(y&plus;z)\}"/>

この項書き換え系の等式を導く力は，向きづけしたことで元の等式集合よりも弱くなっています．先に用いるルールを変えることで，二つの異なる計算結果が得られてしまいます．

<img src="https://latex.codecogs.com/svg.latex?\begin{align*}&space;((-x)&plus;x)&plus;z&\to&space;\mathsf{0}&plus;z\to&space;z\\&space;((-x)&plus;x)&plus;z&\to&space;(-x)&plus;(x&plus;z)&space;\end{align*}"/>

一方，以下の項書き換え系は向きづけしても等式を導く力が衰えておらず，どのルールを用いても同じ計算結果が得られます．
また，式変形を続けると必ず停止するため，コンピュータで扱うことが容易です．

<img src="https://latex.codecogs.com/svg.latex?\mathcal{R}=&space;\{\mathsf{0}&plus;y\to&space;y,&space;\mathsf{succ}(x)&plus;y\to\mathsf{succ}(x&plus;y)\}"/>

このような項書き換え系を完備な項書き換え系といいます．Knuth, Bendix は等式集合から同じ力を持つ完備な項書き換え系を計算する手続きを考案しました．この手続きは必ず停止するとは限りませんが，適切な戦略を考えることで効率化や拡張を行う研究が進められています．

このツールは等式集合に対して上記の完備化手続きを行う機能を提供しています，

## 利用方法

### インストール手順

1. `mlton`をインストールする．
2. `make`コマンドを実行する．
3. `kb`コマンドがビルドされます．

```
$ brew install mlton # Debian 系なら apt install mlton
$ make
$ ./kb # Usage が表示される
```

### 実行方法

以下のように実行します．

```
$ ./kb <サブコマンド> [オプション ... ]
```

以下のサブコマンドを提供しています．

- `comp`: Knuth-Bendix 完備化を与えられた等式集合に適用します．得られた項書き換え系はファイルに書き出します．
- `sn`: 与えられた項書き換え系が停止するかどうか，辞書式経路順序を用いて検証します．
- `cpk`: 与えられた項書き換え系の危険頂を計算し，ファイルに書き出します．
- `lirw`: 与えられた項書き換え系を用いて，項が正規形になるまで (最左最内) 書き換えを行います．
- `info`: 与えられた等式集合と項書き換え系を表示し，ファイルに書き出します．
- `help`: ヘルプメッセージを表示します．

オプションを用いてファイルの入出力先を指定します．

- `-e`: 読み込む等式集合のファイルパスを指定します．指定しないと空集合になります，
- `-r`: 読み込む項書き換え系のファイルパスを指定します．指定しないと空集合になります．
- `-o`: `comp`, `cpk`, `info` を書き出すファイルパスを指定します．指定しないと`./log.txt`に書き出されます．

### 等式集合，項書き換え系の記述方法

角括弧の中に等式および書き換え規則をカンマで区切って書きます．変数は`?`をつけて関数記号と区別します．
記述例を`examples/equations/`および`examples/trs/`に用意しています．

### Knuth-Bendix 完備化の実行例

途中で関数記号の重みを求められるので，スペースか改行で区切って入力してください．

```
$ ./kb comp -e examples/equations/sample2.eqs
Given equations:
   [ +(0,?x) = ?x,
     +(-(?x),?x) = 0,
     +(+(?x,?y),?z) = +(?x,+(?y,?z)) ]

Function symbols:
    - + 0

Please type weights of function symbols (for example: 0 0 0):
3 2 1

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

Writing this TRS to `./log.txt'... Done!
$ cat log.txt
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
```

### 停止性検証の実行例

途中で関数記号の重みを求められるので，スペースか改行で区切って入力してください．

```
$ ./kb sn -r examples/trs/sample3.trs
Given TRS:
   [ *(s(?x),?y) -> +(?y,*(?x,?y)),
     +(s(?x),?y) -> s(+(?x,?y)),
     +(0,?y) -> ?y,
     *(0,?y) -> 0 ]

Function symbols:
    0 s + *

Please type weights of function symbols (for example: 0 0 0 0):
0 1 2 3

This is lexicographic path order decreasing TRS.
Hence this is terminating TRS.
```

### 危険頂計算の実行例

項書き換え系の危険頂とは，ある項とそこから変形できる二つの異なる書き換え先の組です．停止する項書き換え系は全ての危険頂の書き換え先が合流する (書き換え続けることで同じ項になる) とき，そのときに限り完備な項書き換え系です．

```
$ ./kb cpk -r examples/trs/sample4.trs
Given TRS:
   [ +(0,?y) -> ?y,
     +(s(?x),?y) -> s(+(?x,?y)),
     +(?x,+(?y,?z)) -> +(?x,+(?y,?z)) ]

Critical Peaks:
<+(?y_1,?z_1) <- +(0,+(?y_1,?z_1)) -> +(0,+(?y_1,?z_1))>
<+(?x_1,?y) <- +(?x_1,+(0,?y)) -> +(?x_1,+(0,?y))>
<s(+(?x,+(?y_1,?z_1))) <- +(s(?x),+(?y_1,?z_1)) -> +(s(?x),+(?y_1,?z_1))>
<+(?x_1,s(+(?x,?y))) <- +(?x_1,+(s(?x),?y)) -> +(?x_1,+(s(?x),?y))>
<+(0,+(?y,?z)) <- +(0,+(?y,?z)) -> +(?y,?z)>
<+(s(?x_1),+(?y,?z)) <- +(s(?x_1),+(?y,?z)) -> s(+(?x_1,+(?y,?z)))>
<+(?x_1,+(?x,+(?y,?z))) <- +(?x_1,+(?x,+(?y,?z))) -> +(?x_1,+(?x,+(?y,?z)))>

Writing this information to `./log.txt'... Done!
```

### 正規形計算の実行例

途中で書き換える項を求められるので入力してください．

```
./kb lirw -r examples/trs/sample3.trs
Given TRS:
   [ *(s(?x),?y) -> +(?y,*(?x,?y)),
     +(s(?x),?y) -> s(+(?x,?y)),
     +(0,?y) -> ?y,
     *(0,?y) -> 0 ]

Please type a first order term:
+(+(s(s(s(0))),s(s(s(0)))),+(s(s(0)),s(s(0))))

Leftmost innermost rewrite steps:
    +(+(s(s(s(0))),s(s(s(0)))),+(s(s(0)),s(s(0))))
->R +(s(+(s(s(0)),s(s(s(0))))),+(s(s(0)),s(s(0))))
->R +(s(s(+(s(0),s(s(s(0)))))),+(s(s(0)),s(s(0))))
->R +(s(s(s(+(0,s(s(s(0))))))),+(s(s(0)),s(s(0))))
->R +(s(s(s(s(s(s(0)))))),+(s(s(0)),s(s(0))))
->R +(s(s(s(s(s(s(0)))))),s(+(s(0),s(s(0)))))
->R +(s(s(s(s(s(s(0)))))),s(s(+(0,s(s(0))))))
->R +(s(s(s(s(s(s(0)))))),s(s(s(s(0)))))
->R s(+(s(s(s(s(s(0))))),s(s(s(s(0))))))
->R s(s(+(s(s(s(s(0)))),s(s(s(s(0)))))))
->R s(s(s(+(s(s(s(0))),s(s(s(s(0))))))))
->R s(s(s(s(+(s(s(0)),s(s(s(s(0)))))))))
->R s(s(s(s(s(+(s(0),s(s(s(s(0))))))))))
->R s(s(s(s(s(s(+(0,s(s(s(s(0)))))))))))
->R s(s(s(s(s(s(s(s(s(s(0))))))))))

Writing this information to `./log.txt'... Done!
```
