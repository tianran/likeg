# likeg
LIKEG: Logical Inference with Knowledge- and Existential- Graphs

## Introduction

LIKEG is (will become) a logical inference engine with a natural language interface. It uses Existential Graphs ([intro1](http://www.jfsowa.com/pubs/egtut.pdf) [intro2](http://dai.fmph.uniba.sk/~sefranek/kri/handbook/chapter05.pdf)) as the meaning representation. An existential graph is a flexible way to write First Order Logic; it consists of three types of components:

1. A **Definition Node** defines a variable. Example: `[x1], [x2]`.
2. A **Relation Node** states a relation between variables. Example: `John(x1), Mary(x2), loves(x1 x2)`. In this work, we only handle unary and binary relations.
3. A **Scope** is a set that consists of Definition Nodes and Relation Nodes. Requirement from the Existential Graph is that, for any two Scopes, either one is a subset of the other, or the two are disjoint; no other types of intersection is allowed. In other words, all Scopes in an Existential Graph can be modeled by a [tree](https://en.wikipedia.org/wiki/Tree_(data_structure)), where a child is a subset of its parent.

With 1 and 2 in the above, Existential Graphs are able to represent [Knowledge Graphs](https://en.wikipedia.org/wiki/Knowledge_Graph); logically speaking, a Denition Node adds an existential quantifier to the variable, and all facts stated by Relation Nodes are held together with logical conjunction. It is able to store a large set of affirmative facts but will be difficult to handle negative statements. Therefore, Existential Graph adds a *Negation Scope* to negate a subset of variables and facts. Since the negation of an existential quantifier is a universal quantifier, and the negation of logical conjuction is logical disjunction, we get the full representation power of First Order Logic by adding Negation Scopes. Pushing through this idea even further, one can create other types of scopes to implement more powerful logic operations, such as counting and general quantifier (e.g. "*__more than two__ students took the class*").

## Natural Language Interface

Currently, we are able to convert natural language sentences into Existential Graphs. The system takes the [Stanford-Dependency parse tree](https://nlp.stanford.edu/software/dependencies_manual.pdf) (SD tree) of a sentence as input, and makes the conversion by simple (principled) but carefully designed rules. It regards noun phrases as unary relations, and creates binary relations by traversing the SD tree recursively and extracting paths that join two noun phrases. It consults SD labels to make the extracted relations as intuitive, and to cover the SD tree as much as possible. At the same time, it takes syntactive scopes in the SD tree as hint to create Scopes of the Existential Graph. It recognizes simple logical trigger words such as "*not, every, if, and, or, ...*" etc.; so it will be able to generate logical rules automatically from natural language.

### Usage

* Compile:

```
> cd scala
> mkdir classes
> scalac -d classes eg/*.scala tree/*.scala likeg/*.scala
```

* The input is SD parse trees in CoNLL format. Found some in the `data/sample` folder:
```
> cat data/sample/miami.sd
1	Miami	_	NOUN	NNP	_	3	nsubj	_	_
2	was	_	VERB	VBD	_	3	cop	_	_
3	able	_	ADJ	JJ	_	0	ROOT	_	_
4	to	_	PRT	TO	_	5	aux	_	_
5	pull	_	VERB	VB	_	3	xcomp	_	_
6	away	_	PRT	RP	_	5	prt	_	_
7	in	_	ADP	IN	_	5	prep	_	_
8	the	_	DET	DT	_	11	det	_	_
9	last	_	ADJ	JJ	_	11	amod	_	_
10	three	_	NUM	CD	_	11	num	_	_
11	minutes	_	NOUN	NNS	_	7	pobj	_	_
12	of	_	ADP	IN	_	11	prep	_	_
13	the	_	DET	DT	_	15	det	_	_
14	final	_	ADJ	JJ	_	15	amod	_	_
15	period	_	NOUN	NN	_	12	pobj	_	_
16	with	_	ADP	IN	_	5	prep	_	_
17	some	_	DET	DT	_	20	det	_	_
18	outstanding	_	ADJ	JJ	_	20	amod	_	_
19	defensive	_	ADJ	JJ	_	20	amod	_	_
20	work	_	NOUN	NN	_	16	pobj	_	_
21	on	_	ADP	IN	_	20	prep	_	_
22	the	_	DET	DT	_	23	det	_	_
23	part	_	NOUN	NN	_	21	pobj	_	_
24	of	_	ADP	IN	_	23	prep	_	_
25	Hassan	_	NOUN	NNP	_	26	nn	_	_
26	Whiteside	_	NOUN	NNP	_	24	pobj	_	_
27	,	_	.	,	_	26	punct	_	_
28	who	_	PRON	WP	_	29	nsubj	_	_
29	tallied	_	VERB	VBD	_	26	rcmod	_	_
30	a	_	DET	DT	_	33	det	_	_
31	whopping	_	ADJ	JJ	_	33	amod	_	_
32	seven	_	NUM	CD	_	33	num	_	_
33	blocks	_	NOUN	NNS	_	29	dobj	_	_
34	.	_	.	.	_	3	punct	_	_

```
* Generally, one can use a dependency parser such as [SyntaxNet](https://github.com/tensorflow/models/tree/master/research/syntaxnet) to get SD trees from sentences. [Here](https://github.com/tianran/build-syntaxnet) is a derived package of an old (but equally precise) version of SyntaxNet with tested-and-working installation instructions.

* Make conversion:
```
> cat data/sample/miami.sd | scala -cp scala/classes likeg.LikeGBuilder -
+miami (x1)
+able >xcomp:2 (x1 x2)
+to +pull +away +in >pobj:2 (x2 x3)
+final +period (x4)
+last +minutes +of >pobj:2 (x3 x4)
+to +pull +away +with >pobj:2 (x2 x5)
+outstanding +defensive +work +on >pobj:2 (x5 x6)
+hassan +whiteside (x7)
+part +of >pobj:2 (x6 x7)
+whopping +blocks (x8)
+who +tallied >rcmod:1 >dobj:2 (x7 x8)

```

* By default, the converter only displays variables and relations, but scopes are hidden. However, internally the system will hold enough information to convey logical inference.

### Coming soon:

* Train embedding models on the converted graph data
* Graph alignment using similarity from the embeddings
