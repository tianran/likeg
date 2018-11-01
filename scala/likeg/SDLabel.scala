package likeg

/** Enumeration of Stanford Dependency relations */
object SDLabel extends Enumeration {
  type SDLabel = Value

  val
    ROOT,
    dep,
    parataxis,
    advcl,
    mark,
    conj,
    cc,
    preconj,
    punct,
    discourse,
    possessive,
    cop,
    appos,
    nsubj,
    nsubjpass,
    dobj,
    tmod,
    npadvmod,
    iobj,
    pobj,
    poss,
    prep,
    det,
    predet,
    neg,
    goeswith,
    mwe,
    prt,
    nn,
    amod,
    advmod,
    num,
    number,
    quantmod,
    expl,
    aux,
    auxpass,
    ccomp,
    pcomp,
    acomp,
    csubj,
    csubjpass,
    xcomp,
    rcmod,
    partmod,
    infmod = Value

  lazy val fromString: Map[String, SDLabel] = SDLabel.values.map(x => x.toString -> x).toMap
}
