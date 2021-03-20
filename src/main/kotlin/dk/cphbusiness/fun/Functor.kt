package dk.cphbusiness.`fun`

interface Functor<A> {
  fun <B>fmap(mapper: (A) -> B, source: Functor<A>): Functor<B>
  }
