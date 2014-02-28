{-# LANGUAGE TypeOperators #-}
data a :/\: b = a :/\: b
data a :\/: b = L a | R b

data False
type Not a = a -> False

data a :<->: b = Bi { forward :: (a -> b), backward :: (b -> a) }

lhs (a :/\: _) = a
rhs (_ :/\: b) = b

and_commute :: a :/\: b -> b :/\: a
and_commute (p :/\: q) = (q :/\: p)

or_commute :: a :\/: b -> b :\/: a
or_commute (L p) = R p
or_commute (R q) = L q

and_assoc :: a :/\: (b :/\: c) -> (a :/\: b) :/\: c
and_assoc (p :/\: (q :/\: r)) = (p :/\: q) :/\: r

or_assoc :: a :\/: (b :\/: c) -> (a :\/: b) :\/: c
or_assoc (L p) = L (L p)
or_assoc (R (L q)) = L (R q)
or_assoc (R (R r)) = R r

modus_ponens :: a -> (a -> b) -> b
modus_ponens p f = f p

and_idempotent :: a :/\: a -> a
and_idempotent (p :/\: _) = p 

or_idempotent :: a :\/: a -> a
or_idempotent (L p) = p
or_idempotent (R p) = p

and_distributes :: a :/\: (b :\/: c) -> (a :/\: b) :\/: (a :/\: c)
and_distributes (p :/\: L q) = L (p :/\: q)
and_distributes (p :/\: R r) = R (p :/\: r)

or_distributes :: a :\/: (b :/\: c) -> (a :\/: b) :/\: (a :\/: c)
or_distributes (L p)          = L p :/\: L p
or_distributes (R (q :/\: r)) = R q :/\: R r

contrapositive :: (a -> b) -> (Not b -> Not a)
contrapositive f g = g . f

demorgan1 :: Not a :\/: Not b -> Not (a :/\: b)
demorgan1 (L f) (p :/\: q) = f p
demorgan1 (R g) (p :/\: q) = g q

demorgan2a :: Not a :/\: Not b -> Not (a :\/: b)
demorgan2a (f :/\: g) (L p) = f p
demorgan2a (f :/\: g) (R q) = g q

demorgan2b :: Not (a :\/: b) -> Not a :/\: Not b
demorgan2b f = (f . L) :/\: (f . R)

demorgan2 :: Not a :/\: Not b :<->: Not (a :\/: b)
demorgan2 = Bi { forward = demorgan2a, backward = demorgan2b }

data Scottish    = Scottish
data RedSocks    = RedSocks
data WearKilt    = WearKilt
data Married     = Married
data GoOutSunday = GoOutSunday

no_true_scottsman ::
  (Not Scottish -> RedSocks)          -> -- rule 1
  (WearKilt :\/: Not RedSocks)        -> -- rule 2
  (Married -> Not GoOutSunday)        -> -- rule 3
  (Scottish :<->: GoOutSunday)        -> -- rule 4
  (WearKilt -> Scottish :/\: Married) -> -- rule 5
  (Scottish -> WearKilt)              -> -- rule 6
  False

no_true_scottsman
  rule1
  rule2
  rule3
  rule4
  rule5
  rule6 = let
    lemma1 :: Scottish -> Married
    lemma1 = rhs . rule5 . rule6

    lemma2 :: Scottish -> Not GoOutSunday
    lemma2 = rule3 . lemma1

    lemma3 :: Scottish -> GoOutSunday
    lemma3 = forward rule4

    lemma4 :: Not Scottish
    lemma4 scottish = modus_ponens (lemma3 scottish) (lemma2 scottish)

    lemma5 :: RedSocks
    lemma5 = rule1 lemma4

    lemma6 :: WearKilt :\/: False
    lemma6 = case rule2 of
      L kilt -> L kilt
      R f    -> R (f lemma5)

    lemma7 :: Not WearKilt -> False
    lemma7 f = case lemma6 of
      L kilt  -> f kilt
      R false -> false

    lemma8 :: Not Scottish -> Not WearKilt
    lemma8 f kilt = f (lhs (rule5 kilt))

    lemma9 :: Not Scottish -> False
    lemma9 = lemma7 . lemma8

    in (lemma7 . lemma8) lemma4
