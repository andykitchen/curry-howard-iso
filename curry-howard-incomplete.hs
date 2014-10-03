{-# LANGUAGE TypeOperators #-}
data a :/\: b = a :/\: b
data a :\/: b = L a | R b

data False
type Not a = a -> False

data a :<->: b = Bi {
  forward :: (a -> b),
  backward :: (b -> a)
}

lhs :: a :/\: b -> a
rhs :: a :/\: b -> b

lhs (p :/\: _) = p
rhs (_ :/\: q) = q

and_commute :: a :/\: b -> b :/\: a
and_commute = undefined

or_commute :: a :\/: b -> b :\/: a
or_commute = undefined

and_assoc :: a :/\: (b :/\: c) -> (a :/\: b) :/\: c
and_assoc = undefined

or_assoc :: a :\/: (b :\/: c) -> (a :\/: b) :\/: c
or_assoc = undefined

modus_ponens :: a -> (a -> b) -> b
modus_ponens = undefined

and_idempotent :: a :/\: a -> a
and_idempotent = undefined

or_idempotent :: a :\/: a -> a
or_idempotent = undefined

and_distributes :: a :/\: (b :\/: c) -> (a :/\: b) :\/: (a :/\: c)
and_distributes = undefined

or_distributes :: a :\/: (b :/\: c) -> (a :\/: b) :/\: (a :\/: c)
or_distributes = undefined

contrapositive :: (a -> b) -> (Not b -> Not a)
contrapositive = undefined

demorgan1 :: Not a :\/: Not b -> Not (a :/\: b)
demorgan1 = undefined

demorgan2a :: Not a :/\: Not b -> Not (a :\/: b)
demorgan2a = undefined

demorgan2b :: Not (a :\/: b) -> Not a :/\: Not b
demorgan2b = undefined

demorgan2 :: Not a :/\: Not b :<->: Not (a :\/: b)
demorgan2 = undefined

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
    lemma1 = undefined

    lemma2 :: Scottish -> Not GoOutSunday
    lemma2 = undefined

    lemma3 :: Scottish -> GoOutSunday
    lemma3 = undefined

    lemma4 :: Not Scottish
    lemma4 scottish = undefined

    lemma5 :: RedSocks
    lemma5 = undefined

    lemma6 :: WearKilt :\/: False
    lemma6 = undefined

    lemma7 :: Not WearKilt -> False
    lemma7 = undefined

    lemma8 :: Not Scottish -> Not WearKilt
    lemma8 = undefined

    lemma9 :: Not Scottish -> False
    lemma9 = undefined

    in undefined


rule1 :: Not Scottish -> RedSocks
rule2 :: WearKilt :\/: Not RedSocks
rule3 :: Married -> Not GoOutSunday
rule4 :: Scottish :<->: GoOutSunday
rule5 :: WearKilt -> Scottish :/\: Married
rule6 :: Scottish -> WearKilt

lemma1 :: Scottish -> Married
lemma2 :: Scottish -> Not GoOutSunday
lemma3 :: Scottish -> GoOutSunday
lemma4 :: Not Scottish
lemma5 :: RedSocks
lemma6 :: WearKilt :\/: False
lemma7 :: Not WearKilt -> False
lemma8 :: Not Scottish -> Not WearKilt
lemma9 :: Not Scottish -> False


rule1 = undefined
rule2 = undefined
rule3 = undefined
rule4 = undefined
rule5 = undefined
rule6 = undefined

lemma1 = undefined
lemma2 = undefined
lemma3 = undefined
lemma4 = undefined
lemma5 = undefined
lemma6 = undefined
lemma7 = undefined
lemma8 = undefined
lemma9 = undefined
