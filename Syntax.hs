module Syntax where

-- | VarP. Tipo que representa las variables proposicionales
type VarP = Int

-- | Prop. Tipo que representa formulas de logica proposicional
data Prop = TTrue
          | FFalse
          | V VarP
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Imp Prop Prop
          | Equiv Prop Prop

elimEquiv :: Prop -> Prop
elimEquiv phi = case phi of
  Neg p -> Neg (elimEquiv p)
  Conj p q -> Conj (elimEquiv p) (elimEquiv q)
  Disy p q -> Disy (elimEquiv p) (elimEquiv q)
  Imp p q -> Imp (elimEquiv p) (elimEquiv q)
  Equiv p q -> Conj (Imp p' q') (Imp p' q')
    where p' = elimEquiv p
          q' = elimEquiv q
  _ -> phi

elimImp :: Prop -> Prop
elimImp phi = case phi of
  Neg p -> Neg (elimImp p)
  Conj p q -> Conj (elimImp p) (elimImp q)
  Disy p q -> Disy (elimImp p) (elimImp q)
  Imp p q -> Disy (Neg (elimImp p)) (elimImp q)
  Equiv p q -> Equiv (elimImp p) (elimImp q)
  _ -> phi


-- recibe una formula sin -> ni <->
meteNeg :: Prop -> Prop
meteNeg phi = case phi of
  TTrue -> TTrue
  FFalse -> FFalse
  V x -> V x
  Neg psi -> case psi of
    Conj p q -> Disy (meteNeg (Neg p)) (meteNeg  (Neg q))
    Disy p q -> Conj (meteNeg (Neg p)) (meteNeg  (Neg q))
    Neg p -> meteNeg p
    TTrue -> FFalse
    FFalse -> TTrue
    V x -> Neg (V x)
  Conj p q -> Conj (meteNeg p) (meteNeg q)
  Disy p q -> Disy (meteNeg p) (meteNeg q)

fnn :: Prop -> Prop
fnn =  meteNeg.elimImp.elimEquiv

--recibe en fnn
dist :: Prop -> Prop
dist phi = case phi of
  TTrue -> TTrue
  FFalse -> FFalse
  V x -> V x
  Neg p -> Neg p
  Conj p q -> Conj (dist p) (dist q)
  Disy p (Conj q r) -> Conj (dist (Disy p q)) (dist (Disy p r))
  Disy (Conj p q) r -> Conj (dist (Disy p r)) (dist (Disy q r))
  Disy p q -> case (p', q') of
    (Conj _ _, _) -> dist (Conj p' q')
    (_, Conj _ _ ) -> dist (Disy p' q')
    (_, _) -> Disy p' q'
    where p' = dist p
          q' = dist q

fnc :: Prop -> Prop
fnc = dist.fnc
