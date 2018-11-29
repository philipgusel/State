module State where

-- a pure state transformer
newtype Statetp s a = Statetp { run :: s -> (a,s) }



instance Functor (Statetp a) where
        -- take a function f and a Statefull computation and return a Statefull computation with f mapped to the output
        fmap f (Statetp cs) = Statetp $ \s -> let (a, newstate) = cs s in (f a, newstate)

instance Applicative (Statetp a) where
        -- take an a and produce a statefull computation that allways returns a
        pure a = Statetp $ \s -> (a,s)
        -- take two statefull computations, get f aout of the first, a out of the second and return 
        -- a statefull computation with f mapped over the result 
        (<*>) (Statetp sf) (Statetp sn) = 
                Statetp $ \s -> let (f, newstateI) = sf s
                                    (a, newstateII) = sn newstateI
                                in (f a, newstateII)

instance Monad (Statetp a) where
        return = pure
        (>>=) (Statetp c) f = Statetp $ \s -> let (a, newstate) = c s
                                                  Statetp newstatetp = f a
                                                  in newstatetp newstate 


get = Statetp (\s -> (s,s))

put s = Statetp (\_ -> ((),s))

runState (Statetp f) s = f s
