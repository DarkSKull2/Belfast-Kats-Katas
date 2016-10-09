module Lib where

import Data.List

std_ops :: [(String,Float->Float->Float)]
std_ops = [("*",(*)),("+",(+)),("-",(-)),("/",(/))]

wrap_fnc :: (Float->Float->Float)->Maybe Float->Maybe Float->Maybe Float
wrap_fnc (/) _ (Just 0) = Nothing
wrap_fnc f (Just a) (Just b) = Just (f a b)
wrap_fnc _ _ _ = Nothing

wrapped_ops :: [(String,Maybe Float->Maybe Float->Maybe Float)]
wrapped_ops = map (\(a,b) -> (a,wrap_fnc b)) std_ops

combine_rep :: Int->[(String,Maybe Float->Maybe Float->Maybe Float)]->[[(String,Maybe Float->Maybe Float->Maybe Float)]]
combine_rep 1 inp = foldl (\a b -> a++[[b]]) [] inp
combine_rep n inp = foldl (\a b -> a++(map (\x -> b:x) (combine_rep (n-1) inp))) [] inp 
-- combine_rep 1 inp = foldr (\a b -> [[a]]++b) [] inp
-- combine_rep n inp = foldr (\a b -> (map (\x -> [a]++x) (combine_rep (n-1) inp))++b) [] inp      

treePerm :: Int -> Int -> String -> [String]
treePerm n f p | n==0 && f==0 = [p ++ "N"]
               | n >0 && f >0 = (treePerm (n-1) f (p++"N"))++(treePerm (n+1) (f-1) (p++"F"))
               | n >0         =  treePerm (n-1) f (p++"N")
               | f >0         =  treePerm (n+1) (f-1) (p++"F")


calculate :: [Int] -> [(String,Maybe Float->Maybe Float->Maybe Float)]->String->([Int],[(String,Maybe Float->Maybe Float->Maybe Float)],String,Maybe Float)  
calculate ns ((_,f):fs) ('F':ts) = let 
                                (ln,lf,lt,rl) = calculate ns fs ts -- process left
                               in if Nothing == rl -- try early cut
                                   then ([],[],[],Nothing)
                                   else let (rn,rf,rt,rr) = calculate ln lf lt -- process right 
                                        in if Nothing == rr -- try another earlier cut -- useless
                                            then ([],[],[],Nothing)
                                            else (rn,rf,rt,f rl rr)
calculate (n:ns) fs ('N':ts) = (ns,fs,ts,Just (fromIntegral n))


fstop :: ([Int],[(String,Maybe Float->Maybe Float->Maybe Float)],String,Maybe Float,Maybe Float)->Bool
fstop (_,_,_,_,Just 0.0) = True
fstop _ = False


fmin :: ([Int],[(String,Maybe Float->Maybe Float->Maybe Float)],String,Maybe Float,Maybe Float)->([Int],[(String,Maybe Float->Maybe Float->Maybe Float)],String,Maybe Float,Maybe Float)->([Int],[(String,Maybe Float->Maybe Float->Maybe Float)],String,Maybe Float,Maybe Float)
fmin a@(a1,a2,a3,a4,Just da) b@(b1,b2,b3,b4,Just db) | da < db = a 
                                                     | otherwise = b
fmin a@(a1,a2,a3,a4,a5) (_,_,_,_,Nothing) = a
fmin (_,_,_,_,Nothing) b@(b1,b2,b3,b4,b5) = b


fdistance :: Int->Maybe Float->Maybe Float
fdistance i (Just x) = Just $ abs ((fromIntegral i) - x)
fdistance _ _ = Nothing 


treesIterator :: [Int]->[(String,Maybe Float->Maybe Float->Maybe Float)]->[String]->Int->([Int],[(String,Maybe Float->Maybe Float->Maybe Float)],String,Maybe Float,Maybe Float)
treesIterator num ops [th] tg = let 
                                 (n,f,t,r) = calculate num ops th
                                 result = (num,ops,th,r,fdistance tg r)
                                in result
treesIterator num ops (th:tt) tg = let 
                                    (n,f,t,r) = calculate num ops th
                                    result = (num,ops,th,r,fdistance tg r)
                                   in if fstop result
                                       then result
                                       else fmin result ( treesIterator num ops tt tg )


operationsSequencesIter :: [Int]->[[(String,Maybe Float->Maybe Float->Maybe Float)]]->[String]->Int->([Int],[(String,Maybe Float->Maybe Float->Maybe Float)],String,Maybe Float,Maybe Float) 
operationsSequencesIter num [oh] trees tg = treesIterator num oh trees tg
operationsSequencesIter num (oh:ot) trees tg = let result = treesIterator num oh trees tg
                                               in if fstop result
                                                   then result
                                                   else fmin result (operationsSequencesIter num ot trees tg)



inputSequencesInter :: [[Int]]->Int->([Int],[(String,Maybe Float->Maybe Float->Maybe Float)],String,Maybe Float,Maybe Float) 
inputSequencesInter [num] tg = operationsSequencesIter num (combine_rep (length num) wrapped_ops) (treePerm 0 ((length num)-1) []) tg
inputSequencesInter (n:ns) tg = let result = operationsSequencesIter n (combine_rep (length n) wrapped_ops) (treePerm 0 ((length n)-1) []) tg
                                in if fstop result
                                    then result
                                    else fmin result (inputSequencesInter ns tg)

startPointResult :: [Int]->Int->String
startPointResult inputNumSeq target = let
                                       (c,d,e,Just f,Just g) = inputSequencesInter (permutations inputNumSeq) target
                                       (_,_,_,res) = display c d e 
                                      in res ++ "= "++(show f)++ "\ngoal="++(show target)++" [delta="++(show g)++"]"



display :: [Int] -> [(String,Maybe Float->Maybe Float->Maybe Float)]->String->([Int],[(String,Maybe Float->Maybe Float->Maybe Float)],String,String)
display ns ((f,_):fs) ('F':ts) = let 
                                (ln,lf,lt,left) = display ns fs ts -- process left
                                (rn,rf,rt,right) = display ln lf lt -- process right 
                               in (rn,rf,rt,"("++left++f++right++")")
display (n:ns) fs ('N':ts) = (ns,fs,ts,show n)













