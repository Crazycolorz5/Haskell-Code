hill_climbing::(Ord b)=>([a]->b)->([a]->[a])->b->[a]->[a]
hill_climbing quality tweak target cur_solution
    | quality cur_solution > target = cur_solution
    | otherwise = 
        let new_ans = tweak cur_solution
            new_sol = if (quality new_ans)>(quality cur_solution) then new_ans else cur_solution -- keep solution only if it's better.
        in hill_climbing quality tweak target new_sol


hill_climbing_steepest_ascent::(Ord b, Integral c)=>([a]->b)->([a]->[a])->b->c->[a]->[a]
hill_climbing_steepest_ascent quality tweak target n cur_solution
    | quality cur_solution > target = cur_solution
    | otherwise = 
        let post_inner_ans = hcsa_rec quality tweak (tweak cur_solution) n
            post_inner_sol = if (quality post_inner_ans)>(quality cur_solution) then post_inner_ans else cur_solution
        in hill_climbing_steepest_ascent quality tweak target n post_inner_sol
    where
        hcsa_rec quality tweak cur_solution num
            | num==0 = cur_solution
            | otherwise = 
                let new_ans = tweak cur_solution
                    new_sol = if (quality new_ans)>(quality cur_solution) then new_ans else cur_solution
                in hcsa_rec quality tweak new_sol $ pred num


f1::[Double]->Double
f1 = sum . map (^2)
f1_tweak::[Double]->[Double]
f1_tweak = exponential_tweak (-100) 100 23

linear_tweak::(Num a, Ord a)=>a->a->a->[a]->[a]
linear_tweak lower upper interval input =
    finished_tweak
    where
        edited_in = (head input + interval):(tail input)
        finished_tweak = 
            if head edited_in > upper
                then (head edited_in - (upper-lower)):(linear_tweak lower upper interval $ tail input)
                else edited_in

exponential_tweak::(Floating a, Ord a)=>a->a->a->[a]->[a]
exponential_tweak _ _ _ [] = []
exponential_tweak lower upper interval input =
    finished_tweak
    where
        edited_in = (head input + interval):(exponential_tweak lower upper (interval/2) $ tail input)
        finished_tweak = 
            if head edited_in > upper
                then (head edited_in - (upper-lower)):(linear_tweak lower upper interval $ tail input)
                else edited_in
{-
f1_tweak input = 
    where
        changed_first = 
-}
