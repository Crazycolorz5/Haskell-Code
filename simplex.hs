type IsUnbounded = Bool
type Program = ([[Double]],[Double])

--Useful functions
fst3::(a,b,c) -> a --Tools for dealing with 3-tuples
fst3 (x,_,_) = x
snd3::(a,b,c) -> b
snd3 (_,x,_) = x
trd3::(a,b,c) -> c
trd3 (_,_,x) = x

get_col::[[a]]->Int->[a] --Matrix tools
get_col mat col = map (\e->(e!!col)) mat
transpose::[[a]]->[[a]]
transpose mat = map (get_col mat) ((take (length (head mat))) [0..])

--program is of the form tuple2 ((list of lists)lhs, (list)rhs)
make_program_from_2dlist::[[Double]]->Program
make_program_from_2dlist orig = (map init orig, map last orig)
make_program = make_program_from_2dlist

simplex::Program -> Either Program IsUnbounded
simplex program = 
    --simplex_solve program
    if is_normal (fst program) --if has a basic variable for each column
        then simplex_solve program
        else two_phase_simplex program

is_normal lhs = 
    null test
    where
        t_lhs = transpose lhs
        basic_columns = map is_basic_column t_lhs
        rows_with_basics::[Maybe Int]
        rows_with_basics = map (\ele->(if (snd ele) then Just(row_from_col (fst ele)) else Nothing)) (zip t_lhs basic_columns)
        rows_without_variables =  map (\ele->if (not (elem ele rows_with_basics)) then ele else Nothing) (map Just (take (length lhs) [0..]))
        test = filter (/=Nothing) rows_without_variables
    

is_in_basis::[[Double]] -> Int -> Bool
is_in_basis lhs column = 
    is_basic_column colVars
    where
        colVars = foldr (:) [] (map (!!column) lhs)

is_basic_column::[Double]->Bool
is_basic_column colVars = (filter (\e->e/=0) colVars) == [1]

{-
--row_with_basic_variable::[[a]]->a->a
row_with_basic_variable lhs column = 
        row_from_col col
        --Add one until you get to the 1. Then negate a and stop adding. At the end, negate it again to get the rowNum.
        --Start at 1 to prevent row 0 weird stuff. Subtract 1 at the end.
    where 
        col = (transpose lhs)!!column
-}

row_from_col::[Double]->Int
row_from_col col = 
    row_from_col_rec col 0
row_from_col_rec col ind
    | head col == 1 = ind
    | otherwise     = row_from_col_rec (tail col) (succ ind) --if this messes up... SOMETHING WENT WRONG
{-
        pred (-1*(foldl myFunc 1 col))
        --Add one until you get to the 1. Then negate a and stop adding. At the end, negate it again to get the rowNum.
        --Start at 1 to prevent row 0 weird stuff. Subtract 1 at the end.
    where
        myFunc::Int->Double->Int
        myFunc = (\a->(\b->(if (a>0&&b==0) then (succ a) else if (b==1) then (-1*a) else a)))
-}

simplex_solve::Program -> Either Program IsUnbounded
simplex_solve program = 
    if is_optimal (fst program)
        then Left program
        else simplex_solve (pivot_around program exiting_variable entering_variable) 
        {-else if exiting_variable>0
            then simplex_solve (pivot_around program exiting_variable entering_variable) 
            else two_phase_simplex program-}
    where
        entering_variable = get_entering_variable (fst program)
        exiting_variable = get_exiting_variable program (entering_variable)

get_entering_variable::[[Double]]->Int
get_entering_variable lhs = --sans z
    snd3 (foldl (\a->(\b->(if (trd3 a)>b then (succ (fst3 a),snd3 a,trd3 a) else ((succ (fst3 a), fst3 a, b))))) (1,0,0) (tail (head lhs))) 
    -- tail (disregard z) of 0th row
    -- tuple of the form (currentIndex, maximumIndex, reducedCost)
    where

get_exiting_variable::Program->Int->Int
get_exiting_variable program entering = 
    get_min_positive_index (tail ratios) --Disregard row 0
    where 
        lhs = fst program
        rhs = snd program
        ratios = map (\row->if lhs!!row!!entering/=0 then ((rhs!!row)/(lhs!!row!!entering)) else -1) [0..(pred (length lhs))] --computes result of ratio test
        get_min_positive_index lst = trd3 (foldl (\b->(\a->if a>0&&(a<(snd3 b)||(snd3 b)==(-1)) then (succ $ fst3 b, a, fst3 b) else (succ $ fst3 b, snd3 b, trd3 b))) (1, -1, -1) lst)
        --gets the minimum of the ratios that is still positive. In event of failure, returns -1.
        --tuple of the form (cur_index_checking, num, index)

pivot_around::Program->Int->Int->Program
pivot_around program row column = 
        ((take row nlhs)++[fst primed_row]++(drop (succ row) nlhs), (take row nrhs)++[snd primed_row]++(drop (succ row) nrhs))
    where 
        lhs = fst program
        rhs = snd program
        vadd a b = map (\e->(fst e)+(snd e)) (zip a b)
        vmul lst b = map (*b) lst
        coef = lhs!!row!!column
        primed_row = (vmul (lhs!!row) (1/coef), (rhs!!row)/coef)
        get_rid_of_var (rlhs, rrhs) = (vadd rlhs (vmul (fst primed_row) (-1*(rlhs!!column))), rrhs+(snd primed_row)*(-1)*(rlhs!!column)) --eliminates the basic var from this row.
        new_program = map get_rid_of_var (map (\a->(lhs!!a, rhs!!a)) [0..(pred (length lhs))])
        nlhs = map fst new_program
        nrhs = map snd new_program

is_optimal::[[Double]]->Bool
is_optimal lhs = --Always assumed min problem
    null $ filter (>0) (tail $ head lhs) --disregard z

two_phase_simplex::Program->Either Program IsUnbounded
two_phase_simplex program --Takes up one extra stack frame
    | not $ foldr (||) False basic_av_cols = simplex_solve (reinsert_row_zero program (map (take num_non_av) (fst optimal), snd optimal)) --case 2 -- no AV in basis
    | head (snd optimal) == 0              = --case 3 -- AV in basis, optimal value == 0; drop nonbasic avs, and negative-coefficiented-variables in objective function and solve
        let --This doesn't quite work yet
            get_corrisponding_elements::[a]->[Bool]->[a]
            get_corrisponding_elements lst1 lst2 = map fst $ filter snd $ zip lst1 lst2
            remaining_avs = get_corrisponding_elements av_cols basic_av_cols
            basic_cols = take num_non_av columns
            nonnegative_basic_vars = map (>=(0::Double)) $ take num_non_av $ head $ fst optimal
            remaining_basic_vars = get_corrisponding_elements basic_cols nonnegative_basic_vars --Set the negative rows to rows of 0.
            transformed_program = (transpose $ remaining_avs++remaining_basic_vars, snd optimal)
        in  simplex transformed_program
    | otherwise                            = Right False --case 1 -- AV in basis, optimal value > 0; infeasible. Handled last for elegance.
    where
        (new_prgm, num_art) = augment_with_artificial_variables program
        --get_from_just (Just a) = a
        undo_left (Left a) = a
        optimal = undo_left $ simplex new_prgm --optimal phase one tableau -- minimize a1+a3+...+an
        num_vars = length $ head $ fst new_prgm
        num_non_av = num_vars - num_art
        columns = transpose (fst optimal)
        av_cols = drop num_non_av columns
        basic_av_cols = map is_basic_column av_cols

augment_with_artificial_variables::Program->(Program,Int)
augment_with_artificial_variables program = 
    let
        --add_just::Maybe Int->[[Double]]->[[Double]]
        add_just (Just x) = add_artificial_variable_in_row x
        add_just Nothing = \x->x --Shouldn't be reached, but just in case.
    in  (foldl (\acc->(\x->add_just x acc)) (replaced_objective, snd program) rows_without_variables, length $ filter (/=Nothing) rows_without_variables) 
    where
        lhs = fst program
        t_lhs = transpose lhs
        basic_columns = map is_basic_column t_lhs
        --rows_with_basics::[Maybe Int]
        rows_with_basics = map (\ele->(if snd ele then Just $ row_from_col $ fst ele else Nothing)) $ zip t_lhs basic_columns
        
        --isSomething::(Maybe a)->Bool
        --isSomething (Just x) = True;
        --isSomething x = False
        rows_without_variables =  map (\ele->if not $ elem ele rows_with_basics then ele else Nothing) (map Just $ (take . length) lhs [0..])
        replaced_objective = (1:(take (pred $ length $ head lhs) $ repeat 0)):(tail lhs)
        
    --insert_variables (head (fst program)) (tail (fst program)) -- returns a tuple (new program, number of variables added)
    --where insert_variables 

add_artificial_variable_in_row::Int->([[Double]], [Double])->([[Double]], [Double])
add_artificial_variable_in_row row program = 
    ((vadd ((head lhs)++[-1]) new_row):(add_zeros_to $ (take . pred) row $ drop 1 lhs)++[new_row]++(add_zeros_to $ drop (succ row) $ lhs), (head rhs + (rhs!!row)):(tail rhs))
    where 
        lhs = fst program
        rhs = snd program
        add_zeros_to x = (map (\lst->lst++[0])) x
        new_row = (lhs!!row)++[1]
        vadd a b = map (\e->(fst e)+(snd e)) $ zip a b
    --Logic is to augment row 0 with the negation of the new variable,
    --Augment a 0 to all rows before the given row
    --Augment a 1 to that row
    --Augment a 0 to all rows after that row

reinsert_row_zero::Program->Program->Program
reinsert_row_zero original new = 
    foldl cancel_bvs (original_obj:(tail $ fst new), snd new) to_cancel
    where
        vadd a b = map (\e->(fst e)+(snd e)) $ zip a b
        vmul lst b = map (*b) lst
        original_obj = (head $ fst original)
        to_cancel = zip (zip (transpose $ fst new) $ False:(map (/=0) $ tail original_obj)) $ take (length original_obj) ([0..]::[Int])
        cancel_bvs acc col_data
            | (is_basic_column $ fst $ fst col_data)&&(snd $ fst col_data) = 
                let 
                    lhs = fst acc
                    rhs = snd acc
                    coef = (*) (-1) $ (original_obj!!(snd col_data))
                    row = row_from_col $ fst $ fst col_data
                in ((vadd (head lhs) (vmul (lhs!!row) coef)):(tail lhs), (head rhs + (coef * (rhs!!row))):(tail rhs))
            | otherwise                                                    = acc


print_solution::Either Program IsUnbounded->[String]->String
print_solution (Left program) var_names = 
    show $ zip var_names rhs_vals 
    where
        t_program = transpose $ fst program
        rhs_vals = map (\ele->(if is_basic_column ele then (snd program) !! (row_from_col ele) else 0)) $ t_program
        --if it is a basic column, use the righthand value. Otherwise use 0.
print_solution (Right True) x = "Unbounded"
print_solution (Right False) x = "Infeasible"

problem1 = make_program ([[1,-4,-1,0,0,0,0],[0,3,1,-1,0,0,10],[0,1,1,0,-1,0,5],[0,1,0,0,0,-1,3]]::[[Double]])
p1_names = ["z","x1","x2","s1","s2","s3"]
problem2 = make_program [[1.0,200.0,300.0,0.0,0.0,0.0,0.0],[0.0,3.0,2.0,1.0,0.0,0.0,100.0],[0.0,2.0,4.0,0.0,1.0,0.0,120.0],[0.0,1.0,1.0,0.0,0.0,1.0,45.0]]
p2_names = p1_names

--For debugging
{-
problem1 = make_program ([[1,-4,-1,0,0,0,0],[0,3,1,-1,0,0,10],[0,1,1,0,-1,0,5],[0,1,0,0,0,-1,3]]::[[Double]])
p1data = augment_with_artificial_variables problem1
a1 = fst p1data
a1' = step a1
a1'' = step a1'
a1''' = step a1''
a1'''' = step a1'''
a1''''' = step a1''''
a1_fin = extract_just_with_default ([[]],[]) $ simplex a1
problem1_phase_2 = reinsert_row_zero problem1 (map (take num_non_av) $ fst a1_fin, snd a1_fin)
num_vars = length $ head $ fst a1
num_non_av = num_vars - (snd p1data)

problem2 = make_program [[1.0,200.0,300.0,0.0,0.0,0.0,0.0],[0.0,3.0,2.0,1.0,0.0,0.0,100.0],[0.0,2.0,4.0,0.0,1.0,0.0,120.0],[0.0,1.0,1.0,0.0,0.0,1.0,45.0]]
t2 = transpose (fst problem2)
t2' = zip t2 (map is_basic_column t2)
t2_basics = map (\ele->(if (snd ele) then Just(row_from_col (fst ele)) else Nothing)) t2'
t2_bad_rows =  map (\ele->if (not (elem ele t2_basics)) then ele else Nothing) (map Just (take (length (fst problem2)) [0..]))

step program = pivot_around program exiting_variable entering_variable
    where
        entering_variable = get_entering_variable (fst program)
        exiting_variable = get_exiting_variable program (entering_variable)
-}
