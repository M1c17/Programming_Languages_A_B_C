val alternate_test1 = alternate([]) = 0;
val alternate_test2 = alternate([1,2,3,4]) = ~2;
val alternate_test3 = alternate([1,2,3,4,5]) = 3;
val alternate_test4 = alternate([1,2]) = ~ 1;


val min_max_test1 = min_max([1,2,3,4]) = (1,4);
val min_max_test2 = min_max([1,2,3,4,5]) = (1,5);
val min_max_test3 = min_max([5,2,1,0,1000,1]) = (0, 1000);
val min_max_test4 = min_max([8]) = (8,8);
val min_max_test5 = min_max([10,9,8,7,6,5,4,3,2,1,10,9,8,7,6,5,4,3,2,1,10,9,8,7,6,5,4,3,2,1,10,9,8,7,6,5,4,3,2,1,10,9,8,7,6,5,4,3,2,1,10,9,8,7,6,5,4,3,2,1,10,9,8,7,6,5,4,3,2,1,10,9,8,7,6,5,4,3,2,1,10,9,8,7,6,5,4,3,2,1,10,9,8,7,6,5,4,3,2,1,10,9,8,7,6,5,4,3,2,1,10,9,8,7,6,5,4,3,2,1,10,9,8,7,6,5,4,3,2,1,10,9,8,7,6,5,4,3,2,1]) = (1,10);


val cumsum_test1 = cumsum([]) = [];
val cumsum_test2 = cumsum([1,2,3,4,5]) = [1,3,6,10,15];
val cumsum_test3 = cumsum([5,2,1,0,1000,1]) = [5,7,8,8,1008,1009];
val cumsum_test4 = cumsum([8]) = [8];
val cumsum_test5 = cumsum([10,9,8,7,6,5,4,3,2,1]) = [10,19,27,34,40,45,49,52,54,55];


val greeting_test1 = greeting(SOME "naam1") = "Hello there, naam1!";
val greeting_test2 = greeting(SOME "naam2") = "Hello there, naam2!";
val greeting_test3 = greeting(SOME "naam3") = "Hello there, naam3!";
val greeting_test4 = greeting(NONE) = "Hello there, you!";


val repeat_test1 = repeat([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3];
val repeat_test2 = repeat([1,2,3,4,5], [1,2,3,4,5]) = [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5];
val repeat_test3 = repeat([5], [1]) = [5];
val repeat_test4 = repeat([8,2,1],[0,0,1]) = [1];
val repeat_test5 = repeat([10,9,8,7,6,5,4,3,2,1],[0,0,0,0,0,0,0,0,0,0]) = [];


val addOpt_test1 = addOpt(SOME 4, SOME 1) = SOME 5;
val addOpt_test2 = addOpt(SOME 0 ,NONE) = NONE;
val addOpt_test3 = addOpt(NONE, NONE) = NONE;


val addAllOpt_test1 = addAllOpt([SOME 1, SOME 2, NONE, SOME 100]) = SOME 103;
val addAllOpt_test2 = addAllOpt([]) = NONE;
val addAllOpt_test3 = addAllOpt([NONE, NONE]) = NONE;


val any_test1 = any([true,false, true, false]) = true;
val any_test2 = any([]) = false;
val any_test3 = any([false, false]) = false;


val all_test1 = all([true,false, true, false]) = false;
val all_test2 = all([]) = true;
val all_test3 = all([false, true]) = false;
val all_test4 = all([true, true]) = true;


val zip_test1 = zip([2,3], [3,4]) = [(2,3), (3,4)];
val zip_test2 = zip([], [1]) = [];
val zip_test3 = zip([3, 2], [2]) = [(3,2)];
val zip_test4 = zip([2,3,4],[5,6,7,8]) = [(2,5), (3,6), (4,7)];


val zipRecycle_test1 = zipRecycle([2,3], [3,4]) = [(2,3), (3,4)];
val zipRecycle_test2 = zipRecycle([2,3], [3,4,5]) = [(2,3), (3,4),(2,5)];
val zipRecycle_test3 = zipRecycle([1, 2,3], [1,2,3,4,5,6,7]) = [(1,1), (2,2), (3,3), (1,4), (2,5), (3,6), (1,7)];
val zipRecycle_test4 = zipRecycle([2,3,4],[5,6,7,8]) = [(2,5), (3,6), (4,7), (2,8)];


val zipOpt_test1 = zipOpt([2,3], [3,4]) = SOME  [(2,3), (3,4)];
val zipOpt_test2 = zipOpt([2,3], [3,4,5]) = NONE
val zipOpt_test3 = zipOpt([1, 2,3], [1,2,3,4,5,6,7]) = NONE
val zipOpt_test4 = zipOpt([2,3,4],[5,6,7]) = SOME [(2,5), (3,6), (4,7)];


val lookup_test1 = lookup([(1,"a"), (2,"b")] , "b") = SOME  2;
val lookup_test2 = lookup([] , "a") = NONE
val lookup_test3 = lookup([(1,"a"), (2,"b")] , "ab") = NONE;
val lookup_test4 = lookup([(1,"a"), (2,"b"), (3,"d"), (4,"e"), (5,"g")] , "g") = SOME  5;


val splitup_test1 = splitup([1,2,~6,3,~7,2,~9, 0]) =   ([1,2,3,2,0], [~6,~7, ~9]);
val splitup_test2 = splitup([1,2,3,2]) = ([1,2,3,2], []);
val splitup_test3 = splitup([~6,~7,~9]) = ([], [~6,~7,~9])
val splitup_test4 = splitup([]) = ([], [])  


val splitAt_test1 = splitAt([1,2,~6,3,~7,2,~9, 0], 0) =   ([1,2,3,2,0], [~6,~7, ~9]);
val splitAt_test2 = splitAt([1,2,3,2] ,2) = ([2,3,2], [1]);
val splitAt_test3 = splitAt([~6,~7,~9], ~7) = ([~6, ~7], [~9])
val splitAt_test4 = splitAt([], 100) = ([], [])  


val isSorted_test1 = isSorted([1,2,3,4,5,6,7, 8]) = true
val isSorted_test2 = isSorted([1,2,3,2]) = false
val isSorted_test3 = isSorted([~9,~8,~7]) = true
val isSorted_test4 = isSorted([]) = true  


val isSortedDecreasing_test1 = isSortedDecreasing(rev [1,2,3,4,5,6,7, 8]) = true
val isSortedDecreasing_test2 = isSortedDecreasing(rev [1,2,3,2]) = false
val isSortedDecreasing_test3 = isSortedDecreasing(rev [~9,~8,~7]) = true
val isSortedDecreasing_test4 = isSortedDecreasing(rev []) = true  


val isAnySorted_test1 = isAnySorted([1,2,3,4,5,6,7, 8]) = true
val isAnySorted_test1 = isAnySorted(rev [1,2,3,4,5,6,7, 8]) = true
val isAnySorted_test2 = isAnySorted([1,2,3,2]) = false
val isAnySorted_test3 = isAnySorted(rev [~9,~8,~7]) = true
val isAnySorted_test3 = isAnySorted([~9,~8,~7]) = true
val isAnySorted_test4 = isAnySorted(rev []) = true  


val sortedMerge_test1 = sortedMerge([1,2,3,4],[5,6,7,8]) = [1,2,3,4,5,6,7,8]
val sortedMerge_test2 = sortedMerge([1,2,3,4],[]) = [1,2,3,4]
val sortedMerge_test3 = sortedMerge([], [1,2,3]) = [1,2,3]
val sortedMerge_test4 = sortedMerge([~9,~8,~7], [1,2,3]) = [~9,~8,~7, 1,2,3]
val sortedMerge_test5 = sortedMerge([], []) = []


val qsort_test1 = qsort([1,4,3]) = [1, 3, 4]
val qsort_test2 = qsort([1,2,3,4]) = [1,2,3,4]
val qsort_test3 = qsort([3,2,1, 0]) = [0,1,2,3]
val qsort_test4 = qsort([1,5,4,2,3]) = [1,2,3,4,5]
val qsort_test5 = qsort([]) = []


val divide_test1 = divide([1,4,3,0,6,9,2,10,11,15,12,8,7]) = ([1,3,6,2,11,12,7], [4,0,9,10,15,8])
val divide_test2 = divide([1,2,3,4]) = ([1,3], [2,4])
val divide_test3 = divide([3]) = ([3],[])
val divide_test4 = divide([~9,8,~7]) = ([~9,~7],[8])
val divide_test5 = divide([]) = ([], [])


val not_so_quick_sort_test1 = not_so_quick_sort([1,4,3]) = [1, 3, 4]
val not_so_quick_sort_test2 = not_so_quick_sort([1,2,3,4]) = [1,2,3,4];
val not_so_quick_sort_test3 = not_so_quick_sort([3,2,1,0]) = [0,1,2,3]
val not_so_quick_sort_test4 = not_so_quick_sort([1,5,4,2,3]) = [1,2,3,4,5]
val not_so_quick_sort_test5 = not_so_quick_sort([]) = []


val fullDivide_test1 = fullDivide(2,40) = (3,5)
val fullDivide_test2 = fullDivide(3, 10) = (0,10);
val fullDivide_test3 = fullDivide(2, 32) = (5,1)
val fullDivide_test4 = fullDivide(3,18) = (2,2)
val fullDivide_test5 = fullDivide(3,19) = (0,19)


val factorize_test1 = factorize(20) = [(2,2), (5,1)]
val factorize_test2 = factorize(32) = [(2,5)];
val factorize_test3 = factorize(100) = [(2,2),(5,2)]
val factorize_test4 = factorize(36) = [(2,2), (3,2)]
val factorize_test5 = factorize(19) = [(19,1)]
val factorize_test6 = factorize(9) = [(3,2)]
val factorize_test7 = factorize(38) = [(2,1), (19,1)]


val multiply_test1 = multiply([(2,2), (5,1)]) =  20
val multiply_test2 = multiply([(2,5)]) =  32
val multiply_test3 = multiply([(2,2),(5,2)]) = 100
val multiply_test4 = multiply([(2,2), (3,2)]) =  36
val multiply_test5 = multiply([(19,1)]) =  19
val multiply_test6 = multiply([(3,2)]) =  9
val multiply_test7 = multiply([(2,1), (19,1)]) =  38


val all_products_test1 = all_products([(2,2), (5,1)]) =[1,2,4,5,10,20]	
val all_products_test2 = all_products([(2,5)]) =[1,2,4,8,16,32]	
val all_products_test3 = all_products([(2,2),(5,2)]) = [1,2,4,5,10, 20,25,50,100]
val all_products_test4 = all_products([(2,2), (3,2)]) =  [1,2,3,4,6,9,12,18,36]
val all_products_test5 = all_products([(19,1)]) =  [1,19]
val all_products_test6 = all_products([(3,2)]) =  [1,3,9]
val all_products_test7 = all_products([(2,1), (19,1)]) =  [1,2,19,38]