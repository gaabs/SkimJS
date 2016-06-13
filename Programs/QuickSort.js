/*quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (pivo:s) = quicksort [x | x <- s , x < pivo] ++ [pivo] ++ quicksort [x | x <- s , x >= pivo]
*/

function quicksort(data) {
    if (data.length == 0) return [];

    var left = [], right = [], pivot = data[0];

    for (var i = 1; i < data.length; i++) {
        if(data[i] < pivot)
            left = left.concat([data[i]])
        else
            right = right.concat([data[i]]);
    }

    return quicksort(left).concat([pivot], quicksort(right));
}

// var arr = [1,3,5,2,1,67];

var arr = [5,2,3,5,1,1,3,2,9,94,2,0];

var j = quicksort(arr);
//console.log(j);
j;
