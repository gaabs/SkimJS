/*quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (pivo:s) = quicksort [x | x <- s , x < pivo] ++ [pivo] ++ quicksort [x | x <- s , x >= pivo]
*/

function menorQue(x,y){
	var n = (x.length);
	var resp = [];
	for(var i=1; i < n; i++ ){
		if((x[i])<y){
			resp = resp.concat([(x[i])]);
		}
	}
	return resp;
}

function maiorQue(x,y){
	var n = (x.length);
	var resp = [];
	for(var i=1; i < n; i++ ){
		if((x[i])>=y){
			resp = resp.concat([(x[i])]);
		}
	}
	return resp;
}

function quicksort(x){
	if(x.length <= 1){
		return x;
	}
	var pivo = x.head;
	var menor = quicksort(menorQue(x,pivo));
	var maior = quicksort(maiorQue(x,pivo));
	return menor.concat([pivo], maior);
}

var test = [3,2,1];
var arr = [5,2,3,5,1,1,3,2,9,94,2,0];

var t = quicksort(maiorQue([1,2], 1));
var t2 = quicksort(menorQue([1,2], 1));
var resp = t2.concat([1],t);
var resp2 = quicksort([1,2]);

var j = quicksort(arr);
j;
