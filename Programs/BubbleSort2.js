var unsorted = [1,3,5-1,-1,0,4];

function swapPos(arr,i,j){
	var aux = arr[j];
	arr[j] = arr[i];
	arr[i] = aux;
	return arr;
}

function bsort(arr){
	var n = arr.length;
	for(var i = n-1; i > 0; i--){
		for(var j = 0; j < i; j++){
			if (arr[j] > arr[j+1]){
				arr = swapPos(arr,j,j+1);
			}
		}
	}
	return arr;
}

sorted = bsort(unsorted);

//console.log(sorted);
