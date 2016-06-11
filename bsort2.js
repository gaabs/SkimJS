var arr = [1,3,5-1,-1,0,4];
var n = arr.length;
var ini = arr;

function bsort(x){
	for(var i = n-1; i > 0; i--){
		for(var j = 0; j < i; j++){
			if (arr[j] > arr[j+1]){
				var aux = arr[j];
				arr[j] = arr[j+1];
				arr[j+1] = aux;
			}
		}
	}
}

bsort();

//for(var i = 0; i < n; i++) 