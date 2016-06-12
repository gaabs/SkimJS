/* Simple Bubble Sort example
	O(n^2) sorting algorithm
*/

function bubblesort(arr){
	var n = arr.length;
	for(var i = n-1; i > 0; i--){
		for(var j = 0; j < i; j++){
			if (arr[j] > arr[j+1]){ // swap when find elements in wrong order
				var aux = arr[j];
				arr[j] = arr[i];
				arr[i] = aux;
			}
		}
	}
	return arr;
}

var unsorted = [1,3,5-1,-1,0,4];
sorted = bubblesort(unsorted);

wasSorted = unsorted == sorted;
sorted == [-1,0,1,3,4,4];