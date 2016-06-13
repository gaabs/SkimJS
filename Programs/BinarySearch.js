function binarySearch(arr,goal){
	var len = arr.length;
	var st = 0;
	var en = len-1;
	var mid;
	
	while (true){
		if (st > en) break;
		
		if ( (en+st)%2 == 0)
			mid = (en+st)/2;
		else 
			mid = (en+st-1)/2;
		
		if (arr[mid] < goal){
			st = mid+1;
		}else{
			en = mid-1;
		}
	}
	return mid; // returns len when not found
}

var found = binarySearch([1,2,3,4,4,4,5,6,10],4);
var notFound = binarySearch([1,2,3,4,4,4,5,6,10],7);