function reverse(arr){
	//len = length(arr);
	var len = arr.length;
	
	var reversed = [];
	
	var i = 0;
	while (i < len){
		reversed = [arr.head].concat(reversed);
		arr = arr.tail;
		i++;
	}
	arr = reversed;
	return arr;
}

arr = reverse([1,2,3]);
tail = arr.tail;
head = arr.head;
concat = tail.concat([head]);
len = arr.len;