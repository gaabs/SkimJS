function slowestFib(x){
	if (x <= 1) return 1;
	return slowestFib(x-1) + slowestFib(x-2);
}

function slowFib(n,x,y){
	if (n == 0) return x;
	var y2 = x+y;
	
	return slowFib(n-1,y,y2);
}

fib1 = slowestFib(5);
fib2 = slowFib(5,1,1);